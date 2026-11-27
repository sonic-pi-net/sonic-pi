// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! Sonic Pi as a plugin (a VST3, and on macOS an Audio Unit): the editor, which is Sonic Pi's web GUI in a web
//! view inside the host's window, talking to its own instance (instance.rs).
#![cfg(any(target_os = "macos", windows))]

use std::borrow::Cow;
use std::cell::Cell;
use std::ffi::{CStr, CString, c_char, c_int, c_uint, c_void};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::{Component, Path, PathBuf};
use std::ptr::{self, NonNull};
use std::sync::atomic::{AtomicPtr, AtomicU32, Ordering};
use std::sync::{Arc, Mutex};

#[cfg(target_os = "macos")]
use raw_window_handle::AppKitWindowHandle;
#[cfg(windows)]
use raw_window_handle::Win32WindowHandle;
use raw_window_handle::{HandleError, HasWindowHandle, RawWindowHandle, WindowHandle};
use wry::http::{Request, Response};
use wry::{Rect, WebView, WebViewBuilder};
#[cfg(windows)]
use wry::{WebContext, WebViewBuilderExtWindows, WebViewExtWindows};

// The engine: its C++ archives (Clockwork, scsynth), which rustc links only for
// a crate named here, and its Rust half, which the C++ calls into.
use clockwork_native as _;
use clockwork_sys as _;

mod filelog;
pub mod instance;
pub mod licence;

/// The page's storage has the armed buffers under instance::ARMED_KEY, as `len` bytes of JSON at `json` (none: null
/// or 0): the instance takes them, in place of any it had. The plugin's shell calls this when it has an instance,
/// when a host hands it a project's state, and whenever the page writes the key, so the buffers follow the host's
/// transport with no editor open.
///
/// # Safety
/// `instance` is a live instance; `json` is `len` readable bytes, or null with `len` 0.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sp_armed_from_store(instance: *mut instance::RawInstance, json: *const u8, len: usize) {
    if instance.is_null() {
        return;
    }
    // SAFETY: `len` bytes, as the caller says.
    let text = if json.is_null() || len == 0 { "" } else { std::str::from_utf8(unsafe { std::slice::from_raw_parts(json, len) }).unwrap_or("") };
    // unlicensed, nothing is armed: the host's transport starts no code
    let armed = if licence::licensed() { instance::armed_from_json(text) } else { Vec::new() };
    instance::set_armed(instance, &armed);
}

use instance::RawInstance;

unsafe extern "C" {
    fn sp_get_plugin_factory() -> *mut c_void;
    #[cfg(target_os = "macos")]
    fn sp_au_factory(description: *const c_void) -> *mut c_void;
    fn sp_monitor_install(host_view: *mut c_void, editor: *mut c_void) -> *mut c_void;
    fn sp_monitor_host_keys(monitor: *mut c_void, chords: *const c_char);
    fn sp_monitor_take_key(monitor: *mut c_void, down: c_int) -> c_int;
    fn sp_monitor_remove(monitor: *mut c_void);
    fn sp_monitor_full_screen(monitor: *mut c_void, on: c_int) -> c_int;
    fn sp_monitor_is_full_screen(monitor: *mut c_void) -> c_int;
    fn sp_own_path() -> *const c_char;
    fn sp_main_async(ctx: *mut c_void, work: extern "C" fn(*mut c_void));
    fn sp_instance_run(instance: *mut RawInstance, code: *const c_char, group: c_int);
    fn sp_instance_stop(instance: *mut RawInstance);
    fn sp_instance_time_warp(instance: *mut RawInstance, ms: f64);
    fn sp_instance_stop_run(instance: *mut RawInstance, job: c_int, fade: f64);
    fn sp_instance_stop_group(instance: *mut RawInstance, group: c_int, fade: f64);
    fn sp_instance_group_under(instance: *mut RawInstance, group: c_int, parent: c_int);
    fn sp_instance_listen(instance: *mut RawInstance, listener: Option<instance::Listener>, ctx: *mut c_void);
    fn sp_instance_earlier(instance: *mut RawInstance);
    fn sp_instance_number(instance: *const RawInstance) -> c_uint;
    fn sp_instance_copy(
        instance: *mut RawInstance,
        parts: u32,
        wants: *const u32,
        count: u32,
        out: *mut c_void,
        bytes: u32,
    ) -> u32;
    // The page's storage, kept by the plugin with its state (plugin.cpp): `host` is the plugin the editor belongs to.
    fn sp_host_store_put(host: *mut c_void, key: *const u8, key_len: usize, value: *const u8, value_len: usize, remove: c_int);
    fn sp_host_store_clear(host: *mut c_void);
    fn sp_host_resize_start(host: *mut c_void);
    fn sp_host_resize_by(host: *mut c_void, dx: c_int, dy: c_int);
    fn sp_host_store_each(host: *mut c_void, each: unsafe extern "C" fn(*mut c_void, *const u8, usize, *const u8, usize), ctx: *mut c_void);
    // The page's zoom in percent, kept with the state as the editor's size is.
    fn sp_host_zoom(host: *mut c_void) -> c_int;
    fn sp_host_format(host: *mut c_void) -> *const c_char;
    fn sp_host_keep_zoom(host: *mut c_void, percent: c_int);
}

/// The interface's sizes, a browser's (WebKit's): Cmd-= and Cmd-- step through them, Cmd-0 is 100.
const ZOOMS: [c_int; 11] = [50, 75, 85, 100, 115, 125, 150, 175, 200, 250, 300];

/// The size `step` from `now`: 1 larger, -1 smaller, 0 as it was.
fn zoom_after(now: c_int, step: c_int) -> c_int {
    match step {
        0 => 100,
        s if s > 0 => ZOOMS.iter().copied().find(|&z| z > now).unwrap_or(ZOOMS[ZOOMS.len() - 1]),
        _ => ZOOMS.iter().rev().copied().find(|&z| z < now).unwrap_or(ZOOMS[0]),
    }
}

/// The plugin's format as the page names it ("VST", "AU"), from the shell that made the plugin (plugin.h).
fn format_name(plugin: *mut c_void) -> String {
    if plugin.is_null() {
        return "VST".into();
    }
    // SAFETY: a plugin's format is a literal's text, for as long as the binary is loaded.
    unsafe { CStr::from_ptr(sp_host_format(plugin)) }.to_string_lossy().into_owned()
}

/// The page's storage as the plugin keeps it, as a JSON object of strings: the editor's seed (bridge.js).
fn store_json(host: *mut c_void) -> String {
    unsafe extern "C" fn each(ctx: *mut c_void, key: *const u8, key_len: usize, value: *const u8, value_len: usize) {
        // SAFETY: the map below, and the plugin's bytes for the length of the call.
        let map = unsafe { &mut *ctx.cast::<serde_json::Map<String, serde_json::Value>>() };
        let text = |p: *const u8, n: usize| String::from_utf8_lossy(if n == 0 { &[] } else { unsafe { std::slice::from_raw_parts(p, n) } }).into_owned();
        map.insert(text(key, key_len), serde_json::Value::String(text(value, value_len)));
    }
    let mut map = serde_json::Map::new();
    if !host.is_null() {
        // SAFETY: a live plugin (it holds the editor), on the main thread.
        unsafe { sp_host_store_each(host, each, (&mut map as *mut serde_json::Map<String, serde_json::Value>).cast()) };
    }
    serde_json::Value::Object(map).to_string()
}

/// What the plugin tells the page itself, beside what the instance says (instance.h's kinds start at 1).
const SAYS_HELLO: u32 = 0;

static NEXT_ID: AtomicU32 = AtomicU32::new(1);

/// One open editor. Lives on the host's main thread.
struct Editor {
    id: u32,
    webview: WebView,
    instance: *mut RawInstance,
    plugin: *mut c_void,
    mailbox: Arc<Mailbox>,
    monitor: *mut c_void,
    zoom: Cell<c_int>,
    /// WebView2's own folder (web_context), kept for as long as the web view, which goes first.
    #[cfg(windows)]
    _context: WebContext,
}

/// What the instance has said and the page has not yet heard. Filled on the instance's threads; emptied on the
/// main thread, into the editor while it is open.
struct Mailbox {
    said: Mutex<Vec<(u32, Vec<u8>)>>,
    editor: AtomicPtr<Editor>,
}

/// The view the host handed us, as the window handle wry asks for.
struct HostView(NonNull<c_void>);

impl HasWindowHandle for HostView {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        #[cfg(target_os = "macos")]
        let raw = RawWindowHandle::AppKit(AppKitWindowHandle::new(self.0));
        // The host's HWND, which is never 0 (NonNull).
        #[cfg(windows)]
        let raw = RawWindowHandle::Win32(Win32WindowHandle::new(
            std::num::NonZeroIsize::new(self.0.as_ptr() as isize).ok_or(HandleError::Unavailable)?,
        ));
        // The host keeps the view alive between attached() and removed().
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

/// WebView2's folder (its cache, its settings): the user's, as a DAW's own folder may not be written to, which is
/// where WebView2 would otherwise put it.
#[cfg(windows)]
fn web_context() -> WebContext {
    let dir = std::env::var_os("LOCALAPPDATA").map(|d| PathBuf::from(d).join("Sonic Pi").join("Plugin").join("WebView2"));
    WebContext::new(dir)
}

/// A line for the plugin's file log (filelog.rs): editor `id`'s, or the plugin's own for 0.
fn log(id: u32, line: &str) {
    if id == 0 { filelog::instance_line(0, line) } else { filelog::editor_line(id, line) }
}

/// No panic may unwind into the host.
fn fence<T>(fallback: T, body: impl FnOnce() -> T) -> T {
    catch_unwind(AssertUnwindSafe(body)).unwrap_or_else(|_| {
        log(0, "panic stopped at the plugin boundary");
        fallback
    })
}

/// The editor's size as VST3 gives it: points on macOS, pixels on Windows (a host there sizes a view in its
/// window's pixels).
fn bounds(width: u32, height: u32) -> Rect {
    #[cfg(target_os = "macos")]
    use wry::dpi::{LogicalPosition as Position, LogicalSize as Size};
    #[cfg(windows)]
    use wry::dpi::{PhysicalPosition as Position, PhysicalSize as Size};
    Rect {
        position: Position::new(0, 0).into(),
        size: Size::new(width, height).into(),
    }
}

/// `<bundle>/Contents/Resources/page`, found from this binary's own path.
fn page_root() -> Option<PathBuf> {
    let own = unsafe { CStr::from_ptr(sp_own_path()) }.to_str().ok()?;
    Some(Path::new(own).parent()?.parent()?.join("Resources/page"))
}

fn mime(path: &Path) -> &'static str {
    match path.extension().and_then(|e| e.to_str()).unwrap_or("") {
        "html" => "text/html",
        "js" | "mjs" => "text/javascript",
        "css" => "text/css",
        "json" | "webmanifest" => "application/json",
        "wasm" => "application/wasm",
        "svg" => "image/svg+xml",
        "png" => "image/png",
        "jpg" | "jpeg" => "image/jpeg",
        "woff2" => "font/woff2",
        "woff" => "font/woff",
        "ttf" => "font/ttf",
        _ => "application/octet-stream",
    }
}

fn percent_decode(path: &str) -> Option<String> {
    let bytes = path.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' {
            out.push(u8::from_str_radix(path.get(i + 1..i + 3)?, 16).ok()?);
            i += 3;
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }
    String::from_utf8(out).ok()
}

/// A copy of the instance's engine, as the page's Clockwork asks for one (copy.js: `parts`, and `wants` three numbers
/// a window): `/__engine/copy?parts=3&wants=1,4,1200,2,0,2048`. None for a question that is not one, or no engine.
fn engine_copy(instance: *mut RawInstance, query: &str) -> Option<Vec<u8>> {
    if instance.is_null() {
        return None;
    }
    let mut parts = 0u32;
    let mut wants = Vec::new();
    for pair in query.split('&') {
        match pair.split_once('=') {
            Some(("parts", v)) => parts = v.parse().ok()?,
            Some(("wants", v)) if !v.is_empty() => {
                for w in v.split(',') {
                    wants.push(w.parse::<u32>().ok()?);
                }
            }
            _ => {}
        }
    }
    if wants.len() % 3 != 0 || wants.len() > 3 * 256 {
        return None;
    }
    let count = (wants.len() / 3) as u32;
    // SAFETY (both): a live instance, which the processor owns and which outlives this editor; on the main thread,
    // where the page's every request is answered, so one at a time.
    let size = unsafe { sp_instance_copy(instance, parts, wants.as_ptr(), count, ptr::null_mut(), 0) };
    if size == 0 {
        return None;
    }
    let mut out = vec![0u8; size as usize];
    let n = unsafe { sp_instance_copy(instance, parts, wants.as_ptr(), count, out.as_mut_ptr().cast(), size) };
    if n == 0 {
        return None;
    }
    out.truncate(n as usize);
    Some(out)
}

/// A file from the bundle, or a copy of the engine (engine_copy). Only plain relative paths are served.
fn serve(root: &Path, id: u32, instance: *mut RawInstance, request: Request<Vec<u8>>) -> Response<Cow<'static, [u8]>> {
    let asked = percent_decode(request.uri().path()).unwrap_or_default();
    let relative = match asked.trim_start_matches('/') {
        "" => "code.html",
        path => path,
    };
    let found = if relative == "__engine/copy" {
        engine_copy(instance, request.uri().query().unwrap_or(""))
    } else {
        let safe = Path::new(relative).components().all(|part| matches!(part, Component::Normal(_)));
        if safe { std::fs::read(root.join(relative)).ok() } else { None }
    };

    // Nothing outside the bundle: a page that reaches for the network is refused, and says so (bridge.js).
    // WebView2 has no schemes of an app's own: wry serves sonicpi://app/ from https://sonicpi.app/, which it
    // answers itself (nothing goes to the network), so that is the page's origin there.
    #[cfg(windows)]
    const NOTHING_OUTSIDE: &str = "default-src https://sonicpi.app blob: data: 'unsafe-inline' 'wasm-unsafe-eval'; \
        connect-src https://sonicpi.app blob: data:; worker-src https://sonicpi.app blob:";
    #[cfg(target_os = "macos")]
    const NOTHING_OUTSIDE: &str = "default-src sonicpi: blob: data: 'unsafe-inline' 'wasm-unsafe-eval'; \
        connect-src sonicpi: blob: data:; worker-src sonicpi: blob:";
    let response = Response::builder()
        .header("Cross-Origin-Opener-Policy", "same-origin")
        .header("Cross-Origin-Embedder-Policy", "require-corp")
        .header("Content-Security-Policy", NOTHING_OUTSIDE);
    match found {
        Some(bytes) => response
            .status(200)
            .header("Content-Type", mime(Path::new(relative)))
            .body(Cow::Owned(bytes))
            .unwrap(),
        None => {
            log(id, &format!("404 {asked}"));
            response.status(404).body(Cow::Owned(Vec::new())).unwrap()
        }
    }
}

fn base64(bytes: &[u8]) -> String {
    const ABC: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = String::with_capacity(bytes.len().div_ceil(3) * 4);
    for chunk in bytes.chunks(3) {
        let n = (u32::from(chunk[0]) << 16)
            | (u32::from(*chunk.get(1).unwrap_or(&0)) << 8)
            | u32::from(*chunk.get(2).unwrap_or(&0));
        for (k, shift) in [18, 12, 6, 0].into_iter().enumerate() {
            out.push(if k <= chunk.len() { ABC[(n >> shift) as usize & 63] as char } else { '=' });
        }
    }
    out
}

/// Keeps what is said for the page, and tells the main thread the first time since it last delivered.
fn post(mailbox: &Arc<Mailbox>, kind: u32, bytes: Vec<u8>) {
    let first = {
        let mut said = mailbox.said.lock().unwrap();
        said.push((kind, bytes));
        said.len() == 1
    };
    if first {
        let ctx = Arc::into_raw(mailbox.clone()).cast_mut().cast::<c_void>();
        // SAFETY: deliver takes the count back.
        unsafe { sp_main_async(ctx, deliver) };
    }
}

/// The instance said something, on one of its threads.
unsafe extern "C" fn heard(ctx: *mut c_void, kind: u32, bytes: *const u8, size: u32) {
    fence((), || {
        // SAFETY: the editor's mailbox, whose count the listening holds (attach); the instance stops calling
        // before detach gives it back.
        let mailbox = std::mem::ManuallyDrop::new(unsafe { Arc::from_raw(ctx.cast::<Mailbox>()) });
        // SAFETY: `size` bytes, for the call.
        let bytes = if size == 0 { Vec::new() } else { unsafe { std::slice::from_raw_parts(bytes, size as usize) }.to_vec() };
        post(&mailbox, kind, bytes);
    })
}

/// On the main thread: everything said since the last time, to the page, while the editor is open.
extern "C" fn deliver(ctx: *mut c_void) {
    fence((), || {
        // SAFETY: the count post() took.
        let mailbox = unsafe { Arc::from_raw(ctx.cast::<Mailbox>()) };
        let said = std::mem::take(&mut *mailbox.said.lock().unwrap());
        let editor = mailbox.editor.load(Ordering::Acquire);
        if editor.is_null() || said.is_empty() {
            return;
        }
        // SAFETY: set and cleared on this thread, while the editor lives.
        let editor = unsafe { &*editor };
        for (kind, bytes) in &said {
            if *kind == instance::SAYS_RUN && bytes.len() == 4 {
                log(editor.id, &format!("instance: run {} started", i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]])));
            }
        }
        let batch: Vec<String> = said.iter().map(|(k, b)| format!("[{k},\"{}\"]", base64(b))).collect();
        let _ = editor
            .webview
            .evaluate_script(&format!("window.__sonicPiPlugin&&__sonicPiPlugin.receive([{}])", batch.join(",")));
    })
}

/// What the page asked of its instance.
fn page_message(id: u32, instance: *mut RawInstance, host: *mut c_void, mailbox: &Arc<Mailbox>, body: &str) {
    let Ok(m) = serde_json::from_str::<serde_json::Value>(body) else {
        log(id, &format!("page sent what is not JSON: {body}"));
        return;
    };
    let int = |k: &str| m[k].as_i64().unwrap_or(0) as c_int;
    let num = |k: &str| m[k].as_f64().unwrap_or(0.0);
    match m["t"].as_str().unwrap_or("") {
        "hello" => {
            let hello = if instance.is_null() {
                r#"{"ready":false,"why":"the host has not started the plugin's audio yet"}"#
            } else {
                r#"{"ready":true}"#
            };
            post(mailbox, SAYS_HELLO, hello.as_bytes().to_vec());
        }
        "page-error" => log(id, &format!("page error: {}", m["text"].as_str().unwrap_or(""))),
        // the licence: how the plugin stands, asked for as the page opens; and a licence a person entered, with how
        // the plugin stands after it. The page's storage has the armed buffers, which a newly licensed plugin takes.
        "licence" => {
            let standing = licence::standing();
            if !standing.licensed() {
                let (found, behaviour) = licence::copied();
                log(id, &format!("unlicensed; the clipboard: {} (the system's word on reading it: {behaviour})", if found.is_some() { "holds a licence" } else { "no licence read" }));
            }
            post(mailbox, instance::SAYS_LICENCE, licence::said(&standing).into_bytes());
        }
        "licenceEnter" | "licenceUseCopied" => {
            let standing = if m["t"] == "licenceUseCopied" { licence::enter_copied() } else { licence::enter(m["text"].as_str().unwrap_or("")) };
            log(id, &format!("a licence was entered: {}", if standing.licensed() { "taken" } else { "refused" }));
            post(mailbox, instance::SAYS_LICENCE, licence::said(&standing).into_bytes());
        }
        // an editor that has just opened: what its instance said before it, its log's lines and the threads running
        "earlier" if !instance.is_null() => unsafe { sp_instance_earlier(instance) },
        // the keys the host keeps (shortcuts.js), which the page is never to see
        "hostKeys" => {
            let editor = mailbox.editor.load(Ordering::Acquire);
            let keys: Vec<&str> = m["keys"].as_array().map(|a| a.iter().filter_map(|k| k.as_str()).collect()).unwrap_or_default();
            if let (false, Ok(list)) = (editor.is_null(), CString::new(keys.join("\n"))) {
                // SAFETY: set and cleared on this thread, while the editor lives.
                unsafe { sp_monitor_host_keys((*editor).monitor, list.as_ptr()) };
            }
        }
        // the page full screen, in a window of the plugin's own, or back in the host's (mac.mm, win.cpp); the page
        // is told what came of it (sp_editor_full_screen)
        "fullScreen" => {
            let editor = mailbox.editor.load(Ordering::Acquire);
            if !editor.is_null() {
                // SAFETY: set and cleared on this thread, while the editor lives.
                unsafe { sp_monitor_full_screen((*editor).monitor, c_int::from(m["on"].as_bool() == Some(true))) };
            }
        }
        // the page's resize grip (plugin-grip.js): a drag begins, and how far the pointer has gone since
        // SAFETY (each): a live plugin, on the main thread
        "resizeStart" if !host.is_null() => unsafe { sp_host_resize_start(host) },
        "resizeBy" if !host.is_null() => unsafe { sp_host_resize_by(host, num("dx").round() as c_int, num("dy").round() as c_int) },
        // a write to the page's storage, kept by the plugin (bridge.js)
        "store" if !host.is_null() => {
            if m["clear"].as_bool() == Some(true) {
                // SAFETY (each): a live plugin, on the main thread
                unsafe { sp_host_store_clear(host) };
            } else if let Some(key) = m["key"].as_str() {
                let value = m["value"].as_str();
                let v = value.unwrap_or("");
                unsafe { sp_host_store_put(host, key.as_ptr(), key.len(), v.as_ptr(), v.len(), c_int::from(value.is_none())) };
            }
        }
        "clock" => {
            let ours = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map(|d| d.as_secs_f64()).unwrap_or(0.0) + 2_208_988_800.0;
            log(id, &format!("page clock {:+.3} s from the plugin's; the page's wall clock {:+.3} s", num("page") - ours, num("wall") - ours));
        }
        _ if instance.is_null() => log(id, &format!("page asked with no instance: {body}")),
        // SAFETY (each below): a live instance, which the processor owns and which outlives this editor.
        "run" if !licence::licensed() => {
            log(id, "page asked to run code: refused, the plugin is not licensed");
            post(mailbox, instance::SAYS_LICENCE, licence::said(&licence::standing()).into_bytes());
        }
        "run" => {
            log(id, &format!("page runs code in group {}", int("group")));
            let code = std::ffi::CString::new(m["code"].as_str().unwrap_or("").replace('\0', "")).unwrap_or_default();
            unsafe { sp_instance_run(instance, code.as_ptr(), int("group")) };
        }
        "stop" => {
            log(id, "page stops everything");
            unsafe { sp_instance_stop(instance) }
        }
        "stopRun" => {
            log(id, &format!("page stops run {} over {} s", int("job"), num("fade")));
            unsafe { sp_instance_stop_run(instance, int("job"), num("fade")) }
        }
        "stopGroup" => {
            log(id, &format!("page stops group {} over {} s", int("group"), num("fade")));
            unsafe { sp_instance_stop_group(instance, int("group"), num("fade")) }
        }
        "timeWarp" => {
            log(id, &format!("page sets the global time warp to {} ms", num("ms")));
            unsafe { sp_instance_time_warp(instance, num("ms")) }
        }
        "groupUnder" => unsafe { sp_instance_group_under(instance, int("group"), int("parent")) },
        // the test host's probe (probe.js) and anything else: kept in the log as it came
        _ => log(id, &format!("page {body}")),
    }
}

fn attach(parent: *mut c_void, width: u32, height: u32, instance: *mut RawInstance, plugin: *mut c_void) -> Result<*mut Editor, String> {
    let host = NonNull::new(parent).ok_or("the host gave no view")?;
    let root = page_root().ok_or("cannot find the plugin's own folder")?;
    let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
    let mailbox = Arc::new(Mailbox { said: Mutex::new(Vec::new()), editor: AtomicPtr::new(ptr::null_mut()) });

    let for_page = mailbox.clone();
    let instance_addr = instance as usize;
    let plugin_addr = plugin as usize;
    #[cfg(windows)]
    let mut context = web_context();
    #[cfg(target_os = "macos")]
    let builder = WebViewBuilder::new();
    // https: a secure context, for the page's SharedArrayBuffer (COOP and COEP, serve), as sonicpi: is on macOS
    #[cfg(windows)]
    let builder = WebViewBuilder::new_with_web_context(&mut context).with_https_scheme(true);
    let mut builder = builder
        .with_bounds(bounds(width, height))
        // wry's default activates the whole application, which here is the DAW.
        .with_focused(false)
        .with_accept_first_mouse(true)
        // the page's storage as the plugin keeps it, before the bridge that serves it to the page
        .with_initialization_script(&format!("window.__sonicPiPluginSeed = {};", store_json(plugin)))
        // The plugin's version from Cargo.toml, shown in the page's About card (bridge.js: __sonicPiPlugin.version)
        .with_initialization_script(concat!("window.__sonicPiPluginVersion = \"", env!("CARGO_PKG_VERSION"), "\";"))
        // The plugin's format, shown beside the versions (bridge.js: __sonicPiPlugin.format)
        .with_initialization_script(&format!("window.__sonicPiPluginFormat = {};", serde_json::to_string(&format_name(plugin)).unwrap_or_default()))
        // The host's MIDI port by its name, for the page's preferences and its completion of port: (bridge.js)
        .with_initialization_script(&format!("window.__sonicPiPluginMidiPort = {};", serde_json::to_string(instance::midi_port()).unwrap_or_default()))
        // Full screen is the plugin's own to do (mac.mm, win.cpp), not the web view's
        .with_initialization_script("window.__sonicPiPluginFullScreen = true;")
        .with_initialization_script(include_str!("bridge.js"));
    // Escape leaves full screen when the page left it (a handler that takes it stops it, or prevents its default):
    // WebView2 hands back no key the page left, as WebKit does (mac.mm), so the page says so itself. Asked once
    // every listener has had it; out of full screen the asking does nothing (win.cpp setFullScreen).
    #[cfg(windows)]
    {
        builder = builder.with_initialization_script(
            "if (window.top === window) addEventListener('keydown', (e) => { if (e.key !== 'Escape' || e.isComposing) return; \
             setTimeout(() => { if (!e.defaultPrevented) window.ipc.postMessage(JSON.stringify({ t: 'fullScreen', on: false })); }, 0); });",
        );
    }
    // The test host's view of the page (test/host.mm): what reaches it, in the log. Never in a host's process
    // unless asked for.
    if std::env::var_os("SONIC_PI_PLUGIN_PROBE").is_some() {
        builder = builder.with_initialization_script(include_str!("../test/probe.js"));
    }
    let webview = builder
        .with_custom_protocol("sonicpi".to_owned(), move |_, request| {
            fence(Response::builder().status(500).body(Cow::Owned(Vec::new())).unwrap(), || {
                serve(&root, id, instance_addr as *mut RawInstance, request)
            })
        })
        .with_ipc_handler(move |request: Request<String>| {
            fence((), || page_message(id, instance_addr as *mut RawInstance, plugin_addr as *mut c_void, &for_page, request.body()))
        })
        .with_url("sonicpi://app/code.html")
        .build_as_child(&HostView(host))
        .map_err(|error| error.to_string())?;

    // the size the interface was left at
    let zoom = if plugin.is_null() { 100 } else { unsafe { sp_host_zoom(plugin) } };
    if zoom != 100 {
        let _ = webview.zoom(f64::from(zoom) / 100.0);
    }
    let editor = Box::into_raw(Box::new(Editor {
        id,
        webview,
        instance,
        plugin,
        mailbox: mailbox.clone(),
        monitor: ptr::null_mut(),
        zoom: Cell::new(zoom),
        #[cfg(windows)]
        _context: context,
    }));
    // SAFETY: just made, on this thread; the monitor is removed (detach) before the editor is freed.
    unsafe { (*editor).monitor = sp_monitor_install(parent, editor.cast()) };
    mailbox.editor.store(editor, Ordering::Release);
    // SAFETY: a live instance, or none.
    filelog::editor_shows(id, if instance.is_null() { 0 } else { unsafe { sp_instance_number(instance) } });
    if !instance.is_null() {
        // The listening holds a count of the mailbox, given back at detach once the instance has stopped calling.
        let ctx = Arc::into_raw(mailbox).cast_mut().cast::<c_void>();
        unsafe { sp_instance_listen(instance, Some(heard), ctx) };
    }
    log(id, &format!("attached {width}x{height}{}", if instance.is_null() { ", no instance yet" } else { "" }));
    Ok(editor)
}

#[unsafe(no_mangle)]
pub extern "C" fn sp_view_attach(
    parent: *mut c_void,
    width: c_uint,
    height: c_uint,
    instance: *mut RawInstance,
    plugin: *mut c_void,
) -> *mut c_void {
    fence(ptr::null_mut(), || match attach(parent, width, height, instance, plugin) {
        Ok(editor) => editor.cast(),
        Err(message) => {
            log(0, &format!("attach failed: {message}"));
            ptr::null_mut()
        }
    })
}

/// The plugin took a state from the host (a project, a preset) with this editor open: the page starts again on it.
#[unsafe(no_mangle)]
pub extern "C" fn sp_view_store_replaced(view: *mut c_void) {
    fence((), || {
        // SAFETY: an editor attach made and detach has not yet freed, on the main thread.
        let editor = unsafe { &*view.cast::<Editor>() };
        let script = format!("window.__sonicPiPlugin&&__sonicPiPlugin.replaceStore({})", store_json(editor.plugin));
        if let Err(error) = editor.webview.evaluate_script(&script) {
            log(editor.id, &format!("the page could not take the new state: {error}"));
        }
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn sp_view_resize(view: *mut c_void, width: c_uint, height: c_uint) {
    fence((), || {
        let editor = unsafe { &*view.cast::<Editor>() };
        // full screen, the page is not in the host's view: it takes that view's size when it comes back (mac.mm, win.cpp)
        if unsafe { sp_monitor_is_full_screen(editor.monitor) } != 0 {
            return;
        }
        if let Err(error) = editor.webview.set_bounds(bounds(width, height)) {
            log(editor.id, &format!("resize failed: {error}"));
        }
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn sp_view_detach(view: *mut c_void) {
    fence((), || {
        let editor = unsafe { Box::from_raw(view.cast::<Editor>()) };
        // Nothing more is said to this editor: the instance stops calling (and has finished any call under way),
        // its count of the mailbox comes back, and the main thread's next delivery finds no editor.
        if !editor.instance.is_null() {
            unsafe {
                sp_instance_listen(editor.instance, None, ptr::null_mut());
                drop(Arc::from_raw(Arc::as_ptr(&editor.mailbox)));
            }
        }
        editor.mailbox.editor.store(ptr::null_mut(), Ordering::Release);
        // The monitor holds the view, so it goes first.
        unsafe { sp_monitor_remove(editor.monitor) };
        log(editor.id, "detached");
        filelog::editor_closed(editor.id);
    })
}

/// A key the host offered through VST3's own entry point. Non-zero: ours.
#[unsafe(no_mangle)]
pub extern "C" fn sp_host_key(view: *mut c_void, down: c_int, _key: c_int, _code: c_int, _mods: c_int) -> c_int {
    fence(0, || {
        if view.is_null() {
            return 0;
        }
        // Delivering a key can close the editor, so nothing of it is used afterwards.
        let monitor = unsafe { &*view.cast::<Editor>() }.monitor;
        unsafe { sp_monitor_take_key(monitor, down) }
    })
}

/// Cmd-=, Cmd-- or Cmd-0 that the page left, as a browser's zoom (mac.mm): the interface larger, smaller or as it was.
#[unsafe(no_mangle)]
pub extern "C" fn sp_editor_zoom(view: *mut c_void, step: c_int) {
    fence((), || {
        if view.is_null() {
            return;
        }
        // SAFETY: the monitor's editor, which removes the monitor before it is freed, on the main thread.
        let editor = unsafe { &*view.cast::<Editor>() };
        let zoom = zoom_after(editor.zoom.get(), step);
        editor.zoom.set(zoom);
        if let Err(error) = editor.webview.zoom(f64::from(zoom) / 100.0) {
            log(editor.id, &format!("zoom failed: {error}"));
        }
        if !editor.plugin.is_null() {
            // SAFETY: a live plugin (it holds the editor), on the main thread.
            unsafe { sp_host_keep_zoom(editor.plugin, zoom) };
        }
    })
}

/// The web view into another window, or back into the host's (win.cpp, for full screen): under `parent`, filling
/// `width` by `height` of its pixels, and with the keyboard if `focus`. Non-zero: it is there.
#[cfg(windows)]
#[unsafe(no_mangle)]
pub extern "C" fn sp_editor_place(view: *mut c_void, parent: *mut c_void, width: c_uint, height: c_uint, focus: c_int) -> c_int {
    fence(0, || {
        if view.is_null() || parent.is_null() {
            return 0;
        }
        // SAFETY: the monitor's editor, which removes the monitor before it is freed, on the main thread.
        let editor = unsafe { &*view.cast::<Editor>() };
        if let Err(error) = editor.webview.reparent(parent as isize) {
            log(editor.id, &format!("the page could not be moved to another window: {error}"));
            return 0;
        }
        // WebView2 is sized with wry's window, which does not size it of its own accord.
        if let Err(error) = editor.webview.set_bounds(bounds(width, height)) {
            log(editor.id, &format!("resize failed: {error}"));
        }
        if focus != 0 {
            if let Err(error) = editor.webview.focus() {
                log(editor.id, &format!("the page could not be given the keyboard: {error}"));
            }
        }
        1
    })
}

/// The page went full screen or came back (mac.mm, win.cpp), by its own asking or by Escape: it is told, for its
/// button.
#[unsafe(no_mangle)]
pub extern "C" fn sp_editor_full_screen(view: *mut c_void, on: c_int) {
    fence((), || {
        if view.is_null() {
            return;
        }
        // SAFETY: the monitor's editor, which removes the monitor before it is freed, on the main thread.
        let editor = unsafe { &*view.cast::<Editor>() };
        let script = format!("window.__sonicPiPlugin&&__sonicPiPlugin.fullScreenChanged&&__sonicPiPlugin.fullScreenChanged({})", on != 0);
        if let Err(error) = editor.webview.evaluate_script(&script) {
            log(editor.id, &format!("the page could not be told of full screen: {error}"));
        }
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn sp_host_note(text: *const c_char) {
    fence((), || log(0, &unsafe { CStr::from_ptr(text) }.to_string_lossy()))
}

#[unsafe(no_mangle)]
#[allow(non_snake_case)]
pub extern "C" fn GetPluginFactory() -> *mut c_void {
    unsafe { sp_get_plugin_factory() }
}

/// The Audio Unit's factory, which its bundle's Info.plist names (au/shell.mm): the same binary is the VST3 and the
/// Audio Unit, each in a bundle of its own.
///
/// # Safety
/// Called by the system's Audio Unit host code, with the component's description or null.
#[cfg(target_os = "macos")]
#[unsafe(no_mangle)]
#[allow(non_snake_case)]
pub unsafe extern "C" fn SonicPiAUFactory(description: *const c_void) -> *mut c_void {
    // SAFETY: the factory reads nothing of the description.
    unsafe { sp_au_factory(description) }
}

#[unsafe(no_mangle)]
#[allow(non_snake_case)]
pub extern "C" fn bundleEntry(_bundle: *mut c_void) -> bool {
    true
}

#[unsafe(no_mangle)]
#[allow(non_snake_case)]
pub extern "C" fn bundleExit() -> bool {
    true
}

#[cfg(test)]
mod tests {
    use super::zoom_after;

    #[test]
    fn the_interface_steps_through_a_browsers_sizes() {
        assert_eq!(zoom_after(100, 1), 115);
        assert_eq!(zoom_after(100, -1), 85);
        assert_eq!(zoom_after(175, 0), 100);
        // at either end it stays
        assert_eq!(zoom_after(300, 1), 300);
        assert_eq!(zoom_after(50, -1), 50);
        // a size between two steps goes to the next one
        assert_eq!(zoom_after(110, 1), 115);
        assert_eq!(zoom_after(110, -1), 100);
    }
}
