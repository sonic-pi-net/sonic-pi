// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// The Win32 side of the editor, as mac.mm is the AppKit side: the main thread, for what the instance says, and the
// plugin's own path.
//
// Keys: WebView2 is a window of its own (its own process draws it), and while the page has the focus Windows sends
// keys to it, not to the host's window, so there is nothing to route as mac.mm must. The page has every key; the
// host's own shortcuts work when the host has the focus. A host's offer of a key through VST3 (onKeyDown) is
// declined, so the host keeps any it is offered. Ctrl-= / Ctrl-- / Ctrl-0 are WebView2's own zoom.
//
// Full screen: the page in a window of the plugin's own over the whole monitor, as mac.mm does it and for its
// reason (a web view's own full screen stays in the host's floating plugin window). The web view is wry's window
// (WRY_WEBVIEW) with WebView2's inside it; it is given the full screen window for its parent, and the host's
// again on leaving (lib.rs sp_editor_place, which sizes WebView2 too: wry's window does not on its own).
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>

#include <mutex>
#include <string>

extern "C" void sp_host_note(const char* text);
extern "C" void sp_editor_full_screen(void* editor, int on);
// lib.rs: the editor's web view under `parent`, filling `width` by `height` of its pixels, with the keyboard if `focus`
extern "C" int sp_editor_place(void* editor, void* parent, unsigned width, unsigned height, int focus);

namespace {

struct Monitor {
    HWND host = nullptr;
    void* editor = nullptr;   // lib.rs's
    HWND full = nullptr;      // the page's window while it is full screen
};

constexpr wchar_t kFullClass[] = L"SonicPiPluginFullScreen";
HINSTANCE ownModule();
void setFullScreen(Monitor* monitor, bool on, bool tell);

// The whole of the monitor a window is on, taskbar and all.
RECT monitorOf(HWND window)
{
    MONITORINFO info = {};
    info.cbSize = sizeof info;
    GetMonitorInfoW(MonitorFromWindow(window, MONITOR_DEFAULTTONEAREST), &info);
    return info.rcMonitor;
}

// wry's window for the web view, in whichever window holds it.
HWND webIn(HWND parent)
{
    return FindWindowExW(parent, nullptr, L"WRY_WEBVIEW", nullptr);
}

LRESULT CALLBACK fullProc(HWND window, UINT message, WPARAM w, LPARAM l)
{
    auto* monitor = reinterpret_cast<Monitor*>(GetWindowLongPtrW(window, GWLP_USERDATA));
    const bool ours = monitor && monitor->full == window;   // not yet, as it is made; no longer, as it is put away
    switch (message) {
    // The keyboard is the page's whenever it is this window's: as it is shown, and on coming back to it (Alt-Tab).
    case WM_SETFOCUS:
        if (HWND web = webIn(window)) SetFocus(web);
        return 0;
    case WM_SIZE:
        if (ours && w != SIZE_MINIMIZED) sp_editor_place(monitor->editor, window, LOWORD(l), HIWORD(l), 0);
        return 0;
    // The monitor changed its size under the window: it still covers it.
    case WM_DISPLAYCHANGE:
        if (ours) {
            const RECT r = monitorOf(window);
            SetWindowPos(window, nullptr, r.left, r.top, r.right - r.left, r.bottom - r.top, SWP_NOZORDER | SWP_NOACTIVATE);
        }
        return 0;
    // Alt-F4 here closes full screen, not the page.
    case WM_CLOSE:
        if (ours) setFullScreen(monitor, false, true);
        return 0;
    // Destroyed by another hand than setFullScreen's: with its owner, the host's window, when a host destroys
    // that before it says the editor is removed. The page goes back to the host's view while that still stands
    // (this window's children have not yet gone), so that it is the editor that ends it (lib.rs sp_view_detach),
    // and nothing here is used again.
    case WM_DESTROY:
        if (ours) {
            monitor->full = nullptr;
            if (IsWindow(monitor->host)) {
                RECT r = {};
                GetClientRect(monitor->host, &r);
                sp_editor_place(monitor->editor, monitor->host, (unsigned) r.right, (unsigned) r.bottom, 0);
                sp_editor_full_screen(monitor->editor, 0);
            }
            sp_host_note("full screen off: its window was destroyed with the host's");
        }
        return 0;
    }
    return DefWindowProcW(window, message, w, l);
}

// The page into a window over the whole monitor, or back into the host's view. The host's window is left as it is,
// empty while the page is away. `tell`: the editor is told, and given the keyboard (not as it is being closed).
void setFullScreen(Monitor* monitor, bool on, bool tell)
{
    HWND host = monitor->host;
    if (on == (monitor->full != nullptr)) return;
    if (on) {
        if (!IsWindow(host) || !webIn(host)) return;
        HWND top = GetAncestor(host, GA_ROOT);
        // Made as the host's window is for DPI: a window's parent has to be, and the monitor's rectangle is then
        // in the pixels that window is placed in. A host that follows each monitor's scale gets that monitor's.
        const DPI_AWARENESS_CONTEXT before = SetThreadDpiAwarenessContext(GetWindowDpiAwarenessContext(host));
        const RECT r = monitorOf(top);
        // Owned by the host's window that holds the editor, and not topmost: above that window and the host's
        // main one, as an owned window is, while a dialog of the host's (its Save) still comes up over it, and
        // another application brought forward covers it. Borderless over the whole monitor, which is what has
        // the taskbar step back while this is the window in use. An owned window has no button of its own there.
        HWND window = CreateWindowExW(0, kFullClass, L"Sonic Pi", WS_POPUP | WS_CLIPCHILDREN, r.left, r.top,
                                      r.right - r.left, r.bottom - r.top, top, nullptr, ownModule(), nullptr);
        if (before) SetThreadDpiAwarenessContext(before);
        if (!window) { sp_host_note("full screen: no window could be made for the page"); return; }
        SetWindowLongPtrW(window, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(monitor));
        if (!sp_editor_place(monitor->editor, window, (unsigned) (r.right - r.left), (unsigned) (r.bottom - r.top), 0)) {
            DestroyWindow(window);   // not yet the monitor's, so the page is not looked for in it
            sp_host_note("full screen: the page could not be moved to its window");
            return;
        }
        monitor->full = window;
        ShowWindow(window, SW_SHOW);
        SetForegroundWindow(window);
        SetFocus(window);   // on to the page (fullProc), so typing works with no click
        sp_editor_place(monitor->editor, window, (unsigned) (r.right - r.left), (unsigned) (r.bottom - r.top), 1);
    } else {
        HWND window = monitor->full;
        monitor->full = nullptr;   // first: the window's own messages as it goes are then nothing to do with it
        const bool front = GetForegroundWindow() == window;
        if (IsWindow(host)) {
            RECT r = {};
            GetClientRect(host, &r);
            if (tell) {
                // The host's window has the keyboard again and the page in it, if this one had it: not taken
                // from another application that is in front.
                if (front) SetForegroundWindow(GetAncestor(host, GA_ROOT));
                sp_editor_place(monitor->editor, host, (unsigned) r.right, (unsigned) r.bottom, front ? 1 : 0);
            } else if (HWND web = webIn(window)) {
                SetParent(web, host);   // the editor is going: its web view is only kept from going with this window
            }
        }
        DestroyWindow(window);
    }
    sp_host_note(on ? "full screen: the page is in a window of the plugin's own" : "full screen off: the page is back in the host's view");
    if (tell) sp_editor_full_screen(monitor->editor, on);
}

// The main thread's mailbox: a message-only window, made on the main thread (a window's messages are its maker's
// thread's), while any editor is open. What the instance says is posted to it from the instance's threads.
constexpr UINT kWork = WM_APP + 1;
constexpr wchar_t kClass[] = L"SonicPiPluginMainThread";
std::mutex postLock;            // the window, against its going while a thread posts to it
HWND postWindow = nullptr;
int editorsOpen = 0;            // on the main thread

LRESULT CALLBACK postProc(HWND window, UINT message, WPARAM w, LPARAM l)
{
    if (message == kWork) {
        auto work = reinterpret_cast<void (*)(void*)>(w);
        work(reinterpret_cast<void*>(l));
        return 0;
    }
    return DefWindowProcW(window, message, w, l);
}

HINSTANCE ownModule()
{
    HMODULE module = nullptr;
    GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                       reinterpret_cast<LPCWSTR>(&postProc), &module);
    return module;
}

void openPostWindow()
{
    if (editorsOpen++ > 0) return;
    WNDCLASSEXW wc = {};
    wc.cbSize = sizeof wc;
    wc.lpfnWndProc = postProc;
    wc.hInstance = ownModule();
    wc.lpszClassName = kClass;
    RegisterClassExW(&wc);   // fails harmlessly when it is registered already
    // ... and the class of an editor's full screen window, kept for as long
    WNDCLASSEXW full = {};
    full.cbSize = sizeof full;
    full.lpfnWndProc = fullProc;
    full.hInstance = wc.hInstance;
    full.hCursor = LoadCursorW(nullptr, MAKEINTRESOURCEW(32512));   // IDC_ARROW, which is a narrow string without UNICODE
    full.hbrBackground = static_cast<HBRUSH>(GetStockObject(BLACK_BRUSH));
    full.lpszClassName = kFullClass;
    RegisterClassExW(&full);
    HWND window = CreateWindowExW(0, kClass, L"", 0, 0, 0, 0, 0, HWND_MESSAGE, nullptr, wc.hInstance, nullptr);
    if (!window) sp_host_note("no main thread window: the editor will not hear its instance");
    std::lock_guard<std::mutex> lk(postLock);
    postWindow = window;
}

// The last editor closed: the window goes, and its class, so nothing of this module is left in the host's
// windowing once the host unloads it. Work still queued for it is dropped, which only an open editor would need.
void closePostWindow()
{
    if (editorsOpen == 0 || --editorsOpen > 0) return;
    HWND window;
    {
        std::lock_guard<std::mutex> lk(postLock);
        window = postWindow;
        postWindow = nullptr;
    }
    if (window) DestroyWindow(window);
    UnregisterClassW(kClass, ownModule());
    UnregisterClassW(kFullClass, ownModule());   // every editor's full screen window went with its monitor
}

std::string utf8(const std::wstring& wide)
{
    if (wide.empty()) return {};
    const int n = WideCharToMultiByte(CP_UTF8, 0, wide.data(), (int) wide.size(), nullptr, 0, nullptr, nullptr);
    std::string out(n, '\0');
    WideCharToMultiByte(CP_UTF8, 0, wide.data(), (int) wide.size(), out.data(), n, nullptr, nullptr);
    return out;
}

} // namespace

extern "C" int sp_parent_ready(void* parent)
{
    return IsWindow(static_cast<HWND>(parent)) ? 1 : 0;
}

extern "C" void* sp_monitor_install(void* hostView, void* editor)
{
    openPostWindow();
    auto* monitor = new Monitor();
    monitor->host = static_cast<HWND>(hostView);
    monitor->editor = editor;
    return monitor;
}

// The chords the host keeps (the page's list): the host has them whenever it has the focus, which on Windows is
// the only time it sees a key at all.
extern "C" void sp_monitor_host_keys(void*, const char*) {}

// A host that offers keys through IPlugView acts on any we decline: all of them, as the page has its own already.
extern "C" int sp_monitor_take_key(void*, int) { return 0; }

// Full screen on or off, as the page asks (its button; and Escape that it left, lib.rs). Non-zero: it is on.
extern "C" int sp_monitor_full_screen(void* opaque, int on)
{
    auto* monitor = static_cast<Monitor*>(opaque);
    if (!monitor) return 0;
    setFullScreen(monitor, on != 0, true);
    return monitor->full != nullptr;
}

extern "C" int sp_monitor_is_full_screen(void* opaque)
{
    auto* monitor = static_cast<Monitor*>(opaque);
    return monitor && monitor->full != nullptr;
}

extern "C" void sp_monitor_remove(void* opaque)
{
    auto* monitor = static_cast<Monitor*>(opaque);
    if (!monitor) return;
    // The editor is closing while full screen (its window closed, its track deleted, the project shut): the
    // window goes, and the editor, which is told nothing, ends its web view as it always does.
    setFullScreen(monitor, false, false);
    delete monitor;
    closePostWindow();
}

// `work(ctx)` on the main thread, soon: what the instance says reaches the page there. With no editor open there
// is no page to reach, and it runs where it is (lib.rs's deliver finds no editor, and gives back its count).
extern "C" void sp_main_async(void* ctx, void (*work)(void*))
{
    {
        std::lock_guard<std::mutex> lk(postLock);
        if (postWindow && PostMessageW(postWindow, kWork, reinterpret_cast<WPARAM>(work), reinterpret_cast<LPARAM>(ctx)))
            return;
    }
    work(ctx);
}

extern "C" const char* sp_own_path()
{
    static std::string path = [] {
        std::wstring buffer(MAX_PATH, L'\0');
        for (;;) {
            const DWORD n = GetModuleFileNameW(ownModule(), buffer.data(), (DWORD) buffer.size());
            if (n == 0) return std::string();
            if (n < buffer.size()) { buffer.resize(n); break; }
            buffer.resize(buffer.size() * 2);
        }
        return utf8(buffer);
    }();
    return path.c_str();
}
