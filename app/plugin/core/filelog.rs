// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron

//! The plugin's file log: one file a host session in `~/Library/Logs/Sonic Pi/`, the last ten kept, as native keeps
//! its own in `~/.sonic-pi/log`. Each line says how long the session has run, in ms, and whose it is: `[2]` instance
//! 2's (its engine, its runtime, its own), `[2/5]` that of the editor (the fifth this session) showing instance 2,
//! `[0]` the plugin's. An instance writes whether an editor is open or not. Only a plugin a host has loaded writes
//! there (the VST3 shell says so, sp_log_enable): a test's instances write nowhere, unless `SONIC_PI_PLUGIN_LOG`
//! names one file to append to instead (the test host's).

use std::collections::HashMap;
use std::ffi::{CStr, c_char, c_uint};
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, OnceLock};
use std::time::{Instant, SystemTime, UNIX_EPOCH};

const KEEP: usize = 10;
const PREFIX: &str = "plugin ";

static START: OnceLock<Instant> = OnceLock::new();
static FILE: OnceLock<Mutex<Option<File>>> = OnceLock::new();
static EDITORS: OnceLock<Mutex<HashMap<u32, u32>>> = OnceLock::new();
static ENABLED: AtomicBool = AtomicBool::new(false);

/// For the VST3 shell, as a host loads the plugin: from now on this process's lines go to the user's log folder.
#[unsafe(no_mangle)]
pub extern "C" fn sp_log_enable() {
    ENABLED.store(true, Ordering::Release);
}

/// A line of instance `instance`'s (0: the plugin's own).
pub fn instance_line(instance: u32, text: &str) {
    write(&format!("[{instance}]"), text);
}

/// A line of editor `editor`'s, tagged with the instance it shows (editor_shows).
pub fn editor_line(editor: u32, text: &str) {
    let instance = EDITORS.get_or_init(Default::default).lock().map(|m| m.get(&editor).copied()).ok().flatten();
    match instance {
        Some(n) => write(&format!("[{n}/{editor}]"), text),
        None => write(&format!("[{editor}]"), text),
    }
}

/// Editor `editor` shows instance `instance`, until `editor_closed`.
pub fn editor_shows(editor: u32, instance: u32) {
    if let Ok(mut m) = EDITORS.get_or_init(Default::default).lock() {
        m.insert(editor, instance);
    }
}

pub fn editor_closed(editor: u32) {
    if let Ok(mut m) = EDITORS.get_or_init(Default::default).lock() {
        m.remove(&editor);
    }
}

fn write(tag: &str, text: &str) {
    let ms = START.get_or_init(Instant::now).elapsed().as_millis();
    let file = FILE.get_or_init(|| Mutex::new(open()));
    if let Ok(mut file) = file.lock() {
        if let Some(f) = file.as_mut() {
            let _ = writeln!(f, "{ms} {tag} {text}");
        }
    }
}

fn open() -> Option<File> {
    if let Some(path) = std::env::var_os("SONIC_PI_PLUGIN_LOG") {
        return OpenOptions::new().create(true).append(true).open(path).ok();
    }
    if !ENABLED.load(Ordering::Acquire) {
        return None;
    }
    let dir = PathBuf::from(std::env::var_os("HOME")?).join("Library/Logs/Sonic Pi");
    std::fs::create_dir_all(&dir).ok()?;
    prune(&dir);
    let secs = SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_secs()).unwrap_or(0);
    let host = std::env::current_exe()
        .ok()
        .and_then(|p| p.file_stem().map(|s| s.to_string_lossy().into_owned()))
        .unwrap_or_else(|| "a host".to_owned());
    let pid = std::process::id();
    let mut f = OpenOptions::new().create(true).append(true).open(dir.join(session_name(secs, &host, pid))).ok()?;
    let _ = writeln!(f, "Sonic Pi plugin {} in {host} (pid {pid}), from {}", env!("CARGO_PKG_VERSION"), utc(secs));
    Some(f)
}

/// The older sessions' files, leaving room for this one among the KEEP.
fn prune(dir: &Path) {
    let Ok(entries) = std::fs::read_dir(dir) else { return };
    let names: Vec<String> = entries.filter_map(|e| e.ok()?.file_name().into_string().ok()).collect();
    for name in to_remove(names, KEEP - 1) {
        let _ = std::fs::remove_file(dir.join(name));
    }
}

/// Of the session files among `names`, all but the newest `keep` (their names sort by when they began).
fn to_remove(names: Vec<String>, keep: usize) -> Vec<String> {
    let mut ours: Vec<String> = names.into_iter().filter(|n| n.starts_with(PREFIX) && n.ends_with(".log")).collect();
    ours.sort();
    let extra = ours.len().saturating_sub(keep);
    ours.truncate(extra);
    ours
}

/// `plugin 2026-11-10 12.03.04Z REAPER 4242.log`: when the session began (UTC), in which host, which process.
fn session_name(secs: u64, host: &str, pid: u32) -> String {
    let host: String = host.chars().map(|c| if c.is_alphanumeric() || c == ' ' || c == '-' { c } else { '_' }).collect();
    format!("{PREFIX}{} {host} {pid}.log", utc(secs).replace(':', "."))
}

/// `2026-11-10 12:03:04Z`, from seconds since 1970 (the civil calendar's days, Howard Hinnant's algorithm).
fn utc(secs: u64) -> String {
    let days = (secs / 86_400) as i64;
    let rest = secs % 86_400;
    let z = days + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = yoe + era * 400 + i64::from(m <= 2);
    format!("{y:04}-{m:02}-{d:02} {:02}:{:02}:{:02}Z", rest / 3600, rest % 3600 / 60, rest % 60)
}

/// For an instance (instance.cpp): a line of its own, in the file whether an editor is open or not.
#[unsafe(no_mangle)]
pub extern "C" fn sp_log_line(instance: c_uint, text: *const c_char) {
    if text.is_null() {
        return;
    }
    // SAFETY: a NUL-terminated string, for the call.
    let text = unsafe { CStr::from_ptr(text) }.to_string_lossy();
    instance_line(instance, &text);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_session_is_named_for_when_it_began_its_host_and_process() {
        assert_eq!(session_name(1_793_000_000, "REAPER", 4242), "plugin 2026-10-26 07.33.20Z REAPER 4242.log");
        assert_eq!(utc(0), "1970-01-01 00:00:00Z");
        assert_eq!(utc(951_782_400), "2000-02-29 00:00:00Z", "a leap day");
        assert_eq!(session_name(0, "Live 12/Suite", 1), "plugin 1970-01-01 00.00.00Z Live 12_Suite 1.log", "no path in a host's name");
    }

    #[test]
    fn the_newest_sessions_are_kept_and_nothing_else_is_touched() {
        let mut names: Vec<String> = (0..12).map(|i| session_name(1_793_000_000 + i * 60, "REAPER", 1)).collect();
        names.push("SonicPiPlugin.log".to_owned());
        names.push("plugin notes.txt".to_owned());
        names.reverse();
        let gone = to_remove(names, KEEP - 1);
        assert_eq!(gone.len(), 3, "twelve sessions, room for nine and the new one");
        assert_eq!(gone[0], session_name(1_793_000_000, "REAPER", 1), "the oldest go");
        assert!(gone.iter().all(|n| n.starts_with(PREFIX) && n.ends_with(".log")));
        assert!(to_remove(vec!["plugin a.log".into()], KEEP - 1).is_empty());
    }
}
