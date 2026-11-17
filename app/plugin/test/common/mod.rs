// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! Shared by the plugin's test files: where Sonic Pi's files are, the render geometry, and measurements on
//! rendered audio.
#![allow(dead_code)]

use std::path::PathBuf;

pub const RATE: f64 = 48_000.0;
pub const BLOCK: usize = 512;

/// Sonic Pi's files as an instance finds them in a bundle: synthdefs/, samples/ and buffers/, here links into the
/// checkout's etc/.
pub fn resources() -> String {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let etc = std::env::var("SONIC_PI_ETC")
        .map(PathBuf::from)
        .unwrap_or_else(|_| manifest.join("../../etc"));
    let dir = manifest.join("target/test-resources");
    std::fs::create_dir_all(&dir).unwrap();
    for (name, from) in [("synthdefs", "synthdefs/compiled"), ("samples", "samples"), ("buffers", "buffers")] {
        // Tests run on several threads and each calls this, so another may make the link first: that is fine.
        match link_dir(&etc.join(from), &dir.join(name)) {
            Ok(()) => {}
            Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {}
            Err(e) => panic!("could not link {name} into {}: {e}", dir.display()),
        }
    }
    dir.to_string_lossy().into_owned()
}

#[cfg(unix)]
fn link_dir(target: &std::path::Path, link: &std::path::Path) -> std::io::Result<()> {
    std::os::unix::fs::symlink(target, link)
}

/// A junction: Windows' link to a folder, which, unlike a symbolic link, anyone may make.
#[cfg(windows)]
fn link_dir(target: &std::path::Path, link: &std::path::Path) -> std::io::Result<()> {
    if link.exists() {
        return Err(std::io::ErrorKind::AlreadyExists.into());
    }
    // backslashes throughout: cmd takes a / in a path (target/test-resources) for one of its switches
    let windows = |p: &std::path::Path| p.to_string_lossy().replace('/', "\\");
    let made = std::process::Command::new("cmd")
        .args(["/C", "mklink", "/J"])
        .arg(windows(link))
        .arg(windows(target))
        .output()?;
    if made.status.success() || link.exists() {
        Ok(())
    } else {
        Err(std::io::Error::other(String::from_utf8_lossy(&made.stderr).into_owned()))
    }
}

pub fn peak(samples: &[f32]) -> f32 {
    samples.iter().fold(0.0f32, |m, s| m.max(s.abs()))
}


/// Where each sound starts in `samples`: the first frame over a fiftieth of full scale after a tenth of a second
/// under it.
pub fn onsets(samples: &[f32]) -> Vec<usize> {
    let quiet_for = (0.1 * RATE) as usize;
    let mut found = Vec::new();
    let mut quiet = quiet_for;
    for (i, s) in samples.iter().enumerate() {
        if s.abs() > 0.02 {
            if quiet >= quiet_for {
                found.push(i);
            }
            quiet = 0;
        } else {
            quiet += 1;
        }
    }
    found
}


/// The pitch of a steady tone, by its rising zero crossings.
pub fn frequency(samples: &[f32]) -> f64 {
    let rising = samples.windows(2).filter(|w| w[0] <= 0.0 && w[1] > 0.0).count();
    rising as f64 * RATE / samples.len() as f64
}


/// A folder of the player's own samples: two of the built-in ones under names of their own. Tests on several
/// threads call this, so each file is copied under a name of the caller's own and then renamed into place, which
/// either happens whole or not at all.
pub fn own_samples() -> PathBuf {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let etc = std::env::var("SONIC_PI_ETC")
        .map(PathBuf::from)
        .unwrap_or_else(|_| manifest.join("../../etc"));
    let dir = manifest.join("target/test-own-samples");
    std::fs::create_dir_all(&dir).unwrap();
    for (from, to) in [("bd_haus.flac", "kick.flac"), ("sn_dolf.flac", "snare.flac")] {
        let to = dir.join(to);
        if !to.exists() {
            let part = dir.join(format!("{}.{:?}.part", from, std::thread::current().id()));
            std::fs::copy(etc.join("samples").join(from), &part).unwrap();
            std::fs::rename(&part, &to).unwrap();
        }
    }
    dir
}

/// What an instance has said to its listener (Instance::listen): each message's kind and bytes, in order.
pub struct Heard(pub std::sync::Mutex<Vec<(u32, Vec<u8>)>>);

/// The listener that fills a Heard.
pub unsafe extern "C" fn keep(ctx: *mut std::ffi::c_void, kind: u32, bytes: *const u8, size: u32) {
    // SAFETY: the Heard the test handed over, alive until it stops listening; `size` bytes for the call.
    let heard = unsafe { &*ctx.cast::<Heard>() };
    let bytes = if size == 0 { Vec::new() } else { unsafe { std::slice::from_raw_parts(bytes, size as usize) }.to_vec() };
    // what these tests look in: a numbered message without its number, and every line for the System log as one kind
    use sonic_pi_plugin::instance::{NUMBERED, SAYS_ENGINE, SAYS_LOADS, SAYS_LOG, SAYS_RUNTIME};
    let bytes = if NUMBERED.contains(&kind) { bytes[8..].to_vec() } else { bytes };
    let kind = if [SAYS_ENGINE, SAYS_RUNTIME, SAYS_LOADS].contains(&kind) { SAYS_LOG } else { kind };
    heard.0.lock().unwrap().push((kind, bytes));
}


pub fn logged(heard: &Heard, needle: &str) -> Option<String> {
    heard.0.lock().unwrap().iter().find_map(|(kind, b)| {
        let text = String::from_utf8_lossy(b);
        (*kind == sonic_pi_plugin::instance::SAYS_LOG && text.contains(needle)).then(|| text.into_owned())
    })
}
