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
        match std::os::unix::fs::symlink(etc.join(from), dir.join(name)) {
            Ok(()) => {}
            Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {}
            Err(e) => panic!("could not link {name} into {}: {e}", dir.display()),
        }
    }
    dir.to_string_lossy().into_owned()
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
