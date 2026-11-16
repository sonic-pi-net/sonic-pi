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
        let link = dir.join(name);
        if std::fs::symlink_metadata(&link).is_err() {
            std::os::unix::fs::symlink(etc.join(from), &link).unwrap();
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

