// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! Offline instances: time is counted from rendered frames (Instance::offline), so these tests render in a plain
//! loop as fast as the machine goes and use no clock. The same code must give the same audio on every run.

use sonic_pi_plugin::instance::Instance;

mod common;
use common::{onsets, resources, BLOCK, RATE};

/// NTP seconds. Arbitrary, and nowhere near the real time, so a clock read by mistake shows up.
const ORIGIN: f64 = 1000.0;

/// A short beep with an instant attack, so its first loud frame marks when it was scheduled.
const BEEP: &str = "synth :beep, note: 84, attack: 0, sustain: 0, release: 0.05, amp: 0.8";

fn offline() -> Instance {
    Instance::offline(RATE, BLOCK as u32, &resources(), ORIGIN).expect("an offline instance")
}

/// Renders `blocks` buffers and returns the left channel.
fn render(instance: &Instance, blocks: usize) -> Vec<f32> {
    let (mut left, mut right) = (vec![0.0f32; BLOCK], vec![0.0f32; BLOCK]);
    let mut out = Vec::with_capacity(blocks * BLOCK);
    for _ in 0..blocks {
        instance.render(&mut left, &mut right);
        out.extend_from_slice(&left);
    }
    out
}

/// Renders until the instance has nothing left to load. Loading is file IO on other threads, so this is the one
/// place that yields to them; it is bounded by a count of buffers, not by time.
fn render_until_loaded(instance: &Instance) {
    for _ in 0..200_000 {
        render(instance, 1);
        if instance.loading() == 0 {
            return;
        }
        std::thread::yield_now();
    }
    panic!("the instance was still loading after 200,000 buffers");
}

fn seconds(s: f64) -> usize {
    (s * RATE / BLOCK as f64).ceil() as usize
}

#[test]
fn three_beeps_a_beat_apart_start_exactly_48000_frames_apart() {
    let instance = offline();
    render_until_loaded(&instance);
    instance.run(&format!("3.times do\n  {BEEP}\n  sleep 1\nend\n"));
    let starts = onsets(&render(&instance, seconds(3.5)));
    assert_eq!(starts.len(), 3, "three beeps: {starts:?}");
    assert_eq!(starts[1] - starts[0], 48_000, "{starts:?}");
    assert_eq!(starts[2] - starts[1], 48_000, "{starts:?}");
}

#[test]
fn the_same_code_renders_bit_identical_audio_twice() {
    let play = || {
        let instance = offline();
        render_until_loaded(&instance);
        instance.run(&format!("use_bpm 90\n4.times do |i|\n  synth :beep, note: 60 + i * 7, release: 0.3\n  sleep 0.5\nend\n"));
        render(&instance, seconds(2.0))
    };
    let (first, second) = (play(), play());
    assert!(first.iter().any(|s| s.abs() > 0.01), "it sounds");
    assert!(first == second, "two renders of the same code differ");
}

#[test]
fn nothing_sounds_until_code_runs_and_rendering_does_not_wait_for_real_time() {
    let instance = offline();
    render_until_loaded(&instance);
    // Ten minutes of audio. Paced by a clock this would take ten minutes.
    let silent = render(&instance, seconds(600.0));
    assert!(silent.iter().all(|s| *s == 0.0), "an idle instance is silent");
    instance.run(&format!("{BEEP}\n"));
    assert_eq!(onsets(&render(&instance, seconds(1.0))).len(), 1);
}
