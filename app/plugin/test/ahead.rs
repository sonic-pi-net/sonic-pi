// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! A measurement, not a test: how short the schedule-ahead time can be before sounds reach the engine late. An
//! instance is rendered at a host's pace on a thread (no audio device), a program runs with each schedule-ahead
//! time in turn, and the engine's own count of late bundles is read from its metrics.
//! `cargo test --release --test ahead -- --ignored --nocapture`

use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};

use sonic_pi_plugin::instance::Instance;

mod common;
use common::{resources, RATE};

const LIGHT: &str = "live_loop :a do\n  synth :beep, note: 72, release: 0.05, amp: 0.1\n  sleep 0.125\nend\n";

/// Eight loops of sixteenth notes at 240 a minute under a reverb each, with samples, and one that works a sum out
/// on every pass: about 150 sounds a second.
const BUSY: &str = "8.times do |i|\n  live_loop \"l#{i}\".to_sym do\n    with_fx :reverb, mix: 0.2 do\n      16.times do\n        synth [:beep, :saw, :tb303, :prophet].choose, note: scale(:e3, :minor_pentatonic).choose, release: 0.1, amp: 0.05\n        sample :bd_haus, amp: 0.05 if one_in(4)\n        sleep 0.0625\n      end\n    end\n  end\nend\nlive_loop :sums do\n  (1..3000).map { |x| Math.sin(x) }.sum\n  synth :beep, note: 60, release: 0.05, amp: 0.05\n  sleep 0.125\nend\n";

/// The engine's metrics as words (shared_memory.h's PerformanceMetrics, after the copy's 32-byte header).
fn metric(instance: &Instance, slot: usize) -> u32 {
    let copy = instance.copy(2, &[]);
    assert!(copy.len() >= 32 + (slot + 1) * 4, "a copy with metrics");
    u32::from_le_bytes(copy[32 + slot * 4..36 + slot * 4].try_into().unwrap())
}

/// Lates, the worst late in ms, messages the engine took, the longest the host thread itself ran behind (ms), and
/// how many times it was held up by more than a buffer (a dropout).
fn measure(block: usize, ahead: f64, code: &str, seconds: f64) -> (u32, u32, u32, f64, u64) {
    let instance = Arc::new(Instance::new(RATE, block as u32, &resources()).expect("an instance"));
    let quit = Arc::new(AtomicBool::new(false));
    let behind = Arc::new(AtomicU64::new(0));
    let dropouts = Arc::new(AtomicU64::new(0));
    let (i, q, b, d) = (instance.clone(), quit.clone(), behind.clone(), dropouts.clone());
    let host = thread::spawn(move || {
        let (mut left, mut right) = (vec![0.0f32; block], vec![0.0f32; block]);
        let mut start = Instant::now();
        let mut frames = 0u64;
        while !q.load(Ordering::Relaxed) {
            let over = start.elapsed().as_secs_f64() - frames as f64 / RATE;
            b.fetch_max((over.max(0.0) * 1e6) as u64, Ordering::Relaxed);
            // Held up by more than a buffer: an audio device drops out and carries on from there, it does not render
            // the missed buffers back to back to catch up, which would leave the language thread no time between them.
            if over > block as f64 / RATE {
                start += Duration::from_secs_f64(over);
                d.fetch_add(1, Ordering::Relaxed);
            }
            i.render(&mut left, &mut right);
            frames += block as u64;
            if let Some(wait) = Duration::from_secs_f64(frames as f64 / RATE).checked_sub(start.elapsed()) {
                thread::sleep(wait);
            }
        }
    });
    // loaded and settled before the program starts
    let deadline = Instant::now() + Duration::from_secs(20);
    thread::sleep(Duration::from_millis(500));
    while instance.loading() != 0 && Instant::now() < deadline {
        thread::sleep(Duration::from_millis(50));
    }
    behind.store(0, Ordering::Relaxed);
    dropouts.store(0, Ordering::Relaxed);
    let (lates, taken) = (metric(&instance, 8), metric(&instance, 1));
    instance.run(&format!("set_sched_ahead_time! {ahead}\n{code}"));
    thread::sleep(Duration::from_secs_f64(seconds));
    let found = (metric(&instance, 8) - lates, metric(&instance, 23), metric(&instance, 1) - taken, behind.load(Ordering::Relaxed) as f64 / 1000.0, dropouts.load(Ordering::Relaxed));
    instance.stop();
    quit.store(true, Ordering::Relaxed);
    host.join().unwrap();
    found
}

#[test]
#[ignore]
fn how_short_the_schedule_ahead_time_can_be() {
    let seconds = std::env::var("AHEAD_SECONDS").ok().and_then(|s| s.parse().ok()).unwrap_or(12.0);
    println!("program  buffer  ahead_ms  late  worst_ms  messages  host_behind_ms  dropouts");
    let say = |name: &str, block: usize, ahead: f64, code: &str| {
        let (late, worst, messages, behind, dropouts) = measure(block, ahead, code, seconds);
        println!("{name:7}  {block:6}  {:8.1}  {late:4}  {worst:8}  {messages:8}  {behind:14.1}  {dropouts:8}", ahead * 1000.0);
    };
    // AHEAD_CASES="512:0.05,2048:0.073": the busy program at just those buffers and times
    if let Ok(cases) = std::env::var("AHEAD_CASES") {
        for case in cases.split(',') {
            let (block, ahead) = case.split_once(':').expect("buffer:seconds");
            say("busy", block.parse().unwrap(), ahead.parse().unwrap(), BUSY);
        }
        return;
    }
    for (name, code, blocks) in [("busy", BUSY, &[128usize, 512, 1024, 2048][..]), ("light", LIGHT, &[512, 2048])] {
        for &block in blocks {
            for ahead in [0.2, 0.1, 0.05, 0.03, 0.02, 0.01, 0.005] {
                say(name, block, ahead, code);
            }
        }
    }
}
