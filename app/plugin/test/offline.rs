// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! Offline instances: time is counted from rendered frames (Instance::offline), so these tests render in a plain
//! loop as fast as the machine goes and nothing they check depends on a clock. The same code must give the same
//! audio on every run, loads included: nothing here sleeps or reads a clock.

use sonic_pi_plugin::instance::{Instance, Transport};

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

/// Renders until the instance has nothing left to load, and returns how many buffers that took. Offline, loading
/// is done as part of rendering (the files are read in the render call and the engine's answers collected after
/// it), so this needs no waiting and takes the same number of buffers every time. The limit only stops a run in
/// which loading never finishes.
fn render_until_loaded(instance: &Instance) -> usize {
    for buffers in 1..=100_000 {
        render(instance, 1);
        if instance.loading() == 0 {
            return buffers;
        }
    }
    panic!("the instance was still loading after 100,000 buffers");
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

#[test]
fn the_built_in_synthdefs_load_in_the_same_number_of_buffers_every_time() {
    let buffers: Vec<usize> = (0..3).map(|_| render_until_loaded(&offline())).collect();
    assert!(buffers.iter().all(|b| *b == buffers[0]), "buffers rendered until loaded, three times: {buffers:?}");
}

#[test]
fn a_sample_named_in_the_code_plays_on_the_exact_frame_and_identically_twice() {
    let play = || {
        let instance = offline();
        render_until_loaded(&instance);
        instance.run("sample :bd_haus\nsleep 1\nsample :bd_haus\n");
        render(&instance, seconds(2.5))
    };
    let (first, second) = (play(), play());
    let starts = onsets(&first);
    assert_eq!(starts.len(), 2, "two kicks: {starts:?}");
    assert_eq!(starts[1] - starts[0], 48_000, "{starts:?}");
    assert!(first == second, "two renders of the same sample differ");
}

#[test]
fn a_sample_first_named_part_way_through_arrives_at_the_same_frame_every_time() {
    // The path is put together when the code runs, so nothing can ask for the file before the run starts: it is
    // loaded when the line is reached. Whatever that costs, it costs the same on every render.
    let play = || {
        let instance = offline();
        render_until_loaded(&instance);
        instance.run(&format!("{BEEP}\nsleep 1\nsample (\"bd_\" + \"haus\").to_sym\n"));
        onsets(&render(&instance, seconds(2.5)))
    };
    let (first, second) = (play(), play());
    assert_eq!(first.len(), 2, "a beep and a kick: {first:?}");
    assert_eq!(first, second);
}

/// A host's transport in 4/4, said before every buffer as a DAW says it.
struct Daw {
    bpm: f64,
    playing: bool,
    /// The beat the next buffer starts on.
    beat: f64,
}

impl Daw {
    /// Renders `blocks` buffers in this host and returns the left channel.
    fn render(&mut self, instance: &Instance, blocks: usize) -> Vec<f32> {
        let mut out = Vec::with_capacity(blocks * BLOCK);
        for _ in 0..blocks {
            instance.transport(&Transport { bpm: self.bpm, beat_in_bar: self.beat % 4.0, playing: self.playing as i32, meter_num: 4, meter_den: 4 });
            out.extend(render(instance, 1));
            if self.playing {
                self.beat += BLOCK as f64 * self.bpm / 60.0 / RATE;
            }
        }
        out
    }
}

/// The frames between consecutive onsets.
fn gaps(starts: &[usize]) -> Vec<usize> {
    starts.windows(2).map(|w| w[1] - w[0]).collect()
}

/// Whether every onset is within one engine block (64 frames) of a grid `beat` frames apart, counted from the
/// first. The engine starts a sound at the block its time falls in, so a beat that lands on a block's edge can
/// start in either neighbour; measuring each against the grid, not against the one before, also shows any drift.
fn on_the_beat(starts: &[usize], beat: usize) -> bool {
    starts.iter().enumerate().all(|(i, s)| (*s as i64 - (starts[0] + i * beat) as i64).abs() <= 64)
}

#[test]
fn with_no_host_transport_a_beat_is_exactly_a_second() {
    let instance = offline();
    render_until_loaded(&instance);
    instance.run(&format!("{BEEP}\nsleep 1\n{BEEP}\n"));
    assert_eq!(gaps(&onsets(&render(&instance, seconds(2.5)))), [48_000]);
}

#[test]
fn a_stopped_host_at_120_makes_a_beat_exactly_half_a_second() {
    let instance = offline();
    let mut daw = Daw { bpm: 120.0, playing: false, beat: 0.0 };
    render_until_loaded(&instance);
    daw.render(&instance, seconds(1.0));
    instance.run(&format!("{BEEP}\nsleep 1\n{BEEP}\n"));
    let starts = onsets(&daw.render(&instance, seconds(2.5)));
    assert!(starts.len() == 2 && on_the_beat(&starts, 24_000), "{:?}", gaps(&starts));
}

#[test]
fn a_loop_follows_a_playing_hosts_tempo_and_link_starts_it_on_a_bar() {
    let instance = offline();
    let mut daw = Daw { bpm: 120.0, playing: true, beat: 0.0 };
    render_until_loaded(&instance);
    daw.render(&instance, seconds(1.0));

    // Sonic Pi's own tempo is 60: at the host's 120 a beat is half a second, 24,000 frames
    let beat_at_run = daw.beat;
    instance.run(&format!("link\nlive_loop :t do\n  {BEEP}\n  sleep 1\nend\n"));
    let heard = daw.render(&instance, seconds(6.0));
    let starts = onsets(&heard);
    assert!(starts.len() >= 6, "the loop sounded {} times in six seconds", starts.len());
    assert!(on_the_beat(&starts, 24_000), "at 120: {:?}", gaps(&starts));

    // `link` waited for the host's bar line: the first sound is on a beat that is a multiple of 4, to within two
    // engine blocks (128 frames): one for the block the engine takes the host's beat in, one for the block it
    // starts the sound in
    let first_beat = beat_at_run + starts[0] as f64 * 120.0 / 60.0 / RATE;
    let from_bar = (first_beat % 4.0).min(4.0 - first_beat % 4.0);
    assert!(from_bar <= 128.0 * 120.0 / 60.0 / RATE, "the loop's first sound is at the host's beat {first_beat:.4}");

    // the host slows to 90: two thirds of a second a beat, 32,000 frames
    daw.bpm = 90.0;
    daw.render(&instance, seconds(1.5));
    let slower = onsets(&daw.render(&instance, seconds(5.0)));
    assert!(slower.len() >= 5 && on_the_beat(&slower, 32_000), "at 90: {:?}", gaps(&slower));
}

#[test]
fn two_instances_side_by_side_each_render_only_their_own_code() {
    let (a, b) = (offline(), offline());
    render_until_loaded(&a);
    render_until_loaded(&b);
    a.run(&format!("{BEEP}\n"));
    let (heard_a, heard_b) = (render(&a, seconds(1.0)), render(&b, seconds(1.0)));
    assert_eq!(onsets(&heard_a).len(), 1);
    assert!(heard_b.iter().all(|s| *s == 0.0), "the instance that ran nothing is silent");
}
