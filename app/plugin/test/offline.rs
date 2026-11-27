// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! Offline instances: time is counted from rendered frames (Instance::offline), so these tests render in a plain
//! loop as fast as the machine goes and nothing they check depends on a clock. The same code must give the same
//! audio on every run, loads included: nothing here sleeps or reads a clock.

use sonic_pi_plugin::instance::{Armed, Instance, Transport, SAYS_ARMED};

mod common;
use common::{frequency, keep, logged, onsets, own_samples, peak, resources, Heard, BLOCK, RATE};

/// NTP seconds. Arbitrary, and nowhere near the real time, so a clock read by mistake shows up.
const ORIGIN: f64 = 1000.0;

/// A short beep with an instant attack, so its first loud frame marks when it was scheduled.
const BEEP: &str = "synth :beep, note: 84, attack: 0, sustain: 0, release: 0.05, amp: 0.8";

fn offline() -> Instance {
    Instance::offline(RATE, BLOCK as u32, &resources(), ORIGIN).expect("an offline instance")
}

/// Renders `blocks` buffers and returns the left channel.
fn render(instance: &Instance, blocks: usize) -> Vec<f32> {
    render_stereo(instance, blocks, None).0
}

/// Renders `blocks` buffers and returns both channels. `input`: the pitch of a sine at half scale handed to the
/// instance on its left input, its right input silent.
fn render_stereo(instance: &Instance, blocks: usize, input: Option<f64>) -> (Vec<f32>, Vec<f32>) {
    let (mut left, mut right) = (vec![0.0f32; BLOCK], vec![0.0f32; BLOCK]);
    let (mut out_left, mut out_right) = (Vec::with_capacity(blocks * BLOCK), Vec::with_capacity(blocks * BLOCK));
    let mut frame = 0u64;
    for _ in 0..blocks {
        match input {
            Some(pitch) => {
                let tone: Vec<f32> = (0..BLOCK)
                    .map(|i| (0.5 * ((frame + i as u64) as f64 * pitch * std::f64::consts::TAU / RATE).sin()) as f32)
                    .collect();
                instance.render_with(&mut left, &mut right, Some((&tone, &[0.0f32; BLOCK])));
            }
            None => instance.render(&mut left, &mut right),
        }
        frame += BLOCK as u64;
        out_left.extend_from_slice(&left);
        out_right.extend_from_slice(&right);
    }
    (out_left, out_right)
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
fn noise_synths_render_bit_identical_audio_twice() {
    // scsynth seeds its random generators from the clock unless it is given a seed
    let play = || {
        let instance = offline();
        render_until_loaded(&instance);
        instance.run("synth :noise, release: 0.3\nsleep 0.5\nsynth :bnoise, release: 0.3\nsleep 0.5\nsynth :cnoise, release: 0.3\n");
        render(&instance, seconds(2.0))
    };
    let (first, second) = (play(), play());
    assert!(first.iter().any(|s| s.abs() > 0.01), "it sounds");
    assert!(first == second, "two renders of the same noise differ");
}

/// Renders `frames` frames in buffers whose sizes repeat `sizes`, and returns the left channel.
fn render_in(instance: &Instance, frames: usize, sizes: &[usize]) -> Vec<f32> {
    let mut out = Vec::with_capacity(frames);
    let mut turn = 0;
    while out.len() < frames {
        let n = sizes[turn % sizes.len()].min(frames - out.len());
        let (mut left, mut right) = (vec![0.0f32; n], vec![0.0f32; n]);
        instance.render(&mut left, &mut right);
        out.extend_from_slice(&left);
        turn += 1;
    }
    out
}

#[test]
fn the_audio_is_the_same_whatever_sizes_of_buffer_the_host_asks_for() {
    let play = |sizes: &[usize]| {
        let instance = offline();
        render_in(&instance, 48_000 * 600, sizes);   // ten minutes in: time kept as a running sum of seconds has drifted by then
        assert_eq!(instance.loading(), 0);
        instance.run(&format!("live_loop :t do\n  {BEEP}\n  sleep 0.25\nend\n"));
        render_in(&instance, 192_000, sizes)
    };
    let whole = play(&[512]);
    assert!(onsets(&whole).len() >= 12, "it sounds");
    for sizes in [&[256][..], &[64], &[100, 412], &[441], &[37, 512, 3]] {
        let other = play(sizes);
        assert!(onsets(&whole) == onsets(&other), "buffers of {sizes:?}: the sounds start at {:?}, not {:?}", &onsets(&other)[..6], &onsets(&whole)[..6]);
        assert!(whole == other, "buffers of {sizes:?} give different audio from buffers of 512");
    }
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

/// With 4096-frame buffers, SuperSonic's front could send up to 256 KB of synthdefs per buffer into the plugin's
/// 64 KB command ring. The ones that didn't fit were dropped, loading never finished, and no synth played. Found
/// while porting the plugin to Windows, where a host that renders straight away with no editor hit it every time.
#[test]
fn the_built_in_synthdefs_load_and_a_synth_sounds_when_the_host_renders_big_buffers() {
    const BIG: usize = 4096;
    let instance = Instance::offline(RATE, BIG as u32, &resources(), ORIGIN).expect("an offline instance");
    let (mut left, mut right) = (vec![0.0f32; BIG], vec![0.0f32; BIG]);
    let mut buffers = 0;
    loop {
        instance.render(&mut left, &mut right);
        buffers += 1;
        if instance.loading() == 0 {
            break;
        }
        assert!(buffers < 2_000, "still loading after 2,000 buffers of {BIG} frames");
    }
    instance.run(&format!("{BEEP}\n"));
    let mut out = Vec::new();
    for _ in 0..(RATE as usize / BIG + 2) {
        instance.render(&mut left, &mut right);
        out.extend_from_slice(&left);
    }
    assert!(peak(&out) > 0.1, "the beep after loading peaked at {}", peak(&out));
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

/// The page's Global Time Warp reached nothing in the plugin: its session's setTimeWarp was empty, so the control
/// moved its number and no sound (0.1.0). The warp is the runtime's, as on the web and the desktop: every sound
/// that much later, or earlier.
#[test]
fn the_global_time_warp_moves_every_sound_by_its_milliseconds() {
    let first_sound = |warp: Option<f64>| {
        let instance = offline();
        render_until_loaded(&instance);
        if let Some(ms) = warp {
            instance.time_warp(ms);
        }
        render(&instance, seconds(0.5));
        instance.run(&format!("{BEEP}\n"));
        onsets(&render(&instance, seconds(1.5)))[0] as i64
    };
    // 100 ms is 4,800 frames, to within the engine block (64 frames) a sound starts in
    let plain = first_sound(None);
    let later = first_sound(Some(100.0)) - plain;
    assert!((later - 4_800).abs() <= 64, "a warp of 100 ms moved the sound {later} frames");
    assert_eq!(first_sound(Some(0.0)), plain);
    // no warp brings a run's first sound to before it was run: it comes as soon as it does with none
    assert_eq!(first_sound(Some(-100.0)), plain, "the first sound of a run, with a warp of -100 ms");
}

/// A warp below 0 moves a playing loop's sounds earlier by the whole of it, however short the schedule-ahead time:
/// the loop's thread runs that much ahead of the audio. With the schedule-ahead at 25 ms (20 November 2026) a warp
/// of -100 ms moved them 24 ms and no more, each sound reaching the engine after its time.
#[test]
fn a_time_warp_below_zero_moves_a_playing_loop_earlier_by_all_of_it() {
    let instance = offline();
    render_until_loaded(&instance);
    instance.run(&format!("live_loop :t do\n  {BEEP}\n  sleep 0.5\nend\n"));
    let mut heard = render(&instance, seconds(2.2));
    let before = onsets(&heard);
    assert!(before.len() >= 4 && on_the_beat(&before, 24_000), "the loop before the warp: {before:?}");
    instance.time_warp(-100.0);
    heard.extend(render(&instance, seconds(3.0)));
    let all = onsets(&heard);
    // the sounds from the second after the warp on, against the beats the loop kept before it
    let moved: Vec<i64> = all[before.len() + 1..].iter().enumerate()
        .map(|(i, s)| *s as i64 - (before[0] + (before.len() + 1 + i) * 24_000) as i64).collect();
    assert!(moved.len() >= 3, "the loop plays on after the warp: {all:?}");
    assert!(moved.iter().all(|m| (m + 4_800).abs() <= 64), "a warp of -100 ms moved the loop's sounds {moved:?} frames");
}

#[test]
fn a_time_warp_set_while_a_run_plays_moves_the_sounds_still_to_come() {
    let instance = offline();
    render_until_loaded(&instance);
    instance.run(&format!("{BEEP}\nsleep 1\n{BEEP}\nsleep 1\n{BEEP}\n"));
    let mut heard = render(&instance, seconds(0.5));
    instance.time_warp(100.0);
    heard.extend(render(&instance, seconds(1.0)));
    instance.time_warp(0.0);
    heard.extend(render(&instance, seconds(1.5)));
    let apart = gaps(&onsets(&heard));
    let within_a_block = |frames: usize, of: usize| (frames as i64 - of as i64).abs() <= 64;
    assert!(apart.len() == 2 && within_a_block(apart[0], 48_000 + 4_800) && within_a_block(apart[1], 48_000 - 4_800),
            "the second sound 100 ms late, the third back in its place: {apart:?}");
}

#[test]
fn an_editor_that_opens_later_is_told_the_time_warp_the_instance_keeps() {
    let instance = offline();
    render_until_loaded(&instance);
    instance.time_warp(-35.0);
    let heard = listening(&instance);
    instance.earlier();
    let told: Vec<f64> = heard.0.lock().unwrap().iter()
        .filter(|(kind, _)| *kind == sonic_pi_plugin::instance::SAYS_WARP)
        .map(|(_, bytes)| f64::from_le_bytes(bytes[..8].try_into().unwrap()))
        .collect();
    deaf(&instance);
    assert_eq!(told, [-35.0]);
}

/// The page times what it shows (a log line, a cue, a live loop's scope) against a clock, and drops what is more
/// than two seconds behind it. The instance's time is the audio rendered, which stops whenever the host renders
/// nothing, so the page must keep the instance's time and not the wall clock: 0.1.1's page kept the wall clock, and
/// after REAPER had been stopped a while it showed nothing at all. The instance says its time as it moves.
#[test]
fn the_instance_says_its_time_as_the_audio_moves_and_to_an_editor_that_opens() {
    let instance = offline();
    render_until_loaded(&instance);
    let heard = listening(&instance);
    let times = |heard: &Heard| -> Vec<f64> {
        heard.0.lock().unwrap().iter()
            .filter(|(kind, _)| *kind == sonic_pi_plugin::instance::SAYS_CLOCK)
            .map(|(_, bytes)| f64::from_le_bytes(bytes[..8].try_into().unwrap()))
            .collect()
    };
    render(&instance, seconds(1.0));
    let said = times(&heard);
    assert!(said.len() >= 10, "told {} times over a second of audio", said.len());
    assert!(said.windows(2).all(|w| w[1] - w[0] >= 0.05 && w[1] - w[0] < 0.11), "a twentieth of a second apart, or a little more: {said:?}");
    let last = *said.last().unwrap();
    instance.earlier();
    let now = *times(&heard).last().unwrap();
    deaf(&instance);
    assert!(now >= last && now - last < 0.06, "an editor that opens is told the time as it stands: {now} after {last}");
    assert!(now > ORIGIN && now < ORIGIN + 3600.0, "the instance's time, from its origin: {now}");
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

#[test]
fn a_sample_by_its_path_and_one_from_a_folder_by_its_place_both_sound() {
    let dir = own_samples();
    for code in [format!("sample {:?}\n", dir.join("kick.flac").to_string_lossy()), format!("sample {:?}, 1\n", dir.to_string_lossy())] {
        let instance = offline();
        render_until_loaded(&instance);
        instance.run(&code);
        let heard = render(&instance, seconds(1.5));
        assert!(peak(&heard) > 0.05, "{code:?} sounds: peak {}", peak(&heard));
    }
}

#[test]
fn sound_out_plays_on_the_output_it_names_and_not_the_other() {
    let instance = offline();
    render_until_loaded(&instance);
    // on output 2 alone: amp: 0 keeps the fx from passing the sound on to both
    instance.run("with_fx :sound_out, output: 2, amp: 0 do\n  synth :sine, note: 69, sustain: 3, release: 0.1, amp: 0.5\nend\n");
    render(&instance, seconds(1.0));
    let (left, right) = render_stereo(&instance, seconds(1.0), None);
    assert!(peak(&right) > 0.05, "sound_out's sound is on output 2, peak {}", peak(&right));
    assert!((frequency(&right) - 440.0).abs() < 3.0, "output 2 plays the sine, heard {:.1} Hz", frequency(&right));
    assert!(peak(&left) < 1e-3, "and not on output 1, peak {}", peak(&left));
}

#[test]
fn the_hosts_input_is_heard_through_live_audio_and_only_while_it_is_asked_for() {
    let instance = offline();
    render_until_loaded(&instance);
    let quiet = render_stereo(&instance, seconds(0.5), Some(330.0)).0;
    assert!(peak(&quiet) < 1e-6, "the host's input is not heard until a program asks for it, peak {}", peak(&quiet));

    instance.run("live_audio :in\n");
    render_stereo(&instance, seconds(1.0), Some(330.0));
    let heard = render_stereo(&instance, seconds(1.0), Some(330.0)).0;
    assert!(peak(&heard) > 0.05, "live_audio plays the host's input, peak {}", peak(&heard));
    assert!((frequency(&heard) - 330.0).abs() < 3.0, "what live_audio plays is the input at 330 Hz, heard {:.1} Hz", frequency(&heard));

    instance.run("live_audio :in, :stop\n");
    render_stereo(&instance, seconds(1.0), Some(330.0));
    let after = render_stereo(&instance, seconds(0.5), Some(330.0)).0;
    assert!(peak(&after) < 1e-3, "stopped, the input is not heard, peak {}", peak(&after));
}

/// Starts listening to `instance`. The returned Heard must outlive the listening: call `deaf` before it is dropped.
fn listening(instance: &Instance) -> Box<Heard> {
    let heard = Box::new(Heard(std::sync::Mutex::new(Vec::new())));
    // SAFETY: the box is not moved or dropped until `deaf` has stopped the listening.
    unsafe { instance.listen(Some(keep), (&*heard as *const Heard).cast_mut().cast()) };
    heard
}

fn deaf(instance: &Instance) {
    // SAFETY: no listener and no context.
    unsafe { instance.listen(None, std::ptr::null_mut()) };
}

/// The job number of the first run the instance reported.
fn job_of(heard: &Heard) -> i32 {
    heard.0.lock().unwrap().iter()
        .find_map(|(k, b)| (*k == sonic_pi_plugin::instance::SAYS_RUN).then(|| i32::from_le_bytes([b[0], b[1], b[2], b[3]])))
        .expect("the run reported its job")
}

/// Runs `code` as a card's run (group 2000), checks it sounds, stops it by its job with a 0.25 s fade, and checks
/// the last half second of the 2.5 s after that is silent.
fn runs_then_stops_by_its_job(code: &str, warm_up: f64) {
    let instance = offline();
    render_until_loaded(&instance);
    let heard = listening(&instance);
    instance.run_in(code, 2000);
    let playing = render(&instance, seconds(warm_up));
    let job = job_of(&heard);
    instance.stop_run(job, 0.25);
    let after = render(&instance, seconds(2.5));
    deaf(&instance);
    assert!(peak(&playing) > 0.05, "it sounds: peak {}", peak(&playing));
    let tail = &after[after.len() - 24_000..];
    assert!(peak(tail) < 1e-4, "stopped by its job (run {job}), it is silent: peak {}", peak(tail));
}

#[test]
fn a_cards_loop_stops_when_its_run_is_stopped_by_job() {
    runs_then_stops_by_its_job(
        "use_arg_checks true; __warn_unknown_opts true; with_fx :scope_out, scope_num: 1 do\n\
         live_loop :drone do\n  synth :saw, note: 50, sustain: 1, release: 0.1, amp: 0.4\n  sleep 1\nend\nend\n",
        1.5,
    );
}

#[test]
fn an_example_piece_stops_when_its_run_is_stopped_by_job() {
    let manifest = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let example = std::fs::read_to_string(manifest.join("../../etc/examples/wizard/tilburg_2.rb")).unwrap();
    runs_then_stops_by_its_job(&format!("use_arg_checks true; __warn_unknown_opts true; with_fx :scope_out, scope_num: 1 do\n{example}\nend\n"), 3.0);
}

#[test]
fn a_hundred_reverbs_at_once_are_not_refused_by_the_engine() {
    let instance = offline();
    render_until_loaded(&instance);
    let heard = listening(&instance);
    instance.run("100.times do\n  with_fx :reverb, room: 0.9 do\n    play 60, release: 0.5, amp: 0.05\n  end\nend\n");
    let sound = render(&instance, seconds(1.5));
    deaf(&instance);
    assert_eq!(logged(&heard, "/fail"), None, "the engine refused nothing");
    assert!(peak(&sound) > 0.01, "they sound: peak {}", peak(&sound));
}

/// After a sample rate change (the host calls setupProcessing again) the engine is rebuilt. Synthdefs and samples
/// loaded before it are still there, because SuperSonic keeps them as assets. Check that a synth and a sample each
/// play once after the change. Before sp_instance_reconfigure called runtime.engineLost(), both were silent.
#[test]
fn a_new_sample_rate_after_loading_keeps_synths_and_samples_working() {
    let instance = offline();
    render_until_loaded(&instance);
    instance.run("sample :bd_haus");
    render_until_loaded(&instance);
    render(&instance, seconds(1.0));
    assert!(instance.reconfigure(44_100.0, BLOCK as u32), "the instance takes the new rate");
    render_until_loaded(&instance);
    instance.run(BEEP);
    let beep = render(&instance, seconds(1.0));
    assert_eq!(onsets(&beep).len(), 1, "a beep after the new rate: peak {}", peak(&beep));
    instance.run("sample :bd_haus");
    render_until_loaded(&instance);
    let kick = render(&instance, seconds(1.0));
    assert_eq!(onsets(&kick).len(), 1, "a sample after the new rate: peak {}", peak(&kick));
}

/// A sample rate change while the built-in synths are still loading, as when REAPER opens its audio device during a
/// project load. Render one buffer first, so some definitions are committed and the next batch is in flight when the
/// engine is rebuilt. The front then fails the /d_loadDir, and the plugin must resend it rather than show an error.
/// Before the fix, the user saw "Sonic Pi's synths could not be loaded", and each synth was then loaded separately
/// the first time code used it. Play zawa, because it is the last definition loaded (the front sorts a directory by
/// name).
#[test]
fn a_new_sample_rate_while_the_synths_load_does_not_lose_them() {
    const LAST: &str = "synth :zawa, note: 84, attack: 0, sustain: 0, release: 0.05, amp: 0.8";
    let instance = offline();
    let heard = listening(&instance);
    render(&instance, 1);
    assert!(instance.loading() > 0, "the built-in synths are still loading after one buffer");
    assert!(instance.reconfigure(44_100.0, BLOCK as u32), "the instance takes the new rate");
    render_until_loaded(&instance);
    instance.run(LAST);
    let zawa = render(&instance, seconds(1.0));
    deaf(&instance);
    let errors: Vec<String> = heard.0.lock().unwrap().iter()
        .filter(|(kind, _)| *kind == sonic_pi_plugin::instance::SAYS_ERROR)
        .map(|(_, b)| String::from_utf8_lossy(b).into_owned())
        .collect();
    assert!(errors.is_empty(), "no error for a /d_loadDir interrupted by the rebuild: {errors:?}");
    assert!(logged(&heard, &format!("synthdefs: {} loaded", 120)).is_some(), "/d_loadDir reported all 120 synthdefs loaded");
    assert_eq!(onsets(&zawa).len(), 1, "zawa after the new rate: peak {}", peak(&zawa));
}

// ── MIDI: the host's port, `daw` ──────────────────────────────────────────────

use sonic_pi_plugin::instance::Midi;

/// Renders `blocks` buffers: the left channel, and what the instance sent to the host's MIDI port, each with its
/// frame counted from the first buffer.
fn render_midi(instance: &Instance, blocks: usize) -> (Vec<f32>, Vec<(usize, Vec<u8>)>) {
    let (mut left, mut right) = (vec![0.0f32; BLOCK], vec![0.0f32; BLOCK]);
    let mut out = Vec::with_capacity(blocks * BLOCK);
    let mut sent = Vec::new();
    let mut events = [Midi::default(); 64];
    for block in 0..blocks {
        instance.render(&mut left, &mut right);
        loop {
            let n = instance.midi_out(&mut events);
            for e in &events[..n] {
                assert!((e.frame as usize) < BLOCK, "a frame of the buffer just rendered: {}", e.frame);
                sent.push((block * BLOCK + e.frame as usize, e.bytes[..e.length as usize].to_vec()));
            }
            if n < events.len() {
                break;
            }
        }
        out.extend_from_slice(&left);
    }
    (out, sent)
}

#[test]
fn midi_notes_a_beat_apart_reach_the_host_exactly_48000_frames_apart() {
    let instance = offline();
    render_until_loaded(&instance);
    instance.run("3.times do\n  midi_note_on :e3, 100, channel: 2\n  sleep 1\nend\n");
    let (_, sent) = render_midi(&instance, seconds(3.5));
    assert_eq!(sent.len(), 3, "three notes: {sent:?}");
    for (_, bytes) in &sent {
        assert_eq!(bytes, &vec![0x91, 52, 100], "note on, channel 2, e3, velocity 100: {sent:?}");
    }
    assert_eq!(sent[1].0 - sent[0].0, 48_000, "{sent:?}");
    assert_eq!(sent[2].0 - sent[1].0, 48_000, "{sent:?}");
}

#[test]
fn a_midi_note_and_a_sound_played_together_reach_the_host_within_5_ms_the_note_first() {
    let instance = offline();
    render_until_loaded(&instance);
    instance.run(&format!("sleep 0.25\nmidi_note_on 60, 90, channel: 1\n{BEEP}\nsleep 0.5\nmidi_note_on 62, 90, channel: 1\n{BEEP}\n"));
    let (audio, sent) = render_midi(&instance, seconds(2.0));
    let starts = onsets(&audio);
    assert_eq!(starts.len(), 2, "two beeps: {starts:?}");
    assert_eq!(sent.len(), 2, "two notes: {sent:?}");
    for (beep, (note, _)) in starts.iter().zip(&sent) {
        // The note is on the frame of its time. The sound is heard a little after its own: it leaves through the
        // mixer, whose limiter looks 1.5 ms ahead, and an onset is the first frame loud enough to count. Measured
        // 18 November 2026: the first beep's onset 122 frames after its note, the second's 186.
        assert!(note <= beep && beep - note <= (0.005 * RATE) as usize, "the note at {note}, the beep at {beep}");
    }
    assert_eq!(sent[1].0 - sent[0].0, 24_000, "the notes are half a beat apart exactly: {sent:?}");
}

#[test]
fn midi_plays_a_note_on_and_its_note_off_after_the_sustain() {
    let instance = offline();
    render_until_loaded(&instance);
    instance.run("midi :c4, sustain: 0.5, velocity: 80, channel: 1\n");
    let (_, sent) = render_midi(&instance, seconds(1.5));
    assert_eq!(sent.len(), 2, "a note on and a note off: {sent:?}");
    assert_eq!(sent[0].1, vec![0x90, 60, 80]);
    assert_eq!(sent[1].1[..2], [0x80, 60]);
    let held = sent[1].0 - sent[0].0;
    assert!(held.abs_diff((0.5 * RATE) as usize) <= (0.011 * RATE) as usize, "held for about the sustain, {held} frames");
}

#[test]
fn midi_with_no_channel_is_sent_on_all_16_and_with_another_port_not_at_all() {
    let instance = offline();
    render_until_loaded(&instance);
    instance.run("midi_note_on 60, 100\n");
    let (_, sent) = render_midi(&instance, seconds(1.0));
    let channels: Vec<u8> = sent.iter().map(|(_, b)| b[0]).collect();
    assert_eq!(channels, (0x90..=0x9F).collect::<Vec<u8>>(), "every channel: {sent:?}");

    instance.run("midi_note_on 60, 100, channel: 1, port: \"some_other_port\"\nsleep 0.1\nmidi_note_on 61, 100, channel: 1, port: \"daw\"\n");
    let (_, sent) = render_midi(&instance, seconds(1.0));
    assert_eq!(sent.len(), 1, "only the note for the host's port: {sent:?}");
    assert_eq!(sent[0].1, vec![0x90, 61, 100]);
}

#[test]
fn the_same_midi_program_sends_the_same_notes_on_the_same_frames_twice() {
    let run = || {
        let instance = offline();
        render_until_loaded(&instance);
        instance.run("live_loop :m do\n  midi_note_on (ring 60, 64, 67).tick, 100, channel: 1\n  sleep 0.125\nend\n");
        render_midi(&instance, seconds(2.0)).1
    };
    let (first, second) = (run(), run());
    assert!(first.len() >= 15, "about 16 notes in two seconds: {}", first.len());
    assert_eq!(first, second);
}

#[test]
fn a_note_from_the_host_is_a_cue_a_program_can_sync_on() {
    let instance = offline();
    render_until_loaded(&instance);
    instance.run("live_loop :keys do\n  n, v = sync \"/midi:daw:3/note_on\"\n  synth :beep, note: n, attack: 0, sustain: 0.2, release: 0.05, amp: v / 127.0\nend\n");
    let before = render(&instance, seconds(0.5));
    assert!(peak(&before) < 1e-6, "nothing plays until a note comes, peak {}", peak(&before));

    // a5, on channel 3, half way through a buffer
    assert!(instance.midi_in(&[0x92, 81, 100], (BLOCK / 2) as u32));
    let heard = render(&instance, seconds(1.0));
    let starts = onsets(&heard);
    assert_eq!(starts.len(), 1, "one beep for one note: {starts:?}");
    let from = starts[0];
    let pitch = frequency(&heard[from..from + (0.2 * RATE) as usize]);
    assert!((pitch - 880.0).abs() < 5.0, "the beep is the note played, a5 at 880 Hz: heard {pitch:.1} Hz");

    // Another channel is another cue.
    assert!(instance.midi_in(&[0x90, 81, 100], 0));
    let other = render(&instance, seconds(1.0));
    assert!(onsets(&other).is_empty(), "a note on channel 1 does not wake a sync on channel 3");
}

/// A program that answers a note over time. The note is a cue with no beat of its own, and the thread that syncs on
/// it took that for its own beat: its first sleep went back to the session's start, the note was found again, and
/// the program never got past its first sound (26 November 2026, fixed in the runtime's sync).
#[test]
fn a_program_woken_by_a_note_from_the_host_sleeps_and_plays_on() {
    let instance = offline();
    render_until_loaded(&instance);
    // a beat apart: 48,000 frames is a whole number of the engine's blocks, so the gaps are exact
    instance.run(&format!("live_loop :keys do\n  n, v = sync \"/midi:daw:1/note_on\"\n  3.times do\n    {BEEP}\n    sleep 1\n  end\nend\n"));
    render(&instance, seconds(2.0));
    assert!(instance.midi_in(&[0x90, 60, 100], 0));
    let starts = onsets(&render(&instance, seconds(3.5)));
    assert_eq!(starts.len(), 3, "three beeps for one note: {starts:?}");
    assert_eq!(gaps(&starts), vec![48_000, 48_000], "a beat apart, to the frame");

    // and it is waiting again: a second note is answered the same way
    assert!(instance.midi_in(&[0x90, 62, 100], 0));
    let again = onsets(&render(&instance, seconds(3.5)));
    assert_eq!(gaps(&again), vec![48_000, 48_000], "the second note's three: {again:?}");
}

/// The process table the instance last said (SAYS_TABLE, 6): a row of 15 doubles for each thread, fx and sound,
/// as the page's PROCESS_FIELDS has them.
fn last_table(heard: &Heard) -> Vec<[f64; 15]> {
    let said = heard.0.lock().unwrap();
    let bytes = said.iter().rev().find(|(k, _)| *k == 6).map(|(_, b)| b.clone()).unwrap_or_default();
    bytes.chunks_exact(8 * 15).map(|row| std::array::from_fn(|i| f64::from_le_bytes(row[i * 8..i * 8 + 8].try_into().unwrap()))).collect()
}

/// The page draws its tree of threads and sounds from the table the instance says. A loop that has answered a note
/// and waits on its sync again asks for no more ticks, and the sounds it started end, and leave the table, with the
/// runtime idle: the page is told all the same, or its tree keeps sounds that are long over.
#[test]
fn the_page_is_told_a_sound_has_ended_and_gone_while_the_program_only_waits() {
    const KIND: usize = 3;
    const STATE: usize = 4;
    let sounds = |table: &[[f64; 15]]| table.iter().filter(|r| r[KIND] == 7.0 || r[KIND] == 8.0).map(|r| r[STATE]).collect::<Vec<f64>>();
    let instance = offline();
    render_until_loaded(&instance);
    let heard = listening(&instance);
    instance.run(&format!("live_loop :keys do\n  n, v = sync \"/midi:daw:1/note_on\"\n  {BEEP}\nend\n"));
    render(&instance, seconds(1.0));
    assert!(instance.midi_in(&[0x90, 60, 100], 0));
    render(&instance, seconds(0.02));
    assert_eq!(sounds(&last_table(&heard)), vec![0.0], "the note's beep is in the table, sounding");

    // the beep lasts 0.05 s: well over, and not yet a second over (how long an ended sound stays shown)
    render(&instance, seconds(0.5));
    assert_eq!(sounds(&last_table(&heard)), vec![3.0], "half a second on the page has been told it ended");

    render(&instance, seconds(1.5));
    let table = last_table(&heard);
    deaf(&instance);
    assert_eq!(sounds(&table), Vec::<f64>::new(), "and a second after it ended, that it has gone");
    assert!(table.iter().any(|r| r[KIND] == 2.0 && r[STATE] == 2.0), "with the loop still there, waiting on its sync: {table:?}");
}

#[test]
fn a_note_off_from_the_host_is_a_cue_too_and_what_is_not_a_note_is_refused() {
    let instance = offline();
    render_until_loaded(&instance);
    instance.run(&format!("n, v = sync \"/midi:daw:1/note_off\"\n{BEEP}\n"));
    render(&instance, seconds(0.25));
    assert!(instance.midi_in(&[0x80, 60, 0], 0));
    assert_eq!(onsets(&render(&instance, seconds(1.0))).len(), 1, "the note off woke the sync");

    assert!(!instance.midi_in(&[0xF0, 1, 2, 0xF7], 0), "system exclusive is not carried");
    assert!(!instance.midi_in(&[60, 100], 0), "data with no status is no message");
}

/// How far ahead of its time a sound is handed to the engine follows the host's largest buffer: the buffer's length
/// and 15 ms, 25 ms at the least. The same beep, run at the same frame, starts that much later in a host of
/// 2048-frame buffers than in one of 512.
#[test]
fn the_schedule_ahead_time_follows_the_hosts_buffer() {
    let first_sound = |max_frames: u32| {
        let instance = Instance::offline(RATE, max_frames, &resources(), ORIGIN).expect("an offline instance");
        render(&instance, 400);
        assert_eq!(instance.loading(), 0, "loaded");
        instance.run(BEEP);
        *onsets(&render(&instance, seconds(0.5))).first().expect("the beep sounds")
    };
    let (small, large) = (first_sound(512), first_sound(2048));
    // 512 frames are 10.7 ms, so 25.7 ms: 1232 frames, and the engine's own 73 to 201 after a sound's time
    assert!((1232 + 73..=1232 + 201).contains(&small), "with 512-frame buffers the beep starts {small} frames after it was run");
    // 2048 frames are 42.7 ms, so 57.7 ms: 1536 frames more
    assert_eq!(large - small, 1536, "with 2048-frame buffers the beep starts {large} frames after it was run, with 512 {small}");
}

/// An armed buffer needs no editor: the instance itself runs it when the host begins to play, from that moment, and
/// stops it when the host stops. Before this the page did both, and nothing ran with the plugin's window closed.
#[test]
fn an_armed_buffer_runs_when_the_host_plays_and_stops_when_it_stops() {
    let instance = offline();
    let heard = Box::new(Heard(std::sync::Mutex::new(Vec::new())));
    // SAFETY: the box outlives the listening, which ends below
    unsafe { instance.listen(Some(keep), (&*heard as *const Heard).cast_mut().cast()) };
    let mut daw = Daw { bpm: 120.0, playing: false, beat: 0.0 };
    render_until_loaded(&instance);
    instance.armed(&[Armed { buffer: 3, group: 1003, parent: 999, fade: 0.05, code: format!("live_loop :armed do\n  {BEEP}\n  sleep 1\nend\n") }]);
    assert!(onsets(&daw.render(&instance, seconds(1.0))).is_empty(), "nothing runs while the host is stopped");

    daw.playing = true;
    let playing = daw.render(&instance, seconds(2.2));
    let starts = onsets(&playing);
    assert!(starts.len() >= 4, "the armed loop plays: {starts:?}");
    // at 120 a beat is 24,000 frames from the frame the host began, and the engine starts a sound 73 to 201 after
    for (beat, start) in starts.iter().enumerate().skip(1) {
        let after = *start as i64 - beat as i64 * 24_000;
        assert!((73..=201).contains(&after), "beat {beat} starts {after} frames after the host's beat: {starts:?}");
    }
    // the page is told which buffer's run it is
    let said: Vec<(i32, i32)> = heard.0.lock().unwrap().iter().filter(|(kind, _)| *kind == SAYS_ARMED)
        .map(|(_, b)| (i32::from_le_bytes(b[0..4].try_into().unwrap()), i32::from_le_bytes(b[4..8].try_into().unwrap()))).collect();
    assert!(said.len() == 1 && said[0].0 >= 0 && said[0].1 == 3, "one run, of buffer 3: {said:?}");

    // Its first sound is already due when the word of the host's Play comes, and leaves late. Nothing was loading,
    // and the log does not say something was: it once blamed the sound's synthdef for every late sound.
    let blamed = logged(&heard, "waited on");
    assert!(blamed.is_none(), "a load is blamed for a sound that waited on none: {blamed:?}");

    daw.playing = false;
    daw.render(&instance, seconds(0.5));
    assert!(onsets(&daw.render(&instance, seconds(2.0))).is_empty(), "the loop stopped with the host");

    // disarmed, the host's Play runs nothing
    instance.armed(&[]);
    daw.playing = true;
    assert!(onsets(&daw.render(&instance, seconds(1.5))).is_empty(), "nothing armed, nothing runs");
    unsafe { instance.listen(None, std::ptr::null_mut()) };
}

#[test]
fn the_armed_buffers_are_read_from_the_pages_storage_and_nothing_is_guessed() {
    use sonic_pi_plugin::instance::armed_from_json;
    let one = Armed { buffer: 2, group: 1002, parent: 999, fade: 0.05, code: "play 60\n".into() };
    assert_eq!(armed_from_json(r#"[{"buffer":2,"group":1002,"parent":999,"fade":0.05,"code":"play 60\n"}]"#), [one.clone()]);
    // an entry short of a field is left out, the rest kept
    assert_eq!(armed_from_json(r#"[{"buffer":1,"group":1001},{"buffer":2,"group":1002,"parent":999,"fade":0.05,"code":"play 60\n"}]"#), [one]);
    for none in ["", "null", "{}", "[]", "not json", r#"{"buffer":2}"#] {
        assert!(armed_from_json(none).is_empty(), "{none:?}");
    }
}
