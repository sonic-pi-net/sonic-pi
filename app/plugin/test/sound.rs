// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! Instances rendered as a host renders them, offline: no audio device is
//! opened. Each render thread keeps real time, since the runtime schedules on
//! the wall clock the engine follows.

use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use sonic_pi_plugin::instance::{Instance, Transport};

mod common;
use common::{onsets, peak, resources, BLOCK, RATE};

/// Renders `instance` in real time on a thread of its own, as a host's audio thread does, keeping every frame.
struct Host {
    rendered: Arc<Mutex<Vec<f32>>>,
    /// The right channel's frames, beside `rendered`'s left.
    right: Arc<Mutex<Vec<f32>>>,
    /// Buffers this thread started more than 50 ms after they were due: the machine held it up, and the engine's
    /// clock, which follows the wall clock, stepped on past the audio.
    stalls: Arc<AtomicU32>,
    quit: Arc<AtomicBool>,
    thread: Option<thread::JoinHandle<()>>,
}

/// A host's transport, in 4/4: its tempo and whether it plays, and where it has been, so a frame's beat can be asked.
struct Daw {
    bpm: f64,
    playing: bool,
    /// The beat the next buffer starts on.
    beat: f64,
    /// Each stretch at one tempo: the frame it starts at, the beat there, the beats a frame.
    stretches: Vec<(u64, f64, f64)>,
}

impl Daw {
    fn playing_at(bpm: f64) -> Arc<Mutex<Daw>> {
        Arc::new(Mutex::new(Daw { bpm, playing: true, beat: 0.0, stretches: Vec::new() }))
    }

    /// What the host says before the buffer starting at `frame`, and its beat moved on past it.
    fn next(&mut self, frame: u64) -> Transport {
        let per_frame = if self.playing { self.bpm / 60.0 / RATE } else { 0.0 };
        if self.stretches.last().is_none_or(|s| s.2 != per_frame) {
            self.stretches.push((frame, self.beat, per_frame));
        }
        let said = Transport { bpm: self.bpm, beat_in_bar: self.beat % 4.0, playing: self.playing as i32, meter_num: 4, meter_den: 4 };
        self.beat += BLOCK as f64 * per_frame;
        said
    }

    /// The host's beat at a frame it has rendered.
    fn beat_at(&self, frame: u64) -> f64 {
        let s = self.stretches.iter().rev().find(|s| s.0 <= frame).expect("a frame the host has rendered");
        s.1 + (frame - s.0) as f64 * s.2
    }
}

impl Host {
    fn start(instance: Arc<Instance>) -> Host {
        Host::start_with(instance, None, None)
    }

    /// The same, in a host with a transport, said before each buffer.
    fn start_in(instance: Arc<Instance>, daw: Option<Arc<Mutex<Daw>>>) -> Host {
        Host::start_with(instance, daw, None)
    }

    /// The same, and with `input`: the pitch of a sine at half scale the host hands the instance on its left input,
    /// its right input silent.
    fn start_with(instance: Arc<Instance>, daw: Option<Arc<Mutex<Daw>>>, input: Option<f64>) -> Host {
        let rendered = Arc::new(Mutex::new(Vec::new()));
        let right_kept = Arc::new(Mutex::new(Vec::new()));
        let kept = right_kept.clone();
        let quit = Arc::new(AtomicBool::new(false));
        let stalls = Arc::new(AtomicU32::new(0));
        let (r, q, held) = (rendered.clone(), quit.clone(), stalls.clone());
        let thread = thread::spawn(move || {
            let (mut left, mut right) = (vec![0.0f32; BLOCK], vec![0.0f32; BLOCK]);
            let start = Instant::now();
            let mut frames = 0u64;
            while !q.load(Ordering::Relaxed) {
                if start.elapsed().as_secs_f64() - frames as f64 / RATE > 0.05 {
                    held.fetch_add(1, Ordering::Relaxed);
                }
                if let Some(daw) = &daw {
                    let said = daw.lock().unwrap().next(frames);
                    instance.transport(&said);
                }
                match input {
                    Some(pitch) => {
                        let tone: Vec<f32> = (0..BLOCK)
                            .map(|i| (0.5 * ((frames + i as u64) as f64 * pitch * std::f64::consts::TAU / RATE).sin()) as f32)
                            .collect();
                        instance.render_with(&mut left, &mut right, Some((&tone, &[0.0f32; BLOCK])));
                    }
                    None => instance.render(&mut left, &mut right),
                }
                r.lock().unwrap().extend_from_slice(&left);
                kept.lock().unwrap().extend_from_slice(&right);
                frames += BLOCK as u64;
                let due = Duration::from_secs_f64(frames as f64 / RATE);
                if let Some(wait) = due.checked_sub(start.elapsed()) {
                    thread::sleep(wait);
                }
            }
        });
        Host { rendered, right: right_kept, quit, stalls, thread: Some(thread) }
    }

    /// The frames rendered from `from` on.
    fn since(&self, from: usize) -> Vec<f32> {
        self.rendered.lock().unwrap()[from..].to_vec()
    }

    fn len(&self) -> usize {
        self.rendered.lock().unwrap().len()
    }
}

impl Drop for Host {
    fn drop(&mut self) {
        self.quit.store(true, Ordering::Relaxed);
        if let Some(t) = self.thread.take() {
            t.join().unwrap();
        }
    }
}

/// The pitch of a steady tone, by its rising zero crossings.
fn frequency(samples: &[f32]) -> f64 {
    let rising = samples.windows(2).filter(|w| w[0] <= 0.0 && w[1] > 0.0).count();
    rising as f64 * RATE / samples.len() as f64
}

/// Waits until `host` has rendered `seconds` more, and returns that stretch.
fn listen(host: &Host, seconds: f64) -> Vec<f32> {
    listen_all(&[host], seconds).pop().unwrap()
}

/// The same, for several hosts rendering side by side over the same stretch of time.
fn listen_all(hosts: &[&Host], seconds: f64) -> Vec<Vec<f32>> {
    let from: Vec<usize> = hosts.iter().map(|h| h.len()).collect();
    let want = (seconds * RATE) as usize;
    let deadline = Instant::now() + Duration::from_secs_f64(seconds * 4.0 + 2.0);
    while hosts.iter().zip(&from).any(|(h, f)| h.len() < f + want) && Instant::now() < deadline {
        thread::sleep(Duration::from_millis(20));
    }
    hosts.iter().zip(&from).map(|(h, f)| h.since(*f)).collect()
}

/// A loop in a host playing at 120 and then 90, heard against the host's beats. None when the machine held the
/// host's thread up meanwhile: the timing heard then is the machine's.
fn loop_in_a_playing_host() -> Option<()> {
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let daw = Daw::playing_at(120.0);
    let host = Host::start_in(instance.clone(), Some(daw.clone()));
    listen(&host, 1.0);
    let stalls = host.stalls.load(Ordering::Relaxed);
    let kept_time = || host.stalls.load(Ordering::Relaxed) == stalls;

    // Sonic Pi's own tempo is 60: at the host's 120 a beat is half a second
    instance.run("link\nlive_loop :t do\n  synth :beep, note: 84, attack: 0, sustain: 0, release: 0.05, amp: 0.8\n  sleep 1\nend\n");
    let from = host.len();
    let heard = listen(&host, 6.0);
    if !kept_time() {
        return None;
    }
    let starts = onsets(&heard);
    assert!(starts.len() >= 6, "the loop sounded {} times in six seconds", starts.len());
    let apart: Vec<f64> = starts.windows(2).map(|w| (w[1] - w[0]) as f64 / RATE).collect();
    assert!(apart.iter().all(|g| (g - 0.5).abs() < 0.003), "at the host's 120 the loop's sounds are not half a second apart: {apart:?}");

    // `link` waited for the host's bar line
    let beat = daw.lock().unwrap().beat_at((from + starts[0]) as u64);
    let from_bar = (beat % 4.0).min(4.0 - beat % 4.0);
    assert!(from_bar < 0.03, "the loop's first sound is at the host's beat {beat:.3}, not on a bar line");

    // the host slows to 90: two thirds of a second a beat
    daw.lock().unwrap().bpm = 90.0;
    listen(&host, 1.5);
    let from = host.len();
    let slower = onsets(&listen(&host, 5.0));
    if !kept_time() {
        return None;
    }
    let apart: Vec<f64> = slower.windows(2).map(|w| (w[1] - w[0]) as f64 / RATE).collect();
    let beats: Vec<f64> = slower.iter().map(|f| daw.lock().unwrap().beat_at((from + f) as u64)).collect();
    assert!(
        apart.len() >= 4 && apart.iter().all(|g| (g - 60.0 / 90.0).abs() < 0.003),
        "at the host's 90 the loop's sounds are not two thirds of a second apart: {apart:?}, at the host's beats {beats:?}"
    );
    Some(())
}

#[test]
fn a_loop_keeps_its_hosts_tempo_and_link_waits_for_its_bar() {
    let heard = (0..4).find_map(|_| loop_in_a_playing_host());
    assert!(heard.is_some(), "the machine held the host's thread up in each of four tries: nothing to judge the timing by");
}

/// Two short sounds a Sonic Pi beat apart, and the seconds between them as rendered.
fn seconds_a_beat(host: &Host, instance: &Instance) -> f64 {
    instance.run("synth :beep, note: 84, attack: 0, sustain: 0, release: 0.05, amp: 0.8\nsleep 1\nsynth :beep, note: 84, attack: 0, sustain: 0, release: 0.05, amp: 0.8\n");
    let starts = onsets(&listen(host, 3.0));
    assert_eq!(starts.len(), 2, "two sounds were played, {} heard", starts.len());
    (starts[1] - starts[0]) as f64 / RATE
}

/// A host with no transport says nothing of a tempo, and the timeline's own starting tempo (120, as a Link session's)
/// is not the host's: a beat is Sonic Pi's own second.
#[test]
fn without_a_hosts_transport_a_beat_is_a_second() {
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let host = Host::start(instance.clone());
    listen(&host, 1.0);
    let apart = seconds_a_beat(&host, &instance);
    assert!((apart - 1.0).abs() < 0.01, "with no host transport a beat took {apart:.3} s, not Sonic Pi's own second");
}

/// A host whose transport is at the tempo the timeline started at, and stopped: nothing of the timeline's moves when
/// it first says so, and its tempo is followed all the same.
#[test]
fn a_stopped_host_at_the_timelines_starting_tempo_is_followed() {
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let daw = Arc::new(Mutex::new(Daw { bpm: 120.0, playing: false, beat: 0.0, stretches: Vec::new() }));
    let host = Host::start_in(instance.clone(), Some(daw));
    listen(&host, 1.0);
    let apart = seconds_a_beat(&host, &instance);
    assert!((apart - 0.5).abs() < 0.01, "at the host's 120 a beat took {apart:.3} s, not half a second");
}

#[test]
fn the_hosts_input_sounds_through_live_audio() {
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let host = Host::start_with(instance.clone(), None, Some(330.0));
    let quiet = listen(&host, 0.5);
    assert!(peak(&quiet) < 1e-6, "the host's input is not heard until a program asks for it, peak {}", peak(&quiet));

    instance.run("live_audio :in\n");
    listen(&host, 1.5);
    let heard = listen(&host, 1.0);
    assert!(peak(&heard) > 0.05, "live_audio plays the host's input, peak {}", peak(&heard));
    let pitch = frequency(&heard);
    assert!((pitch - 330.0).abs() < 3.0, "what live_audio plays is the host's input at 330 Hz, heard {pitch:.1} Hz");

    instance.run("live_audio :in, :stop\n");
    listen(&host, 1.5);
    let after = listen(&host, 0.5);
    assert!(peak(&after) < 1e-3, "stopped, the input is not heard, peak {}", peak(&after));
}

#[test]
fn sound_out_plays_on_the_output_it_names() {
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let host = Host::start(instance.clone());
    listen(&host, 0.5);
    // on output 2 alone: amp: 0 keeps the fx from passing the sound on to both
    instance.run("with_fx :sound_out, output: 2, amp: 0 do\n  synth :sine, note: 69, sustain: 3, release: 0.1, amp: 0.5\nend\n");
    listen(&host, 1.5);
    let from = host.len();
    listen(&host, 1.0);
    let left = host.since(from);
    let right = host.right.lock().unwrap()[from..from + left.len()].to_vec();
    assert!(peak(&right) > 0.05, "sound_out's sound is on output 2, peak {}", peak(&right));
    assert!((frequency(&right) - 440.0).abs() < 3.0, "output 2 plays the sine, heard {:.1} Hz", frequency(&right));
    assert!(peak(&left) < 1e-3, "and not on output 1, peak {}", peak(&left));
}

#[test]
fn a_run_sounds_through_the_render() {
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let host = Host::start(instance.clone());
    let quiet = listen(&host, 0.5);
    assert!(peak(&quiet) < 1e-6, "silent before anything runs, peak {}", peak(&quiet));

    instance.run("synth :sine, note: 69, sustain: 2, release: 0.1, amp: 0.5\n");
    // the run's sound starts a schedule-ahead after it, once the synthdefs are in
    let heard = listen(&host, 1.5);
    let loud = heard.iter().position(|s| s.abs() > 0.05).expect("the sine sounds within a second and a half");
    let tone = &heard[loud + 4800..(loud + 4800 + 24000).min(heard.len())];
    let hz = frequency(tone);
    assert!((hz - 440.0).abs() < 5.0, "note 69 is 440 Hz, heard {hz:.1}");
    drop(host);
}

/// The windows of a copy (Clockwork's clockwork_client_copy, as js/lib/copy.js reads it): (source, slot, channels,
/// frames, the samples), in the order wanted.
fn copy_windows(copy: &[u8]) -> Vec<(u32, u32, u32, u32, Vec<f32>)> {
    let word = |at: usize| u32::from_le_bytes(copy[at..at + 4].try_into().unwrap());
    assert_eq!(word(0), 0x5950_4F43, "a copy");
    let align = |n: usize| (n + 7) & !7;
    let mut at = 32 + align(word(16) as usize) + align(word(20) as usize);
    (0..word(24))
        .map(|_| {
            let (source, slot, channels, frames) = (word(at), word(at + 4), word(at + 8), word(at + 12));
            at += 32;
            let n = (channels * frames) as usize;
            let samples = (0..n).map(|i| f32::from_le_bytes(copy[at + i * 4..at + i * 4 + 4].try_into().unwrap())).collect();
            at += align(n * 4);
            (source, slot, channels, frames, samples)
        })
        .collect()
}

#[test]
fn the_page_copy_holds_the_scopes_it_draws() {
    // what the page's scopes read in the plugin: the engine's output, and a scope on Sonic Pi's last slot (31: the
    // plugin profile once had four, and scsynth refused the rest)
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let host = Host::start(instance.clone());
    instance.run("with_fx :scope_out, scope_num: 31 do\n  synth :sine, note: 69, sustain: 2, release: 0.1, amp: 0.5\nend\n");
    let heard = listen(&host, 1.5);
    assert!(peak(&heard) > 0.05, "the sine sounds, peak {}", peak(&heard));

    const TAP: u32 = 1;
    const SCOPE: u32 = 2;
    let windows = copy_windows(&instance.copy(2, &[TAP, 31, 1024, SCOPE, 0, 2048]));
    assert_eq!(windows.len(), 2);
    let (source, slot, channels, frames, samples) = &windows[0];
    assert_eq!((*source, *slot, *channels, *frames), (TAP, 31, 2, 1024), "scope 31's window");
    assert!(peak(samples) > 0.05, "scope 31 holds the sine, peak {}", peak(samples));
    let (source, slot, channels, frames, samples) = &windows[1];
    assert_eq!((*source, *slot, *channels, *frames), (SCOPE, 0, 2, 2048), "the output's window");
    assert!(peak(samples) > 0.05, "the output holds the sine, peak {}", peak(samples));
    drop(host);
}

#[test]
fn two_instances_each_play_their_own() {
    let resources = resources();
    let a = Arc::new(Instance::new(RATE, BLOCK as u32, &resources).expect("instance a"));
    let b = Arc::new(Instance::new(RATE, BLOCK as u32, &resources).expect("instance b"));
    let c = Arc::new(Instance::new(RATE, BLOCK as u32, &resources).expect("instance c, left alone"));
    let (ha, hb, hc) = (Host::start(a.clone()), Host::start(b.clone()), Host::start(c.clone()));
    listen(&ha, 0.3);

    a.run("synth :sine, note: 69, sustain: 3, release: 0.1, amp: 0.5\n");
    b.run("synth :sine, note: 81, sustain: 8, release: 0.1, amp: 0.5\n");
    let heard = listen_all(&[&ha, &hb, &hc], 2.0);
    let (sa, sb, sc) = (&heard[0], &heard[1], &heard[2]);
    let tone = |s: &[f32], who: &str| {
        let at = s.iter().position(|x| x.abs() > 0.05).unwrap_or_else(|| panic!("{who} sounds"));
        frequency(&s[at + 4800..(at + 4800 + 24000).min(s.len())])
    };
    let (fa, fb) = (tone(sa, "a"), tone(sb, "b"));
    assert!((fa - 440.0).abs() < 5.0, "a plays its own note 69 (440 Hz), heard {fa:.1}");
    assert!((fb - 880.0).abs() < 8.0, "b plays its own note 81 (880 Hz), heard {fb:.1}");
    assert!(peak(sc) < 1e-6, "c, which ran nothing, stays silent: peak {}", peak(sc));

    a.stop();
    let after = listen_all(&[&ha, &hb], 1.6);
    assert!(peak(&after[0][after[0].len() - 4800..]) < 1e-4, "a's Stop fades it out within its second");
    assert!(peak(&after[1][after[1].len() - 4800..]) > 0.05, "b plays on through a's Stop");
}

/// What an instance says, kept: its records and log lines, as the page would hear them.
struct Heard(Mutex<Vec<(u32, Vec<u8>)>>);

unsafe extern "C" fn keep(ctx: *mut std::ffi::c_void, kind: u32, bytes: *const u8, size: u32) {
    // SAFETY: the Heard the test handed over, alive until it stops listening; `size` bytes for the call.
    let heard = unsafe { &*ctx.cast::<Heard>() };
    let bytes = if size == 0 { Vec::new() } else { unsafe { std::slice::from_raw_parts(bytes, size as usize) }.to_vec() };
    // what these tests look in: a numbered message without its number, and every line for the System log as one kind
    use sonic_pi_plugin::instance::{NUMBERED, SAYS_ENGINE, SAYS_LOADS, SAYS_LOG, SAYS_RUNTIME};
    let bytes = if NUMBERED.contains(&kind) { bytes[8..].to_vec() } else { bytes };
    let kind = if [SAYS_ENGINE, SAYS_RUNTIME, SAYS_LOADS].contains(&kind) { SAYS_LOG } else { kind };
    heard.0.lock().unwrap().push((kind, bytes));
}

/// Everything said, as it was said: its kind and its bytes, its number left on.
unsafe extern "C" fn keep_as_said(ctx: *mut std::ffi::c_void, kind: u32, bytes: *const u8, size: u32) {
    // SAFETY: as keep's.
    let heard = unsafe { &*ctx.cast::<Heard>() };
    let bytes = if size == 0 { Vec::new() } else { unsafe { std::slice::from_raw_parts(bytes, size as usize) }.to_vec() };
    heard.0.lock().unwrap().push((kind, bytes));
}

fn number(bytes: &[u8]) -> u64 {
    u64::from_le_bytes(bytes[..8].try_into().unwrap())
}

/// An editor that opens later hears what came before it: the lines printed, and the start of the thread still
/// running that printed them, in the order said, numbered below what is said after.
#[test]
fn an_editor_that_opens_later_hears_what_came_before() {
    use sonic_pi_plugin::instance::{NUMBERED, SAYS_EARLIER, SAYS_RECORD};
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let host = Host::start(instance.clone());
    // printed with no editor listening
    instance.run("live_loop :printing do\n  puts \"before the editor\"\n  sleep 0.25\nend\n");
    listen(&host, 1.0);
    let heard = Heard(Mutex::new(Vec::new()));
    // SAFETY: `heard` outlives the listening, which stops below.
    unsafe { instance.listen(Some(keep_as_said), (&heard as *const Heard).cast_mut().cast()) };
    instance.earlier();
    listen(&host, 0.5);
    unsafe { instance.listen(None, std::ptr::null_mut()) };
    drop(host);
    let said = heard.0.lock().unwrap().clone();
    let earlier: Vec<(u32, u64, &[u8])> = said.iter().filter(|(k, _)| *k == SAYS_EARLIER)
        .map(|(_, b)| (u32::from(b[0]), number(&b[1..]), &b[9..])).collect();
    let has = |b: &[u8], text: &str| b.windows(text.len()).any(|w| w == text.as_bytes());
    assert!(earlier.iter().any(|(k, _, b)| *k == SAYS_RECORD && has(b, "/sonic-pi/output") && has(b, "before the editor")),
            "the line printed before the editor opened");
    let start = earlier.iter().position(|(k, _, b)| *k == SAYS_RECORD && has(b, "/sonic-pi/thread_start") && has(b, "printing"));
    let first_line = earlier.iter().position(|(_, _, b)| has(b, "before the editor"));
    assert!(start.is_some() && start < first_line, "the loop's start, before its lines, so they keep its name");
    assert!(earlier.windows(2).all(|w| w[0].1 < w[1].1), "in the order said");
    // and what is said live after it is numbered after it: the page shows each once
    let last = earlier.last().map(|e| e.1).unwrap();
    let end = said.iter().rposition(|(k, _)| *k == SAYS_EARLIER).unwrap();
    assert!(said[end + 1..].iter().filter(|(k, _)| NUMBERED.contains(k)).all(|(_, b)| number(b) > last));
}

fn said(heard: &Heard, needle: &str, seconds: f64) -> bool {
    let deadline = Instant::now() + Duration::from_secs_f64(seconds);
    loop {
        let found = heard.0.lock().unwrap().iter().any(|(kind, b)| {
            *kind == sonic_pi_plugin::instance::SAYS_RECORD && b.windows(needle.len()).any(|w| w == needle.as_bytes())
        });
        if found || Instant::now() > deadline {
            return found;
        }
        thread::sleep(Duration::from_millis(20));
    }
}

/// A host whose locale writes a decimal comma (setlocale(LC_ALL, "de_DE") in its process) must not change what a
/// program's numbers are: 1.0 / 4 is 0.25, printed with a point, as Sonic Pi prints it everywhere.
#[test]
fn numbers_are_the_same_under_a_host_with_a_decimal_comma() {
    // SAFETY: this test's process; setlocale is not called concurrently here (the tests run one at a time).
    let set = unsafe { libc_setlocale(6 /* LC_ALL */, c"de_DE.UTF-8".as_ptr()) };
    assert!(!set.is_null(), "this machine has the de_DE.UTF-8 locale");
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let heard = Heard(Mutex::new(Vec::new()));
    // SAFETY: `heard` outlives the listening, which stops below.
    unsafe { instance.listen(Some(keep), (&heard as *const Heard).cast_mut().cast()) };
    let host = Host::start(instance.clone());
    instance.run("puts 1.0 / 4\nputs 2.5 * 2\n");
    let point = said(&heard, "0.25", 3.0);
    let comma = said(&heard, "0,25", 0.1);
    unsafe { instance.listen(None, std::ptr::null_mut()) };
    unsafe { libc_setlocale(6, c"C".as_ptr()) };
    drop(host);
    assert!(point && !comma, "1.0 / 4 printed as 0.25 (point {point}, comma {comma})");
}

unsafe extern "C" {
    #[link_name = "setlocale"]
    fn libc_setlocale(category: i32, locale: *const std::ffi::c_char) -> *mut std::ffi::c_char;
}

fn logged(heard: &Heard, needle: &str) -> Option<String> {
    heard.0.lock().unwrap().iter().find_map(|(kind, b)| {
        let text = String::from_utf8_lossy(b);
        (*kind == sonic_pi_plugin::instance::SAYS_LOG && text.contains(needle)).then(|| text.into_owned())
    })
}

/// A sample loads into the instance's engine (its assets area) and sounds.
#[test]
fn a_sample_loads_and_sounds() {
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let heard = Heard(Mutex::new(Vec::new()));
    // SAFETY: `heard` outlives the listening, which stops below.
    unsafe { instance.listen(Some(keep), (&heard as *const Heard).cast_mut().cast()) };
    let host = Host::start(instance.clone());
    instance.run("sample :loop_industrial\n");
    let sound = listen(&host, 2.0);
    unsafe { instance.listen(None, std::ptr::null_mut()) };
    drop(host);
    let said: Vec<String> = heard.0.lock().unwrap().iter()
        .filter(|(k, _)| *k == sonic_pi_plugin::instance::SAYS_LOG)
        .map(|(_, b)| String::from_utf8_lossy(b).into_owned()).collect();
    assert_eq!(logged(&heard, "/fail"), None, "the engine refused nothing; the log: {said:#?}");
    assert!(peak(&sound) > 0.05, "the sample sounds: peak {}", peak(&sound));
}

/// The engine has the real-time memory the desktop app gives scsynth: a hundred reverbs at once, each holding its
/// delay lines in that memory, all start.
#[test]
fn many_reverbs_at_once_have_the_memory_they_need() {
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let heard = Heard(Mutex::new(Vec::new()));
    // SAFETY: as above.
    unsafe { instance.listen(Some(keep), (&heard as *const Heard).cast_mut().cast()) };
    let host = Host::start(instance.clone());
    instance.run("100.times do\n  with_fx :reverb, room: 0.9 do\n    play 60, release: 0.5, amp: 0.05\n  end\nend\n");
    let sound = listen(&host, 1.5);
    unsafe { instance.listen(None, std::ptr::null_mut()) };
    drop(host);
    assert_eq!(logged(&heard, "/fail"), None, "the engine refused nothing");
    assert!(peak(&sound) > 0.01, "they sound: peak {}", peak(&sound));
}

/// A folder of the player's own samples, outside Sonic Pi's: two files copied from the built-in ones.
fn own_samples() -> PathBuf {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let etc = std::env::var("SONIC_PI_ETC")
        .map(PathBuf::from)
        .unwrap_or_else(|_| manifest.join("../../etc"));
    let dir = manifest.join("target/test-own-samples");
    std::fs::create_dir_all(&dir).unwrap();
    for (from, to) in [("bd_haus.flac", "kick.flac"), ("sn_dolf.flac", "snare.flac")] {
        let to = dir.join(to);
        if !to.exists() {
            std::fs::copy(etc.join("samples").join(from), &to).unwrap();
        }
    }
    dir
}

/// What a program plays, heard: no /fail from the engine, and something louder than `level`.
fn plays(code: &str, level: f32) {
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let heard = Heard(Mutex::new(Vec::new()));
    // SAFETY: `heard` outlives the listening, which stops below.
    unsafe { instance.listen(Some(keep), (&heard as *const Heard).cast_mut().cast()) };
    let host = Host::start(instance.clone());
    instance.run(code);
    let sound = listen(&host, 2.0);
    unsafe { instance.listen(None, std::ptr::null_mut()) };
    drop(host);
    let said: Vec<String> = heard.0.lock().unwrap().iter()
        .filter(|(k, _)| *k == sonic_pi_plugin::instance::SAYS_LOG || *k == sonic_pi_plugin::instance::SAYS_ERROR)
        .map(|(_, b)| String::from_utf8_lossy(b).into_owned()).collect();
    assert!(!said.iter().any(|s| s.contains("/fail")), "the engine refused nothing: {said:#?}");
    assert!(peak(&sound) > level, "{code:?} sounds: peak {}; the log: {said:#?}", peak(&sound));
}

/// A sample of the player's own, by its path, as native Sonic Pi has always taken one.
#[test]
fn a_sample_by_its_path_sounds() {
    let kick = own_samples().join("kick.flac");
    plays(&format!("sample {:?}\n", kick.to_string_lossy()), 0.05);
}

/// A folder of the player's own samples, and one picked from it by its place.
#[test]
fn a_sample_from_a_folder_sounds() {
    let dir = own_samples();
    plays(&format!("sample {:?}, 1\n", dir.to_string_lossy()), 0.05);
}

/// A quickstart card's Stop: its run, wrapped as the page wraps a card's (with_fx :scope_out, in the cards' group),
/// stopped by its job, fades out and goes.
#[test]
fn a_cards_run_stops_by_its_job() {
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let heard = Heard(Mutex::new(Vec::new()));
    // SAFETY: `heard` outlives the listening, which stops below.
    unsafe { instance.listen(Some(keep), (&heard as *const Heard).cast_mut().cast()) };
    let host = Host::start(instance.clone());
    let code = "use_arg_checks true; __warn_unknown_opts true; with_fx :scope_out, scope_num: 1 do\n\
                live_loop :drone do\n  synth :saw, note: 50, sustain: 1, release: 0.1, amp: 0.4\n  sleep 1\nend\nend\n";
    instance.run_in(code, 2000);
    let playing = listen(&host, 1.5);
    let job = heard.0.lock().unwrap().iter().find_map(|(k, b)| {
        (*k == sonic_pi_plugin::instance::SAYS_RUN).then(|| i32::from_le_bytes([b[0], b[1], b[2], b[3]]))
    });
    let job = job.expect("the run says its job");
    instance.stop_run(job, 0.25);
    let after = listen(&host, 2.5);
    unsafe { instance.listen(None, std::ptr::null_mut()) };
    drop(host);
    assert!(peak(&playing) > 0.05, "the card's loop sounds: peak {}", peak(&playing));
    assert!(peak(&after[after.len() - 24_000..]) < 1e-4, "stopped by its job, it is silent: peak {}", peak(&after[after.len() - 24_000..]));
}

/// The process table the page reads for a card's run, as the plugin last said it: the run is live (the page's deck
/// keeps a card's Stop live by it, web/runtime.js liveRuns).
#[test]
fn a_cards_run_is_live_in_the_table_the_page_reads() {
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let heard = Heard(Mutex::new(Vec::new()));
    // SAFETY: `heard` outlives the listening, which stops below.
    unsafe { instance.listen(Some(keep), (&heard as *const Heard).cast_mut().cast()) };
    let host = Host::start(instance.clone());
    let code = "use_arg_checks true; __warn_unknown_opts true; with_fx :scope_out, scope_num: 1 do\n\
                live_loop :drone do\n  synth :saw, note: 50, sustain: 1, release: 0.1, amp: 0.4\n  sleep 1\nend\nend\n";
    instance.run_in(code, 2000);
    listen(&host, 1.5);
    unsafe { instance.listen(None, std::ptr::null_mut()) };
    drop(host);
    let said = heard.0.lock().unwrap();
    let job = said.iter().find_map(|(k, b)| (*k == sonic_pi_plugin::instance::SAYS_RUN).then(|| i32::from_le_bytes([b[0], b[1], b[2], b[3]]))).expect("a job");
    let tables: Vec<Vec<f64>> = said.iter().filter(|(k, _)| *k == 6)
        .map(|(_, b)| b.chunks_exact(8).map(|c| f64::from_le_bytes(c.try_into().unwrap())).collect()).collect();
    let last = tables.last().expect("a table was said");
    // web/runtime.js PROCESS_FIELDS: uid parent job kind state line wake beat bpm active events redefs ended node group
    let rows: Vec<&[f64]> = last.chunks_exact(15).collect();
    let live = |r: &[f64]| { let (kind, state) = (r[3], r[4]); (1.0..=5.0).contains(&kind) && state <= 2.0 || kind == 6.0 && state < 3.0 || (kind == 7.0 || kind == 8.0) && state == 0.0 };
    let runs_live: Vec<f64> = rows.iter().filter(|r| live(r)).filter_map(|r| {
        let mut at = *r;
        for _ in 0..64 {
            if at[3] == 0.0 { return Some(at[2]); }
            at = *rows.iter().find(|x| x[0] == at[1])?;
        }
        None
    }).collect();
    let shown: Vec<String> = rows.iter().map(|r| format!("uid {} parent {} job {} kind {} state {} group {}", r[0], r[1], r[2], r[3], r[4], r[14])).collect();
    assert!(runs_live.contains(&f64::from(job)), "job {job} is live in the last of {} tables: {shown:#?}", tables.len());
}

/// An Examples card's Stop on a real example (wizard/tilburg_2.rb): its live loops, one inside a reverb, wrapped as
/// the page wraps a card and stopped by its job, fade out and go.
#[test]
fn an_example_card_stops_by_its_job() {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let example = std::fs::read_to_string(manifest.join("../../etc/examples/wizard/tilburg_2.rb")).unwrap();
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let heard = Heard(Mutex::new(Vec::new()));
    // SAFETY: `heard` outlives the listening, which stops below.
    unsafe { instance.listen(Some(keep), (&heard as *const Heard).cast_mut().cast()) };
    let host = Host::start(instance.clone());
    let code = format!("use_arg_checks true; __warn_unknown_opts true; with_fx :scope_out, scope_num: 1 do\n{example}\nend\n");
    instance.run_in(&code, 2000);
    let playing = listen(&host, 3.0);
    let job = heard.0.lock().unwrap().iter().find_map(|(k, b)| {
        (*k == sonic_pi_plugin::instance::SAYS_RUN).then(|| i32::from_le_bytes([b[0], b[1], b[2], b[3]]))
    }).expect("the run says its job");
    instance.stop_run(job, 0.25);
    let after = listen(&host, 3.0);
    unsafe { instance.listen(None, std::ptr::null_mut()) };
    drop(host);
    let tail = &after[after.len() - 48_000..];
    let said: Vec<String> = heard.0.lock().unwrap().iter()
        .filter(|(k, _)| *k == sonic_pi_plugin::instance::SAYS_LOG || *k == sonic_pi_plugin::instance::SAYS_ERROR)
        .map(|(_, b)| String::from_utf8_lossy(b).into_owned()).filter(|s| !s.contains("[clock]")).collect();
    assert!(peak(&playing) > 0.05, "the example sounds: peak {}", peak(&playing));
    assert!(peak(tail) < 1e-4, "stopped by its job (run {job}), it is silent: peak {}; the log: {said:#?}", peak(tail));
}

/// The records a program makes, as the page will read them, kept for test/read-records.mjs: one per line, hex.
#[test]
fn records_are_kept_for_the_page_reader() {
    let instance = Arc::new(Instance::new(RATE, BLOCK as u32, &resources()).expect("an instance"));
    let heard = Heard(Mutex::new(Vec::new()));
    // SAFETY: `heard` outlives the listening, which stops below.
    unsafe { instance.listen(Some(keep), (&heard as *const Heard).cast_mut().cast()) };
    let host = Host::start(instance.clone());
    instance.run("puts \"hello from the plugin\"\nplay 60\nsleep 0.25\nputs [1, 2.5, :three, nil, true]\n");
    listen(&host, 1.5);
    unsafe { instance.listen(None, std::ptr::null_mut()) };
    drop(host);
    let lines: Vec<String> = heard.0.lock().unwrap().iter()
        .filter(|(k, _)| *k == sonic_pi_plugin::instance::SAYS_RECORD)
        .map(|(_, b)| b.iter().map(|x| format!("{x:02x}")).collect()).collect();
    assert!(!lines.is_empty(), "the program made records");
    std::fs::write(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/records.hex"), lines.join("\n")).unwrap();
}
