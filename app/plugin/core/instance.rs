// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! One Sonic Pi (core/instance.h): the engine the host renders, its front, and
//! the runtime. A handle that frees it when dropped.

use std::ffi::{CString, c_char, c_int, c_void};
use std::ptr::NonNull;

#[repr(C)]
pub struct RawInstance {
    _private: [u8; 0],
}

#[repr(C)]
struct Config {
    struct_bytes: u32,
    sample_rate: f64,
    max_frames: u32,
    resources: *const c_char,
    time_origin_ntp: f64,
}

/// What the instance says (instance.h): a record for the page, a log line, whether it runs, a run's job, an error.
pub const SAYS_RECORD: u32 = 1;
pub const SAYS_LOG: u32 = 2;
pub const SAYS_STATE: u32 = 3;
pub const SAYS_RUN: u32 = 4;
pub const SAYS_ERROR: u32 = 5;
pub const SAYS_EARLIER: u32 = 7;
pub const SAYS_ENGINE: u32 = 8;
pub const SAYS_RUNTIME: u32 = 9;
pub const SAYS_LOADS: u32 = 10;
pub const SAYS_TEMPO: u32 = 11;
pub const SAYS_WARP: u32 = 12;
pub const SAYS_CLOCK: u32 = 13;
/// The kinds whose messages start with their number, a little-endian u64 (instance.h).
pub const NUMBERED: [u32; 5] = [SAYS_RECORD, SAYS_LOG, SAYS_ENGINE, SAYS_RUNTIME, SAYS_LOADS];

/// The host's own transport at the next render's first frame (`SpTransport`, instance.h).
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Transport {
    /// Its tempo; 0: it does not say.
    pub bpm: f64,
    /// Where it is in its bar, in quarter notes; below 0: it does not say.
    pub beat_in_bar: f64,
    /// 1 playing, 0 stopped; below 0: it does not say.
    pub playing: i32,
    /// Its time signature; 0: it does not say.
    pub meter_num: i32,
    pub meter_den: i32,
}

pub type Listener = unsafe extern "C" fn(ctx: *mut c_void, kind: u32, bytes: *const u8, size: u32);

unsafe extern "C" {
    fn sp_instance_new(config: *const Config, why: *mut c_char, why_bytes: u32) -> *mut RawInstance;
    fn sp_instance_free(instance: *mut RawInstance);
    fn sp_instance_loading(instance: *mut RawInstance) -> u32;
    fn sp_instance_reconfigure(instance: *mut RawInstance, rate: f64, max_frames: u32) -> c_int;
    fn sp_instance_render(
        instance: *mut RawInstance,
        out: *const *mut f32,
        out_channels: u32,
        input: *const *const f32,
        in_channels: u32,
        frames: u32,
    ) -> u32;
    fn sp_instance_transport(instance: *mut RawInstance, transport: *const Transport);
    fn sp_instance_align_bars(instance: *mut RawInstance, on: c_int);
    fn sp_instance_time_warp(instance: *mut RawInstance, ms: f64);
    fn sp_instance_run(instance: *mut RawInstance, code: *const c_char, group: c_int);
    fn sp_instance_stop(instance: *mut RawInstance);
    fn sp_instance_stop_run(instance: *mut RawInstance, job: c_int, fade: f64);
    fn sp_instance_listen(instance: *mut RawInstance, listener: Option<Listener>, ctx: *mut c_void);
    fn sp_instance_earlier(instance: *mut RawInstance);
    fn sp_instance_copy(
        instance: *mut RawInstance,
        parts: u32,
        wants: *const u32,
        count: u32,
        out: *mut c_void,
        bytes: u32,
    ) -> u32;
}

/// An instance this side owns.
pub struct Instance(NonNull<RawInstance>);

// SAFETY: every call but render is any thread's (instance.h); render is the caller's to keep to one thread.
unsafe impl Send for Instance {}
unsafe impl Sync for Instance {}

impl Instance {
    /// An engine at `sample_rate`, rendered in at most `max_frames` at a time, with Sonic Pi's files under
    /// `resources` (synthdefs/, samples/, buffers/). Blocks while the runtime boots.
    pub fn new(sample_rate: f64, max_frames: u32, resources: &str) -> Result<Instance, String> {
        Instance::with_origin(sample_rate, max_frames, resources, 0.0)
    }

    /// An offline instance. Its time is `time_origin_ntp` (NTP seconds, not 0) plus the frames rendered so far, and
    /// it reads no clock. Each render first runs the Sonic Pi code due up to the end of its buffer, so render can
    /// block: use it for offline rendering and tests, not for live audio.
    pub fn offline(sample_rate: f64, max_frames: u32, resources: &str, time_origin_ntp: f64) -> Result<Instance, String> {
        Instance::with_origin(sample_rate, max_frames, resources, time_origin_ntp)
    }

    fn with_origin(sample_rate: f64, max_frames: u32, resources: &str, time_origin_ntp: f64) -> Result<Instance, String> {
        let resources = CString::new(resources).map_err(|_| "a NUL in the resources path".to_owned())?;
        let config = Config {
            struct_bytes: size_of::<Config>() as u32,
            sample_rate,
            max_frames,
            resources: resources.as_ptr(),
            time_origin_ntp,
        };
        let mut why = [0 as c_char; 512];
        // SAFETY: a config that says its own size, live for the call; `why` holds what it is told it holds.
        let raw = unsafe { sp_instance_new(&config, why.as_mut_ptr(), why.len() as u32) };
        NonNull::new(raw).map(Instance).ok_or_else(|| {
            // SAFETY: written NUL-terminated within its length, or left zeroed.
            unsafe { std::ffi::CStr::from_ptr(why.as_ptr()) }.to_string_lossy().into_owned()
        })
    }

    pub fn as_ptr(&self) -> *mut RawInstance {
        self.0.as_ptr()
    }

    /// Calls sp_instance_reconfigure with a new sample rate and largest buffer size, as vst3/shell.cpp does from
    /// setupProcessing. A new rate rebuilds the engine. Returns true on success.
    pub fn reconfigure(&self, sample_rate: f64, max_frames: u32) -> bool {
        // SAFETY: a live instance. Hosts call setupProcessing with audio stopped, so nothing is rendering it.
        unsafe { sp_instance_reconfigure(self.0.as_ptr(), sample_rate, max_frames) == 0 }
    }

    /// How many synthdefs and samples it has asked for that have neither loaded nor failed.
    pub fn loading(&self) -> u32 {
        // SAFETY: a live instance; the count is read under the instance's own lock.
        unsafe { sp_instance_loading(self.0.as_ptr()) }
    }

    /// Stereo out, no input: the audio thread's.
    pub fn render(&self, left: &mut [f32], right: &mut [f32]) {
        self.render_with(left, right, None);
    }

    /// Stereo out, from the host's stereo input when it has one: the audio thread's.
    pub fn render_with(&self, left: &mut [f32], right: &mut [f32], input: Option<(&[f32], &[f32])>) {
        let frames = left.len().min(right.len()).min(input.map_or(usize::MAX, |i| i.0.len().min(i.1.len()))) as u32;
        let out = [left.as_mut_ptr(), right.as_mut_ptr()];
        let ins = input.map(|i| [i.0.as_ptr(), i.1.as_ptr()]);
        let (in_ptr, in_channels) = ins.as_ref().map_or((std::ptr::null(), 0), |i| (i.as_ptr(), 2));
        // SAFETY: a live instance; every pointer names `frames` floats that live across the call.
        unsafe { sp_instance_render(self.as_ptr(), out.as_ptr(), 2, in_ptr, in_channels, frames) };
    }

    /// The host's transport at the next render's first frame: the render's thread, before it.
    pub fn transport(&self, transport: &Transport) {
        // SAFETY: a live instance; the struct is read during the call.
        unsafe { sp_instance_transport(self.as_ptr(), transport) };
    }

    /// Whether the instance's bars follow its host's (as it starts) or only its tempo.
    pub fn align_bars(&self, on: bool) {
        // SAFETY: a live instance.
        unsafe { sp_instance_align_bars(self.as_ptr(), on as c_int) };
    }

    /// The global time warp, in milliseconds: every sound from then on is that much later (below 0, earlier).
    pub fn time_warp(&self, ms: f64) {
        // SAFETY: a live instance.
        unsafe { sp_instance_time_warp(self.as_ptr(), ms) };
    }

    pub fn run(&self, code: &str) {
        self.run_in(code, 0);
    }

    /// A program as a new run in `group` (the page's: a buffer's, the cards').
    pub fn run_in(&self, code: &str, group: i32) {
        let code = CString::new(code.replace('\0', "")).unwrap_or_default();
        // SAFETY: a live instance and a NUL-terminated string, copied before the call returns.
        unsafe { sp_instance_run(self.as_ptr(), code.as_ptr(), group) };
    }

    /// One run stops, by its job, its sounds faded over `fade` seconds.
    pub fn stop_run(&self, job: i32, fade: f64) {
        // SAFETY: a live instance.
        unsafe { sp_instance_stop_run(self.as_ptr(), job, fade) };
    }

    pub fn stop(&self) {
        // SAFETY: a live instance.
        unsafe { sp_instance_stop(self.as_ptr()) };
    }

    /// A copy of the engine, as the page's Clockwork reads it (clockwork_client_copy): `wants` three numbers a window
    /// (source, slot, frames). Empty when there is nothing to copy.
    pub fn copy(&self, parts: u32, wants: &[u32]) -> Vec<u8> {
        let count = (wants.len() / 3) as u32;
        // SAFETY (both): a live instance; `wants` holds `count` wants, `out` the bytes the first call said.
        let size = unsafe { sp_instance_copy(self.as_ptr(), parts, wants.as_ptr(), count, std::ptr::null_mut(), 0) };
        let mut out = vec![0u8; size as usize];
        let n = unsafe { sp_instance_copy(self.as_ptr(), parts, wants.as_ptr(), count, out.as_mut_ptr().cast(), size) };
        out.truncate(n as usize);
        out
    }

    /// What came before, to the listener as SAYS_EARLIER (instance.h): for an editor that has just opened.
    pub fn earlier(&self) {
        // SAFETY: a live instance.
        unsafe { sp_instance_earlier(self.as_ptr()) };
    }

    /// # Safety
    /// `ctx` must stay valid for `listener` until this is called again with another, or with None.
    pub unsafe fn listen(&self, listener: Option<Listener>, ctx: *mut c_void) {
        // SAFETY: as the caller promises.
        unsafe { sp_instance_listen(self.as_ptr(), listener, ctx) };
    }
}

impl Drop for Instance {
    fn drop(&mut self) {
        // SAFETY: ours, freed once.
        unsafe { sp_instance_free(self.as_ptr()) };
    }
}
