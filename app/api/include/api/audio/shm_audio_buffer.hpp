//  Audio buffer — shared protocol header.
//
//  Fixed-layout single-producer / single-consumer ring of interleaved
//  float audio. The same struct is laid out in two storage segments:
//
//   - Native: inside the cross-process server_shm segment. Sonic Pi's
//     GUI reads recordings while supersonic runs in a separate process.
//     C++ tests read the slots in-place.
//   - Web: inside ring_buffer_storage (the WASM SharedArrayBuffer). JS
//     and Playwright read directly via typed-array views.
//
//  One wire format, one reader/writer. The data array is inline because
//  both segments are flat fixed-layout regions with no dynamic allocator.
//
//  Slot 0 is the master output mix, written by the audio thread's
//  post-block hook in audio_processor.cpp while the slot's `enabled`
//  flag is set; JS startCapture/stopCapture toggle that flag. Slots
//  1..N-1 are written by AudioOut2 UGen instances (DelayUGens.cpp).
//  User synthdefs use AudioOut2.ar(slot, signal) to tap stems or FX
//  into those slots.
//
//  Copyright (C) 2026 SuperSonic contributors.
//  Dual-licensed under MIT and GPLv3-or-later, at the user's option.

#pragma once

#include <atomic>
#include <cstddef>
#include <cstdint>
#include <cstring>

namespace detail_shm_audio {

// Slot count. Must be small enough that the inline data arrays fit
// inside ring_buffer_storage / server_shm at the configured ring size.
inline constexpr uint32_t MAX_SHM_AUDIO_BUFFERS = 4;
inline constexpr uint32_t SHM_AUDIO_MASTER_SLOT = 0;

// Per-slot ring capacity in frames (interleaved float).
// SUPERSONIC_SHM_AUDIO_SECONDS sets the duration: 1s in production, the
// test build bumps it so test windows don't wrap. The macro lives in
// this header (not shared_memory.h) so the header has no upstream
// dependencies and can be included by shared_memory.h itself.
#ifndef SUPERSONIC_SHM_AUDIO_SECONDS
#define SUPERSONIC_SHM_AUDIO_SECONDS 1
#endif
inline constexpr uint32_t SHM_AUDIO_SAMPLE_RATE = 48000;
inline constexpr uint32_t SHM_AUDIO_SECONDS     = SUPERSONIC_SHM_AUDIO_SECONDS;
inline constexpr uint32_t SHM_AUDIO_FRAMES      = SHM_AUDIO_SAMPLE_RATE * SHM_AUDIO_SECONDS;
inline constexpr uint32_t SHM_AUDIO_CHANNELS    = 2;
// Per-slot header bytes (atomics + sizes + 64-bit write_position +
// padding). Guaranteed 32 by the static_assert on offsetof(..., data).
inline constexpr uint32_t SHM_AUDIO_HEADER_SIZE = 32;

struct alignas(16) shm_audio_buffer {
    // Producer toggles this; consumers gate on it. 0 = idle / no writes,
    // 1 = active / writes flowing. Atomic because consumer may poll from
    // a different thread / process / browser worker.
    std::atomic<uint32_t> enabled;
    uint32_t              sample_rate;
    uint32_t              channels;
    uint32_t              capacity_frames;

    // Total frames the writer has produced since the slot was activated.
    // Reader subtracts capacity_frames to find the oldest still-readable
    // frame; falling more than capacity_frames behind = data loss (gap).
    std::atomic<uint64_t> write_position;

    // Pad to 32 bytes (header) so data starts at a 16-aligned offset.
    uint32_t              _padding[2];

    // Interleaved float ring [ch0_f0, ch1_f0, ch0_f1, ch1_f1, ...].
    // Wraps modulo capacity_frames.
    float                 data[SHM_AUDIO_FRAMES * SHM_AUDIO_CHANNELS];
};

static_assert(std::is_trivially_destructible<shm_audio_buffer>::value,
              "shm_audio_buffer must be trivially destructible (lives in shm)");
static_assert(offsetof(shm_audio_buffer, data) == 32,
              "shm_audio_buffer header must be 32 bytes (data 16-aligned)");

inline constexpr uint32_t SHM_AUDIO_SLOT_SIZE  = sizeof(shm_audio_buffer);
inline constexpr uint32_t SHM_AUDIO_TOTAL_SIZE =
    MAX_SHM_AUDIO_BUFFERS * SHM_AUDIO_SLOT_SIZE;

// ──── Producer side ─────────────────────────────────────────────────────
//
// Activate a slot at a given sample rate / channel count. Returns false
// if the slot pointer is null. After activate(), call write*() per audio
// block; deactivate() to stop writes.

class shm_audio_buffer_writer {
public:
    shm_audio_buffer_writer() = default;
    explicit shm_audio_buffer_writer(shm_audio_buffer* buf) : _buf(buf) {}

    bool valid() const { return _buf != nullptr; }

    // One-time slot setup. `channels` must be <= SHM_AUDIO_CHANNELS;
    // `capacity_frames` must be <= SHM_AUDIO_FRAMES (the inline ring size).
    bool activate(uint32_t channels, uint32_t sample_rate,
                  uint32_t capacity_frames) {
        if (!_buf) return false;
        if (channels == 0 || channels > SHM_AUDIO_CHANNELS) return false;
        if (capacity_frames == 0 || capacity_frames > SHM_AUDIO_FRAMES) return false;
        _buf->channels = channels;
        _buf->sample_rate = sample_rate;
        _buf->capacity_frames = capacity_frames;
        _buf->write_position.store(0, std::memory_order_relaxed);
        memset(_buf->data, 0, sizeof(_buf->data));
        _buf->enabled.store(1, std::memory_order_release);
        return true;
    }

    void deactivate() {
        if (!_buf) return;
        _buf->enabled.store(0, std::memory_order_release);
    }

    // Append num_frames of audio. channel_data[ch] points to num_frames
    // samples for that channel. Real-time safe: no allocation, lock-free,
    // atomic write_position update.
    void write(const float* const* channel_data, uint32_t num_frames) {
        if (!_buf || num_frames == 0) return;
        uint64_t pos = _buf->write_position.load(std::memory_order_relaxed);
        uint32_t cap = _buf->capacity_frames;
        uint32_t channels = _buf->channels;
        uint32_t slot = static_cast<uint32_t>(pos % cap);
        uint32_t first = (cap - slot < num_frames) ? (cap - slot) : num_frames;
        float* dst = _buf->data + slot * channels;
        for (uint32_t f = 0; f < first; ++f) {
            for (uint32_t c = 0; c < channels; ++c)
                dst[f * channels + c] = channel_data[c][f];
        }
        if (first < num_frames) {
            uint32_t wrap = num_frames - first;
            dst = _buf->data;
            for (uint32_t f = 0; f < wrap; ++f) {
                for (uint32_t c = 0; c < channels; ++c)
                    dst[f * channels + c] = channel_data[c][first + f];
            }
        }
        _buf->write_position.store(pos + num_frames, std::memory_order_release);
    }

    // Variant for already-interleaved input.
    void write_interleaved(const float* data, uint32_t num_frames) {
        if (!_buf || num_frames == 0) return;
        uint64_t pos = _buf->write_position.load(std::memory_order_relaxed);
        uint32_t cap = _buf->capacity_frames;
        uint32_t channels = _buf->channels;
        uint32_t slot = static_cast<uint32_t>(pos % cap);
        uint32_t first = (cap - slot < num_frames) ? (cap - slot) : num_frames;
        memcpy(_buf->data + slot * channels, data,
               static_cast<size_t>(first) * channels * sizeof(float));
        if (first < num_frames) {
            memcpy(_buf->data, data + first * channels,
                   static_cast<size_t>(num_frames - first) * channels * sizeof(float));
        }
        _buf->write_position.store(pos + num_frames, std::memory_order_release);
    }

    shm_audio_buffer* slot() const { return _buf; }

private:
    shm_audio_buffer* _buf = nullptr;
};

// ──── Consumer side ─────────────────────────────────────────────────────
//
// Reads new frames since the last call. Tracks read position locally;
// the writer's authoritative position is in the shm. If the reader falls
// more than capacity_frames behind, pull() reports a gap and resyncs.

class shm_audio_buffer_reader {
public:
    shm_audio_buffer_reader() = default;
    explicit shm_audio_buffer_reader(shm_audio_buffer* buf) : _buf(buf) {}

    bool valid() const { return _buf != nullptr; }
    bool is_active() const {
        return _buf && _buf->enabled.load(std::memory_order_acquire) != 0;
    }
    uint32_t sample_rate() const { return _buf ? _buf->sample_rate : 0; }
    uint32_t channels()    const { return _buf ? _buf->channels    : 0; }
    uint32_t capacity_frames() const { return _buf ? _buf->capacity_frames : 0; }

    // Snapshot the writer's current position. Useful for catching up
    // cleanly at the start of a session: call seek_to_live(), then loop
    // on pull() until done.
    uint64_t writer_position() const {
        return _buf ? _buf->write_position.load(std::memory_order_acquire) : 0;
    }

    void seek_to_live() { _last_read_pos = writer_position(); }

    // Reset to the absolute start of the buffer (only meaningful if the
    // reader was created before any writes happened). Used by tests that
    // want to capture from t=0 of a session.
    void seek_to_start() { _last_read_pos = 0; }

    // Pull up to max_frames of new audio. Output is interleaved
    // [ch0_f0, ch1_f0, ch0_f1, ...]. Returns frames written. If the
    // writer has lapped us by more than capacity, reports the gap and
    // resyncs to the oldest still-valid frame.
    uint32_t pull(float* out, uint32_t max_frames,
                  uint64_t* gap_frames_out = nullptr) {
        if (!_buf) return 0;
        uint64_t writer = _buf->write_position.load(std::memory_order_acquire);
        if (writer <= _last_read_pos) {
            if (gap_frames_out) *gap_frames_out = 0;
            return 0;
        }
        uint64_t avail = writer - _last_read_pos;
        uint32_t cap = _buf->capacity_frames;
        uint32_t channels = _buf->channels;

        if (avail > cap) {
            uint64_t gap = avail - cap;
            if (gap_frames_out) *gap_frames_out = gap;
            _last_read_pos = writer - cap;
            avail = cap;
        } else if (gap_frames_out) {
            *gap_frames_out = 0;
        }

        uint32_t to_read = (avail < max_frames)
                              ? static_cast<uint32_t>(avail) : max_frames;
        if (to_read == 0) return 0;

        uint32_t slot = static_cast<uint32_t>(_last_read_pos % cap);
        uint32_t first = (cap - slot < to_read) ? (cap - slot) : to_read;
        memcpy(out, _buf->data + slot * channels,
               static_cast<size_t>(first) * channels * sizeof(float));
        if (first < to_read) {
            memcpy(out + first * channels, _buf->data,
                   static_cast<size_t>(to_read - first) * channels * sizeof(float));
        }
        _last_read_pos += to_read;
        return to_read;
    }

    uint64_t last_read_position() const { return _last_read_pos; }

private:
    shm_audio_buffer* _buf = nullptr;
    uint64_t      _last_read_pos = 0;
};

} // namespace detail_shm_audio

using detail_shm_audio::shm_audio_buffer;
using detail_shm_audio::shm_audio_buffer_writer;
using detail_shm_audio::shm_audio_buffer_reader;
using detail_shm_audio::SHM_AUDIO_SAMPLE_RATE;
using detail_shm_audio::SHM_AUDIO_SECONDS;
using detail_shm_audio::SHM_AUDIO_CHANNELS;
using detail_shm_audio::SHM_AUDIO_FRAMES;
using detail_shm_audio::SHM_AUDIO_HEADER_SIZE;
using detail_shm_audio::SHM_AUDIO_SLOT_SIZE;
using detail_shm_audio::SHM_AUDIO_TOTAL_SIZE;
using detail_shm_audio::SHM_AUDIO_MASTER_SLOT;
using detail_shm_audio::MAX_SHM_AUDIO_BUFFERS;

// Process-global pointer to the slot array. Assigned once during
// audio_processor init; AudioOut2 UGens read it to locate their slot.
// The SuperCollider plugin interface has no route to the segment, and
// extending InterfaceTable for one UGen is heavier than this extern.
// Read-only after init.
extern shm_audio_buffer* g_shm_audio_buffers;
