/*
    SuperCollider real time audio synthesis system
    Copyright (c) 2002 James McCartney. All rights reserved.
    http://www.audiosynth.com

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*/

#pragma once

#include <stdint.h>

typedef struct SNDFILE_tag SNDFILE;

#ifdef SUPERNOVA

#    include <atomic>
#    include <cassert>

#    ifdef __SSE2__
#        include <emmintrin.h>
#    endif


class rw_spinlock {
    static const uint32_t unlocked_state = 0;
    static const uint32_t locked_state = 0x80000000;
    static const uint32_t reader_mask = 0x7fffffff;

#    ifdef __SSE2__
    static inline void pause() { _mm_pause(); }
#    else
    static inline void pause() {}
#    endif

public:
    struct unique_lock {
        explicit unique_lock(rw_spinlock& sl): sl_(sl) { sl_.lock(); }
        ~unique_lock() { sl_.unlock(); }

    private:
        rw_spinlock& sl_;
    };

    typedef unique_lock unique_lock;

    struct shared_lock {
        explicit shared_lock(rw_spinlock& sl): sl_(sl) { sl_.lock_shared(); }
        ~shared_lock() { sl_.unlock_shared(); }

    private:
        rw_spinlock& sl_;
    };

    rw_spinlock() = default;
    rw_spinlock(rw_spinlock const& rhs) = delete;
    rw_spinlock& operator=(rw_spinlock const& rhs) = delete;
    rw_spinlock(rw_spinlock&& rhs) = delete;

    ~rw_spinlock() { assert(state == unlocked_state); }

    void lock() {
        for (;;) {
            while (state.load(std::memory_order_relaxed) != unlocked_state)
                pause();

            uint32_t expected = unlocked_state;
            if (state.compare_exchange_weak(expected, locked_state, std::memory_order_acquire))
                break;
        }
    }

    bool try_lock() {
        uint32_t expected = unlocked_state;
        if (state.compare_exchange_strong(expected, locked_state, std::memory_order_acquire))
            return true;
        else
            return false;
    }

    void unlock() {
        assert(state.load(std::memory_order_relaxed) == locked_state);
        state.store(unlocked_state, std::memory_order_release);
    }

    void lock_shared() {
        for (;;) {
            /* with the mask, the cas will fail, locked exclusively */
            uint32_t current_state = state.load(std::memory_order_acquire) & reader_mask;
            const uint32_t next_state = current_state + 1;

            if (state.compare_exchange_weak(current_state, next_state, std::memory_order_acquire))
                break;
            pause();
        }
    }

    bool try_lock_shared() {
        /* with the mask, the cas will fail, locked exclusively */
        uint32_t current_state = state.load(std::memory_order_acquire) & reader_mask;
        const uint32_t next_state = current_state + 1;

        if (state.compare_exchange_strong(current_state, next_state, std::memory_order_acquire))
            return true;
        else
            return false;
    }

    void unlock_shared() {
        for (;;) {
            uint32_t current_state = state.load(std::memory_order_relaxed); /* we don't need the reader_mask */
            const uint32_t next_state = current_state - 1;

            if (state.compare_exchange_weak(current_state, uint32_t(next_state)))
                break;
            pause();
        }
    }

private:
    std::atomic<uint32_t> state { unlocked_state };
};

#endif

struct SndBuf {
    double samplerate;
    double sampledur; // = 1/ samplerate
    float* data;
    int channels;
    int samples;
    int frames;
    int mask; // for delay lines
    int mask1; // for interpolating oscillators.
    int coord; // used by fft ugens
    SNDFILE* sndfile; // used by disk i/o
    // SF_INFO fileinfo; // used by disk i/o
#ifdef SUPERNOVA
    bool isLocal;
    mutable rw_spinlock lock;
#endif
};

typedef struct SndBuf SndBuf;

struct SndBufUpdates {
    int reads;
    int writes;
};
typedef struct SndBufUpdates SndBufUpdates;

enum { coord_None, coord_Complex, coord_Polar };

inline float PhaseFrac(uint32_t inPhase) {
    union {
        uint32_t itemp;
        float ftemp;
    } u;
    u.itemp = 0x3F800000 | (0x007FFF80 & ((inPhase) << 7));
    return u.ftemp - 1.f;
}

inline float PhaseFrac1(uint32_t inPhase) {
    union {
        uint32_t itemp;
        float ftemp;
    } u;
    u.itemp = 0x3F800000 | (0x007FFF80 & ((inPhase) << 7));
    return u.ftemp;
}

inline float lookup(const float* table, int32_t phase, int32_t mask) { return table[(phase >> 16) & mask]; }


#define xlobits 14
#define xlobits1 13

inline float lookupi(const float* table, uint32_t phase, uint32_t mask) {
    float frac = PhaseFrac(phase);
    const float* tbl = table + ((phase >> 16) & mask);
    float a = tbl[0];
    float b = tbl[1];
    return a + frac * (b - a);
}

inline float lookupi2(const float* table, uint32_t phase, uint32_t mask) {
    float frac = PhaseFrac1(phase);
    const float* tbl = table + ((phase >> 16) & mask);
    float a = tbl[0];
    float b = tbl[1];
    return a + frac * b;
}

inline float lookupi1(const float* table0, const float* table1, uint32_t pphase, int32_t lomask) {
    float pfrac = PhaseFrac1(pphase);
    uint32_t index = ((pphase >> xlobits1) & (uint32_t)lomask);
    float val1 = *(const float*)((const char*)table0 + index);
    float val2 = *(const float*)((const char*)table1 + index);
    return val1 + val2 * pfrac;
}


inline float lininterp(float x, float a, float b) { return a + x * (b - a); }

inline float cubicinterp(float x, float y0, float y1, float y2, float y3) {
    // 4-point, 3rd-order Hermite (x-form)
    float c0 = y1;
    float c1 = 0.5f * (y2 - y0);
    float c2 = y0 - 2.5f * y1 + 2.f * y2 - 0.5f * y3;
    float c3 = 0.5f * (y3 - y0) + 1.5f * (y1 - y2);

    return ((c3 * x + c2) * x + c1) * x + c0;
}
