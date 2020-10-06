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

#include "SC_Types.h"
#include "SC_SndBuf.h"

typedef void (*UnitCtorFunc)(struct Unit* inUnit);
typedef void (*UnitDtorFunc)(struct Unit* inUnit);

typedef void (*UnitCalcFunc)(struct Unit* inThing, int inNumSamples);

struct SC_Unit_Extensions {
    float* todo;
};

struct Unit {
    struct World* mWorld;
    struct UnitDef* mUnitDef;
    struct Graph* mParent;
    uint32 mNumInputs, mNumOutputs; // changed from uint16 for synthdef ver 2
    int16 mCalcRate;
    int16 mSpecialIndex; // used by unary and binary ops
    int16 mParentIndex;
    int16 mDone;
    struct Wire **mInput, **mOutput;
    struct Rate* mRate;
    SC_Unit_Extensions*
        mExtensions; // future proofing and backwards compatibility; used to be SC_Dimension struct pointer
    float **mInBuf, **mOutBuf;

    UnitCalcFunc mCalcFunc;
    int mBufLength;
};

typedef struct Unit Unit;

enum { kUnitDef_CantAliasInputsToOutputs = 1 };

#ifdef _WIN32
// Win32 headers (included by C std library headers) define IN and OUT macros
// for their own purposes.
#    undef IN
#    undef OUT
#endif

// These return float* pointers to input and output buffers.
#define IN(index) (unit->mInBuf[index])
#define OUT(index) (unit->mOutBuf[index])

// These return a float value. Used for control rate inputs and outputs.
#define IN0(index) (IN(index)[0])
#define OUT0(index) (OUT(index)[0])

// get the rate of the input.
#define INRATE(index) (unit->mInput[index]->mCalcRate)

// get the blocksize of the input
#define INBUFLENGTH(index) (unit->mInput[index]->mFromUnit->mBufLength)

// set the calculation function
#define SETCALC(func) (unit->mCalcFunc = (UnitCalcFunc)&func)

// calculate a slope for control rate interpolation to audio rate.
#define CALCSLOPE(next, prev) ((next - prev) * sc_typeof_cast(next) unit->mRate->mSlopeFactor)

// get useful values
#define SAMPLERATE (unit->mRate->mSampleRate)
#define SAMPLEDUR (unit->mRate->mSampleDur)
#define BUFLENGTH (unit->mBufLength)
#define BUFRATE (unit->mRate->mBufRate)
#define BUFDUR (unit->mRate->mBufDuration)
#define FULLRATE (unit->mWorld->mFullRate.mSampleRate)
#define FULLBUFLENGTH (unit->mWorld->mFullRate.mBufLength)

#ifdef SUPERNOVA

template <bool shared1, bool shared2> struct buffer_lock2 {
    buffer_lock2(const SndBuf* buf1, const SndBuf* buf2): buf1_(buf1), buf2_(buf2) {
        if (buf1 == buf2) {
            lock1();
            return;
        }

        for (;;) {
            lock1();

            if (lock2())
                return;
            unlock1();
        }
    }

    ~buffer_lock2(void) {
        unlock1();
        if (buf1_ != buf2_)
            unlock2();
    }

private:
    void lock1(void) {
        if (buf1_->isLocal)
            return;

        if (!shared1)
            buf1_->lock.lock();
        else
            buf1_->lock.lock_shared();
    }

    bool lock2(void) {
        if (buf2_->isLocal)
            return true;

        if (!shared2)
            return buf2_->lock.try_lock();
        else
            return buf2_->lock.try_lock_shared();
    }

    void unlock1(void) {
        if (buf1_->isLocal)
            return;

        if (!shared1)
            buf1_->lock.unlock();
        else
            buf1_->lock.unlock_shared();
    }

    void unlock2(void) {
        if (buf2_->isLocal)
            return;

        if (!shared2)
            buf2_->lock.unlock();
        else
            buf2_->lock.unlock_shared();
    }

    const SndBuf* buf1_;
    const SndBuf* buf2_;
};

template <bool shared> struct buffer_lock {
    buffer_lock(const SndBuf* buf): buf_(buf) {
        if (!buf->isLocal) {
            if (shared)
                buf->lock.lock_shared();
            else
                buf->lock.lock();
        }
    }

    ~buffer_lock(void) {
        if (!buf_->isLocal) {
            if (shared)
                buf_->lock.unlock_shared();
            else
                buf_->lock.unlock();
        }
    }

    const SndBuf* buf_;
};

#    define ACQUIRE_BUS_AUDIO(index) unit->mWorld->mAudioBusLocks[index].lock()
#    define ACQUIRE_BUS_AUDIO_SHARED(index) unit->mWorld->mAudioBusLocks[index].lock_shared()
#    define RELEASE_BUS_AUDIO(index) unit->mWorld->mAudioBusLocks[index].unlock()
#    define RELEASE_BUS_AUDIO_SHARED(index) unit->mWorld->mAudioBusLocks[index].unlock_shared()

#    define LOCK_SNDBUF(buf) buffer_lock<false> lock_##buf(buf)
#    define LOCK_SNDBUF_SHARED(buf) buffer_lock<true> lock_##buf(buf);

#    define LOCK_SNDBUF2(buf1, buf2) buffer_lock2<false, false> lock_##buf1##_##buf2(buf1, buf2);
#    define LOCK_SNDBUF2_SHARED(buf1, buf2) buffer_lock2<true, true> lock_##buf1##_##buf2(buf1, buf2);
#    define LOCK_SNDBUF2_EXCLUSIVE_SHARED(buf1, buf2) buffer_lock2<false, true> lock_##buf1##_##buf2(buf1, buf2);
#    define LOCK_SNDBUF2_SHARED_EXCLUSIVE(buf1, buf2) buffer_lock2<true, false> lock_##buf1##_##buf2(buf1, buf2);

#    define ACQUIRE_SNDBUF(buf)                                                                                        \
        do {                                                                                                           \
            if (!buf->isLocal)                                                                                         \
                buf->lock.lock();                                                                                      \
        } while (false)
#    define ACQUIRE_SNDBUF_SHARED(buf)                                                                                 \
        do {                                                                                                           \
            if (!buf->isLocal)                                                                                         \
                buf->lock.lock_shared();                                                                               \
        } while (false)
#    define RELEASE_SNDBUF(buf)                                                                                        \
        do {                                                                                                           \
            if (!buf->isLocal)                                                                                         \
                buf->lock.unlock();                                                                                    \
        } while (false)
#    define RELEASE_SNDBUF_SHARED(buf)                                                                                 \
        do {                                                                                                           \
            if (!buf->isLocal)                                                                                         \
                buf->lock.unlock_shared();                                                                             \
        } while (false)


#    define ACQUIRE_BUS_CONTROL(index) unit->mWorld->mControlBusLock->lock()
#    define RELEASE_BUS_CONTROL(index) unit->mWorld->mControlBusLock->unlock()

#else

#    define ACQUIRE_BUS_AUDIO(index)
#    define ACQUIRE_BUS_AUDIO_SHARED(index)
#    define RELEASE_BUS_AUDIO(index)
#    define RELEASE_BUS_AUDIO_SHARED(index)

#    define LOCK_SNDBUF(buf)
#    define LOCK_SNDBUF_SHARED(buf)

#    define LOCK_SNDBUF2(buf1, buf2)
#    define LOCK_SNDBUF2_SHARED(buf1, buf2)
#    define LOCK_SNDBUF2_EXCLUSIVE_SHARED(buf1, buf2)
#    define LOCK_SNDBUF2_SHARED_EXCLUSIVE(buf1, buf2)

#    define ACQUIRE_SNDBUF(buf)
#    define ACQUIRE_SNDBUF_SHARED(buf)
#    define RELEASE_SNDBUF(buf)
#    define RELEASE_SNDBUF_SHARED(buf)

#    define ACQUIRE_BUS_CONTROL(index)
#    define RELEASE_BUS_CONTROL(index)

#endif

// macros to grab a Buffer reference from the buffer indicated by the UGen's FIRST input
#define GET_BUF                                                                                                        \
    float fbufnum = ZIN0(0);                                                                                           \
    if (fbufnum < 0.f) {                                                                                               \
        fbufnum = 0.f;                                                                                                 \
    }                                                                                                                  \
    if (fbufnum != unit->m_fbufnum) {                                                                                  \
        uint32 bufnum = (int)fbufnum;                                                                                  \
        World* world = unit->mWorld;                                                                                   \
        if (bufnum >= world->mNumSndBufs) {                                                                            \
            int localBufNum = bufnum - world->mNumSndBufs;                                                             \
            Graph* parent = unit->mParent;                                                                             \
            if (localBufNum <= parent->localBufNum) {                                                                  \
                unit->m_buf = parent->mLocalSndBufs + localBufNum;                                                     \
            } else {                                                                                                   \
                bufnum = 0;                                                                                            \
                unit->m_buf = world->mSndBufs + bufnum;                                                                \
            }                                                                                                          \
        } else {                                                                                                       \
            unit->m_buf = world->mSndBufs + bufnum;                                                                    \
        }                                                                                                              \
        unit->m_fbufnum = fbufnum;                                                                                     \
    }                                                                                                                  \
    SndBuf* buf = unit->m_buf;                                                                                         \
    LOCK_SNDBUF(buf);                                                                                                  \
    float* bufData __attribute__((__unused__)) = buf->data;                                                            \
    uint32 bufChannels __attribute__((__unused__)) = buf->channels;                                                    \
    uint32 bufSamples __attribute__((__unused__)) = buf->samples;                                                      \
    uint32 bufFrames = buf->frames;                                                                                    \
    int mask __attribute__((__unused__)) = buf->mask;                                                                  \
    int guardFrame __attribute__((__unused__)) = bufFrames - 2;

#define GET_BUF_SHARED                                                                                                 \
    float fbufnum = ZIN0(0);                                                                                           \
    if (fbufnum < 0.f) {                                                                                               \
        fbufnum = 0.f;                                                                                                 \
    }                                                                                                                  \
    if (fbufnum != unit->m_fbufnum) {                                                                                  \
        uint32 bufnum = (int)fbufnum;                                                                                  \
        World* world = unit->mWorld;                                                                                   \
        if (bufnum >= world->mNumSndBufs) {                                                                            \
            int localBufNum = bufnum - world->mNumSndBufs;                                                             \
            Graph* parent = unit->mParent;                                                                             \
            if (localBufNum <= parent->localBufNum) {                                                                  \
                unit->m_buf = parent->mLocalSndBufs + localBufNum;                                                     \
            } else {                                                                                                   \
                bufnum = 0;                                                                                            \
                unit->m_buf = world->mSndBufs + bufnum;                                                                \
            }                                                                                                          \
        } else {                                                                                                       \
            unit->m_buf = world->mSndBufs + bufnum;                                                                    \
        }                                                                                                              \
        unit->m_fbufnum = fbufnum;                                                                                     \
    }                                                                                                                  \
    const SndBuf* buf = unit->m_buf;                                                                                   \
    LOCK_SNDBUF_SHARED(buf);                                                                                           \
    const float* bufData __attribute__((__unused__)) = buf->data;                                                      \
    uint32 bufChannels __attribute__((__unused__)) = buf->channels;                                                    \
    uint32 bufSamples __attribute__((__unused__)) = buf->samples;                                                      \
    uint32 bufFrames = buf->frames;                                                                                    \
    int mask __attribute__((__unused__)) = buf->mask;                                                                  \
    int guardFrame __attribute__((__unused__)) = bufFrames - 2;

#define SIMPLE_GET_BUF                                                                                                 \
    float fbufnum = ZIN0(0);                                                                                           \
    fbufnum = sc_max(0.f, fbufnum);                                                                                    \
    if (fbufnum != unit->m_fbufnum) {                                                                                  \
        uint32 bufnum = (int)fbufnum;                                                                                  \
        World* world = unit->mWorld;                                                                                   \
        if (bufnum >= world->mNumSndBufs) {                                                                            \
            int localBufNum = bufnum - world->mNumSndBufs;                                                             \
            Graph* parent = unit->mParent;                                                                             \
            if (localBufNum <= parent->localBufNum) {                                                                  \
                unit->m_buf = parent->mLocalSndBufs + localBufNum;                                                     \
            } else {                                                                                                   \
                bufnum = 0;                                                                                            \
                unit->m_buf = world->mSndBufs + bufnum;                                                                \
            }                                                                                                          \
        } else {                                                                                                       \
            unit->m_buf = world->mSndBufs + bufnum;                                                                    \
        }                                                                                                              \
        unit->m_fbufnum = fbufnum;                                                                                     \
    }                                                                                                                  \
    SndBuf* buf = unit->m_buf;

#define SIMPLE_GET_BUF_EXCLUSIVE                                                                                       \
    SIMPLE_GET_BUF;                                                                                                    \
    LOCK_SNDBUF(buf);

#define SIMPLE_GET_BUF_SHARED                                                                                          \
    SIMPLE_GET_BUF;                                                                                                    \
    LOCK_SNDBUF_SHARED(buf);

// macros to get pseudo-random number generator, and put its state in registers
#define RGET                                                                                                           \
    RGen& rgen = *unit->mParent->mRGen;                                                                                \
    uint32 s1 = rgen.s1;                                                                                               \
    uint32 s2 = rgen.s2;                                                                                               \
    uint32 s3 = rgen.s3;
#define RPUT                                                                                                           \
    rgen.s1 = s1;                                                                                                      \
    rgen.s2 = s2;                                                                                                      \
    rgen.s3 = s3;

typedef void (*UnitCmdFunc)(struct Unit* unit, struct sc_msg_iter* args);
typedef void (*PlugInCmdFunc)(World* inWorld, void* inUserData, struct sc_msg_iter* args, void* replyAddr);
