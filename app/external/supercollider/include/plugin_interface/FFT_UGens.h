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

#include "SC_PlugIn.h"
#include "SCComplex.h"

#include <string.h>

struct SCComplexBuf {
    float dc, nyq;
    SCComplex bin[1];
};

struct SCPolarBuf {
    float dc, nyq;
    SCPolar bin[1];
};

static inline SCPolarBuf* ToPolarApx(SndBuf* buf) {
    if (buf->coord == coord_Complex) {
        SCComplexBuf* p = (SCComplexBuf*)buf->data;
        int numbins = (buf->samples - 2) >> 1;
        for (int i = 0; i < numbins; ++i) {
            p->bin[i].ToPolarApxInPlace();
        }
        buf->coord = coord_Polar;
    }

    return (SCPolarBuf*)buf->data;
}

static inline SCComplexBuf* ToComplexApx(SndBuf* buf) {
    if (buf->coord == coord_Polar) {
        SCPolarBuf* p = (SCPolarBuf*)buf->data;
        int numbins = (buf->samples - 2) >> 1;
        for (int i = 0; i < numbins; ++i) {
            p->bin[i].ToComplexApxInPlace();
        }
        buf->coord = coord_Complex;
    }
    return (SCComplexBuf*)buf->data;
}

struct PV_Unit : Unit {};


#define sc_clipbuf(x, hi) ((x) >= (hi) ? 0 : ((x) < 0 ? 0 : (x)))

// for operation on one buffer
#define PV_GET_BUF                                                                                                     \
    float fbufnum = ZIN0(0);                                                                                           \
    if (fbufnum < 0.f) {                                                                                               \
        ZOUT0(0) = -1.f;                                                                                               \
        return;                                                                                                        \
    }                                                                                                                  \
    ZOUT0(0) = fbufnum;                                                                                                \
    uint32 ibufnum = (uint32)fbufnum;                                                                                  \
    World* world = unit->mWorld;                                                                                       \
    SndBuf* buf;                                                                                                       \
    if (ibufnum >= world->mNumSndBufs) {                                                                               \
        int localBufNum = ibufnum - world->mNumSndBufs;                                                                \
        Graph* parent = unit->mParent;                                                                                 \
        if (localBufNum <= parent->localBufNum) {                                                                      \
            buf = parent->mLocalSndBufs + localBufNum;                                                                 \
        } else {                                                                                                       \
            buf = world->mSndBufs;                                                                                     \
        }                                                                                                              \
    } else {                                                                                                           \
        buf = world->mSndBufs + ibufnum;                                                                               \
    }                                                                                                                  \
    LOCK_SNDBUF(buf);                                                                                                  \
    int numbins = (buf->samples - 2) >> 1;


// for operation on two input buffers, result goes in first one.
#define PV_GET_BUF2                                                                                                    \
    float fbufnum1 = ZIN0(0);                                                                                          \
    float fbufnum2 = ZIN0(1);                                                                                          \
    if (fbufnum1 < 0.f || fbufnum2 < 0.f) {                                                                            \
        ZOUT0(0) = -1.f;                                                                                               \
        return;                                                                                                        \
    }                                                                                                                  \
    ZOUT0(0) = fbufnum1;                                                                                               \
    uint32 ibufnum1 = (int)fbufnum1;                                                                                   \
    uint32 ibufnum2 = (int)fbufnum2;                                                                                   \
    World* world = unit->mWorld;                                                                                       \
    SndBuf* buf1;                                                                                                      \
    SndBuf* buf2;                                                                                                      \
    if (ibufnum1 >= world->mNumSndBufs) {                                                                              \
        int localBufNum = ibufnum1 - world->mNumSndBufs;                                                               \
        Graph* parent = unit->mParent;                                                                                 \
        if (localBufNum <= parent->localBufNum) {                                                                      \
            buf1 = parent->mLocalSndBufs + localBufNum;                                                                \
        } else {                                                                                                       \
            buf1 = world->mSndBufs;                                                                                    \
        }                                                                                                              \
    } else {                                                                                                           \
        buf1 = world->mSndBufs + ibufnum1;                                                                             \
    }                                                                                                                  \
    if (ibufnum2 >= world->mNumSndBufs) {                                                                              \
        int localBufNum = ibufnum2 - world->mNumSndBufs;                                                               \
        Graph* parent = unit->mParent;                                                                                 \
        if (localBufNum <= parent->localBufNum) {                                                                      \
            buf2 = parent->mLocalSndBufs + localBufNum;                                                                \
        } else {                                                                                                       \
            buf2 = world->mSndBufs;                                                                                    \
        }                                                                                                              \
    } else {                                                                                                           \
        buf2 = world->mSndBufs + ibufnum2;                                                                             \
    }                                                                                                                  \
    LOCK_SNDBUF2(buf1, buf2);                                                                                          \
    if (buf1->samples != buf2->samples)                                                                                \
        return;                                                                                                        \
    int numbins = (buf1->samples - 2) >> 1;

#define MAKE_TEMP_BUF                                                                                                  \
    if (!unit->m_tempbuf) {                                                                                            \
        unit->m_tempbuf = (float*)RTAlloc(unit->mWorld, buf->samples * sizeof(float));                                 \
        unit->m_numbins = numbins;                                                                                     \
    } else if (numbins != unit->m_numbins)                                                                             \
        return;

extern InterfaceTable* ft;
