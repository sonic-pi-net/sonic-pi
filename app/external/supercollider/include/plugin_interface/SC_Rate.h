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

enum { calc_ScalarRate, calc_BufRate, calc_FullRate, calc_DemandRate };

struct Rate {
    double mSampleRate; // samples per second
    double mSampleDur; // seconds per sample
    double mBufDuration; // seconds per buffer
    double mBufRate; // buffers per second
    double mSlopeFactor; // 1. / NumSamples
    double mRadiansPerSample; // 2pi / SampleRate
    int mBufLength; // length of the buffer
    // second order filter loops are often unrolled by 3
    int mFilterLoops, mFilterRemain;
    double mFilterSlope;
};
typedef struct Rate Rate;
