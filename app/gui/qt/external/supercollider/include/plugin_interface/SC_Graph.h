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

#include "SC_Node.h"
#include "SC_Rate.h"
#include "SC_SndBuf.h"

/*
 changes to this struct likely also mean that a change is needed for
    static const int sc_api_version = x;
 value in SC_InterfaceTable.h file.
 */
struct Graph {
    Node mNode;

    uint32 mNumWires;
    struct Wire* mWire;

    uint32 mNumControls;
    float* mControls;
    float** mMapControls;
    int32* mAudioBusOffsets;

    // try this for setting the rate of a control
    int* mControlRates;

    uint32 mNumUnits;
    struct Unit** mUnits;

    uint32 mNumCalcUnits;
    struct Unit** mCalcUnits; // excludes i-rate units.

    int mSampleOffset;
    struct RGen* mRGen;

    struct Unit* mLocalAudioBusUnit;
    struct Unit* mLocalControlBusUnit;

    float mSubsampleOffset;

    SndBuf* mLocalSndBufs;
    int localBufNum;
    int localMaxBufNum;
};
typedef struct Graph Graph;
