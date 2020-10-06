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

#include <stdarg.h>
#include "SC_Reply.h"
#include "SC_Types.h"
#include "SC_Export.h"

typedef int (*PrintFunc)(const char* format, va_list ap);

struct WorldOptions {
    WorldOptions() {}

    const char* mPassword = nullptr;
    uint32 mNumBuffers = 1024;
    uint32 mMaxLogins = 64;
    uint32 mMaxNodes = 1024;
    uint32 mMaxGraphDefs = 1024;
    uint32 mMaxWireBufs = 64;
    uint32 mNumAudioBusChannels = 1024;
    uint32 mNumInputBusChannels = 8;
    uint32 mNumOutputBusChannels = 8;
    uint32 mNumControlBusChannels = 16384;
    uint32 mBufLength = 64;
    uint32 mRealTimeMemorySize = 8192; // in kilobytes

    int mNumSharedControls = 0;
    float* mSharedControls = nullptr;

    bool mRealTime = true;
    bool mMemoryLocking = false;

    const char* mNonRealTimeCmdFilename = nullptr;
    const char* mNonRealTimeInputFilename = nullptr;
    const char* mNonRealTimeOutputFilename = nullptr;
    const char* mNonRealTimeOutputHeaderFormat = nullptr;
    const char* mNonRealTimeOutputSampleFormat = nullptr;

    uint32 mPreferredSampleRate = 0;

    uint32 mNumRGens = 64;

    uint32 mPreferredHardwareBufferFrameSize = 0;

    uint32 mLoadGraphDefs = 1;

    const char* mInputStreamsEnabled = nullptr;
    const char* mOutputStreamsEnabled = nullptr;
    const char* mInDeviceName = nullptr;

    int mVerbosity = 0;

    bool mRendezvous = true;

    const char* mUGensPluginPath = nullptr;

    const char* mOutDeviceName = nullptr;

    const char* mRestrictedPath = nullptr;

    int mSharedMemoryID = 0;
};

struct SndBuf;

SCSYNTH_DLLEXPORT_C void SetPrintFunc(PrintFunc func);
SCSYNTH_DLLEXPORT_C struct World* World_New(struct WorldOptions* inOptions);
SCSYNTH_DLLEXPORT_C void World_Cleanup(struct World* inWorld, bool unload_plugins = false);
SCSYNTH_DLLEXPORT_C void World_NonRealTimeSynthesis(struct World* inWorld, struct WorldOptions* inOptions);
SCSYNTH_DLLEXPORT_C int World_OpenUDP(struct World* inWorld, const char* bindTo, int inPort);
SCSYNTH_DLLEXPORT_C int World_OpenTCP(struct World* inWorld, const char* bindTo, int inPort, int inMaxConnections,
                                      int inBacklog);
SCSYNTH_DLLEXPORT_C void World_WaitForQuit(struct World* inWorld, bool unload_plugins = false);
SCSYNTH_DLLEXPORT_C bool World_SendPacket(struct World* inWorld, int inSize, char* inData, ReplyFunc inFunc);
SCSYNTH_DLLEXPORT_C bool World_SendPacketWithContext(struct World* inWorld, int inSize, char* inData, ReplyFunc inFunc,
                                                     void* inContext);
SCSYNTH_DLLEXPORT_C int World_CopySndBuf(struct World* world, uint32 index, struct SndBuf* outBuf, bool onlyIfChanged,
                                         bool* didChange);
SCSYNTH_DLLEXPORT_C int scprintf(const char* fmt, ...);
