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

typedef void (*FifoMsgFunc)(struct FifoMsg*);

struct FifoMsg {
    FifoMsg(): mPerformFunc(0), mFreeFunc(0), mData(0), mWorld(0) {}

    void Set(struct World* inWorld, FifoMsgFunc inPerform, FifoMsgFunc inFree, void* inData);
    void Perform();
    void Free();

    FifoMsgFunc mPerformFunc;
    FifoMsgFunc mFreeFunc;
    void* mData;
    struct World* mWorld;
};

inline void FifoMsg::Set(World* inWorld, FifoMsgFunc inPerform, FifoMsgFunc inFree, void* inData) {
    mWorld = inWorld;
    mPerformFunc = inPerform;
    mFreeFunc = inFree;
    mData = inData;
}

inline void FifoMsg::Perform() {
    if (mPerformFunc)
        (mPerformFunc)(this);
}

inline void FifoMsg::Free() {
    if (mFreeFunc)
        (mFreeFunc)(this);
}
