// MIT License

// Copyright (c) 2016-2021 Luis Lloret

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#pragma once

#include <vector>
#include <string>
#include <rtmidi/RtMidi.h>
#include "midicommon.h"
#include "midi_port_info.h"

// This class manages a MIDI input device as seen by JUCE
class MidiIn : public MidiCommon {
public:
    MidiIn(const std::string& portName, const std::string& normalizedPortName, int portId, bool isVirtual = false);
    MidiIn(const MidiIn&) = delete;
    MidiIn& operator=(const MidiIn&) = delete;

    virtual ~MidiIn();

    static std::vector<std::string> getNormalizedInputNames();
    static std::vector<MidiPortInfo> getInputPortInfo();

protected:

    std::unique_ptr<RtMidiIn> m_midiIn;
    std::mutex m_cb_mutex;
    static void staticMidiCallback(double timeStamp, std::vector< unsigned char > *message, void *userData);
    void midiCallback(double timeStamp, std::vector< unsigned char > *message);

    // TODO: do we need to send the raw message to Sonic Pi?
    bool m_oscRawMidiMessage;

};
