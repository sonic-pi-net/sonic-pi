// MIT License

// Copyright (c) 2016 Luis Lloret

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

#include <iostream>
#include "midiout.h"
#include "utils.h"

using namespace std;
using namespace juce;

MidiOut::MidiOut(const string& portName)
{
    m_logger.debug("MidiOut constructor for {}", portName);
    updateMidiDevicesNamesMapping();
    m_portName = portName;
    m_normalizedPortName = portName;
    local_utils::safeOscString(m_normalizedPortName);

    if (!nameInStickyTable(m_portName))
        m_stickyId = addNameToStickyTable(m_portName);
    else
        m_stickyId = getStickyIdFromName(m_portName);

    m_rtMidiId = getRtMidiIdFromName(m_portName);

    // FIXME: need to check if name does not exist
    m_midiOut = make_unique<RtMidiOut>();
    m_midiOut->openPort(m_rtMidiId);
}

MidiOut::~MidiOut()
{
    m_logger.trace("MidiOut destructor for {}", m_portName);
    m_midiOut->closePort();
}

void MidiOut::send(const std::vector< unsigned char >* msg)
{
    m_logger.info("Sending MIDI to: {} ->", m_portName);
    for (int i = 0; i < msg->size(); i++) {
        m_logger.info("   [{:02x}]", (*msg)[i]);
    }
    m_midiOut->sendMessage(msg);
}

vector<string> MidiOut::getOutputNames()
{
    RtMidiOut outs;
    int nPorts = outs.getPortCount();
    vector<string> names(nPorts);

    for (int i = 0; i < nPorts; i++) {
        auto name = outs.getPortName(i);
        local_utils::safeOscString(name);
        names[i] = name;
    }
    return names;
}

void MidiOut::updateMidiDevicesNamesMapping()
{
    m_midiRtMidiIdToName = MidiOut::getOutputNames();
    for (int i = 0; i < m_midiRtMidiIdToName.size(); i++) {
        m_midiNameToRtMidiId[m_midiRtMidiIdToName[i]] = i;
    }
}
