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

#include <iostream>
#include "midiout.h"
#include "utils.h"

using namespace std;

MidiOut::MidiOut(const std::string& portName, const std::string& normalizedPortName, int portId)
{
    m_logger.debug("MidiOut constructor for {}", portName);

    m_portName = portName;
    m_normalizedPortName = normalizedPortName;
    m_rtMidiId = portId;

    // FIXME: need to check if name does not exist
    m_midiOut = make_unique<RtMidiOut>();

    try
    {
        m_midiOut->openPort(m_rtMidiId);
    }
    catch(const RtMidiError& err)
    {
        m_logger.debug("Failed to open midi out port");
    }
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
    try
    {
        m_midiOut->sendMessage(msg);
    }
    catch(const RtMidiError& err)
    {
        m_logger.warn(err.what());
    }
}

vector<MidiPortInfo> MidiOut::getOutputPortInfo()
{
    RtMidiOut outs;
    auto outs_info = getPortInfo(outs);
    return outs_info;
}

vector<string> MidiOut::getNormalizedOutputNames()
{
    vector<MidiPortInfo> info = getOutputPortInfo();
    return getNormalizedNamesFromPortInfos(info);
}
