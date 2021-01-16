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
#include "midicommon.h"

using namespace std;

map<string, int> MidiCommon::m_midiNameToRtMidiId;
map<string, int> MidiCommon::m_midiNameToStickyId;
vector<string> MidiCommon::m_midiRtMidiIdToName;
unsigned int MidiCommon::m_nStickyIds = 0;

MidiCommon::MidiCommon() {}

MidiCommon::~MidiCommon()
{
}

string MidiCommon::getPortName() const
{
    return m_portName;
}

string MidiCommon::getNormalizedPortName() const
{
    return m_normalizedPortName;
}

int MidiCommon::getPortId() const
{
    return m_stickyId;
}

int MidiCommon::getRtMidiIdFromName(const string& portName)
{
    return m_midiNameToRtMidiId.at(portName);
}

bool MidiCommon::nameInStickyTable(const string& portName)
{
    auto search = m_midiNameToStickyId.find(portName);
    return (search != m_midiNameToStickyId.end());
}

unsigned int MidiCommon::addNameToStickyTable(const string& portName)
{
    m_midiNameToStickyId[portName] = m_nStickyIds;
    return m_nStickyIds++;
}

unsigned int MidiCommon::getStickyIdFromName(const string& portName)
{
    return m_midiNameToStickyId[portName];
}
