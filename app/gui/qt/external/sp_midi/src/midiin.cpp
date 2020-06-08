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
#include "midiin.h"
#include "utils.h"

using namespace std;

MidiIn::MidiIn(const string& portName, MidiInputCallback* midiInputCallback, bool isVirtual)
{
    m_logger.debug("MidiIn constructor for {}", portName);
    updateMidiDevicesNamesMapping();
    m_portName = portName;
    m_normalizedPortName = portName;
    local_utils::safeOscString(m_normalizedPortName);

    if (!nameInStickyTable(m_portName))
        m_stickyId = addNameToStickyTable(m_portName);
    else
        m_stickyId = getStickyIdFromName(m_portName);

    // FIXME: need to check if name does not exist
    if (!isVirtual) {
        m_juceMidiId = getJuceMidiIdFromName(m_portName);
        m_midiIn = MidiInput::openDevice(m_juceMidiId, midiInputCallback);
    }
    else {
#ifndef WIN32
        m_logger.trace("*** Creating new MIDI device: ", m_portName);
        m_midiIn = MidiInput::createNewDevice(m_portName, midiInputCallback);
#else
        m_logger.error("Virtual MIDI ports are not supported on Windows");
        exit(-1);
#endif
    }

    m_midiIn->start();
}

MidiIn::~MidiIn()
{
    m_logger.trace("MidiIn destructor for {}", m_portName);
    m_midiIn->stop();
}

vector<string> MidiIn::getInputNames()
{
    auto strArray = MidiInput::getDevices();
    int nPorts = strArray.size();
    vector<string> names(nPorts);

    for (int i = 0; i < nPorts; i++) {
        names[i] = strArray[i].toStdString();
    }
    return names;
}

void MidiIn::updateMidiDevicesNamesMapping()
{
    m_midiJuceMidiIdToName = MidiIn::getInputNames();
    for (int i = 0; i < m_midiJuceMidiIdToName.size(); i++) {
        m_midiNameToJuceMidiId[m_midiJuceMidiIdToName[i]] = i;
    }
}
