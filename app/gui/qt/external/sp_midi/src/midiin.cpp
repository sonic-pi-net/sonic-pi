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
#include "sp_midi.h"
#include "midiin.h"
#include "utils.h"

using namespace std;

MidiIn::MidiIn(const string& portName, bool isVirtual) : m_oscRawMidiMessage(false)
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
        m_rtMidiId = getRtMidiIdFromName(m_portName);
        m_midiIn = make_unique<RtMidiIn>();
        m_midiIn->openPort(m_rtMidiId);
        m_midiIn->ignoreTypes( false, false, false );
    }
// TODO: do the virtual ports
#if 0    
    else {
#ifndef WIN32
        m_logger.trace("*** Creating new MIDI device: ", m_portName);
        m_midiIn = MidiInput::createNewDevice(m_portName, midiInputCallback);
#else
        m_logger.error("Virtual MIDI ports are not supported on Windows");
        exit(-1);
#endif
    }
#endif

    m_midiIn->setCallback(MidiIn::staticMidiCallback, this);
}

MidiIn::~MidiIn()
{
    m_logger.trace("MidiIn destructor for {}", m_portName);
    m_midiIn->closePort();
    //m_midiIn->stop();
}


void MidiIn::staticMidiCallback(double timeStamp, std::vector< unsigned char > *midiMessage, void *userData)
{    
    MidiIn *midiIn = (MidiIn *)userData;
    midiIn->midiCallback(timeStamp, midiMessage);
}


int send_midi_osc_to_erlang(const char* device_name, const unsigned char* data, size_t size);
void MidiIn::midiCallback(double timeStamp, std::vector< unsigned char > *midiMessage)
{
    lock_guard<mutex> lock(m_cb_mutex);
    m_logger.info("received MIDI message: ");
    for (int i = 0; i < midiMessage->size(); i++) {
        m_logger.info("   [{:02x}]", (*midiMessage)[i]);
    }
    // And send the message to the erlang process
    send_midi_osc_to_erlang(getNormalizedPortName().c_str(), midiMessage->data(), midiMessage->size());
}



vector<string> MidiIn::getInputNames()
{
    RtMidiIn ins;
    int nPorts = ins.getPortCount();
    vector<string> names(nPorts);

    for (int i = 0; i < nPorts; i++) {
        auto name = ins.getPortName(i);
        local_utils::safeOscString(name);
        names[i] = name;
    }
    return names;
}

void MidiIn::updateMidiDevicesNamesMapping()
{
    m_midiRtMidiIdToName = MidiIn::getInputNames();
    for (int i = 0; i < m_midiRtMidiIdToName.size(); i++) {
        m_midiNameToRtMidiId[m_midiRtMidiIdToName[i]] = i;
    }
}
