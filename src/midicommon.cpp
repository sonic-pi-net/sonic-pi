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
#include "midicommon.h"
#include "utils.h"

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

std::vector<MidiPortInfo> MidiCommon::getPortInfo(RtMidi& ports)
{
    int nPorts = ports.getPortCount();
    std::vector<MidiPortInfo> connectedInputPortsInfo;

    for (int i = 0; i < nPorts; i++) {
        auto name = ports.getPortName(i);
        auto normalizedPortName = name;
        local_utils::safeOscString(normalizedPortName);

        if (normalizedPortName.rfind("rtmidi_", 0) == 0) {
            // The fact that the port name starts with rtmidi tells us that
            // this is a virtual midi port name created by RtMidi - ignore it
        } else {
            // Now we need to check for duplicate port names and if they exist,
            // append an integer count to subsequent port names to ensure that
            // they are all unique.  So if there were three devices
            // simultaneously connected all with the port name
            // nanokontrol_slider_knob, their "safe names" will become:
            //
            // nanokontrol_slider_knob
            // nanokontrol_slider_knob_2
            // nanokontrol_slider_knob_3

            int cnt = 1;
            for (int j = 0; j < connectedInputPortsInfo.size(); j++) {
                if(connectedInputPortsInfo[j].normalizedPortName == normalizedPortName) {
                    cnt += 1;
                }
            }

            if(cnt != 1) {
                normalizedPortName += "_";
                normalizedPortName += std::to_string(cnt);
            }

            MidiPortInfo info{name, normalizedPortName, i};
            connectedInputPortsInfo.push_back(info);
        }
    }
    return connectedInputPortsInfo;
}

vector<string> MidiCommon::getNormalizedNamesFromPortInfos(std::vector<MidiPortInfo>& info)
{
    vector<string> all_names;

    for (int i = 0; i < info.size(); i++) {
        auto s = info[i];
        all_names.push_back(s.normalizedPortName);
    }
    return all_names;
}
