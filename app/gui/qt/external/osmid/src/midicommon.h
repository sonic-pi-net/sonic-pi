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


#pragma once

#include <thread>
#include <memory>
#include <vector>
#include <map>
#include <string>
#include "../JuceLibraryCode/JuceHeader.h"
#include "monitorlogger.h"

// This class manages the common parts of our MIDI handling, like sticky ids
class MidiCommon {
public:
    MidiCommon();
    MidiCommon(const MidiCommon&) = delete;
    MidiCommon& operator=(const MidiCommon&) = delete;

    virtual ~MidiCommon();

    bool checkValid() const;

    std::string getPortName() const;
    std::string getNormalizedPortName() const;
    int getPortId() const;

    static int getJuceMidiIdFromName(const std::string& portName);

protected:
    virtual void updateMidiDevicesNamesMapping() = 0;
    std::string m_portName;
    std::string m_normalizedPortName;
    int m_juceMidiId;
    int m_stickyId;
    static bool nameInStickyTable(const std::string& portName);
    unsigned int addNameToStickyTable(const std::string& portName);
    unsigned int getStickyIdFromName(const std::string& portName);
    static std::map<std::string, int> m_midiNameToJuceMidiId;
    static std::vector<std::string> m_midiJuceMidiIdToName;
    static std::map<std::string, int> m_midiNameToStickyId;
    static unsigned int m_nStickyIds;
    MonitorLogger &m_logger{ MonitorLogger::getInstance() };
};
