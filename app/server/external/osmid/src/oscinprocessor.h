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
#include <memory.h>
#include <vector>
#include <string>
#include "../JuceLibraryCode/JuceHeader.h"
#include "oscin.h"
#include "midiout.h"
#include "monitorlogger.h"

class OscInProcessor : public osc::OscPacketListener {
public:
    OscInProcessor(bool local, int oscListenPort);

    void prepareOutputs(const std::vector<std::string>& outputNames);

    void run()
    {
        m_input->run();
    }

    void asyncBreak()
    {
        m_input->asyncBreak();
    }

    virtual void ProcessMessage(const osc::ReceivedMessage& m, const IpEndpointName& remoteEndpoint) override;
    virtual void ProcessBundle(const osc::ReceivedBundle& b, const IpEndpointName& remoteEndpoint) override;

    ~OscInProcessor()
    {
        m_logger.trace("OscInProcessor destructor");
    }

    int getNMidiOuts() const;
    std::string getMidiOutName(int n) const;
    std::string getNormalizedMidiOutName(int n) const;
    int getMidiOutId(int n) const;

    static const std::vector<std::string> getKnownOscMessages();

private:
    void send(const std::string& outDevice, const MidiMessage& msg);
    void processClockMessage(const std::string& outDevice);
    void processStartMessage(const std::string& outDevice);
    void processContinueMessage(const std::string& outDevice);
    void processStopMessage(const std::string& outDevice);
    void processActiveSenseMessage(const std::string& outDevice);
    void processRawMessage(const std::string& outDevice, const osc::ReceivedMessage& message);
    void processNoteOnMessage(const std::string& outDevice, const osc::ReceivedMessage& message);
    void processNoteOffMessage(const std::string& outDevice, const osc::ReceivedMessage& message);
    void processControlChangeMessage(const std::string& outDevice, const osc::ReceivedMessage& message);
    void processPitchBendMessage(const std::string& outDevice, const osc::ReceivedMessage& message);
    void processChannelPressureMessage(const std::string& outDevice, const osc::ReceivedMessage& message);
    void processPolyPressureMessage(const std::string& outDevice, const osc::ReceivedMessage& message);
    void processProgramChangeMessage(const std::string& outDevice, const osc::ReceivedMessage& message);
    void processLogLevelMessage(const osc::ReceivedMessage& message);
    void processLogToOscMessage(const osc::ReceivedMessage& message);

    //bool validateMessage(const std::string& warningPre, const std::string& validationString, const osc::ReceivedMessage& message);
    void dumpOscBody(const osc::ReceivedMessage& message);

    std::unique_ptr<OscIn> m_input;
    std::vector<std::unique_ptr<MidiOut> > m_outputs;
    MonitorLogger& m_logger{ MonitorLogger::getInstance() };
};
