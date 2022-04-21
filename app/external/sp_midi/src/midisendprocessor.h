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
#include <memory.h>
#include <vector>
#include <string>
#include <thread>
#include <mutex>
#include "blockingconcurrentqueue.h"
#include "midiout.h"
#include "monitorlogger.h"

extern std::atomic<bool> g_threadsShouldFinish;

class MidiSendProcessor
{
private:
    typedef struct{
        std::string device_name;
        std::vector<unsigned char> midi;
    } MidiDeviceAndMessage;

public:
    MidiSendProcessor() : m_flushing(false) {};
    ~MidiSendProcessor();

    void startThread();

    void prepareOutputs(const std::vector<MidiPortInfo>& portsInfo);

    void processMessage(const MidiDeviceAndMessage& message_from_c);


    int getNMidiOuts() const;
    std::string getMidiOutName(int n) const;
    std::string getNormalizedMidiOutName(int n) const;
    int getMidiOutId(int n) const;

    bool addMessage(const char* device_name, const unsigned char* c_message, std::size_t size);
    void flushMessages();

    static const std::vector<std::string> getKnownOscMessages();

private:
    void send(const std::string& outDevice, const std::vector< unsigned char >* msg);

    std::vector<std::unique_ptr<MidiOut> > m_outputs;
    MonitorLogger& m_logger{ MonitorLogger::getInstance() };

    moodycamel::BlockingConcurrentQueue<MidiDeviceAndMessage> m_messages;

    std::thread m_thread;
    std::atomic<bool> m_flushing;
    void run();
};
