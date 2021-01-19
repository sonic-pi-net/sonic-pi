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

#include <regex>
#include "midisendprocessor.h"
#include "utils.h"


using namespace std;
using namespace moodycamel;


void MidiSendProcessor::startThread()
{
    m_thread = std::thread(&MidiSendProcessor::run, this);
}

MidiSendProcessor::~MidiSendProcessor()
{
    m_logger.trace("MidiSendProcessor destructor");
    if (m_thread.joinable()){
        m_thread.join();
    }
}


void MidiSendProcessor::prepareOutputs(const vector<MidiPortInfo>& portsInfo)
{
    m_outputs.clear();
    for (auto& output : portsInfo) {
        try {
            auto midiOut = make_unique<MidiOut>(output.portName, output.normalizedPortName, output.portId);
            m_outputs.push_back(std::move(midiOut));
        }
        catch (const RtMidiError& e) {
            cout << "Could not open output device " << output.portName << ": " << e.what() << endl;
            //throw;
        }
    }
}

// TODO: during initial testing to measure latency of async calls
void print_time_stamp(char type);

bool MidiSendProcessor::addMessage(const char* device_name, const unsigned char* c_message, std::size_t size)
{
    vector<unsigned char> midi_data;
    midi_data.assign(c_message, c_message + size);
    MidiDeviceAndMessage msg{ device_name, midi_data };
    m_messages.enqueue(std::move(msg));
    return true;
}


void MidiSendProcessor::flushMessages()
{
    m_flushing = true;
    MidiDeviceAndMessage msg;
    while (m_messages.try_dequeue(msg)) {
        // Just discard the message
    }
    m_flushing = false;
}


void MidiSendProcessor::run()
{
    MidiDeviceAndMessage msg;
    while (!g_threadsShouldFinish){
        bool available = m_messages.wait_dequeue_timed(msg, std::chrono::milliseconds(500));
        if (available && !m_flushing){
            processMessage(msg);
        }
    }
}


void MidiSendProcessor::processMessage(const MidiDeviceAndMessage& message_from_c)
{
    try{
        //print_time_stamp('B');
        send(message_from_c.device_name, &message_from_c.midi);
    }
    catch (const std::exception& e){
        m_logger.error("Exception thrown in MidiSendProcessor::ProcessMessage: {}!!!", e.what());
    }
}


void MidiSendProcessor::send(const string& outDevice, const std::vector< unsigned char >* msg)
{
    if (outDevice == "*") {
        // send to every known midi device
        for (auto& output : m_outputs) {
            output->send(msg);
        }
    } else {
        // send to the specified midi device
        // Look for it
        for (auto& output : m_outputs) {
            if (output->getNormalizedPortName() == outDevice) {
                output->send(msg);
                return;
            }
        }
        m_logger.error("Could not find the specified MIDI device: {}", outDevice);
    }
}


// TODO: can we remove these?
int MidiSendProcessor::getNMidiOuts() const
{
    return static_cast<int>(m_outputs.size());
}

int MidiSendProcessor::getMidiOutId(int n) const
{
    return m_outputs[n]->getPortId();
}

string MidiSendProcessor::getMidiOutName(int n) const
{
    return m_outputs[n]->getPortName();
}

string MidiSendProcessor::getNormalizedMidiOutName(int n) const
{
    return m_outputs[n]->getNormalizedPortName();
}
