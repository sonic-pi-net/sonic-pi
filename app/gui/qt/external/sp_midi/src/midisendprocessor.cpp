// MIT License

// Copyright (c) 2016-2020 Luis Lloret

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


void MidiSendProcessor::prepareOutputs(const vector<string>& outputNames)
{
    m_outputs.clear();
    for (auto& outputName : outputNames) {
        try {
            auto midiOut = make_unique<MidiOut>(outputName);
            m_outputs.push_back(std::move(midiOut));
        }
        catch (const RtMidiError& e) {
            cout << "Could not open output device " << outputName << ": " << e.what() << endl;
            //throw;
        }
    }
}

// TODO: during initial testing to measure latency of async calls
void print_time_stamp(char type);

bool MidiSendProcessor::addMessage(const char* device_name, const unsigned char* c_message, std::size_t size)
{
    lock_guard<mutex> lock(m_messages_mutex);
    vector<unsigned char> midi_data;
    midi_data.assign(c_message, c_message + size);
    MidiDeviceAndMessage msg{ device_name, midi_data };
    m_messages.emplace_back(msg);
    m_data_in_midi_queue.signal();
    return true;
}


void MidiSendProcessor::flushMessages()
{
    lock_guard<mutex> lock(m_messages_mutex);
    m_messages.clear();
}


void MidiSendProcessor::run()
{
    while (!threadShouldExit()){
        if (m_data_in_midi_queue.wait(500) == true){
            lock_guard<mutex> lock(m_messages_mutex);
            while (!m_messages.empty()){
                processMessage(m_messages.front());
                m_messages.pop_front();
            }
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
        m_logger.error("Could not find the MIDI device specified in the OSC message: {}", outDevice);
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
