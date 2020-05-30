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

#include <regex>
#include "oscinprocessor.h"
#include "utils.h"

using namespace std;
using namespace juce;

OscInProcessor::OscInProcessor(bool local, int oscListenPort)
{
    m_input = make_unique<OscIn>(local, oscListenPort, this);
}

void OscInProcessor::prepareOutputs(const vector<string>& outputNames)
{
    m_outputs.clear();
    for (auto& outputName : outputNames) {
        auto midiOut = make_unique<MidiOut>(outputName);
        m_outputs.push_back(std::move(midiOut));
    }
}

void OscInProcessor::ProcessMessage(const osc::ReceivedMessage& message, const IpEndpointName& remoteEndpoint)
{
    string addressPattern(message.AddressPattern());
    m_logger.info("Received OSC message with address pattern: {}", addressPattern);
    dumpOscBody(message);

    regex addressRegex("/(.+?)/(.+)");
    smatch match;
    if (regex_match(addressPattern, match, addressRegex)) {
        // We are interested in groups [1] and [2]. [1] -> device, [3] -> command / raw
        string rawOutDeviceName = match[1];
        // don't normalize name if we are given the wildcard name of *
        if (rawOutDeviceName != "*") {
            local_utils::safeOscString(rawOutDeviceName);
        }

        const string& command = match[2];
        const string& outDevice = rawOutDeviceName;

        if (command == "clock") {
            processClockMessage(outDevice);
        } else if (command == "raw") {
            processRawMessage(outDevice, message);
        } else if (command == "note_on") {
            processNoteOnMessage(outDevice, message);
        } else if (command == "note_off") {
            processNoteOffMessage(outDevice, message);
        } else if (command == "control_change") {
            processControlChangeMessage(outDevice, message);
        } else if (command == "pitch_bend") {
            processPitchBendMessage(outDevice, message);
        } else if (command == "channel_pressure") {
            processChannelPressureMessage(outDevice, message);
        } else if (command == "poly_pressure") {
            processPolyPressureMessage(outDevice, message);
        } else if (command == "start") {
            processStartMessage(outDevice);
        } else if (command == "continue") {
            processContinueMessage(outDevice);
        } else if (command == "stop") {
            processStopMessage(outDevice);
        } else if (command == "active_sensing") {
            processActiveSenseMessage(outDevice);
        } else if (command == "program_change") {
            processProgramChangeMessage(outDevice, message);
        } else if (command == "log_level") {
            processLogLevelMessage(message);
        } else if (command == "log_to_osc") {
            processLogToOscMessage(message);
        } else {
            m_logger.error("Unknown command on OSC message: {}. Ignoring", command);
        }
    } else {
        m_logger.error("No match on address pattern: {}", addressPattern);
    }
}

void OscInProcessor::dumpOscBody(const osc::ReceivedMessage& message)
{
    m_logger.debug("Got {} arguments", message.ArgumentCount());

    auto arg = message.ArgumentsBegin();
    while (arg != message.ArgumentsEnd()) {
        if (arg->IsFloat())
            m_logger.debug("F: {}", arg->AsFloat());
        else if (arg->IsInt32())
            m_logger.debug("I: {}", arg->AsInt32());
        else if (arg->IsString())
            m_logger.debug("S: {}", arg->AsString());
        else if (arg->IsBlob())
            m_logger.debug("B: this is a blob");
        else
            m_logger.debug("X: not sure what this field is");

        arg++;
    }
}

void OscInProcessor::send(const string& outDevice, const MidiMessage& msg)
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

// FIXME: For now send to all outputs. Later send only to the appropriate outputs
void OscInProcessor::processRawMessage(const string& outDevice, const osc::ReceivedMessage& message)
{
    auto arg = message.ArgumentsBegin();
    if (arg->IsBlob()) {
        const void* blobData;
        osc::int32 blobSize; // Use OSC datatype, otherwise croaks on RPi
        arg->AsBlob(blobData, blobSize);
        MidiMessage raw(blobData, blobSize);
        for (auto& output : m_outputs) {
            output->send(raw);
        }
    } else {
        unsigned char midiMessage[1024];
        int midiMessageSize = 0;

        while (arg != message.ArgumentsEnd()) {
            if (arg->IsInt32()) {
                midiMessage[midiMessageSize++] = arg->AsInt32();
            }
            arg++;
        }
        MidiMessage raw(midiMessage, midiMessageSize);
        send(outDevice, raw);
    }
}

// note_on OSC messages have this layout: channel (int32), note (int32), velocity (int32)
void OscInProcessor::processNoteOnMessage(const string& outDevice, const osc::ReceivedMessage& message)
{
    osc::ReceivedMessage::const_iterator arg = message.ArgumentsBegin();
    int channel, note, velocity;
    try {
        channel = (arg++)->AsInt32();
        note = (arg++)->AsInt32();
        velocity = (arg++)->AsInt32();
        if (arg != message.ArgumentsEnd()) {
            throw(osc::WrongArgumentTypeException());
            return;
        }
    } catch (const osc::WrongArgumentTypeException&) {
        m_logger.error("OSC note_on message: Error parsing args. Expected int32, int32, int32.");
        return;
    }

    if (channel > 0) {
        // Send to specific channel
        MidiMessage midiMessage{ MidiMessage::noteOn(channel, note, (uint8)velocity) };
        send(outDevice, midiMessage);
    } else if (channel <= 0) {
        for (int chan = 1; chan <= 16; chan++) {
            // Send to all channels
            MidiMessage midiMessage{ MidiMessage::noteOn(chan, note, (uint8)velocity) };
            send(outDevice, midiMessage);
        }
    }
}

void OscInProcessor::processClockMessage(const string& outDevice)
{
    MidiMessage midiMessage{ MidiMessage::midiClock() };
    send(outDevice, midiMessage);
}

void OscInProcessor::processStartMessage(const string& outDevice)
{
    MidiMessage midiMessage{ MidiMessage::midiStart() };
    send(outDevice, midiMessage);
}

void OscInProcessor::processContinueMessage(const string& outDevice)
{
    MidiMessage midiMessage{ MidiMessage::midiContinue() };
    send(outDevice, midiMessage);
}

void OscInProcessor::processStopMessage(const string& outDevice)
{
    MidiMessage midiMessage{ MidiMessage::midiStop() };
    send(outDevice, midiMessage);
}

void OscInProcessor::processActiveSenseMessage(const string& outDevice)
{
    MidiMessage midiMessage{ MidiMessage() };
    send(outDevice, midiMessage);
}

// note_off OSC messages have this layout: channel (int32), note (int32), velocity (int32)
void OscInProcessor::processNoteOffMessage(const string& outDevice, const osc::ReceivedMessage& message)
{
    osc::ReceivedMessage::const_iterator arg = message.ArgumentsBegin();
    int channel, note, velocity;
    try {
        channel = (arg++)->AsInt32();
        note = (arg++)->AsInt32();
        velocity = (arg++)->AsInt32();
        if (arg != message.ArgumentsEnd()) {
            throw(osc::WrongArgumentTypeException());
        }
    } catch (const osc::WrongArgumentTypeException&) {
        m_logger.error("OSC note_off message: Error parsing args. Expected int32, int32, int32.");
        return;
    }

    if (channel > 0) {
        // Send to specific channel
        MidiMessage midiMessage{ MidiMessage::noteOff(channel, note, (uint8)velocity) };
        send(outDevice, midiMessage);
    } else {
        // Send to all channels
        for (int chan = 1; chan <= 16; chan++) {
            MidiMessage midiMessage{ MidiMessage::noteOff(chan, note, (uint8)velocity) };
            send(outDevice, midiMessage);
        }
    }
}

// control_change OSC messages have this layout: channel (int32), number (int32), velocity (int32)
void OscInProcessor::processControlChangeMessage(const string& outDevice, const osc::ReceivedMessage& message)
{
    osc::ReceivedMessage::const_iterator arg = message.ArgumentsBegin();
    int channel, number, value;
    try {
        channel = (arg++)->AsInt32();
        number = (arg++)->AsInt32();
        value = (arg++)->AsInt32();
        if (arg != message.ArgumentsEnd()) {
            throw(osc::WrongArgumentTypeException());
        }
    } catch (const osc::WrongArgumentTypeException&) {
        m_logger.error("OSC control_change message: Error parsing args. Expected int32, int32, int32.");
        return;
    }

    if (channel > 0) {
        // Send to specific channel
        MidiMessage midiMessage{ MidiMessage::controllerEvent(channel, number, value) };
        send(outDevice, midiMessage);
    } else {
        // Send to all channels
        for (int chan = 1; chan <= 16; chan++) {
            MidiMessage midiMessage{ MidiMessage::controllerEvent(chan, number, value) };
            send(outDevice, midiMessage);
        }
    }
}

// pitch_bend OSC messages have this layout: channel (int32), value (int32). Note that the midi resolution for pitch_bend value is 14 bits
void OscInProcessor::processPitchBendMessage(const string& outDevice, const osc::ReceivedMessage& message)
{
    osc::ReceivedMessage::const_iterator arg = message.ArgumentsBegin();
    int channel, value;
    try {
        channel = (arg++)->AsInt32();
        value = (arg++)->AsInt32();
        if (arg != message.ArgumentsEnd()) {
            throw(osc::WrongArgumentTypeException());
        }
    } catch (const osc::WrongArgumentTypeException&) {
        m_logger.error("OSC pitch_bend message: Error parsing args. Expected int32, int32.");
        return;
    }

    if (channel > 0) {
        // Send to specific channel
        MidiMessage midiMessage{ MidiMessage::pitchWheel(channel, value) };
        send(outDevice, midiMessage);
    } else {
        // Send to all channels
        for (int chan = 1; chan <= 16; chan++) {
            MidiMessage midiMessage{ MidiMessage::pitchWheel(chan, value) };
            send(outDevice, midiMessage);
        }
    }
}

// channel_pressure OSC messages have this layout: channel (int32), value (int32).
void OscInProcessor::processChannelPressureMessage(const string& outDevice, const osc::ReceivedMessage& message)
{
    osc::ReceivedMessage::const_iterator arg = message.ArgumentsBegin();
    int channel, value;
    try {
        channel = (arg++)->AsInt32();
        value = (arg++)->AsInt32();
        if (arg != message.ArgumentsEnd()) {
            throw(osc::WrongArgumentTypeException());
        }
    } catch (const osc::WrongArgumentTypeException&) {
        m_logger.error("OSC channel_pressure message: Error parsing args. Expected int32, int32.");
        return;
    }

    if (channel > 0) {
        // Send to specific channel
        MidiMessage midiMessage{ MidiMessage::channelPressureChange(channel, value) };
        send(outDevice, midiMessage);
    } else {
        // Send to all channels
        for (int chan = 1; chan <= 16; chan++) {
            MidiMessage midiMessage{ MidiMessage::channelPressureChange(chan, value) };
            send(outDevice, midiMessage);
        }
    }
}

// poly_pressure OSC messages have this layout: channel (int32), note (int32), velocity (int32)
void OscInProcessor::processPolyPressureMessage(const string& outDevice, const osc::ReceivedMessage& message)
{
    osc::ReceivedMessage::const_iterator arg = message.ArgumentsBegin();
    int channel, note, value;
    try {
        channel = (arg++)->AsInt32();
        note = (arg++)->AsInt32();
        value = (arg++)->AsInt32();
        if (arg != message.ArgumentsEnd()) {
            throw(osc::WrongArgumentTypeException());
        }
    } catch (const osc::WrongArgumentTypeException&) {
        m_logger.error("OSC poly_pressure message: Error parsing args. Expected int32, int32, int32.");
        return;
    }

    if (channel > 0) {
        // Send to specific channel
        MidiMessage midiMessage{ MidiMessage::aftertouchChange(channel, note, value) };
        send(outDevice, midiMessage);
    } else {
        // Send to all channels
        for (int chan = 1; chan <= 16; chan++) {
            MidiMessage midiMessage{ MidiMessage::aftertouchChange(chan, note, value) };
            send(outDevice, midiMessage);
        }
    }
}

// program_change OSC messages have this layout: channel (int32), program (int32)
void OscInProcessor::processProgramChangeMessage(const string& outDevice, const osc::ReceivedMessage& message)
{
    osc::ReceivedMessage::const_iterator arg = message.ArgumentsBegin();
    int channel, program;
    try {
        channel = (arg++)->AsInt32();
        program = (arg++)->AsInt32();
        if (arg != message.ArgumentsEnd()) {
            throw(osc::WrongArgumentTypeException());
        }
    } catch (const osc::WrongArgumentTypeException&) {
        m_logger.error("OSC program_change message: Error parsing args. Expected int32, int32.");
        return;
    }

    if (channel > 0) {
        // Send to specific channel
        MidiMessage midiMessage{ MidiMessage::programChange(channel, program) };
        send(outDevice, midiMessage);
    } else {
        // Send to all channels
        for (int chan = 1; chan <= 16; chan++) {
            MidiMessage midiMessage{ MidiMessage::programChange(chan, program) };
            send(outDevice, midiMessage);
        }
    }
}

void OscInProcessor::processLogLevelMessage(const osc::ReceivedMessage& message)
{
    osc::ReceivedMessage::const_iterator arg = message.ArgumentsBegin();
    int level;
    try {
        level = (arg++)->AsInt32();
        if (arg != message.ArgumentsEnd()) {
            throw(osc::WrongArgumentTypeException());
        }
    } catch (const osc::WrongArgumentTypeException&) {
        m_logger.error("OSC log_level message: Error parsing args. Expected int32.");
        return;
    }
    m_logger.setLogLevel(level);
}

void OscInProcessor::processLogToOscMessage(const osc::ReceivedMessage& message)
{
    osc::ReceivedMessage::const_iterator arg = message.ArgumentsBegin();
    int enable;
    try {
        enable = (arg++)->AsInt32();
        if (arg != message.ArgumentsEnd()) {
            throw(osc::WrongArgumentTypeException());
        }
    } catch (const osc::WrongArgumentTypeException&) {
        m_logger.error("OSC log_to_osc message: Error parsing args. Expected int32.");
        return;
    }
    m_logger.setSendToOSC(enable != 0);
}

void OscInProcessor::ProcessBundle(const osc::ReceivedBundle& b, const IpEndpointName& remoteEndpoint)
{
    m_logger.error("Received OSC bundle. Ignoring for now!");
}

int OscInProcessor::getNMidiOuts() const
{
    return static_cast<int>(m_outputs.size());
}

int OscInProcessor::getMidiOutId(int n) const
{
    return m_outputs[n]->getPortId();
}

const std::vector<std::string> OscInProcessor::getKnownOscMessages()
{
    return std::vector<std::string>{"clock", "raw", "note_on", "note_off", "control_change",
        "pitch_bend", "channel_pressure", "poly_pressure", "start", "continue", "stop",
        "active_sensing", "program_change", "log_level", "log_to_osc"};
}

string OscInProcessor::getMidiOutName(int n) const
{
    return m_outputs[n]->getPortName();
}

string OscInProcessor::getNormalizedMidiOutName(int n) const
{
    return m_outputs[n]->getNormalizedPortName();
}
