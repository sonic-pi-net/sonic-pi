//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

// OSC stuff
#include <algorithm>
#include <iostream>
#include <cassert>

#include "api/logger.h"
#include "api/osc/osc_handler.h"
#include "api/osc/osc_pkt.hh"
#include "api/sonicpi_api.h"

#include "api/logger.h"
#include "api/string_utils.h"

#include "memory_profile.h"   // the scope slots the plugin tracks write

#undef max
#undef min

using namespace std::chrono;

namespace SonicPi
{

OscHandler::OscHandler(IAPIClient* pClient)
    : m_pClient(pClient)
{
    assert(pClient);
}

void OscHandler::oscMessage(std::vector<char> buffer)
{
    //QColor bg;

    pr.init(&buffer[0], buffer.size());

    oscpkt::Message* msg;
    while (pr.isOk() && (msg = pr.popMessage()) != 0)
    {
        if (msg->match("/log/multi_message"))
        {
            MessageInfo message;
            message.type = MessageType::Multi;

            oscpkt::Message::ArgReader ar = msg->arg();
            ar.popInt32(message.jobId);
            ar.popStr(message.threadName);
            ar.popStr(message.runtime);

            int msg_count;
            ar.popInt32(msg_count);

            for (int i = 0; i < msg_count; i++)
            {
                MessageData messageData;
                ar.popInt32(messageData.style);
                ar.popStr(messageData.text);
                message.multi.push_back(messageData);
            }

            m_pClient->Report(message);
        }
        else if (msg->match("/incoming/osc"))
        {
            std::string time;
            int id;
            std::string address;
            std::string args;
            if (msg->arg().popStr(time).popInt32(id).popStr(address).popStr(args).isOkNoMoreArgs())
            {
                CueInfo info;
                info.address = address;
                info.args = args;
                info.id = id;
                info.time = time;
                info.index = m_currentQueueIndex++;
                info.arrivalTime = std::chrono::high_resolution_clock::now();
                m_pClient->Cue(info);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /incoming/osc");
            }
        }
        else if (msg->match("/log/info"))
        {
            MessageInfo message;
            message.type = MessageType::Info;
            if (msg->arg().popInt32(message.style).popStr(message.text).isOkNoMoreArgs())
            {
                LOG(DBG, "/log/info: " << message.text);
                m_pClient->Report(message);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /info");
            }
        }
        else if (msg->match("/error"))
        {
            MessageInfo message;
            message.type = MessageType::RuntimeError;
            if (msg->arg().popInt32(message.jobId).popStr(message.text).popStr(message.backtrace).popInt32(message.line).popInt32(message.errorColStart).popInt32(message.errorColEnd).popStr(message.errorLineString).isOkNoMoreArgs())
            {
                LOG(DBG, "/error: " << message.text << " " << message.backtrace);
                m_pClient->Report(message);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /error: ");
            }
        }
        else if (msg->match("/syntax_error"))
        {
            MessageInfo message;
            message.type = MessageType::SyntaxError;
            if (msg->arg().popInt32(message.jobId).popStr(message.text).popStr(message.errorLineString).popInt32(message.line).popStr(message.lineNumString).popInt32(message.errorColStart).popInt32(message.errorColEnd).isOkNoMoreArgs())
            {
                LOG(DBG, "/syntax_error: " << message.text << " : " << message.errorLineString);
                m_pClient->Report(message);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /error: ");
            }
        }
        else if (msg->match("/buffer/replace"))
        {
            BufferInfo info;
            info.type = BufferType::Replace;
            if (msg->arg().popStr(info.id).popStr(info.content).popInt32(info.line).popInt32(info.index).popInt32(info.lineNumber).isOkNoMoreArgs())
            {
                // Lets supply the buffer index as an integer too
                info.bufferIndex = string_number_from_name(info.id);

                LOG(DBG, "/buffer/replace: " << info.id);
                m_pClient->Buffer(info);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /replace-buffer: ");
            }
        }
        else if (msg->match("/buffer/replace-idx"))
        {
            BufferInfo info;
            info.type = BufferType::ReplaceIndex;
            if (msg->arg().popInt32(info.bufferIndex).popStr(info.content).popInt32(info.line).popInt32(info.index).popInt32(info.lineNumber).isOkNoMoreArgs())
            {
                LOG(DBG, "/buffer/replace-idx: " << ": " << info.index);
                m_pClient->Buffer(info);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /replace-buffer: ");
            }
        }
        else if (msg->match("/update-info-text"))
        {
            MessageInfo message;
            message.type = MessageType::InfoText;
            if (msg->arg().popStr(message.text).isOkNoMoreArgs())
            {
                LOG(DBG, "/update-info-text: " << message.text);
                m_pClient->Report(message);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /update_info_text: ");
            }
        }
        else if (msg->match("/buffer/replace-lines"))
        {
            BufferInfo info;
            info.type = BufferType::ReplaceLines;
            if (msg->arg().popStr(info.id).popStr(info.content).popInt32(info.startLine).popInt32(info.finishLine).popInt32(info.pointLine).popInt32(info.pointIndex).isOkNoMoreArgs())
            {
                LOG(DBG, "/buffer/replace-lines: " << info.index);
                m_pClient->Buffer(info);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /replace-lines");
            }
        }
        else if (msg->match("/buffer/run-idx"))
        {
            BufferInfo info;
            info.type = BufferType::RunIndex;
            if (msg->arg().popInt32(info.bufferIndex).isOkNoMoreArgs())
            {
                LOG(DBG, "/buffer/run-idx: " << info.index);
                m_pClient->Buffer(info);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /buffer/run-idx");
            }
        }
        else if (msg->match("/exited"))
        {
            StatusInfo status;
            status.type = StatusType::Exited;
            if (msg->arg().isOkNoMoreArgs())
            {
                LOG(DBG, "/exited: Server asked us to exit");
                m_signal_server_stop = true;
                m_pClient->Status(status);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /exited");
            }
        }
        else if (msg->match("/exited-with-boot-error"))
        {
            MessageInfo message;
            message.type = MessageType::StartupError;
            if (msg->arg().popStr(message.text).isOkNoMoreArgs())
            {
                LOG(DBG, "/exited-with-boot-error: Sonic Pi Server failed to start with this error message:\n > " << message.text);
                m_signal_server_stop = true;
                m_pClient->Report(message);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /exited-with-boot-error");
            }
        }
        else if (msg->match("/clockwork/info"))
        {
            ScsynthInfo message;
            oscpkt::Message::ArgReader ar = msg->arg();

            if (!ar.popStr(message.text).isOk())
            {
                LOG(ERR, "/clockwork/info: failed to pop text arg");
            }
            else if (ar.isOkNoMoreArgs())
            {
                // Simple format: text only
                LOG(INFO, "/clockwork/info (simple): > " << message.text);
                m_pClient->Scsynth(message);
            }
            else
            {
                // Extended format: text, sampleRate, bufferSize,
                // numRates, rates..., numBufs, bufs..., numDrivers, drivers...,
                // currentDriver, outCh, inCh,
                // intendedDriver (trailing, absent on older engines)
                ar.popInt32(message.sampleRate);
                ar.popInt32(message.bufferSize);

                int numRates = 0;
                ar.popInt32(numRates);
                for (int i = 0; i < numRates; i++)
                {
                    int rate = 0;
                    ar.popInt32(rate);
                    message.availableSampleRates.push_back(rate);
                }

                int numBufs = 0;
                ar.popInt32(numBufs);
                for (int i = 0; i < numBufs; i++)
                {
                    int buf = 0;
                    ar.popInt32(buf);
                    message.availableBufferSizes.push_back(buf);
                }

                int numDrivers = 0;
                ar.popInt32(numDrivers);
                for (int i = 0; i < numDrivers; i++)
                {
                    std::string driver;
                    ar.popStr(driver);
                    message.availableDrivers.push_back(driver);
                }

                ar.popStr(message.currentDriver);

                int outCh = 0, inCh = 0;
                ar.popInt32(outCh);
                ar.popInt32(inCh);

                // Trailing fields, each optional so reports from engines
                // predating them simply run out of args here. Wire order:
                // outputLatencySamples(int32), then intendedDriver(str).
                int outputLatencySamples = 0;
                ar.popInt32(outputLatencySamples);
                std::string intendedDriver;
                bool hasIntendedDriver = ar.popStr(intendedDriver).isOk();

                LOG(INFO, "/clockwork/info (extended): > " << message.text
                    << " sr=" << message.sampleRate
                    << " bs=" << message.bufferSize
                    << " out=" << outCh << " in=" << inCh
                    << " outLat=" << outputLatencySamples);
                m_pClient->Scsynth(message);

                AudioDeviceConfigInfo config;
                config.sampleRate = message.sampleRate;
                config.bufferSize = message.bufferSize;
                config.outputChannels = outCh;
                config.inputChannels = inCh;
                config.outputLatencySamples = outputLatencySamples;
                config.availableSampleRates = std::move(message.availableSampleRates);
                config.availableBufferSizes = std::move(message.availableBufferSizes);
                config.availableDrivers = std::move(message.availableDrivers);
                config.currentDriver = std::move(message.currentDriver);
                config.intendedDriver = std::move(intendedDriver);
                config.hasIntendedDriver = hasIntendedDriver;
                m_pClient->AudioDeviceConfig(config);
            }
        }
        else if (msg->match("/clockwork/device-table"))
        {
            // Wire format (counts-first throughout):
            //   currentDriver(str), intendedDriver(str), numDrivers(int32),
            //   then per driver: name(str),
            //   numOutputs(int32), then per output: name(str), flags(str),
            //   numInputs(int32),  then per input:  name(str), flags(str).
            //   Flags are comma-separated capability tokens (see
            //   AudioDeviceTableInfo::DriverDevices), "" = plain device.
            AudioDeviceTableInfo table;
            oscpkt::Message::ArgReader ar = msg->arg();
            ar.popStr(table.currentDriver);
            ar.popStr(table.intendedDriver);
            int numDrivers = 0;
            ar.popInt32(numDrivers);
            bool ok = ar.isOk();
            for (int d = 0; ok && d < numDrivers; d++)
            {
                AudioDeviceTableInfo::DriverDevices group;
                ar.popStr(group.driver);
                int n = 0;
                ar.popInt32(n);
                for (int i = 0; ar.isOk() && i < n; i++)
                {
                    std::string s, f;
                    ar.popStr(s);
                    ar.popStr(f);
                    group.outputs.push_back(s);
                    group.outputFlags.push_back(f);
                }
                n = 0;
                ar.popInt32(n);
                for (int i = 0; ar.isOk() && i < n; i++)
                {
                    std::string s, f;
                    ar.popStr(s);
                    ar.popStr(f);
                    group.inputs.push_back(s);
                    group.inputFlags.push_back(f);
                }
                ok = ar.isOk();
                if (ok)
                    table.drivers.push_back(std::move(group));
            }
            if (!ok)
            {
                LOG(ERR, "/clockwork/device-table: malformed message, discarding");
            }
            else
            {
                LOG(INFO, "/clockwork/device-table: " << table.drivers.size()
                    << " drivers, current=" << table.currentDriver);
                m_pClient->AudioDeviceTable(table);
            }
        }
        else if (msg->match("/clockwork/devices"))
        {
            // Wire format:
            //   mode(str), current(str),
            //   name1(str), ..., nameN(str),
            //   sampleRate(int32),
            //   compat1(int32), ..., compatN(int32),
            //   type1(str), ..., typeN(str)   [trailing per-device drivers]
            AudioDevicesInfo devicesInfo;
            oscpkt::Message::ArgReader ar = msg->arg();

            ar.popStr(devicesInfo.mode);
            ar.popStr(devicesInfo.currentDevice);

            // Read device name strings until popStr fails (hits an int).
            std::string s;
            while (ar.popStr(s).isOk())
            {
                devicesInfo.devices.push_back(s);
            }

            // Re-create and skip past the strings to read the trailing
            // ints (sampleRate + per-device rate-compat flags), then
            // resume reading per-device driver type strings.
            ar = msg->arg();
            ar.popStr(s); // mode
            ar.popStr(s); // current
            for (size_t i = 0; i < devicesInfo.devices.size(); i++)
                ar.popStr(s);
            ar.popInt32(devicesInfo.sampleRate);
            // Skip the N rate-compat ints — we don't need them here.
            int dummy = 0;
            for (size_t i = 0; i < devicesInfo.devices.size(); i++)
                ar.popInt32(dummy);
            // Then per-device driver types.
            for (size_t i = 0; i < devicesInfo.devices.size(); i++) {
                std::string t;
                if (!ar.popStr(t).isOk()) break;
                devicesInfo.deviceTypes.push_back(t);
            }

            LOG(INFO, "/clockwork/devices: " << devicesInfo.devices.size()
                << " devices, mode=" << devicesInfo.mode
                << ", current=" << devicesInfo.currentDevice);
            m_pClient->AudioDevices(devicesInfo);
        }
        else if (msg->match("/clockwork/input-devices"))
        {
            // Wire format:
            //   currentInput(str), numDevices(int32),
            //   name1(str), ..., nameN(str),
            //   type1(str), ..., typeN(str)
            AudioInputDevicesInfo info;
            oscpkt::Message::ArgReader ar = msg->arg();

            ar.popStr(info.currentDevice);

            int numDevices = 0;
            ar.popInt32(numDevices);
            for (int i = 0; i < numDevices; i++)
            {
                std::string s;
                ar.popStr(s);
                info.devices.push_back(s);
            }
            // Per-device driver types.
            for (int i = 0; i < numDevices; i++) {
                std::string t;
                if (!ar.popStr(t).isOk()) break;
                info.deviceTypes.push_back(t);
            }

            LOG(INFO, "/clockwork/input-devices: " << info.devices.size()
                << " devices, current=" << info.currentDevice);
            m_pClient->AudioInputDevices(info);
        }
        else if (msg->match("/clockwork/statechange"))
        {
            std::string state, reason;
            msg->arg().popStr(state).popStr(reason);
            LOG(INFO, "/clockwork/statechange: " << state << " (" << reason << ")");
            if (state == "restarting") {
                // Show "switching" status in GUI while device change is in progress
                ScsynthInfo info;
                info.text = "Switching audio device...";
                m_pClient->Scsynth(info);
            }
        }
        else if (msg->match("/clockwork/setup"))
        {
            int sampleRate = 0, bufferSize = 0;
            msg->arg().popInt32(sampleRate).popInt32(bufferSize);
            LOG(DBG, "/clockwork/setup: sr=" << sampleRate << " bs=" << bufferSize);
            m_pClient->SupersonicSetup(sampleRate, bufferSize);
        }
        else if (msg->match("/clockwork/devices/reopen.reply"))
        {
            int accepted = 0;
            std::string reason;
            msg->arg().popInt32(accepted).popStr(reason);
            LOG(INFO, "/clockwork/devices/reopen.reply: accepted=" << accepted
                      << " reason='" << reason << "'");
            m_pClient->AudioDeviceReopenReply(accepted != 0, reason);
        }
        else if (msg->match("/clockwork/devices/reopen.done"))
        {
            int success = 0, bufferSize = 0;
            std::string deviceName, error;
            float sampleRate = 0;
            msg->arg().popInt32(success).popStr(deviceName).popFloat(sampleRate)
                      .popInt32(bufferSize).popStr(error);
            LOG(INFO, "/clockwork/devices/reopen.done: success=" << success
                      << " device='" << deviceName << "'"
                      << " sr=" << sampleRate << " bs=" << bufferSize
                      << (error.empty() ? "" : (" error='" + error + "'")));
        }
        else if (msg->match("/clockwork/devices/switch.done"))
        {
            // Wire format (see OscUdpServer::sendSwitchDone):
            //   success(int32),
            //   requestedOutput(str), requestedInput(str),
            //   actualOutput(str),    actualInput(str),
            //   error(str),
            //   inputUnavailable(int32), inputUnavailableReason(str)
            AudioSwitchOutcome outcome;
            int success = 0, inputUnavailable = 0;
            msg->arg().popInt32(success)
                      .popStr(outcome.requestedOutput)
                      .popStr(outcome.requestedInput)
                      .popStr(outcome.actualOutput)
                      .popStr(outcome.actualInput)
                      .popStr(outcome.error)
                      .popInt32(inputUnavailable)
                      .popStr(outcome.inputUnavailableReason);
            outcome.success          = (success != 0);
            outcome.inputUnavailable = (inputUnavailable != 0);
            LOG(INFO, "/clockwork/devices/switch.done: success=" << success
                      << " out req='" << outcome.requestedOutput << "' actual='" << outcome.actualOutput << "'"
                      << " in req='"  << outcome.requestedInput  << "' actual='" << outcome.actualInput  << "'"
                      << (outcome.error.empty() ? "" : (" error='" + outcome.error + "'"))
                      << (outcome.inputUnavailable ? (" inputUnavailable reason='" + outcome.inputUnavailableReason + "'") : ""));
            m_pClient->AudioSwitchDone(outcome);
        }
        else if (msg->match("/spider/ready"))
        {
            LOG(INFO, "/spider/ready");
            m_pClient->SpiderReady();
        }
        else if (msg->match("/ack"))
        {
            StatusInfo status;
            status.type = StatusType::Ack;
            if (msg->arg().popStr(status.id).isOkNoMoreArgs())
            {
                LOG(DBG, "/ack");
                m_server_started = true;
                m_pClient->Status(status);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /ack ");
            }
        }
        else if (msg->match("/clockwork/midi/out-ports"))
        {
            MidiInfo midi;
            midi.type = MidiType::Out;
            if (msg->arg().popStr(midi.portInfo).isOkNoMoreArgs())
            {
                LOG(DBG, "/clockwork/midi/out-ports/: " << midi.portInfo);
                m_pClient->Midi(midi);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /midi/out-ports");
            }
        }
        else if (msg->match("/clockwork/midi/in-ports"))
        {
            MidiInfo midi;
            midi.type = MidiType::In;
            if (msg->arg().popStr(midi.portInfo).isOkNoMoreArgs())
            {
                LOG(DBG, "/clockwork/midi/in-ports/: " << midi.portInfo);
                m_pClient->Midi(midi);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /midi/in-ports");
            }
        }
        else if (msg->match("/clockwork/track/list"))
        {
            // <lane_base> <count> then per track <id> <slot> <name> <send>
            // <return> <gain> <mute> <node_count>, then per node <handle>
            // <is_instrument> <bypass> <channel> <name> <vendor> <format>
            // <path> <index> <latency>; then, after the last track, per
            // track again: <timeline>. clockwork/docs/TRACKS.md, "The OSC
            // surface". The timelines trail the message; a message with one
            // track is a message with one more argument than its tracks and
            // nodes account for, and it must be read, not rejected.
            int32_t laneBase = 0, count = 0;
            auto arg = msg->arg().popInt32(laneBase).popInt32(count);
            std::vector<TrackInfo> tracks;
            bool ok = arg.isOk();
            for (int32_t i = 0; ok && i < count; ++i)
            {
                TrackInfo t;
                int32_t id = 0, slot = 0, send = 0, ret = 0, mute = 0, nodeCount = 0;
                float gain = 1.0f;
                arg = arg.popInt32(id).popInt32(slot).popStr(t.name).popInt32(send).popInt32(ret)
                         .popFloat(gain).popInt32(mute).popInt32(nodeCount);
                ok = arg.isOk();
                t.id = id; t.slot = slot; t.sendChannel = send; t.returnChannel = ret;
                t.gain = gain; t.mute = mute != 0;
                if (slot >= 0 && slot < SHM_SCOPE_TRACK_SLOTS)
                    t.scopeSlot = SHM_SCOPE_TRACK_SLOT_BASE + slot;
                for (int32_t k = 0; ok && k < nodeCount; ++k)
                {
                    TrackNodeInfo n;
                    int32_t h = 0, inst = 0, byp = 0, ch = 0, index = 0, latency = 0;
                    arg = arg.popInt32(h).popInt32(inst).popInt32(byp).popInt32(ch).popStr(n.name)
                             .popStr(n.vendor).popStr(n.format).popStr(n.path).popInt32(index)
                             .popInt32(latency);
                    ok = arg.isOk();
                    n.handle = h; n.instrument = inst != 0; n.bypass = byp != 0; n.channel = ch;
                    n.index = index; n.latency = latency;
                    if (ok) t.nodes.push_back(n);
                }
                if (ok) tracks.push_back(t);
            }
            for (size_t i = 0; ok && i < tracks.size(); ++i)
            {
                arg = arg.popStr(tracks[i].timeline);
                ok = arg.isOk();
            }
            if (ok && arg.isOkNoMoreArgs())
            {
                m_pClient->Tracks(laneBase, tracks);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /clockwork/track/list");
            }
        }
        else if (msg->match("/clockwork/track/state"))
        {
            // <id> <gain> <mute> <timeline>
            int32_t id = 0, mute = 0;
            float gain = 1.0f;
            std::string timeline;
            if (msg->arg().popInt32(id).popFloat(gain).popInt32(mute).popStr(timeline).isOkNoMoreArgs())
            {
                (void)timeline;   // read so the message parses; the panel does not show it yet
                m_pClient->TrackState(id, gain, mute != 0);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /clockwork/track/state");
            }
        }
        else if (msg->match("/clockwork/track/folders"))
        {
            // <n_extra> <dir…> <n_platform> <dir…>
            int32_t n = 0;
            auto arg = msg->arg().popInt32(n);
            std::vector<std::string> extra, platform;
            bool ok = arg.isOk();
            for (int32_t i = 0; ok && i < n; ++i)
            {
                std::string d;
                arg = arg.popStr(d);
                ok = arg.isOk();
                if (ok) extra.push_back(d);
            }
            arg = arg.popInt32(n);
            ok = ok && arg.isOk();
            for (int32_t i = 0; ok && i < n; ++i)
            {
                std::string d;
                arg = arg.popStr(d);
                ok = arg.isOk();
                if (ok) platform.push_back(d);
            }
            if (ok && arg.isOkNoMoreArgs())
            {
                m_pClient->TrackFolders(extra, platform);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /clockwork/track/folders");
            }
        }
        else if (msg->match("/clockwork/clock/audio/channels.reply"))
        {
            // <count> then per channel <channelId> <channelName> <peerId>
            // <peerName>. The answer to /clockwork/clock/audio/channels/get.
            int32_t count = 0;
            auto arg = msg->arg().popInt32(count);
            std::vector<LinkAudioChannelInfo> channels;
            bool ok = arg.isOk();
            for (int32_t i = 0; ok && i < count; ++i)
            {
                LinkAudioChannelInfo c;
                arg = arg.popStr(c.channelId).popStr(c.channelName).popStr(c.peerId).popStr(c.peerName);
                ok = arg.isOk();
                if (ok) channels.push_back(c);
            }
            if (ok && arg.isOkNoMoreArgs())
            {
                m_pClient->LinkAudioChannels(channels);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /clockwork/clock/audio/channels.reply");
            }
        }
        else if (msg->match("/clockwork/clock/audio/inputs.reply"))
        {
            // <count> then per subscription <peerName> <channelName> <busIdx>
            // <sampleRate> <sourceNumChannels> <bufferedMs> <connectionState>
            // <droppedSourceBuffers> <networkGapBuffers>
            // <totalSourceBufferCalls> <duplicateCountCalls> <latencySeconds>.
            // The four counters are read so the message parses; nothing
            // shows them yet.
            int32_t count = 0;
            auto arg = msg->arg().popInt32(count);
            std::vector<LinkAudioInputInfo> inputs;
            bool ok = arg.isOk();
            for (int32_t i = 0; ok && i < count; ++i)
            {
                LinkAudioInputInfo in;
                int32_t bus = -1, rate = 0, srcCh = 0, state = 0;
                int32_t dropped = 0, gaps = 0, total = 0, dup = 0;
                arg = arg.popStr(in.peerName).popStr(in.channelName).popInt32(bus).popInt32(rate)
                         .popInt32(srcCh).popFloat(in.bufferedMs).popInt32(state).popInt32(dropped)
                         .popInt32(gaps).popInt32(total).popInt32(dup).popFloat(in.latencySeconds);
                ok = arg.isOk();
                in.busIdx = bus; in.sampleRate = rate; in.numChannels = srcCh; in.state = state;
                if (ok) inputs.push_back(in);
            }
            if (ok && arg.isOkNoMoreArgs())
            {
                m_pClient->LinkAudioInputs(inputs);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /clockwork/clock/audio/inputs.reply");
            }
        }
        else if (msg->match("/clockwork/track/plugin/params"))
        {
            // <handle> <total> <offset> <count> then per param <id> <name>
            // <min> <max> <value> <group> <group_name> <automatable>
            int32_t handle = 0, total = 0, offset = 0, count = 0;
            auto arg = msg->arg().popInt32(handle).popInt32(total).popInt32(offset).popInt32(count);
            std::vector<TrackParamInfo> params;
            bool ok = arg.isOk();
            for (int32_t i = 0; ok && i < count; ++i)
            {
                TrackParamInfo p;
                int32_t id = 0, group = 0, automatable = 1;
                arg = arg.popInt32(id).popStr(p.name).popFloat(p.min).popFloat(p.max).popFloat(p.value)
                         .popInt32(group).popStr(p.groupName).popInt32(automatable);
                ok = arg.isOk();
                p.id = static_cast<uint32_t>(id); p.group = group; p.automatable = automatable != 0;
                if (ok) params.push_back(p);
            }
            if (ok && arg.isOkNoMoreArgs())
            {
                m_pClient->TrackParams(handle, static_cast<uint32_t>(total),
                                       static_cast<uint32_t>(offset), params);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /clockwork/track/plugin/params");
            }
        }
        else if (msg->match("/clockwork/track/plugin/param/edit")
                 || msg->match("/clockwork/track/plugin/param/value"))
        {
            // A parameter moved: the mirror image of /clockwork/track/plugin/param,
            // so a control the GUI draws agrees with the plugin. `edit` is the
            // plugin's own editor being used; `value` is code setting it by
            // name (track_control), which the GUI follows but does not treat
            // as someone reaching for a knob.
            const bool own = msg->match("/clockwork/track/plugin/param/edit");
            int32_t handle = 0, id = 0;
            float normalized = 0.0f;
            if (msg->arg().popInt32(handle).popInt32(id).popFloat(normalized).isOkNoMoreArgs())
            {
                m_pClient->TrackParamEdit(handle, static_cast<uint32_t>(id),
                                          static_cast<double>(normalized), own);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /clockwork/track/plugin/param/*");
            }
        }
        else if (msg->match("/clockwork/track/plugins"))
        {
            // <total> <offset> <count> then per plugin <name> <vendor>
            // <format> <path> <index> <is_instrument> — a page, like
            // plugin/params
            int32_t total = 0, offset = 0, count = 0;
            auto arg = msg->arg().popInt32(total).popInt32(offset).popInt32(count);
            std::vector<TrackPluginInfo> plugins;
            bool ok = arg.isOk();
            for (int32_t i = 0; ok && i < count; ++i)
            {
                TrackPluginInfo p;
                int32_t index = 0, inst = 0;
                arg = arg.popStr(p.name).popStr(p.vendor).popStr(p.format).popStr(p.path)
                         .popInt32(index).popInt32(inst);
                ok = arg.isOk();
                p.index = index; p.instrument = inst != 0;
                if (ok) plugins.push_back(p);
            }
            if (ok && arg.isOkNoMoreArgs())
            {
                m_pClient->TrackPlugins(static_cast<uint32_t>(total),
                                        static_cast<uint32_t>(offset), plugins);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /clockwork/track/plugins");
            }
        }
        else if (msg->match("/clockwork/track/error"))
        {
            std::string verb, detail;
            int handle = 0;
            if (msg->arg().popStr(verb).popStr(detail).popInt32(handle).isOkNoMoreArgs())
            {
                m_pClient->TrackError(verb, detail, handle);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /clockwork/track/error");
            }
        }
        else if (msg->match("/clockwork/gamepad/devices-list"))
        {
            std::string devices;
            if (msg->arg().popStr(devices).isOkNoMoreArgs())
            {
                LOG(DBG, "/clockwork/gamepad/devices-list: " << devices);
                m_pClient->GamepadDevices(devices);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /gamepad/devices-list");
            }
        }
        else if (msg->match("/version"))
        {
            VersionInfo versionInfo;
            if (msg->arg().popStr(versionInfo.version).popInt32(versionInfo.num).popStr(versionInfo.latestVersion).popInt32(versionInfo.latestVersionNum).popInt32(versionInfo.lastCheckedDay).popInt32(versionInfo.lastCheckedMonth).popInt32(versionInfo.lastCheckedYear).popStr(versionInfo.platform).isOkNoMoreArgs())
            {
                LOG(DBG, "/version: " << versionInfo.version);
                m_pClient->Version(versionInfo);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /version ");
            }
        }
        else if (msg->match("/run/started"))
        {
            int jobId;
            std::string workspace;
            if (msg->arg().popInt32(jobId).popStr(workspace).isOkNoMoreArgs())
            {
                LOG(DBG, "/run/started: " << jobId << " " << workspace);
                m_pClient->RunStarted(jobId, workspace);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /run/started");
            }
        }
        else if (msg->match("/flash"))
        {
            int jobId;
            std::string workspace;
            int line;
            if (msg->arg().popInt32(jobId).popStr(workspace).popInt32(line).isOkNoMoreArgs())
            {
                m_pClient->Flash(workspace, line);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /flash");
            }
        }
        else if (msg->match("/live_loop/scope"))
        {
            int jobId;
            std::string name;
            std::string workspace;
            int line;
            int scopeNum;
            if (msg->arg().popInt32(jobId).popStr(name).popStr(workspace).popInt32(line).popInt32(scopeNum).isOkNoMoreArgs())
            {
                m_pClient->LiveLoopScope(jobId, name, workspace, line, scopeNum);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /live_loop/scope");
            }
        }
        else if (msg->match("/live_loop/scope-ended"))
        {
            int jobId;
            std::string name;
            if (msg->arg().popInt32(jobId).popStr(name).isOkNoMoreArgs())
            {
                m_pClient->LiveLoopScopeEnded(jobId, name);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /live_loop/scope-ended");
            }
        }
        else if (msg->match("/run/ended"))
        {
            int jobId;
            if (msg->arg().popInt32(jobId).isOkNoMoreArgs())
            {
                LOG(DBG, "/run/ended: " << jobId);
                m_pClient->RunEnded(jobId);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /run/ended");
            }
        }
        else if (msg->match("/runs/all-completed"))
        {
            LOG(DBG, "/runs/all-completed: ");
            if (msg->arg().isOkNoMoreArgs())
            {
                StatusInfo status;
                status.type = StatusType::AllComplete;
                m_pClient->Status(status);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /runs/all-completed ");
            }
        }
        else if (msg->match("/link-num-peers"))
        {
          int numPeers;
          if(msg->arg().popInt32(numPeers).isOkNoMoreArgs())
          {
            m_pClient->ActiveLinks(numPeers);
            LOG(DBG, "/link-num-peers: " << numPeers);
          }
          else
          {
            LOG(ERR, "Unhandled OSC msg /link-num-peers");
          }
        }
        else if (msg->match("/link-bpm"))
        {
          float bpmf;
          if(msg->arg().popFloat(bpmf).isOkNoMoreArgs())
          {

            double bpm = (double) bpmf;
            if(bpm < 20) {
              bpm = 20.0 ;
            } else if(bpm > 999) {
              bpm = 999.0 ;
            }
            m_pClient->BPM(bpm);
            LOG(DBG, "/link-bpm: " << bpm);
          }
          else
          {
            LOG(ERR, "Unhandled OSC msg /link-bpm");
          }
        }
        else if (msg->match("/mixer/settings"))
        {
          float drive, outputVolume;
          if (msg->arg().popFloat(drive).popFloat(outputVolume).isOkNoMoreArgs())
          {
            m_pClient->MixerSettings((double)drive, (double)outputVolume);
            LOG(DBG, "/mixer/settings: drive=" << drive << " outputVolume=" << outputVolume);
          }
          else
          {
            LOG(ERR, "Unhandled OSC msg /mixer/settings");
          }
        }
        else if (msg->partialMatch("/n_").isOk()
              || msg->partialMatch("/done").isOk()
              || msg->partialMatch("/fail").isOk()
              || msg->partialMatch("/synced").isOk())
        {
            // scsynth-protocol broadcasts that reach the GUI as a side
            // effect of /notify subscription; Spider consumes these.
        }
        else
        {
            LOG(DBG, "Unhandled OSC message: " << msg->addressPattern());
        }
    }
    std::cout << std::flush;
}

} // namespace SonicPi
