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
            if (msg->arg().popInt32(message.jobId).popStr(message.text).popStr(message.backtrace).popInt32(message.line).isOkNoMoreArgs())
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
            if (msg->arg().popInt32(message.jobId).popStr(message.text).popStr(message.errorLineString).popInt32(message.line).popStr(message.lineNumString).isOkNoMoreArgs())
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
        else if (msg->match("/supersonic/info"))
        {
            ScsynthInfo message;
            oscpkt::Message::ArgReader ar = msg->arg();

            if (!ar.popStr(message.text).isOk())
            {
                LOG(ERR, "/supersonic/info: failed to pop text arg");
            }
            else if (ar.isOkNoMoreArgs())
            {
                // Simple format: text only
                LOG(INFO, "/supersonic/info (simple): > " << message.text);
                m_pClient->Scsynth(message);
            }
            else
            {
                // Extended format: text, sampleRate, bufferSize,
                // numRates, rates..., numBufs, bufs..., numDrivers, drivers..., currentDriver
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

                LOG(INFO, "/supersonic/info (extended): > " << message.text
                    << " sr=" << message.sampleRate
                    << " bs=" << message.bufferSize
                    << " out=" << outCh << " in=" << inCh);
                m_pClient->Scsynth(message);

                AudioDeviceConfigInfo config;
                config.sampleRate = message.sampleRate;
                config.bufferSize = message.bufferSize;
                config.outputChannels = outCh;
                config.inputChannels = inCh;
                config.availableSampleRates = std::move(message.availableSampleRates);
                config.availableBufferSizes = std::move(message.availableBufferSizes);
                config.availableDrivers = std::move(message.availableDrivers);
                config.currentDriver = std::move(message.currentDriver);
                m_pClient->AudioDeviceConfig(config);
            }
        }
        else if (msg->match("/supersonic/devices"))
        {
            // Format: mode(str), current(str), device1(str), ..., sampleRate(int32)
            AudioDevicesInfo devicesInfo;
            oscpkt::Message::ArgReader ar = msg->arg();

            ar.popStr(devicesInfo.mode);
            ar.popStr(devicesInfo.currentDevice);

            // Read device name strings until popStr fails (hits an int or end)
            std::string s;
            while (ar.popStr(s).isOk())
            {
                devicesInfo.devices.push_back(s);
            }

            // popStr failure sets err in ArgReader, blocking further pops.
            // Re-create and skip past the strings to read the trailing int.
            ar = msg->arg();
            ar.popStr(s); // mode
            ar.popStr(s); // current
            for (size_t i = 0; i < devicesInfo.devices.size(); i++)
                ar.popStr(s);
            ar.popInt32(devicesInfo.sampleRate);

            LOG(INFO, "/supersonic/devices: " << devicesInfo.devices.size()
                << " devices, mode=" << devicesInfo.mode
                << ", current=" << devicesInfo.currentDevice);
            m_pClient->AudioDevices(devicesInfo);
        }
        else if (msg->match("/supersonic/input-devices"))
        {
            // Format: currentInput(str), numDevices(int32), device1(str), ..., deviceN(str)
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

            LOG(INFO, "/supersonic/input-devices: " << info.devices.size()
                << " devices, current=" << info.currentDevice);
            m_pClient->AudioInputDevices(info);
        }
        else if (msg->match("/supersonic/statechange"))
        {
            std::string state, reason;
            msg->arg().popStr(state).popStr(reason);
            LOG(INFO, "/supersonic/statechange: " << state << " (" << reason << ")");
            if (state == "restarting") {
                // Show "switching" status in GUI while device change is in progress
                ScsynthInfo info;
                info.text = "Switching audio device...";
                m_pClient->Scsynth(info);
            }
        }
        else if (msg->match("/supersonic/setup"))
        {
            int sampleRate = 0, bufferSize = 0;
            msg->arg().popInt32(sampleRate).popInt32(bufferSize);
            LOG(DBG, "/supersonic/setup: sr=" << sampleRate << " bs=" << bufferSize);
            m_pClient->SupersonicSetup(sampleRate, bufferSize);
        }
        else if (msg->match("/supersonic/devices/reopen.reply"))
        {
            int accepted = 0;
            std::string reason;
            msg->arg().popInt32(accepted).popStr(reason);
            LOG(INFO, "/supersonic/devices/reopen.reply: accepted=" << accepted
                      << " reason='" << reason << "'");
        }
        else if (msg->match("/supersonic/devices/reopen.done"))
        {
            int success = 0, bufferSize = 0;
            std::string deviceName, error;
            float sampleRate = 0;
            msg->arg().popInt32(success).popStr(deviceName).popFloat(sampleRate)
                      .popInt32(bufferSize).popStr(error);
            LOG(INFO, "/supersonic/devices/reopen.done: success=" << success
                      << " device='" << deviceName << "'"
                      << " sr=" << sampleRate << " bs=" << bufferSize
                      << (error.empty() ? "" : (" error='" + error + "'")));
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
        else if (msg->match("/midi/out-ports"))
        {
            MidiInfo midi;
            midi.type = MidiType::Out;
            if (msg->arg().popStr(midi.portInfo).isOkNoMoreArgs())
            {
                LOG(DBG, "/midi/out-ports/: " << midi.portInfo);
                m_pClient->Midi(midi);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /midi/out-ports");
            }
        }
        else if (msg->match("/midi/in-ports"))
        {
            MidiInfo midi;
            midi.type = MidiType::In;
            if (msg->arg().popStr(midi.portInfo).isOkNoMoreArgs())
            {
                LOG(DBG, "/midi/in-ports/: " << midi.portInfo);
                m_pClient->Midi(midi);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /midi/in-ports");
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
