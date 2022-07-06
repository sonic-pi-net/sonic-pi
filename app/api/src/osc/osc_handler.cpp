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
        else if (msg->match("/scsynth/info"))
        {

            ScsynthInfo message;
            if (msg->arg().popStr(message.text).isOkNoMoreArgs())
            {
                LOG(DBG, "/scsynth/info: > " << message.text);
                m_pClient->Scsynth(message);
            }
            else
            {
                LOG(ERR, "Unhandled OSC msg /scsynth/info");
            }
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
        else
        {
            LOG(ERR, "Unhandled OSC message: " << msg->addressPattern());
        }
    }
    std::cout << std::flush;
}

} // namespace SonicPi
