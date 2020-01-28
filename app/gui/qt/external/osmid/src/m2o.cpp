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

#include <stdexcept>
#include <iostream>
#include "cxxopts.hpp"
#include "midiin.h"
#include "oscout.h"
#include "midiinprocessor.h"
#include "osc/OscOutboundPacketStream.h"
#include "version.h"
#include "utils.h"

using namespace std;

void listAvailablePorts()
{
    auto inputs = MidiIn::getInputNames();
    cout << "Found " << inputs.size() << " MIDI inputs." << endl;
    for (unsigned int i = 0; i < inputs.size(); i++) {
        cout << "   (" << i << "): " << inputs[i] << endl;
    }
}

struct ProgramOptions {
    vector<string> midiInputNames;
    bool allMidiInputs;
    string oscOutputHost;
    vector<int> oscOutputPorts;
    bool useOscTemplate;
    string oscTemplate;
    bool oscRawMidiMessage;
    bool oscHeartbeat;
    bool useVirtualPort;
    string virtualPortName;
    unsigned int monitor;
    bool listPorts;
};

void showVersion()
{
    cout << "m2o version " << M2O_VERSION << endl;
}

int setup_and_parse_program_options(int argc, char* argv[], ProgramOptions& programOptions)
{
    cxxopts::Options options("m2o", "Bridges MIDI to OSC");

    options.add_options()
    ("l,list", "List input MIDI devices", cxxopts::value<bool>(programOptions.listPorts))
    ("v,virtualport", "Create a Virtual MIDI output port that will be monitored for MIDI (useful to have MIDI->OSC inside your favourite DAW)", cxxopts::value<string>(programOptions.virtualPortName))
    ("i,midiin", "MIDI Input device - can be specified multiple times (default: all)", cxxopts::value<vector<string> >(programOptions.midiInputNames))
    ("H,oschost", "OSC Output host", cxxopts::value<string>(programOptions.oscOutputHost)->default_value("127.0.0.1"))
    ("o,oscport", "OSC Output port - can be specified multiple times (default:57120)", cxxopts::value<vector<int> >(programOptions.oscOutputPorts))
    ("t,osctemplate", "OSC output template (use $n: midi port name, $i: midi port id, $c: midi channel, $m: message_type", cxxopts::value<string>(programOptions.oscTemplate))
    ("r,oscrawmidimessage", "OSC send the raw MIDI data as part of the OSC message", cxxopts::value<bool>(programOptions.oscRawMidiMessage))
    ("b,heartbeat", "OSC send the heartbeat with info about the active MIDI devices", cxxopts::value<bool>(programOptions.oscHeartbeat))
    ("m,monitor", "Monitor and logging level (lower more verbose)", cxxopts::value<unsigned int>(programOptions.monitor)->default_value("2")->implicit_value("1"))
    ("h,help", "Display this help message")
    ("version", "Show the version number");

    try{
        options.parse(argc, argv);
    } catch(const cxxopts::OptionParseException& e){
        cout << e.what() << "\n\n";
        cout << options.help() << endl;
        return -1;
    }

    if (options.count("help")) {
        cout << options.help() << endl;
        return 1;
    }

    if (options.count("version")) {
        showVersion();
    }

    programOptions.useOscTemplate = (options.count("osctemplate") ? true : false);
    programOptions.oscRawMidiMessage = (options.count("oscrawmidimessage") ? true : false);
    programOptions.oscHeartbeat = (options.count("heartbeat") ? true : false);
    programOptions.useVirtualPort = (options.count("virtualport") ? true : false);
    programOptions.listPorts = (options.count("list") ? true : false);

    if (!options.count("midiin")) {
        // by default add all input devices
        programOptions.midiInputNames = MidiIn::getInputNames();
        programOptions.allMidiInputs = true;
    } else {
        programOptions.allMidiInputs = false;
    }

    if (!options.count("oscport")) {
        programOptions.oscOutputPorts.push_back(57120);
    }

    return 0;
}

void prepareMidiProcessors(vector<unique_ptr<MidiInProcessor> >& midiInputProcessors, const ProgramOptions& popts, vector<shared_ptr<OscOutput> >& oscOutputs)
{
    // Should we open all devices, or just the ones passed as parameters?
    vector<string> midiInputsToOpen = (popts.allMidiInputs ? MidiIn::getInputNames() : popts.midiInputNames);

    for (auto& input : midiInputsToOpen) {
        try {
            auto midiInputProcessor = make_unique<MidiInProcessor>(input, oscOutputs, false);
            if (popts.useOscTemplate)
                midiInputProcessor->setOscTemplate(popts.oscTemplate);
            midiInputProcessor->setOscRawMidiMessage(popts.oscRawMidiMessage);
            midiInputProcessors.push_back(std::move(midiInputProcessor));
        } catch (const std::out_of_range&) {
            cout << "The device " << input << " does not exist";
            throw;
        }
    }
}

static std::atomic<bool> g_wantToExit(false);

#if WIN32
BOOL ctrlHandler(DWORD fdwCtrlType)
{
    if (fdwCtrlType == CTRL_C_EVENT) {
        g_wantToExit = true;
    }
    return TRUE;
}
#else
void ctrlHandler(int signal)
{
    cout << "Ctrl-C event" << endl;
    g_wantToExit = true;
}
#endif

void sendHeartBeat(const vector<unique_ptr<MidiInProcessor> >& midiProcessors, const vector<shared_ptr<OscOutput> >& oscOutputs)
{
    char buffer[2048];
    osc::OutboundPacketStream p(buffer, 2048);
    p << osc::BeginMessage("/m2o/heartbeat");
    for (const auto& midiProcessor : midiProcessors) {

      p << midiProcessor->getInputId() << midiProcessor->getInputPortname().c_str() << midiProcessor->getInputNormalizedPortName().c_str();
    }
    p << osc::EndMessage;
    MonitorLogger::getInstance().debug("sending OSC: [/m2o/heartbeat] -> ");
    for (const auto& midiProcessor : midiProcessors) {
        MonitorLogger::getInstance().debug("   {}, {}", midiProcessor->getInputId(), midiProcessor->getInputPortname());
    }

    for (auto& output : oscOutputs) {
        output->sendUDP(p.Data(), p.Size());
        local_utils::logOSCMessage(p.Data(), p.Size());
    }
}

int main(int argc, char* argv[])
{
    // midiInputProcessors will contain the list of active MidiIns at a given time
    vector<unique_ptr<MidiInProcessor> > midiInputProcessors;
    // oscOutputs will contain the list of active OSC output ports
    vector<shared_ptr<OscOutput> > oscOutputs;
    ProgramOptions popts;

    int rc = setup_and_parse_program_options(argc, argv, popts);
    if (rc != 0) {
        return rc;
    }

    if (popts.listPorts) {
        listAvailablePorts();
        return 0;
    }

    MonitorLogger::getInstance().setLogLevel(popts.monitor);

    // Open the OSC output ports
    for (auto port : popts.oscOutputPorts) {
        auto oscOutput = make_shared<OscOutput>(popts.oscOutputHost, port);
        oscOutputs.push_back(std::move(oscOutput));
    }

    // Will configure logging on the first OSC port (may want to change this in the future, so that it sends to every port, or be able to select one)
    MonitorLogger::getInstance().setOscOutput(oscOutputs[0]);

// Create the virtual output port?
#ifndef WIN32
    unique_ptr<MidiInProcessor> virtualIn;
    if (popts.useVirtualPort) {
        virtualIn = make_unique<MidiInProcessor>(popts.virtualPortName, oscOutputs, true);
    }
#endif

    // Open the MIDI input ports
    try {
        prepareMidiProcessors(midiInputProcessors, popts, oscOutputs);
    } catch (const std::out_of_range&) {
        return -1;
    }

    // Exit nicely with CTRL-C
#if WIN32
    SetConsoleCtrlHandler((PHANDLER_ROUTINE)ctrlHandler, TRUE);
#else
    struct sigaction intHandler;

    intHandler.sa_handler = ctrlHandler;
    sigemptyset(&intHandler.sa_mask);
    intHandler.sa_flags = 0;
    sigaction(SIGINT, &intHandler, NULL);
#endif


    // For hotplugging
    vector<string> lastAvailablePorts = MidiIn::getInputNames();
    while (!g_wantToExit) {
        std::chrono::milliseconds timespan(1000);
        std::this_thread::sleep_for(timespan);
        vector<string> newAvailablePorts = MidiIn::getInputNames();
        // Was something added or removed?
        if (newAvailablePorts != lastAvailablePorts) {
            midiInputProcessors.clear();
            prepareMidiProcessors(midiInputProcessors, popts, oscOutputs);
            lastAvailablePorts = newAvailablePorts;
            listAvailablePorts();
        }
        if (popts.oscHeartbeat)
            sendHeartBeat(midiInputProcessors, oscOutputs);
    }
}
