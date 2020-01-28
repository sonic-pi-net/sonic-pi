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
#include <chrono>
#include <thread>
#include <mutex>
#include <iostream>
#include <atomic>
#include "cxxopts.hpp"
#include "midiout.h"
#include "oscin.h"
#include "oscout.h"
#include "oscinprocessor.h"
#include "osc/OscOutboundPacketStream.h"
#include "version.h"
#include "utils.h"
#include "monitorlogger.h"

using namespace std;

void listAvailablePorts()
{
    auto outputs = MidiOut::getOutputNames();
    cout << "Found " << outputs.size() << " MIDI outputs." << endl;
    for (unsigned int i = 0; i < outputs.size(); i++) {
        cout << "   (" << i << "): " << outputs[i] << endl;
    }
}

struct ProgramOptions {
    vector<string> midiOutputNames;
    bool allMidiOutputs;
    unsigned int oscInputPort;
    string oscOutputHost;
    unsigned int oscOutputPort;
    bool oscHeartbeat;
    unsigned int monitor;
    bool listPorts;
    bool oscLocal;
};

void showVersion()
{
    cout << "o2m version " << O2M_VERSION << endl;
}

int setup_and_parse_program_options(int argc, char* argv[], ProgramOptions& programOptions)
{
    // Prepare the help string
    string helpString("Bridges OSC to MIDI\no2m understands the following OSC messages:\n");
    for (const auto& message: OscInProcessor::getKnownOscMessages()){
        helpString += "- ";
        helpString += message;
        helpString += "\n";
    }
    cxxopts::Options options("o2m", helpString);

    options.add_options()
    ("l,list", "List output MIDI devices", cxxopts::value<bool>(programOptions.listPorts))
    ("o,midiout", "MIDI Output devices - can be specified multiple times (default: all)", cxxopts::value<vector<string> >(programOptions.midiOutputNames))
    ("i,oscport", "OSC Input port", cxxopts::value<unsigned int>(programOptions.oscInputPort)->default_value("57200"))
    ("L,local", "OSC listen only on the local network interface", cxxopts::value<bool>(programOptions.oscLocal))
    ("b,heartbeat", "OSC send the heartbeat with info about the active MIDI devices", cxxopts::value<bool>(programOptions.oscHeartbeat))
    ("H,oscoutputhost", "OSC Output host. Used for heartbeat", cxxopts::value<string>(programOptions.oscOutputHost)->default_value("127.0.0.1"))
    ("O,oscoutputport", "OSC Output port. Used for heartbeat", cxxopts::value<unsigned int>(programOptions.oscOutputPort)->default_value("57120"))
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

    programOptions.oscLocal = (options.count("local") ? true : false);
    programOptions.oscHeartbeat = (options.count("heartbeat") ? true : false);
    programOptions.listPorts = (options.count("list") ? true : false);

    if (!options.count("midiout")) {
        // by default add all input devices
        programOptions.midiOutputNames = MidiOut::getOutputNames();
        programOptions.allMidiOutputs = true;
    } else {
        programOptions.allMidiOutputs = false;
    }

    return 0;
}

static mutex g_oscinMutex;

void prepareOscProcessorOutputs(unique_ptr<OscInProcessor>& oscInputProcessor, const ProgramOptions& popts)
{
    // Should we open all devices, or just the ones passed as parameters?
    vector<string> midiOutputsToOpen = (popts.allMidiOutputs ? MidiOut::getOutputNames() : popts.midiOutputNames);
    {
        lock_guard<mutex> lock(g_oscinMutex);
        oscInputProcessor->prepareOutputs(midiOutputsToOpen);
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

void asyncBreakThread(OscInProcessor* oscInputProcessor)
{
    while (!g_wantToExit) {
        std::chrono::milliseconds timespan(1000);
        std::this_thread::sleep_for(timespan);
        {
            lock_guard<mutex> lock(g_oscinMutex);
            oscInputProcessor->asyncBreak();
        }
    }
}

void sendHeartBeat(const OscInProcessor& oscInputProcessor, OscOutput& oscOutput)
{
    char buffer[2048];
    osc::OutboundPacketStream p(buffer, 2048);
    p << osc::BeginMessage("/o2m/heartbeat");
    for (int i = 0; i < oscInputProcessor.getNMidiOuts(); i++) {
      p << oscInputProcessor.getMidiOutId(i) << oscInputProcessor.getMidiOutName(i).c_str() << oscInputProcessor.getNormalizedMidiOutName(i).c_str();
    }
    p << osc::EndMessage;
    MonitorLogger::getInstance().debug("sending OSC: [/o2m/heartbeat] -> ");
    for (int i = 0; i < oscInputProcessor.getNMidiOuts(); i++) {
        MonitorLogger::getInstance().debug("   {}, {}", oscInputProcessor.getMidiOutId(i), oscInputProcessor.getMidiOutName(i));
    }

    oscOutput.sendUDP(p.Data(), p.Size());
    local_utils::logOSCMessage(p.Data(), p.Size());
}

int main(int argc, char* argv[])
{

    ProgramOptions popts;
    shared_ptr<OscOutput> oscOutput;

    int rc = setup_and_parse_program_options(argc, argv, popts);
    if (rc != 0) {
        return rc;
    }

    if (popts.listPorts) {
        listAvailablePorts();
        return 0;
    }

    MonitorLogger::getInstance().setLogLevel(popts.monitor);

    // Open the OSC output port, for heartbeats and logging
    oscOutput = make_shared<OscOutput>(popts.oscOutputHost, popts.oscOutputPort);
    MonitorLogger::getInstance().setOscOutput(oscOutput);

    auto oscInputProcessor = make_unique<OscInProcessor>(popts.oscLocal, popts.oscInputPort);
    // Prepare the OSC input and MIDI outputs
    try {
        prepareOscProcessorOutputs(oscInputProcessor, popts);
    } catch (const std::out_of_range&) {
        cout << "Error opening MIDI outputs" << endl;
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

    std::thread thr(asyncBreakThread, oscInputProcessor.get());

    // For hotplugging
    vector<string> lastAvailablePorts = MidiOut::getOutputNames();
    while (!g_wantToExit) {
        oscInputProcessor->run(); // will run until asyncBreak is called from another thread
        vector<string> newAvailablePorts = MidiOut::getOutputNames();
        // Was something added or removed?
        if (newAvailablePorts != lastAvailablePorts) {
            prepareOscProcessorOutputs(oscInputProcessor, popts);
            lastAvailablePorts = newAvailablePorts;
            listAvailablePorts();
        }
        if (popts.oscHeartbeat)
            sendHeartBeat(*oscInputProcessor, *oscOutput);
    }
    thr.join();
}
