#pragma once
#include <vector>
#include <memory>
#include <string>
#include "midiin.h"
#include "midisendprocessor.h"
#include "midi_port_info.h"

extern std::atomic<bool> g_threadsShouldFinish;

// FIXME: this should go into a header file
void prepareMidiInputs(std::vector<std::unique_ptr<MidiIn> >& midiInputs);
extern std::vector<std::unique_ptr<MidiIn> > midiInputs;
void prepareMidiSendProcessorOutputs(std::unique_ptr<MidiSendProcessor>& midiSendProcessor);
extern std::unique_ptr<MidiSendProcessor> midiSendProcessor;

class HotPlugThread
{
public:
    ~HotPlugThread()
    {
        if (m_thread.joinable()){
            m_thread.join();
        }
    }

    void startThread(){
        m_thread = std::thread(&HotPlugThread::run, this);
    }

    void run()
    {
        std::vector<MidiPortInfo> lastAvailableInputPorts = MidiIn::getInputPortInfo();
        std::vector<MidiPortInfo> lastAvailableOutputPorts = MidiOut::getOutputPortInfo();

        while (!g_threadsShouldFinish){

            std::this_thread::sleep_for(std::chrono::milliseconds(500));

            auto newAvailableInputPorts = MidiIn::getInputPortInfo();
            // Was something added or removed?
            if(!((newAvailableInputPorts.size() == lastAvailableInputPorts.size()) &&
                    (std::equal(newAvailableInputPorts.begin(), newAvailableInputPorts.end(), lastAvailableInputPorts.begin())))) {
                try {
                    prepareMidiInputs(midiInputs);
                } catch (const std::out_of_range&) {
                    std::cout << "Error opening MIDI inputs" << std::endl;
                }
                lastAvailableInputPorts = newAvailableInputPorts;
            }

            auto newAvailableOutputPorts = MidiOut::getOutputPortInfo();
            // Was something added or removed?
            if(!((newAvailableOutputPorts.size() == lastAvailableOutputPorts.size()) &&
                    (std::equal(newAvailableOutputPorts.begin(), newAvailableOutputPorts.end(), lastAvailableOutputPorts.begin())))) {
                try {
                    prepareMidiSendProcessorOutputs(midiSendProcessor);
                } catch (const std::out_of_range&) {
                    std::cout << "Error opening MIDI outputs" << std::endl;
                }

                lastAvailableOutputPorts = newAvailableOutputPorts;
            }

        }
    }

private:
    std::thread m_thread;
};
