#pragma once
#include <vector>
#include <memory>
#include <string>
#include "../JuceLibraryCode/JuceHeader.h"
#include "midiin.h"
#include "midisendprocessor.h"


// FIXME: this should go into a header file
void prepareMidiInputs(std::vector<std::unique_ptr<MidiIn> >& midiInputs);
extern std::vector<std::unique_ptr<MidiIn> > midiInputs;
void prepareMidiSendProcessorOutputs(std::unique_ptr<MidiSendProcessor>& midiSendProcessor);
extern std::unique_ptr<MidiSendProcessor> midiSendProcessor;

class HotPlugThread : public juce::Thread
{
public:
    HotPlugThread() : Thread("hotplug thread") { };

    void run() override
    {
        std::vector<std::string> lastAvailableInputPorts = MidiIn::getInputNames();
        std::vector<std::string> lastAvailableOutputPorts = MidiOut::getOutputNames();
        while (!threadShouldExit()){
            wait(500);
            auto newAvailableInputPorts = MidiIn::getInputNames();
            // Was something added or removed?
            if (newAvailableInputPorts != lastAvailableInputPorts) {
                prepareMidiInputs(midiInputs);
                lastAvailableInputPorts = newAvailableInputPorts;
            }

            auto newAvailableOutputPorts = MidiOut::getOutputNames();
            // Was something added or removed?
            if (newAvailableOutputPorts != lastAvailableOutputPorts) {
                prepareMidiSendProcessorOutputs(midiSendProcessor);
                lastAvailableOutputPorts = newAvailableOutputPorts;
            }

        }
    }
};
