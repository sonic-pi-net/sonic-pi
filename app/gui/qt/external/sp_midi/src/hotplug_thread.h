#pragma once
#include <vector>
#include <string>
#include "../JuceLibraryCode/JuceHeader.h"
#include "midiin.h"


// FIXME: this should go into a header file
extern void prepareMidiInputs(std::vector<std::unique_ptr<MidiIn> >& midiInputs);
extern std::vector<std::unique_ptr<MidiIn> > midiInputs;

class HotPlugThread : public juce::Thread
{
public:
    HotPlugThread() : Thread("hotplug thread") { };

    void run() override
    {
        std::vector<std::string> lastAvailablePorts = MidiIn::getInputNames();
        while (!threadShouldExit()){
            wait(500);
            auto newAvailablePorts = MidiIn::getInputNames();
            // Was something added or removed?
            if (newAvailablePorts != lastAvailablePorts) {
                prepareMidiInputs(midiInputs);
                lastAvailablePorts = newAvailablePorts;
            }
        }
    }
};
