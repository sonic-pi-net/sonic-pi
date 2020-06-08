#pragma once
#include <vector>
#include <string>
#include "../JuceLibraryCode/JuceHeader.h"
#include "midiinprocessor.h"


// FIXME: this should go into a header file
extern void prepareMidiProcessors(std::vector<std::unique_ptr<MidiInProcessor> >& midiInputProcessors);
extern std::vector<std::unique_ptr<MidiInProcessor> > midiInputProcessors;

class HotPlugThread : public Thread
{
public:
    HotPlugThread() : Thread("hotplug thread") { };

    void run() override
    {
        std::vector<std::string> lastAvailablePorts = MidiIn::getInputNames();
        while (!threadShouldExit()){
            wait(1000);
            auto newAvailablePorts = MidiIn::getInputNames();
            // Was something added or removed?
            if (newAvailablePorts != lastAvailablePorts) {
                prepareMidiProcessors(midiInputProcessors);
                lastAvailablePorts = newAvailablePorts;
            }
        }
    }
};
