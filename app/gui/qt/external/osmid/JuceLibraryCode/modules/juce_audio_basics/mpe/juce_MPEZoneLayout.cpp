/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   JUCE is an open source library subject to commercial or open-source
   licensing.

   The code included in this file is provided under the terms of the ISC license
   http://www.isc.org/downloads/software-support-policy/isc-license. Permission
   To use, copy, modify, and/or distribute this software for any purpose with or
   without fee is hereby granted provided that the above copyright notice and
   this permission notice appear in all copies.

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

namespace juce
{

MPEZoneLayout::MPEZoneLayout() noexcept
{
}

MPEZoneLayout::MPEZoneLayout (const MPEZoneLayout& other)
    : zones (other.zones)
{
}

MPEZoneLayout& MPEZoneLayout::operator= (const MPEZoneLayout& other)
{
    zones = other.zones;
    listeners.call (&MPEZoneLayout::Listener::zoneLayoutChanged, *this);
    return *this;
}

//==============================================================================
bool MPEZoneLayout::addZone (MPEZone newZone)
{
    bool noOtherZonesModified = true;

    for (int i = zones.size(); --i >= 0;)
    {
        MPEZone& zone = zones.getReference (i);

        if (zone.overlapsWith (newZone))
        {
            if (! zone.truncateToFit (newZone))
                zones.removeRange (i, 1);
                // can't use zones.remove (i) because that requires a default c'tor :-(

            noOtherZonesModified = false;
        }
    }

    zones.add (newZone);
    listeners.call (&MPEZoneLayout::Listener::zoneLayoutChanged, *this);
    return noOtherZonesModified;
}

//==============================================================================
int MPEZoneLayout::getNumZones() const noexcept
{
    return zones.size();
}

MPEZone* MPEZoneLayout::getZoneByIndex (int index) const noexcept
{
    if (zones.size() < index)
        return nullptr;

    return &(zones.getReference (index));
}

void MPEZoneLayout::clearAllZones()
{
    zones.clear();
    listeners.call (&MPEZoneLayout::Listener::zoneLayoutChanged, *this);
}

//==============================================================================
void MPEZoneLayout::processNextMidiEvent (const MidiMessage& message)
{
    if (! message.isController())
        return;

    MidiRPNMessage rpn;

    if (rpnDetector.parseControllerMessage (message.getChannel(),
                                            message.getControllerNumber(),
                                            message.getControllerValue(),
                                            rpn))
    {
        processRpnMessage (rpn);
    }
}

void MPEZoneLayout::processRpnMessage (MidiRPNMessage rpn)
{
    if (rpn.parameterNumber == MPEMessages::zoneLayoutMessagesRpnNumber)
        processZoneLayoutRpnMessage (rpn);
    else if (rpn.parameterNumber == 0)
        processPitchbendRangeRpnMessage (rpn);
}

void MPEZoneLayout::processZoneLayoutRpnMessage (MidiRPNMessage rpn)
{
    if (rpn.value < 16)
        addZone (MPEZone (rpn.channel - 1, rpn.value));
    else
        clearAllZones();
}

//==============================================================================
void MPEZoneLayout::processPitchbendRangeRpnMessage (MidiRPNMessage rpn)
{
    if (MPEZone* zone = getZoneByFirstNoteChannel (rpn.channel))
    {
        if (zone->getPerNotePitchbendRange() != rpn.value)
        {
            zone->setPerNotePitchbendRange (rpn.value);
            listeners.call (&MPEZoneLayout::Listener::zoneLayoutChanged, *this);
            return;
        }
    }

    if (MPEZone* zone = getZoneByMasterChannel (rpn.channel))
    {
        if (zone->getMasterPitchbendRange() != rpn.value)
        {
            zone->setMasterPitchbendRange (rpn.value);
            listeners.call (&MPEZoneLayout::Listener::zoneLayoutChanged, *this);
            return;
        }
    }
}

//==============================================================================
void MPEZoneLayout::processNextMidiBuffer (const MidiBuffer& buffer)
{
    MidiBuffer::Iterator iter (buffer);
    MidiMessage message;
    int samplePosition; // not actually used, so no need to initialise.

    while (iter.getNextEvent (message, samplePosition))
        processNextMidiEvent (message);
}

//==============================================================================
MPEZone* MPEZoneLayout::getZoneByChannel (int channel) const noexcept
{
    for (MPEZone* zone = zones.begin(); zone != zones.end(); ++zone)
        if (zone->isUsingChannel (channel))
            return zone;

    return nullptr;
}

MPEZone* MPEZoneLayout::getZoneByMasterChannel (int channel) const noexcept
{
    for (MPEZone* zone = zones.begin(); zone != zones.end(); ++zone)
        if (zone->getMasterChannel() == channel)
            return zone;

    return nullptr;
}

MPEZone* MPEZoneLayout::getZoneByFirstNoteChannel (int channel) const noexcept
{
    for (MPEZone* zone = zones.begin(); zone != zones.end(); ++zone)
        if (zone->getFirstNoteChannel() == channel)
            return zone;

    return nullptr;
}

MPEZone* MPEZoneLayout::getZoneByNoteChannel (int channel) const noexcept
{
    for (MPEZone* zone = zones.begin(); zone != zones.end(); ++zone)
        if (zone->isUsingChannelAsNoteChannel (channel))
            return zone;

    return nullptr;
}

//==============================================================================
void MPEZoneLayout::addListener (Listener* const listenerToAdd) noexcept
{
    listeners.add (listenerToAdd);
}

void MPEZoneLayout::removeListener (Listener* const listenerToRemove) noexcept
{
    listeners.remove (listenerToRemove);
}

//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS


class MPEZoneLayoutTests  : public UnitTest
{
public:
    MPEZoneLayoutTests() : UnitTest ("MPEZoneLayout class", "MIDI/MPE") {}

    void runTest() override
    {
        beginTest ("initialisation");
        {
            MPEZoneLayout layout;
            expectEquals (layout.getNumZones(), 0);
        }

        beginTest ("adding zones");
        {
            MPEZoneLayout layout;

            expect (layout.addZone (MPEZone (1, 7)));

            expectEquals (layout.getNumZones(), 1);
            expectEquals (layout.getZoneByIndex (0)->getMasterChannel(), 1);
            expectEquals (layout.getZoneByIndex (0)->getNumNoteChannels(), 7);

            expect (layout.addZone (MPEZone (9, 7)));

            expectEquals (layout.getNumZones(), 2);
            expectEquals (layout.getZoneByIndex (0)->getMasterChannel(), 1);
            expectEquals (layout.getZoneByIndex (0)->getNumNoteChannels(), 7);
            expectEquals (layout.getZoneByIndex (1)->getMasterChannel(), 9);
            expectEquals (layout.getZoneByIndex (1)->getNumNoteChannels(), 7);

            expect (! layout.addZone (MPEZone (5, 3)));

            expectEquals (layout.getNumZones(), 3);
            expectEquals (layout.getZoneByIndex (0)->getMasterChannel(), 1);
            expectEquals (layout.getZoneByIndex (0)->getNumNoteChannels(), 3);
            expectEquals (layout.getZoneByIndex (1)->getMasterChannel(), 9);
            expectEquals (layout.getZoneByIndex (1)->getNumNoteChannels(), 7);
            expectEquals (layout.getZoneByIndex (2)->getMasterChannel(), 5);
            expectEquals (layout.getZoneByIndex (2)->getNumNoteChannels(), 3);

            expect (! layout.addZone (MPEZone (5, 4)));

            expectEquals (layout.getNumZones(), 2);
            expectEquals (layout.getZoneByIndex (0)->getMasterChannel(), 1);
            expectEquals (layout.getZoneByIndex (0)->getNumNoteChannels(), 3);
            expectEquals (layout.getZoneByIndex (1)->getMasterChannel(), 5);
            expectEquals (layout.getZoneByIndex (1)->getNumNoteChannels(), 4);

            expect (! layout.addZone (MPEZone (6, 4)));

            expectEquals (layout.getNumZones(), 2);
            expectEquals (layout.getZoneByIndex (0)->getMasterChannel(), 1);
            expectEquals (layout.getZoneByIndex (0)->getNumNoteChannels(), 3);
            expectEquals (layout.getZoneByIndex (1)->getMasterChannel(), 6);
            expectEquals (layout.getZoneByIndex (1)->getNumNoteChannels(), 4);
        }

        beginTest ("querying zones");
        {
            MPEZoneLayout layout;

            layout.addZone (MPEZone (2, 5));
            layout.addZone (MPEZone (9, 4));

            expect (layout.getZoneByMasterChannel (1)  == nullptr);
            expect (layout.getZoneByMasterChannel (2)  != nullptr);
            expect (layout.getZoneByMasterChannel (3)  == nullptr);
            expect (layout.getZoneByMasterChannel (8)  == nullptr);
            expect (layout.getZoneByMasterChannel (9)  != nullptr);
            expect (layout.getZoneByMasterChannel (10) == nullptr);

            expectEquals (layout.getZoneByMasterChannel (2)->getNumNoteChannels(), 5);
            expectEquals (layout.getZoneByMasterChannel (9)->getNumNoteChannels(), 4);

            expect (layout.getZoneByFirstNoteChannel (2)  == nullptr);
            expect (layout.getZoneByFirstNoteChannel (3)  != nullptr);
            expect (layout.getZoneByFirstNoteChannel (4)  == nullptr);
            expect (layout.getZoneByFirstNoteChannel (9)  == nullptr);
            expect (layout.getZoneByFirstNoteChannel (10) != nullptr);
            expect (layout.getZoneByFirstNoteChannel (11) == nullptr);

            expectEquals (layout.getZoneByFirstNoteChannel (3)->getNumNoteChannels(), 5);
            expectEquals (layout.getZoneByFirstNoteChannel (10)->getNumNoteChannels(), 4);

            expect (layout.getZoneByNoteChannel (2)  == nullptr);
            expect (layout.getZoneByNoteChannel (3)  != nullptr);
            expect (layout.getZoneByNoteChannel (4)  != nullptr);
            expect (layout.getZoneByNoteChannel (6)  != nullptr);
            expect (layout.getZoneByNoteChannel (7)  != nullptr);
            expect (layout.getZoneByNoteChannel (8)  == nullptr);
            expect (layout.getZoneByNoteChannel (9)  == nullptr);
            expect (layout.getZoneByNoteChannel (10) != nullptr);
            expect (layout.getZoneByNoteChannel (11) != nullptr);
            expect (layout.getZoneByNoteChannel (12) != nullptr);
            expect (layout.getZoneByNoteChannel (13) != nullptr);
            expect (layout.getZoneByNoteChannel (14) == nullptr);

            expectEquals (layout.getZoneByNoteChannel (5)->getNumNoteChannels(), 5);
            expectEquals (layout.getZoneByNoteChannel (13)->getNumNoteChannels(), 4);
        }

        beginTest ("clear all zones");
        {
            MPEZoneLayout layout;

            expect (layout.addZone (MPEZone (1, 7)));
            expect (layout.addZone (MPEZone (10, 2)));
            layout.clearAllZones();

            expectEquals (layout.getNumZones(), 0);
        }

        beginTest ("process MIDI buffers");
        {
            MPEZoneLayout layout;
            MidiBuffer buffer;

            buffer = MPEMessages::addZone (MPEZone (1, 7));
            layout.processNextMidiBuffer (buffer);

            expectEquals (layout.getNumZones(), 1);
            expectEquals (layout.getZoneByIndex (0)->getMasterChannel(), 1);
            expectEquals (layout.getZoneByIndex (0)->getNumNoteChannels(), 7);

            buffer = MPEMessages::addZone (MPEZone (9, 7));
            layout.processNextMidiBuffer (buffer);

            expectEquals (layout.getNumZones(), 2);
            expectEquals (layout.getZoneByIndex (0)->getMasterChannel(), 1);
            expectEquals (layout.getZoneByIndex (0)->getNumNoteChannels(), 7);
            expectEquals (layout.getZoneByIndex (1)->getMasterChannel(), 9);
            expectEquals (layout.getZoneByIndex (1)->getNumNoteChannels(), 7);

            MPEZone zone (1, 10);

            buffer = MPEMessages::addZone (zone);
            layout.processNextMidiBuffer (buffer);

            expectEquals (layout.getNumZones(), 1);
            expectEquals (layout.getZoneByIndex (0)->getMasterChannel(), 1);
            expectEquals (layout.getZoneByIndex (0)->getNumNoteChannels(), 10);

            zone.setPerNotePitchbendRange (33);
            zone.setMasterPitchbendRange (44);

            buffer = MPEMessages::masterPitchbendRange (zone);
            buffer.addEvents (MPEMessages::perNotePitchbendRange (zone), 0, -1, 0);

            layout.processNextMidiBuffer (buffer);

            expectEquals (layout.getZoneByIndex (0)->getPerNotePitchbendRange(), 33);
            expectEquals (layout.getZoneByIndex (0)->getMasterPitchbendRange(), 44);
        }

        beginTest ("process individual MIDI messages");
        {
            MPEZoneLayout layout;

            layout.processNextMidiEvent (MidiMessage (0x80, 0x59, 0xd0));  // unrelated note-off msg
            layout.processNextMidiEvent (MidiMessage (0xb1, 0x64, 0x06));  // RPN part 1
            layout.processNextMidiEvent (MidiMessage (0xb1, 0x65, 0x00));  // RPN part 2
            layout.processNextMidiEvent (MidiMessage (0xb8, 0x0b, 0x66));  // unrelated CC msg
            layout.processNextMidiEvent (MidiMessage (0xb1, 0x06, 0x03));  // RPN part 3
            layout.processNextMidiEvent (MidiMessage (0x90, 0x60, 0x00));  // unrelated note-on msg

            expectEquals (layout.getNumZones(), 1);
            expectEquals (layout.getZoneByIndex (0)->getMasterChannel(), 1);
            expectEquals (layout.getZoneByIndex (0)->getNumNoteChannels(), 3);
        }
    }
};

static MPEZoneLayoutTests MPEZoneLayoutUnitTests;


#endif // JUCE_UNIT_TESTS

} // namespace juce
