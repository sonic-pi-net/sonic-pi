/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2020 - Raw Material Software Limited

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

MPEChannelAssigner::MPEChannelAssigner (MPEZoneLayout::Zone zoneToUse)
    : zone                    (new MPEZoneLayout::Zone (zoneToUse)),
      channelIncrement        (zone->isLowerZone() ? 1 : -1),
      numChannels             (zone->numMemberChannels),
      firstChannel            (zone->getFirstMemberChannel()),
      lastChannel             (zone->getLastMemberChannel()),
      midiChannelLastAssigned (firstChannel - channelIncrement)
{
    // must be an active MPE zone!
    jassert (numChannels > 0);
}

MPEChannelAssigner::MPEChannelAssigner (Range<int> channelRange)
    : isLegacy                (true),
      channelIncrement        (1),
      numChannels             (channelRange.getLength()),
      firstChannel            (channelRange.getStart()),
      lastChannel             (channelRange.getEnd() - 1),
      midiChannelLastAssigned (firstChannel - channelIncrement)
{
    // must have at least one channel!
    jassert (! channelRange.isEmpty());
}

int MPEChannelAssigner::findMidiChannelForNewNote (int noteNumber) noexcept
{
    if (numChannels <= 1)
        return firstChannel;

    for (auto ch = firstChannel; (isLegacy || zone->isLowerZone() ? ch <= lastChannel : ch >= lastChannel); ch += channelIncrement)
    {
        if (midiChannels[ch].isFree() && midiChannels[ch].lastNotePlayed == noteNumber)
        {
            midiChannelLastAssigned = ch;
            midiChannels[ch].notes.add (noteNumber);
            return ch;
        }
    }

    for (auto ch = midiChannelLastAssigned + channelIncrement; ; ch += channelIncrement)
    {
        if (ch == lastChannel + channelIncrement)  // loop wrap-around
            ch = firstChannel;

        if (midiChannels[ch].isFree())
        {
            midiChannelLastAssigned = ch;
            midiChannels[ch].notes.add (noteNumber);
            return ch;
        }

        if (ch == midiChannelLastAssigned)
            break; // no free channels!
    }

    midiChannelLastAssigned = findMidiChannelPlayingClosestNonequalNote (noteNumber);
    midiChannels[midiChannelLastAssigned].notes.add (noteNumber);

    return midiChannelLastAssigned;
}

void MPEChannelAssigner::noteOff (int noteNumber, int midiChannel)
{
    const auto removeNote = [] (MidiChannel& ch, int noteNum)
    {
        if (ch.notes.removeAllInstancesOf (noteNum) > 0)
        {
            ch.lastNotePlayed = noteNum;
            return true;
        }

        return false;
    };

    if (midiChannel >= 0 && midiChannel < 17)
    {
        removeNote (midiChannels[midiChannel], noteNumber);
        return;
    }

    for (auto& ch : midiChannels)
    {
        if (removeNote (ch, noteNumber))
            return;
    }
}

void MPEChannelAssigner::allNotesOff()
{
    for (auto& ch : midiChannels)
    {
        if (ch.notes.size() > 0)
            ch.lastNotePlayed = ch.notes.getLast();

        ch.notes.clear();
    }
}

int MPEChannelAssigner::findMidiChannelPlayingClosestNonequalNote (int noteNumber) noexcept
{
    auto channelWithClosestNote = firstChannel;
    int closestNoteDistance = 127;

    for (auto ch = firstChannel; (isLegacy || zone->isLowerZone() ? ch <= lastChannel : ch >= lastChannel); ch += channelIncrement)
    {
        for (auto note : midiChannels[ch].notes)
        {
            auto noteDistance = std::abs (note - noteNumber);

            if (noteDistance > 0 && noteDistance < closestNoteDistance)
            {
                closestNoteDistance = noteDistance;
                channelWithClosestNote = ch;
            }
        }
    }

    return channelWithClosestNote;
}

//==============================================================================
MPEChannelRemapper::MPEChannelRemapper (MPEZoneLayout::Zone zoneToRemap)
    : zone             (zoneToRemap),
      channelIncrement (zone.isLowerZone() ? 1 : -1),
      firstChannel     (zone.getFirstMemberChannel()),
      lastChannel      (zone.getLastMemberChannel())
{
    // must be an active MPE zone!
    jassert (zone.numMemberChannels > 0);
    zeroArrays();
}

void MPEChannelRemapper::remapMidiChannelIfNeeded (MidiMessage& message, uint32 mpeSourceID) noexcept
{
    auto channel = message.getChannel();

    if (! zone.isUsingChannelAsMemberChannel (channel))
        return;

    if (channel == zone.getMasterChannel() && (message.isResetAllControllers() || message.isAllNotesOff()))
    {
        clearSource (mpeSourceID);
        return;
    }

    auto sourceAndChannelID = (((uint32) mpeSourceID << 5) | (uint32) (channel));

    if (messageIsNoteData (message))
    {
        ++counter;

        // fast path - no remap
        if (applyRemapIfExisting (channel, sourceAndChannelID, message))
            return;

        // find existing remap
        for (int chan = firstChannel; (zone.isLowerZone() ? chan <= lastChannel : chan >= lastChannel); chan += channelIncrement)
            if (applyRemapIfExisting (chan, sourceAndChannelID, message))
                return;

        // no remap necessary
        if (sourceAndChannel[channel] == notMPE)
        {
            lastUsed[channel] = counter;
            sourceAndChannel[channel] = sourceAndChannelID;
            return;
        }

        // remap source & channel to new channel
        auto chan = getBestChanToReuse();

        sourceAndChannel[chan] = sourceAndChannelID;
        lastUsed[chan] = counter;
        message.setChannel (chan);
    }
}

void MPEChannelRemapper::reset() noexcept
{
    for (auto& s : sourceAndChannel)
        s = notMPE;
}

void MPEChannelRemapper::clearChannel (int channel) noexcept
{
    sourceAndChannel[channel] = notMPE;
}

void MPEChannelRemapper::clearSource (uint32 mpeSourceID)
{
    for (auto& s : sourceAndChannel)
    {
        if (uint32 (s >> 5) == mpeSourceID)
        {
            s = notMPE;
            return;
        }
    }
}

bool MPEChannelRemapper::applyRemapIfExisting (int channel, uint32 sourceAndChannelID, MidiMessage& m) noexcept
{
    if (sourceAndChannel[channel] == sourceAndChannelID)
    {
        if (m.isNoteOff())
            sourceAndChannel[channel] = notMPE;
        else
            lastUsed[channel] = counter;

        m.setChannel (channel);
        return true;
    }

    return false;
}

int MPEChannelRemapper::getBestChanToReuse() const noexcept
{
    for (int chan = firstChannel; (zone.isLowerZone() ? chan <= lastChannel : chan >= lastChannel); chan += channelIncrement)
        if (sourceAndChannel[chan] == notMPE)
            return chan;

    auto bestChan = firstChannel;
    auto bestLastUse = counter;

    for (int chan = firstChannel; (zone.isLowerZone() ? chan <= lastChannel : chan >= lastChannel); chan += channelIncrement)
    {
        if (lastUsed[chan] < bestLastUse)
        {
            bestLastUse = lastUsed[chan];
            bestChan = chan;
        }
    }

    return bestChan;
}

void MPEChannelRemapper::zeroArrays()
{
    for (int i = 0; i < 17; ++i)
    {
        sourceAndChannel[i] = 0;
        lastUsed[i] = 0;
    }
}


//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

struct MPEUtilsUnitTests  : public UnitTest
{
    MPEUtilsUnitTests()
        : UnitTest ("MPE Utilities", UnitTestCategories::midi)
    {}

    void runTest() override
    {
        beginTest ("MPEChannelAssigner");
        {
            MPEZoneLayout layout;

            // lower
            {
                layout.setLowerZone (15);

                // lower zone
                MPEChannelAssigner channelAssigner (layout.getLowerZone());

                // check that channels are assigned in correct order
                int noteNum = 60;
                for (int ch = 2; ch <= 16; ++ch)
                    expectEquals (channelAssigner.findMidiChannelForNewNote (noteNum++), ch);

                // check that note-offs are processed
                channelAssigner.noteOff (60);
                expectEquals (channelAssigner.findMidiChannelForNewNote (60), 2);

                channelAssigner.noteOff (61);
                expectEquals (channelAssigner.findMidiChannelForNewNote (61), 3);

                // check that assigned channel was last to play note
                channelAssigner.noteOff (65);
                channelAssigner.noteOff (66);
                expectEquals (channelAssigner.findMidiChannelForNewNote (66), 8);
                expectEquals (channelAssigner.findMidiChannelForNewNote (65), 7);

                // find closest channel playing nonequal note
                expectEquals (channelAssigner.findMidiChannelForNewNote (80), 16);
                expectEquals (channelAssigner.findMidiChannelForNewNote (55), 2);

                // all notes off
                channelAssigner.allNotesOff();

                // last note played
                expectEquals (channelAssigner.findMidiChannelForNewNote (66), 8);
                expectEquals (channelAssigner.findMidiChannelForNewNote (65), 7);
                expectEquals (channelAssigner.findMidiChannelForNewNote (80), 16);
                expectEquals (channelAssigner.findMidiChannelForNewNote (55), 2);

                // normal assignment
                expectEquals (channelAssigner.findMidiChannelForNewNote (101), 3);
                expectEquals (channelAssigner.findMidiChannelForNewNote (20), 4);
            }

            // upper
            {
                layout.setUpperZone (15);

                // upper zone
                MPEChannelAssigner channelAssigner (layout.getUpperZone());

                // check that channels are assigned in correct order
                int noteNum = 60;
                for (int ch = 15; ch >= 1; --ch)
                    expectEquals (channelAssigner.findMidiChannelForNewNote (noteNum++), ch);

                // check that note-offs are processed
                channelAssigner.noteOff (60);
                expectEquals (channelAssigner.findMidiChannelForNewNote (60), 15);

                channelAssigner.noteOff (61);
                expectEquals (channelAssigner.findMidiChannelForNewNote (61), 14);

                // check that assigned channel was last to play note
                channelAssigner.noteOff (65);
                channelAssigner.noteOff (66);
                expectEquals (channelAssigner.findMidiChannelForNewNote (66), 9);
                expectEquals (channelAssigner.findMidiChannelForNewNote (65), 10);

                // find closest channel playing nonequal note
                expectEquals (channelAssigner.findMidiChannelForNewNote (80), 1);
                expectEquals (channelAssigner.findMidiChannelForNewNote (55), 15);

                // all notes off
                channelAssigner.allNotesOff();

                // last note played
                expectEquals (channelAssigner.findMidiChannelForNewNote (66), 9);
                expectEquals (channelAssigner.findMidiChannelForNewNote (65), 10);
                expectEquals (channelAssigner.findMidiChannelForNewNote (80), 1);
                expectEquals (channelAssigner.findMidiChannelForNewNote (55), 15);

                // normal assignment
                expectEquals (channelAssigner.findMidiChannelForNewNote (101), 14);
                expectEquals (channelAssigner.findMidiChannelForNewNote (20), 13);
            }

            // legacy
            {
                MPEChannelAssigner channelAssigner;

                // check that channels are assigned in correct order
                int noteNum = 60;
                for (int ch = 1; ch <= 16; ++ch)
                    expectEquals (channelAssigner.findMidiChannelForNewNote (noteNum++), ch);

                // check that note-offs are processed
                channelAssigner.noteOff (60);
                expectEquals (channelAssigner.findMidiChannelForNewNote (60), 1);

                channelAssigner.noteOff (61);
                expectEquals (channelAssigner.findMidiChannelForNewNote (61), 2);

                // check that assigned channel was last to play note
                channelAssigner.noteOff (65);
                channelAssigner.noteOff (66);
                expectEquals (channelAssigner.findMidiChannelForNewNote (66), 7);
                expectEquals (channelAssigner.findMidiChannelForNewNote (65), 6);

                // find closest channel playing nonequal note
                expectEquals (channelAssigner.findMidiChannelForNewNote (80), 16);
                expectEquals (channelAssigner.findMidiChannelForNewNote (55), 1);

                // all notes off
                channelAssigner.allNotesOff();

                // last note played
                expectEquals (channelAssigner.findMidiChannelForNewNote (66), 7);
                expectEquals (channelAssigner.findMidiChannelForNewNote (65), 6);
                expectEquals (channelAssigner.findMidiChannelForNewNote (80), 16);
                expectEquals (channelAssigner.findMidiChannelForNewNote (55), 1);

                // normal assignment
                expectEquals (channelAssigner.findMidiChannelForNewNote (101), 2);
                expectEquals (channelAssigner.findMidiChannelForNewNote (20), 3);
            }
        }

        beginTest ("MPEChannelRemapper");
        {
            // 3 different MPE 'sources', constant IDs
            const int sourceID1 = 0;
            const int sourceID2 = 1;
            const int sourceID3 = 2;

            MPEZoneLayout layout;

            {
                layout.setLowerZone (15);

                // lower zone
                MPEChannelRemapper channelRemapper (layout.getLowerZone());

                // first source, shouldn't remap
                for (int ch = 2; ch <= 16; ++ch)
                {
                    auto noteOn = MidiMessage::noteOn (ch, 60, 1.0f);

                    channelRemapper.remapMidiChannelIfNeeded (noteOn, sourceID1);
                    expectEquals (noteOn.getChannel(), ch);
                }

                auto noteOn = MidiMessage::noteOn (2, 60, 1.0f);

                // remap onto oldest last-used channel
                channelRemapper.remapMidiChannelIfNeeded (noteOn, sourceID2);
                expectEquals (noteOn.getChannel(), 2);

                // remap onto oldest last-used channel
                channelRemapper.remapMidiChannelIfNeeded (noteOn, sourceID3);
                expectEquals (noteOn.getChannel(), 3);

                // remap to correct channel for source ID
                auto noteOff = MidiMessage::noteOff (2, 60, 1.0f);
                channelRemapper.remapMidiChannelIfNeeded (noteOff, sourceID3);
                expectEquals (noteOff.getChannel(), 3);
            }

            {
                layout.setUpperZone (15);

                // upper zone
                MPEChannelRemapper channelRemapper (layout.getUpperZone());

                // first source, shouldn't remap
                for (int ch = 15; ch >= 1; --ch)
                {
                    auto noteOn = MidiMessage::noteOn (ch, 60, 1.0f);

                    channelRemapper.remapMidiChannelIfNeeded (noteOn, sourceID1);
                    expectEquals (noteOn.getChannel(), ch);
                }

                auto noteOn = MidiMessage::noteOn (15, 60, 1.0f);

                // remap onto oldest last-used channel
                channelRemapper.remapMidiChannelIfNeeded (noteOn, sourceID2);
                expectEquals (noteOn.getChannel(), 15);

                // remap onto oldest last-used channel
                channelRemapper.remapMidiChannelIfNeeded (noteOn, sourceID3);
                expectEquals (noteOn.getChannel(), 14);

                // remap to correct channel for source ID
                auto noteOff = MidiMessage::noteOff (15, 60, 1.0f);
                channelRemapper.remapMidiChannelIfNeeded (noteOff, sourceID3);
                expectEquals (noteOff.getChannel(), 14);
            }
        }
    }
};

static MPEUtilsUnitTests MPEUtilsUnitTests;

#endif

} // namespace juce
