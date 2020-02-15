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

namespace
{
    void checkAndLimitZoneParameters (int minValue,
                                      int maxValue,
                                      int& valueToCheckAndLimit) noexcept
    {
        if (valueToCheckAndLimit < minValue || valueToCheckAndLimit > maxValue)
        {
            // if you hit this, one of the parameters you supplied for MPEZone
            // was not within the allowed range!
            // we fit this back into the allowed range here to maintain a valid
            // state for the zone, but probably the resulting zone is not what you
            //wanted it to be!
            jassertfalse;

            valueToCheckAndLimit = jlimit (minValue, maxValue, valueToCheckAndLimit);
        }
    }
}

//==============================================================================
MPEZone::MPEZone (int masterChannel_,
                  int numNoteChannels_,
                  int perNotePitchbendRange_,
                  int masterPitchbendRange_) noexcept
    : masterChannel (masterChannel_),
      numNoteChannels (numNoteChannels_),
      perNotePitchbendRange (perNotePitchbendRange_),
      masterPitchbendRange (masterPitchbendRange_)
{
    checkAndLimitZoneParameters (1, 15,                  masterChannel);
    checkAndLimitZoneParameters (1, 16 - masterChannel,  numNoteChannels);
    checkAndLimitZoneParameters (0, 96,                  perNotePitchbendRange);
    checkAndLimitZoneParameters (0, 96,                  masterPitchbendRange);
}

//==============================================================================
int MPEZone::getMasterChannel() const noexcept
{
    return masterChannel;
}

int MPEZone::getNumNoteChannels() const noexcept
{
    return numNoteChannels;
}

int MPEZone::getFirstNoteChannel() const noexcept
{
    return masterChannel + 1;
}

int MPEZone::getLastNoteChannel() const noexcept
{
    return masterChannel + numNoteChannels;
}

Range<int> MPEZone::getNoteChannelRange() const noexcept
{
    return Range<int>::withStartAndLength (getFirstNoteChannel(), getNumNoteChannels());
}

bool MPEZone::isUsingChannel (int channel) const noexcept
{
    jassert (channel > 0 && channel <= 16);
    return channel >= masterChannel && channel <= masterChannel + numNoteChannels;
}

bool MPEZone::isUsingChannelAsNoteChannel (int channel) const noexcept
{
    jassert (channel > 0 && channel <= 16);
    return channel > masterChannel && channel <= masterChannel + numNoteChannels;
}

int MPEZone::getPerNotePitchbendRange() const noexcept
{
    return perNotePitchbendRange;
}

int MPEZone::getMasterPitchbendRange() const noexcept
{
    return masterPitchbendRange;
}

void MPEZone::setPerNotePitchbendRange (int rangeInSemitones) noexcept
{
    checkAndLimitZoneParameters (0, 96, rangeInSemitones);
    perNotePitchbendRange = rangeInSemitones;
}

void MPEZone::setMasterPitchbendRange (int rangeInSemitones) noexcept
{
    checkAndLimitZoneParameters (0, 96, rangeInSemitones);
    masterPitchbendRange = rangeInSemitones;
}

//==============================================================================
bool MPEZone::overlapsWith (MPEZone other) const noexcept
{
    if (masterChannel == other.masterChannel)
        return true;

    if (masterChannel > other.masterChannel)
        return other.overlapsWith (*this);

    return masterChannel + numNoteChannels >= other.masterChannel;
}

//==============================================================================
bool MPEZone::truncateToFit (MPEZone other) noexcept
{
    const int masterChannelDiff = other.masterChannel - masterChannel;

    // we need at least 2 channels to be left after truncation:
    // 1 master channel and 1 note channel. otherwise we can't truncate.
    if (masterChannelDiff < 2)
        return false;

    numNoteChannels = jmin (numNoteChannels, masterChannelDiff - 1);
    return true;
}

//==============================================================================
bool MPEZone::operator== (const MPEZone& other) const noexcept
{
    return masterChannel         == other.masterChannel
        && numNoteChannels       == other.numNoteChannels
        && perNotePitchbendRange == other.perNotePitchbendRange
        && masterPitchbendRange  == other.masterPitchbendRange;
}

bool MPEZone::operator!= (const MPEZone& other) const noexcept
{
    return ! operator== (other);
}

//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

class MPEZoneTests   : public UnitTest
{
public:
    MPEZoneTests()  : UnitTest ("MPEZone class", "MIDI/MPE") {}

    void runTest() override
    {
        beginTest ("initialisation");
        {
            {
                MPEZone zone (1, 10);

                expectEquals (zone.getMasterChannel(), 1);
                expectEquals (zone.getNumNoteChannels(), 10);
                expectEquals (zone.getFirstNoteChannel(), 2);
                expectEquals (zone.getLastNoteChannel(), 11);
                expectEquals (zone.getPerNotePitchbendRange(), 48);
                expectEquals (zone.getMasterPitchbendRange(), 2);

                expect (zone.isUsingChannel (1));
                expect (zone.isUsingChannel (2));
                expect (zone.isUsingChannel (10));
                expect (zone.isUsingChannel (11));
                expect (! zone.isUsingChannel (12));
                expect (! zone.isUsingChannel (16));

                expect (! zone.isUsingChannelAsNoteChannel (1));
                expect (zone.isUsingChannelAsNoteChannel (2));
                expect (zone.isUsingChannelAsNoteChannel (10));
                expect (zone.isUsingChannelAsNoteChannel (11));
                expect (! zone.isUsingChannelAsNoteChannel (12));
                expect (! zone.isUsingChannelAsNoteChannel (16));
            }
            {
                MPEZone zone (5, 4);

                expectEquals (zone.getMasterChannel(), 5);
                expectEquals (zone.getNumNoteChannels(), 4);
                expectEquals (zone.getFirstNoteChannel(), 6);
                expectEquals (zone.getLastNoteChannel(), 9);
                expectEquals (zone.getPerNotePitchbendRange(), 48);
                expectEquals (zone.getMasterPitchbendRange(), 2);

                expect (! zone.isUsingChannel (1));
                expect (! zone.isUsingChannel (4));
                expect (zone.isUsingChannel (5));
                expect (zone.isUsingChannel (6));
                expect (zone.isUsingChannel (8));
                expect (zone.isUsingChannel (9));
                expect (! zone.isUsingChannel (10));
                expect (! zone.isUsingChannel (16));

                expect (! zone.isUsingChannelAsNoteChannel (5));
                expect (zone.isUsingChannelAsNoteChannel (6));
                expect (zone.isUsingChannelAsNoteChannel (8));
                expect (zone.isUsingChannelAsNoteChannel (9));
                expect (! zone.isUsingChannelAsNoteChannel (10));
            }

        }

        beginTest ("getNoteChannelRange");
        {
            MPEZone zone (2, 10);

            Range<int> noteChannelRange = zone.getNoteChannelRange();
            expectEquals (noteChannelRange.getStart(), 3);
            expectEquals (noteChannelRange.getEnd(), 13);
        }

        beginTest ("setting master pitchbend range");
        {
            MPEZone zone (1, 10);

            zone.setMasterPitchbendRange (96);
            expectEquals (zone.getMasterPitchbendRange(), 96);
            zone.setMasterPitchbendRange (0);
            expectEquals (zone.getMasterPitchbendRange(), 0);

            expectEquals (zone.getPerNotePitchbendRange(), 48);
        }

        beginTest ("setting per-note pitchbend range");
        {
            MPEZone zone (1, 10);

            zone.setPerNotePitchbendRange (96);
            expectEquals (zone.getPerNotePitchbendRange(), 96);
            zone.setPerNotePitchbendRange (0);
            expectEquals (zone.getPerNotePitchbendRange(), 0);

            expectEquals (zone.getMasterPitchbendRange(), 2);
        }

        beginTest ("checking overlap");
        {
            testOverlapsWith (1, 10, 1, 10, true);
            testOverlapsWith (1, 4,  6, 3,  false);
            testOverlapsWith (1, 4,  8, 3,  false);
            testOverlapsWith (2, 10, 2, 8,  true);
            testOverlapsWith (1, 10, 3, 2,  true);
            testOverlapsWith (3, 10, 5, 9,  true);
        }

        beginTest ("truncating");
        {
            testTruncateToFit (1, 10, 3, 10, true,  1, 1);
            testTruncateToFit (3, 10, 1, 10, false, 3, 10);
            testTruncateToFit (1, 10, 5, 8,  true,  1, 3);
            testTruncateToFit (5, 8,  1, 10, false, 5, 8);
            testTruncateToFit (1, 10, 4, 3,  true,  1, 2);
            testTruncateToFit (4, 3,  1, 10, false, 4, 3);
            testTruncateToFit (1, 3,  5, 3,  true,  1, 3);
            testTruncateToFit (5, 3,  1, 3,  false, 5, 3);
            testTruncateToFit (1, 3,  7, 3,  true,  1, 3);
            testTruncateToFit (7, 3,  1, 3,  false, 7, 3);
            testTruncateToFit (1, 10, 2, 10, false, 1, 10);
            testTruncateToFit (2, 10, 1, 10, false, 2, 10);
        }
    }

private:
    //==============================================================================
    void testOverlapsWith (int masterChannelFirst, int numNoteChannelsFirst,
                           int masterChannelSecond, int numNoteChannelsSecond,
                           bool expectedRetVal)
    {
        MPEZone first (masterChannelFirst, numNoteChannelsFirst);
        MPEZone second (masterChannelSecond, numNoteChannelsSecond);

        expect (first.overlapsWith (second) == expectedRetVal);
        expect (second.overlapsWith (first) == expectedRetVal);
    }

    //==============================================================================
    void testTruncateToFit (int masterChannelFirst, int numNoteChannelsFirst,
                            int masterChannelSecond, int numNoteChannelsSecond,
                            bool expectedRetVal,
                            int masterChannelFirstAfter, int numNoteChannelsFirstAfter)
    {
        MPEZone first (masterChannelFirst, numNoteChannelsFirst);
        MPEZone second (masterChannelSecond, numNoteChannelsSecond);

        expect (first.truncateToFit (second) == expectedRetVal);
        expectEquals (first.getMasterChannel(), masterChannelFirstAfter);
        expectEquals (first.getNumNoteChannels(), numNoteChannelsFirstAfter);
    }
};

static MPEZoneTests MPEZoneUnitTests;

#endif // JUCE_UNIT_TESTS

} // namespace juce
