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

MidiBuffer MPEMessages::addZone (MPEZone zone)
{
    MidiBuffer buffer (MidiRPNGenerator::generate (zone.getFirstNoteChannel(),
                                                   zoneLayoutMessagesRpnNumber,
                                                   zone.getNumNoteChannels(),
                                                   false, false));

    buffer.addEvents (perNotePitchbendRange (zone), 0, -1, 0);
    buffer.addEvents (masterPitchbendRange (zone), 0, -1, 0);

    return buffer;
}

MidiBuffer MPEMessages::perNotePitchbendRange (MPEZone zone)
{
    return MidiRPNGenerator::generate (zone.getFirstNoteChannel(), 0,
                                       zone.getPerNotePitchbendRange(),
                                       false, false);
}

MidiBuffer MPEMessages::masterPitchbendRange (MPEZone zone)
{
    return MidiRPNGenerator::generate (zone.getMasterChannel(), 0,
                                       zone.getMasterPitchbendRange(),
                                       false, false);
}

MidiBuffer MPEMessages::clearAllZones()
{
    return MidiRPNGenerator::generate (1, zoneLayoutMessagesRpnNumber, 16, false, false);
}

MidiBuffer MPEMessages::setZoneLayout (const MPEZoneLayout& layout)
{
    MidiBuffer buffer;

    buffer.addEvents (clearAllZones(), 0, -1, 0);

    for (int i = 0; i < layout.getNumZones(); ++i)
        buffer.addEvents (addZone (*layout.getZoneByIndex (i)), 0, -1, 0);

    return buffer;
}

//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

class MPEMessagesTests  : public UnitTest
{
public:
    MPEMessagesTests() : UnitTest ("MPEMessages class", "MIDI/MPE") {}

    void runTest() override
    {
        beginTest ("add zone");
        {
            {
                MidiBuffer buffer = MPEMessages::addZone (MPEZone (1, 7));

                const uint8 expectedBytes[] =
                {
                    0xb1, 0x64, 0x06, 0xb1, 0x65, 0x00, 0xb1, 0x06, 0x07, // set up zone
                    0xb1, 0x64, 0x00, 0xb1, 0x65, 0x00, 0xb1, 0x06, 0x30, // per-note pbrange (default = 48)
                    0xb0, 0x64, 0x00, 0xb0, 0x65, 0x00, 0xb0, 0x06, 0x02  // master pbrange (default = 2)
                };

                testMidiBuffer (buffer, expectedBytes, sizeof (expectedBytes));
            }
            {
                MidiBuffer buffer = MPEMessages::addZone (MPEZone (11, 5, 96, 0));

                const uint8 expectedBytes[] =
                {
                    0xbb, 0x64, 0x06, 0xbb, 0x65, 0x00, 0xbb, 0x06, 0x05, // set up zone
                    0xbb, 0x64, 0x00, 0xbb, 0x65, 0x00, 0xbb, 0x06, 0x60, // per-note pbrange (custom)
                    0xba, 0x64, 0x00, 0xba, 0x65, 0x00, 0xba, 0x06, 0x00  // master pbrange (custom)
                };

                testMidiBuffer (buffer, expectedBytes, sizeof (expectedBytes));
            }
        }

        beginTest ("set per-note pitchbend range");
        {
            MPEZone zone (3, 7, 96);
            MidiBuffer buffer = MPEMessages::perNotePitchbendRange (zone);

            const uint8 expectedBytes[] = { 0xb3, 0x64, 0x00, 0xb3, 0x65, 0x00, 0xb3, 0x06, 0x60 };

            testMidiBuffer (buffer, expectedBytes, sizeof (expectedBytes));
        }


        beginTest ("set master pitchbend range");
        {
            MPEZone zone (3, 7, 48, 60);
            MidiBuffer buffer = MPEMessages::masterPitchbendRange (zone);

            const uint8 expectedBytes[] = { 0xb2, 0x64, 0x00, 0xb2, 0x65, 0x00, 0xb2, 0x06, 0x3c };

            testMidiBuffer (buffer, expectedBytes, sizeof (expectedBytes));
        }

        beginTest ("clear all zones");
        {
            MidiBuffer buffer = MPEMessages::clearAllZones();

            const uint8 expectedBytes[] = { 0xb0, 0x64, 0x06, 0xb0, 0x65, 0x00, 0xb0, 0x06, 0x10 };

            testMidiBuffer (buffer, expectedBytes, sizeof (expectedBytes));
        }

        beginTest ("set complete state");
        {
            MPEZoneLayout layout;
            layout.addZone (MPEZone (1, 7, 96, 0));
            layout.addZone (MPEZone (9, 7));
            layout.addZone (MPEZone (5, 3));
            layout.addZone (MPEZone (5, 4));
            layout.addZone (MPEZone (6, 4));

            MidiBuffer buffer = MPEMessages::setZoneLayout (layout);

            const uint8 expectedBytes[] = {
                0xb0, 0x64, 0x06, 0xb0, 0x65, 0x00, 0xb0, 0x06, 0x10,  // clear all zones
                0xb1, 0x64, 0x06, 0xb1, 0x65, 0x00, 0xb1, 0x06, 0x03,  // set zone 1 (1, 3)
                0xb1, 0x64, 0x00, 0xb1, 0x65, 0x00, 0xb1, 0x06, 0x60,  // per-note pbrange (custom)
                0xb0, 0x64, 0x00, 0xb0, 0x65, 0x00, 0xb0, 0x06, 0x00,  // master pbrange (custom)
                0xb6, 0x64, 0x06, 0xb6, 0x65, 0x00, 0xb6, 0x06, 0x04,  // set zone 2 (6, 4)
                0xb6, 0x64, 0x00, 0xb6, 0x65, 0x00, 0xb6, 0x06, 0x30,  // per-note pbrange (default = 48)
                0xb5, 0x64, 0x00, 0xb5, 0x65, 0x00, 0xb5, 0x06, 0x02   // master pbrange (default = 2)
            };

            testMidiBuffer (buffer, expectedBytes, sizeof (expectedBytes));
        }
    }

private:
    //==============================================================================
    void testMidiBuffer (MidiBuffer& buffer, const uint8* expectedBytes, int expectedBytesSize)
    {
        uint8 actualBytes[128] = { 0 };
        extractRawBinaryData (buffer, actualBytes, sizeof (actualBytes));

        expectEquals (std::memcmp (actualBytes, expectedBytes, (std::size_t) expectedBytesSize), 0);
    }

    //==============================================================================
    void extractRawBinaryData (const MidiBuffer& midiBuffer, const uint8* bufferToCopyTo, std::size_t maxBytes)
    {
        std::size_t pos = 0;
        MidiBuffer::Iterator iter (midiBuffer);
        MidiMessage midiMessage;
        int samplePosition; // Note: not actually used, so no need to initialise.

        while (iter.getNextEvent (midiMessage, samplePosition))
        {
            const uint8* data = midiMessage.getRawData();
            std::size_t dataSize = (std::size_t) midiMessage.getRawDataSize();

            if (pos + dataSize > maxBytes)
                return;

            std::memcpy ((void*) (bufferToCopyTo + pos), data, dataSize);
            pos += dataSize;
        }
    }
};

static MPEMessagesTests MPEMessagesUnitTests;

#endif // JUCE_UNIT_TESTS

} // namespace juce
