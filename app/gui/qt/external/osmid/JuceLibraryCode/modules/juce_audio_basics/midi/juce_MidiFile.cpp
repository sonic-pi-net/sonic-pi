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

namespace MidiFileHelpers
{
    static void writeVariableLengthInt (OutputStream& out, unsigned int v)
    {
        unsigned int buffer = v & 0x7f;

        while ((v >>= 7) != 0)
        {
            buffer <<= 8;
            buffer |= ((v & 0x7f) | 0x80);
        }

        for (;;)
        {
            out.writeByte ((char) buffer);

            if (buffer & 0x80)
                buffer >>= 8;
            else
                break;
        }
    }

    static bool parseMidiHeader (const uint8* &data, short& timeFormat, short& fileType, short& numberOfTracks) noexcept
    {
        unsigned int ch = ByteOrder::bigEndianInt (data);
        data += 4;

        if (ch != ByteOrder::bigEndianInt ("MThd"))
        {
            bool ok = false;

            if (ch == ByteOrder::bigEndianInt ("RIFF"))
            {
                for (int i = 0; i < 8; ++i)
                {
                    ch = ByteOrder::bigEndianInt (data);
                    data += 4;

                    if (ch == ByteOrder::bigEndianInt ("MThd"))
                    {
                        ok = true;
                        break;
                    }
                }
            }

            if (! ok)
                return false;
        }

        unsigned int bytesRemaining = ByteOrder::bigEndianInt (data);
        data += 4;
        fileType = (short) ByteOrder::bigEndianShort (data);
        data += 2;
        numberOfTracks = (short) ByteOrder::bigEndianShort (data);
        data += 2;
        timeFormat = (short) ByteOrder::bigEndianShort (data);
        data += 2;
        bytesRemaining -= 6;
        data += bytesRemaining;

        return true;
    }

    static double convertTicksToSeconds (const double time,
                                         const MidiMessageSequence& tempoEvents,
                                         const int timeFormat)
    {
        if (timeFormat < 0)
            return time / (-(timeFormat >> 8) * (timeFormat & 0xff));

        double lastTime = 0.0, correctedTime = 0.0;
        const double tickLen = 1.0 / (timeFormat & 0x7fff);
        double secsPerTick = 0.5 * tickLen;
        const int numEvents = tempoEvents.getNumEvents();

        for (int i = 0; i < numEvents; ++i)
        {
            const MidiMessage& m = tempoEvents.getEventPointer(i)->message;
            const double eventTime = m.getTimeStamp();

            if (eventTime >= time)
                break;

            correctedTime += (eventTime - lastTime) * secsPerTick;
            lastTime = eventTime;

            if (m.isTempoMetaEvent())
                secsPerTick = tickLen * m.getTempoSecondsPerQuarterNote();

            while (i + 1 < numEvents)
            {
                const MidiMessage& m2 = tempoEvents.getEventPointer(i + 1)->message;

                if (m2.getTimeStamp() != eventTime)
                    break;

                if (m2.isTempoMetaEvent())
                    secsPerTick = tickLen * m2.getTempoSecondsPerQuarterNote();

                ++i;
            }
        }

        return correctedTime + (time - lastTime) * secsPerTick;
    }

    // a comparator that puts all the note-offs before note-ons that have the same time
    struct Sorter
    {
        static int compareElements (const MidiMessageSequence::MidiEventHolder* const first,
                                    const MidiMessageSequence::MidiEventHolder* const second) noexcept
        {
            const double diff = (first->message.getTimeStamp() - second->message.getTimeStamp());

            if (diff > 0) return 1;
            if (diff < 0) return -1;
            if (first->message.isNoteOff() && second->message.isNoteOn())   return -1;
            if (first->message.isNoteOn()  && second->message.isNoteOff())  return 1;

            return 0;
        }
    };

    template <typename MethodType>
    static void findAllMatchingEvents (const OwnedArray<MidiMessageSequence>& tracks,
                                       MidiMessageSequence& results,
                                       MethodType method)
    {
        for (int i = 0; i < tracks.size(); ++i)
        {
            const MidiMessageSequence& track = *tracks.getUnchecked(i);
            const int numEvents = track.getNumEvents();

            for (int j = 0; j < numEvents; ++j)
            {
                const MidiMessage& m = track.getEventPointer(j)->message;

                if ((m.*method)())
                    results.addEvent (m);
            }
        }
    }
}

//==============================================================================
MidiFile::MidiFile()
   : timeFormat ((short) (unsigned short) 0xe728)
{
}

MidiFile::~MidiFile()
{
}

MidiFile::MidiFile (const MidiFile& other)
    : timeFormat (other.timeFormat)
{
    tracks.addCopiesOf (other.tracks);
}

MidiFile& MidiFile::operator= (const MidiFile& other)
{
    timeFormat = other.timeFormat;
    tracks.clear();
    tracks.addCopiesOf (other.tracks);

    return *this;
}

void MidiFile::clear()
{
    tracks.clear();
}

//==============================================================================
int MidiFile::getNumTracks() const noexcept
{
    return tracks.size();
}

const MidiMessageSequence* MidiFile::getTrack (const int index) const noexcept
{
    return tracks [index];
}

void MidiFile::addTrack (const MidiMessageSequence& trackSequence)
{
    tracks.add (new MidiMessageSequence (trackSequence));
}

//==============================================================================
short MidiFile::getTimeFormat() const noexcept
{
    return timeFormat;
}

void MidiFile::setTicksPerQuarterNote (const int ticks) noexcept
{
    timeFormat = (short) ticks;
}

void MidiFile::setSmpteTimeFormat (const int framesPerSecond,
                                   const int subframeResolution) noexcept
{
    timeFormat = (short) (((-framesPerSecond) << 8) | subframeResolution);
}

//==============================================================================
void MidiFile::findAllTempoEvents (MidiMessageSequence& results) const
{
    MidiFileHelpers::findAllMatchingEvents (tracks, results, &MidiMessage::isTempoMetaEvent);
}

void MidiFile::findAllTimeSigEvents (MidiMessageSequence& results) const
{
    MidiFileHelpers::findAllMatchingEvents (tracks, results, &MidiMessage::isTimeSignatureMetaEvent);
}

void MidiFile::findAllKeySigEvents (MidiMessageSequence& results) const
{
    MidiFileHelpers::findAllMatchingEvents (tracks, results, &MidiMessage::isKeySignatureMetaEvent);
}

double MidiFile::getLastTimestamp() const
{
    double t = 0.0;

    for (int i = tracks.size(); --i >= 0;)
        t = jmax (t, tracks.getUnchecked(i)->getEndTime());

    return t;
}

//==============================================================================
bool MidiFile::readFrom (InputStream& sourceStream)
{
    clear();
    MemoryBlock data;

    const int maxSensibleMidiFileSize = 200 * 1024 * 1024;

    // (put a sanity-check on the file size, as midi files are generally small)
    if (sourceStream.readIntoMemoryBlock (data, maxSensibleMidiFileSize))
    {
        size_t size = data.getSize();
        const uint8* d = static_cast<const uint8*> (data.getData());
        short fileType, expectedTracks;

        if (size > 16 && MidiFileHelpers::parseMidiHeader (d, timeFormat, fileType, expectedTracks))
        {
            size -= (size_t) (d - static_cast<const uint8*> (data.getData()));

            int track = 0;

            while (size > 0 && track < expectedTracks)
            {
                const int chunkType = (int) ByteOrder::bigEndianInt (d);
                d += 4;
                const int chunkSize = (int) ByteOrder::bigEndianInt (d);
                d += 4;

                if (chunkSize <= 0)
                    break;

                if (chunkType == (int) ByteOrder::bigEndianInt ("MTrk"))
                    readNextTrack (d, chunkSize);

                size -= (size_t) chunkSize + 8;
                d += chunkSize;
                ++track;
            }

            return true;
        }
    }

    return false;
}

void MidiFile::readNextTrack (const uint8* data, int size)
{
    double time = 0;
    uint8 lastStatusByte = 0;

    MidiMessageSequence result;

    while (size > 0)
    {
        int bytesUsed;
        const int delay = MidiMessage::readVariableLengthVal (data, bytesUsed);
        data += bytesUsed;
        size -= bytesUsed;
        time += delay;

        int messSize = 0;
        const MidiMessage mm (data, size, messSize, lastStatusByte, time);

        if (messSize <= 0)
            break;

        size -= messSize;
        data += messSize;

        result.addEvent (mm);

        const uint8 firstByte = *(mm.getRawData());
        if ((firstByte & 0xf0) != 0xf0)
            lastStatusByte = firstByte;
    }

    // use a sort that puts all the note-offs before note-ons that have the same time
    MidiFileHelpers::Sorter sorter;
    result.list.sort (sorter, true);

    addTrack (result);
    tracks.getLast()->updateMatchedPairs();
}

//==============================================================================
void MidiFile::convertTimestampTicksToSeconds()
{
    MidiMessageSequence tempoEvents;
    findAllTempoEvents (tempoEvents);
    findAllTimeSigEvents (tempoEvents);

    if (timeFormat != 0)
    {
        for (int i = 0; i < tracks.size(); ++i)
        {
            const MidiMessageSequence& ms = *tracks.getUnchecked(i);

            for (int j = ms.getNumEvents(); --j >= 0;)
            {
                MidiMessage& m = ms.getEventPointer(j)->message;
                m.setTimeStamp (MidiFileHelpers::convertTicksToSeconds (m.getTimeStamp(), tempoEvents, timeFormat));
            }
        }
    }
}

//==============================================================================
bool MidiFile::writeTo (OutputStream& out, int midiFileType)
{
    jassert (midiFileType >= 0 && midiFileType <= 2);

    if (! out.writeIntBigEndian ((int) ByteOrder::bigEndianInt ("MThd"))) return false;
    if (! out.writeIntBigEndian (6))                                      return false;
    if (! out.writeShortBigEndian ((short) midiFileType))                 return false;
    if (! out.writeShortBigEndian ((short) tracks.size()))                return false;
    if (! out.writeShortBigEndian (timeFormat))                           return false;

    for (int i = 0; i < tracks.size(); ++i)
        if (! writeTrack (out, i))
            return false;

    out.flush();
    return true;
}

bool MidiFile::writeTrack (OutputStream& mainOut, const int trackNum)
{
    MemoryOutputStream out;
    const MidiMessageSequence& ms = *tracks.getUnchecked (trackNum);

    int lastTick = 0;
    uint8 lastStatusByte = 0;
    bool endOfTrackEventWritten = false;

    for (int i = 0; i < ms.getNumEvents(); ++i)
    {
        const MidiMessage& mm = ms.getEventPointer(i)->message;

        if (mm.isEndOfTrackMetaEvent())
            endOfTrackEventWritten = true;

        const int tick = roundToInt (mm.getTimeStamp());
        const int delta = jmax (0, tick - lastTick);
        MidiFileHelpers::writeVariableLengthInt (out, (uint32) delta);
        lastTick = tick;

        const uint8* data = mm.getRawData();
        int dataSize = mm.getRawDataSize();

        const uint8 statusByte = data[0];

        if (statusByte == lastStatusByte
             && (statusByte & 0xf0) != 0xf0
             && dataSize > 1
             && i > 0)
        {
            ++data;
            --dataSize;
        }
        else if (statusByte == 0xf0)  // Write sysex message with length bytes.
        {
            out.writeByte ((char) statusByte);

            ++data;
            --dataSize;

            MidiFileHelpers::writeVariableLengthInt (out, (uint32) dataSize);
        }

        out.write (data, (size_t) dataSize);
        lastStatusByte = statusByte;
    }

    if (! endOfTrackEventWritten)
    {
        out.writeByte (0); // (tick delta)
        const MidiMessage m (MidiMessage::endOfTrack());
        out.write (m.getRawData(), (size_t) m.getRawDataSize());
    }

    if (! mainOut.writeIntBigEndian ((int) ByteOrder::bigEndianInt ("MTrk"))) return false;
    if (! mainOut.writeIntBigEndian ((int) out.getDataSize()))                return false;

    mainOut << out;

    return true;
}

} // namespace juce
