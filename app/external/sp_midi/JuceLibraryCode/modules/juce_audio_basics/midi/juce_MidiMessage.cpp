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

namespace MidiHelpers
{
    inline uint8 initialByte (const int type, const int channel) noexcept
    {
        return (uint8) (type | jlimit (0, 15, channel - 1));
    }

    inline uint8 validVelocity (const int v) noexcept
    {
        return (uint8) jlimit (0, 127, v);
    }
}

//==============================================================================
uint8 MidiMessage::floatValueToMidiByte (const float v) noexcept
{
    jassert (v >= 0 && v <= 1.0f);  // if your value is > 1, maybe you're passing an
                                    // integer value to a float method by mistake?

    return MidiHelpers::validVelocity (roundToInt (v * 127.0f));
}

uint16 MidiMessage::pitchbendToPitchwheelPos (const float pitchbend,
                                 const float pitchbendRange) noexcept
{
    // can't translate a pitchbend value that is outside of the given range!
    jassert (std::abs (pitchbend) <= pitchbendRange);

    return static_cast<uint16> (pitchbend > 0.0f
                                  ? jmap (pitchbend, 0.0f, pitchbendRange, 8192.0f, 16383.0f)
                                  : jmap (pitchbend, -pitchbendRange, 0.0f, 0.0f, 8192.0f));
}

//==============================================================================
int MidiMessage::readVariableLengthVal (const uint8* data, int& numBytesUsed) noexcept
{
    numBytesUsed = 0;
    int v = 0, i;

    do
    {
        i = (int) *data++;

        if (++numBytesUsed > 6)
            break;

        v = (v << 7) + (i & 0x7f);

    } while (i & 0x80);

    return v;
}

int MidiMessage::getMessageLengthFromFirstByte (const uint8 firstByte) noexcept
{
    // this method only works for valid starting bytes of a short midi message
    jassert (firstByte >= 0x80 && firstByte != 0xf0 && firstByte != 0xf7);

    static const char messageLengths[] =
    {
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        1, 2, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    };

    return messageLengths[firstByte & 0x7f];
}

//==============================================================================
MidiMessage::MidiMessage() noexcept
   : size (2)
{
    packedData.asBytes[0] = 0xf0;
    packedData.asBytes[1] = 0xf7;
}

MidiMessage::MidiMessage (const void* const d, const int dataSize, const double t)
   : timeStamp (t), size (dataSize)
{
    jassert (dataSize > 0);
    // this checks that the length matches the data..
    jassert (dataSize > 3 || *(uint8*)d >= 0xf0 || getMessageLengthFromFirstByte (*(uint8*)d) == size);

    memcpy (allocateSpace (dataSize), d, (size_t) dataSize);
}

MidiMessage::MidiMessage (const int byte1, const double t) noexcept
   : timeStamp (t), size (1)
{
    packedData.asBytes[0] = (uint8) byte1;

    // check that the length matches the data..
    jassert (byte1 >= 0xf0 || getMessageLengthFromFirstByte ((uint8) byte1) == 1);
}

MidiMessage::MidiMessage (const int byte1, const int byte2, const double t) noexcept
   : timeStamp (t), size (2)
{
    packedData.asBytes[0] = (uint8) byte1;
    packedData.asBytes[1] = (uint8) byte2;

    // check that the length matches the data..
    jassert (byte1 >= 0xf0 || getMessageLengthFromFirstByte ((uint8) byte1) == 2);
}

MidiMessage::MidiMessage (const int byte1, const int byte2, const int byte3, const double t) noexcept
   : timeStamp (t), size (3)
{
    packedData.asBytes[0] = (uint8) byte1;
    packedData.asBytes[1] = (uint8) byte2;
    packedData.asBytes[2] = (uint8) byte3;

    // check that the length matches the data..
    jassert (byte1 >= 0xf0 || getMessageLengthFromFirstByte ((uint8) byte1) == 3);
}

MidiMessage::MidiMessage (const MidiMessage& other)
   : timeStamp (other.timeStamp), size (other.size)
{
    if (isHeapAllocated())
        memcpy (allocateSpace (size), other.getData(), (size_t) size);
    else
        packedData.allocatedData = other.packedData.allocatedData;
}

MidiMessage::MidiMessage (const MidiMessage& other, const double newTimeStamp)
   : timeStamp (newTimeStamp), size (other.size)
{
    if (isHeapAllocated())
        memcpy (allocateSpace (size), other.getData(), (size_t) size);
    else
        packedData.allocatedData = other.packedData.allocatedData;
}

MidiMessage::MidiMessage (const void* srcData, int sz, int& numBytesUsed, const uint8 lastStatusByte,
                          double t, bool sysexHasEmbeddedLength)
    : timeStamp (t)
{
    auto src = static_cast<const uint8*> (srcData);
    auto byte = (unsigned int) *src;

    if (byte < 0x80)
    {
        byte = (unsigned int) lastStatusByte;
        numBytesUsed = -1;
    }
    else
    {
        numBytesUsed = 0;
        --sz;
        ++src;
    }

    if (byte >= 0x80)
    {
        if (byte == 0xf0)
        {
            auto d = src;
            bool haveReadAllLengthBytes = ! sysexHasEmbeddedLength;
            int numVariableLengthSysexBytes = 0;

            while (d < src + sz)
            {
                if (*d >= 0x80)
                {
                    if (*d == 0xf7)
                    {
                        ++d;  // include the trailing 0xf7 when we hit it
                        break;
                    }

                    if (haveReadAllLengthBytes) // if we see a 0x80 bit set after the initial data length
                        break;                  // bytes, assume it's the end of the sysex

                    ++numVariableLengthSysexBytes;
                }
                else if (! haveReadAllLengthBytes)
                {
                    haveReadAllLengthBytes = true;
                    ++numVariableLengthSysexBytes;
                }

                ++d;
            }

            src += numVariableLengthSysexBytes;
            size = 1 + (int) (d - src);

            auto dest = allocateSpace (size);
            *dest = (uint8) byte;
            memcpy (dest + 1, src, (size_t) (size - 1));

            numBytesUsed += (numVariableLengthSysexBytes + size);  // (these aren't counted in the size)
        }
        else if (byte == 0xff)
        {
            if (sz == 1)
            {
                size = 1;
            }
            else
            {
                int n;
                const int bytesLeft = readVariableLengthVal (src + 1, n);
                size = jmin (sz + 1, n + 2 + bytesLeft);
            }

            auto dest = allocateSpace (size);
            *dest = (uint8) byte;
            memcpy (dest + 1, src, (size_t) size - 1);

            numBytesUsed += size;
        }
        else
        {
            size = getMessageLengthFromFirstByte ((uint8) byte);
            packedData.asBytes[0] = (uint8) byte;

            if (size > 1)
            {
                packedData.asBytes[1] = (sz > 0 ? src[0] : 0);

                if (size > 2)
                    packedData.asBytes[2] = (sz > 1 ? src[1] : 0);
            }

            numBytesUsed += jmin (size, sz + 1);
        }
    }
    else
    {
        packedData.allocatedData = nullptr;
        size = 0;
    }
}

MidiMessage& MidiMessage::operator= (const MidiMessage& other)
{
    if (this != &other)
    {
        if (other.isHeapAllocated())
        {
            if (isHeapAllocated())
                packedData.allocatedData = static_cast<uint8*> (std::realloc (packedData.allocatedData, (size_t) other.size));
            else
                packedData.allocatedData = static_cast<uint8*> (std::malloc ((size_t) other.size));

            memcpy (packedData.allocatedData, other.packedData.allocatedData, (size_t) other.size);
        }
        else
        {
            if (isHeapAllocated())
                std::free (packedData.allocatedData);

            packedData.allocatedData = other.packedData.allocatedData;
        }

        timeStamp = other.timeStamp;
        size = other.size;
    }

    return *this;
}

MidiMessage::MidiMessage (MidiMessage&& other) noexcept
   : timeStamp (other.timeStamp), size (other.size)
{
    packedData.allocatedData = other.packedData.allocatedData;
    other.size = 0;
}

MidiMessage& MidiMessage::operator= (MidiMessage&& other) noexcept
{
    packedData.allocatedData = other.packedData.allocatedData;
    timeStamp = other.timeStamp;
    size = other.size;
    other.size = 0;
    return *this;
}

MidiMessage::~MidiMessage() noexcept
{
    if (isHeapAllocated())
        std::free (packedData.allocatedData);
}

uint8* MidiMessage::allocateSpace (int bytes)
{
    if (bytes > (int) sizeof (packedData))
    {
        auto d = static_cast<uint8*> (std::malloc ((size_t) bytes));
        packedData.allocatedData = d;
        return d;
    }

    return packedData.asBytes;
}

String MidiMessage::getDescription() const
{
    if (isNoteOn())           return "Note on "  + MidiMessage::getMidiNoteName (getNoteNumber(), true, true, 3) + " Velocity " + String (getVelocity()) + " Channel " + String (getChannel());
    if (isNoteOff())          return "Note off " + MidiMessage::getMidiNoteName (getNoteNumber(), true, true, 3) + " Velocity " + String (getVelocity()) + " Channel " + String (getChannel());
    if (isProgramChange())    return "Program change " + String (getProgramChangeNumber()) + " Channel " + String (getChannel());
    if (isPitchWheel())       return "Pitch wheel " + String (getPitchWheelValue()) + " Channel " + String (getChannel());
    if (isAftertouch())       return "Aftertouch " + MidiMessage::getMidiNoteName (getNoteNumber(), true, true, 3) +  ": " + String (getAfterTouchValue()) + " Channel " + String (getChannel());
    if (isChannelPressure())  return "Channel pressure " + String (getChannelPressureValue()) + " Channel " + String (getChannel());
    if (isAllNotesOff())      return "All notes off Channel " + String (getChannel());
    if (isAllSoundOff())      return "All sound off Channel " + String (getChannel());
    if (isMetaEvent())        return "Meta event";

    if (isController())
    {
        String name (MidiMessage::getControllerName (getControllerNumber()));

        if (name.isEmpty())
            name = String (getControllerNumber());

        return "Controller " + name + ": " + String (getControllerValue()) + " Channel " + String (getChannel());
    }

    return String::toHexString (getRawData(), getRawDataSize());
}

MidiMessage MidiMessage::withTimeStamp (double newTimestamp) const
{
    return { *this, newTimestamp };
}

int MidiMessage::getChannel() const noexcept
{
    auto data = getRawData();

    if ((data[0] & 0xf0) != 0xf0)
        return (data[0] & 0xf) + 1;

    return 0;
}

bool MidiMessage::isForChannel (const int channel) const noexcept
{
    jassert (channel > 0 && channel <= 16); // valid channels are numbered 1 to 16

    auto data = getRawData();

    return ((data[0] & 0xf) == channel - 1)
             && ((data[0] & 0xf0) != 0xf0);
}

void MidiMessage::setChannel (const int channel) noexcept
{
    jassert (channel > 0 && channel <= 16); // valid channels are numbered 1 to 16

    auto data = getData();

    if ((data[0] & 0xf0) != (uint8) 0xf0)
        data[0] = (uint8) ((data[0] & (uint8) 0xf0)
                            | (uint8)(channel - 1));
}

bool MidiMessage::isNoteOn (const bool returnTrueForVelocity0) const noexcept
{
    auto data = getRawData();

    return ((data[0] & 0xf0) == 0x90)
             && (returnTrueForVelocity0 || data[2] != 0);
}

bool MidiMessage::isNoteOff (const bool returnTrueForNoteOnVelocity0) const noexcept
{
    auto data = getRawData();

    return ((data[0] & 0xf0) == 0x80)
            || (returnTrueForNoteOnVelocity0 && (data[2] == 0) && ((data[0] & 0xf0) == 0x90));
}

bool MidiMessage::isNoteOnOrOff() const noexcept
{
    auto d = getRawData()[0] & 0xf0;
    return (d == 0x90) || (d == 0x80);
}

int MidiMessage::getNoteNumber() const noexcept
{
    return getRawData()[1];
}

void MidiMessage::setNoteNumber (const int newNoteNumber) noexcept
{
    if (isNoteOnOrOff() || isAftertouch())
        getData()[1] = (uint8) (newNoteNumber & 127);
}

uint8 MidiMessage::getVelocity() const noexcept
{
    if (isNoteOnOrOff())
        return getRawData()[2];

    return 0;
}

float MidiMessage::getFloatVelocity() const noexcept
{
    return getVelocity() * (1.0f / 127.0f);
}

void MidiMessage::setVelocity (const float newVelocity) noexcept
{
    if (isNoteOnOrOff())
        getData()[2] = floatValueToMidiByte (newVelocity);
}

void MidiMessage::multiplyVelocity (const float scaleFactor) noexcept
{
    if (isNoteOnOrOff())
    {
        auto data = getData();
        data[2] = MidiHelpers::validVelocity (roundToInt (scaleFactor * data[2]));
    }
}

bool MidiMessage::isAftertouch() const noexcept
{
    return (getRawData()[0] & 0xf0) == 0xa0;
}

int MidiMessage::getAfterTouchValue() const noexcept
{
    jassert (isAftertouch());
    return getRawData()[2];
}

MidiMessage MidiMessage::aftertouchChange (const int channel,
                                           const int noteNum,
                                           const int aftertouchValue) noexcept
{
    jassert (channel > 0 && channel <= 16); // valid channels are numbered 1 to 16
    jassert (isPositiveAndBelow (noteNum, 128));
    jassert (isPositiveAndBelow (aftertouchValue, 128));

    return MidiMessage (MidiHelpers::initialByte (0xa0, channel),
                        noteNum & 0x7f,
                        aftertouchValue & 0x7f);
}

bool MidiMessage::isChannelPressure() const noexcept
{
    return (getRawData()[0] & 0xf0) == 0xd0;
}

int MidiMessage::getChannelPressureValue() const noexcept
{
    jassert (isChannelPressure());
    return getRawData()[1];
}

MidiMessage MidiMessage::channelPressureChange (const int channel, const int pressure) noexcept
{
    jassert (channel > 0 && channel <= 16); // valid channels are numbered 1 to 16
    jassert (isPositiveAndBelow (pressure, 128));

    return MidiMessage (MidiHelpers::initialByte (0xd0, channel), pressure & 0x7f);
}

bool MidiMessage::isSustainPedalOn() const noexcept     { return isControllerOfType (0x40) && getRawData()[2] >= 64; }
bool MidiMessage::isSustainPedalOff() const noexcept    { return isControllerOfType (0x40) && getRawData()[2] <  64; }

bool MidiMessage::isSostenutoPedalOn() const noexcept   { return isControllerOfType (0x42) && getRawData()[2] >= 64; }
bool MidiMessage::isSostenutoPedalOff() const noexcept  { return isControllerOfType (0x42) && getRawData()[2] <  64; }

bool MidiMessage::isSoftPedalOn() const noexcept        { return isControllerOfType (0x43) && getRawData()[2] >= 64; }
bool MidiMessage::isSoftPedalOff() const noexcept       { return isControllerOfType (0x43) && getRawData()[2] <  64; }


bool MidiMessage::isProgramChange() const noexcept
{
    return (getRawData()[0] & 0xf0) == 0xc0;
}

int MidiMessage::getProgramChangeNumber() const noexcept
{
    jassert (isProgramChange());
    return getRawData()[1];
}

MidiMessage MidiMessage::programChange (const int channel, const int programNumber) noexcept
{
    jassert (channel > 0 && channel <= 16); // valid channels are numbered 1 to 16

    return MidiMessage (MidiHelpers::initialByte (0xc0, channel), programNumber & 0x7f);
}

bool MidiMessage::isPitchWheel() const noexcept
{
    return (getRawData()[0] & 0xf0) == 0xe0;
}

int MidiMessage::getPitchWheelValue() const noexcept
{
    jassert (isPitchWheel());
    auto data = getRawData();
    return data[1] | (data[2] << 7);
}

MidiMessage MidiMessage::pitchWheel (const int channel, const int position) noexcept
{
    jassert (channel > 0 && channel <= 16); // valid channels are numbered 1 to 16
    jassert (isPositiveAndBelow (position, 0x4000));

    return MidiMessage (MidiHelpers::initialByte (0xe0, channel),
                        position & 127, (position >> 7) & 127);
}

bool MidiMessage::isController() const noexcept
{
    return (getRawData()[0] & 0xf0) == 0xb0;
}

bool MidiMessage::isControllerOfType (const int controllerType) const noexcept
{
    auto data = getRawData();
    return (data[0] & 0xf0) == 0xb0 && data[1] == controllerType;
}

int MidiMessage::getControllerNumber() const noexcept
{
    jassert (isController());
    return getRawData()[1];
}

int MidiMessage::getControllerValue() const noexcept
{
    jassert (isController());
    return getRawData()[2];
}

MidiMessage MidiMessage::controllerEvent (const int channel, const int controllerType, const int value) noexcept
{
    // the channel must be between 1 and 16 inclusive
    jassert (channel > 0 && channel <= 16);

    return MidiMessage (MidiHelpers::initialByte (0xb0, channel),
                        controllerType & 127, value & 127);
}

MidiMessage MidiMessage::noteOn (const int channel, const int noteNumber, const uint8 velocity) noexcept
{
    jassert (channel > 0 && channel <= 16);
    jassert (isPositiveAndBelow (noteNumber, 128));

    return MidiMessage (MidiHelpers::initialByte (0x90, channel),
                        noteNumber & 127, MidiHelpers::validVelocity (velocity));
}

MidiMessage MidiMessage::noteOn (const int channel, const int noteNumber, const float velocity) noexcept
{
    return noteOn (channel, noteNumber, floatValueToMidiByte (velocity));
}

MidiMessage MidiMessage::noteOff (const int channel, const int noteNumber, uint8 velocity) noexcept
{
    jassert (channel > 0 && channel <= 16);
    jassert (isPositiveAndBelow (noteNumber, 128));

    return MidiMessage (MidiHelpers::initialByte (0x80, channel),
                        noteNumber & 127, MidiHelpers::validVelocity (velocity));
}

MidiMessage MidiMessage::noteOff (const int channel, const int noteNumber, float velocity) noexcept
{
    return noteOff (channel, noteNumber, floatValueToMidiByte (velocity));
}

MidiMessage MidiMessage::noteOff (const int channel, const int noteNumber) noexcept
{
    jassert (channel > 0 && channel <= 16);
    jassert (isPositiveAndBelow (noteNumber, 128));

    return MidiMessage (MidiHelpers::initialByte (0x80, channel), noteNumber & 127, 0);
}

MidiMessage MidiMessage::allNotesOff (const int channel) noexcept
{
    return controllerEvent (channel, 123, 0);
}

bool MidiMessage::isAllNotesOff() const noexcept
{
    auto data = getRawData();
    return (data[0] & 0xf0) == 0xb0 && data[1] == 123;
}

MidiMessage MidiMessage::allSoundOff (const int channel) noexcept
{
    return controllerEvent (channel, 120, 0);
}

bool MidiMessage::isAllSoundOff() const noexcept
{
    auto data = getRawData();
    return data[1] == 120 && (data[0] & 0xf0) == 0xb0;
}

bool MidiMessage::isResetAllControllers() const noexcept
{
    auto data = getRawData();
    return (data[0] & 0xf0) == 0xb0 && data[1] == 121;
}

MidiMessage MidiMessage::allControllersOff (const int channel) noexcept
{
    return controllerEvent (channel, 121, 0);
}

MidiMessage MidiMessage::masterVolume (const float volume)
{
    auto vol = jlimit (0, 0x3fff, roundToInt (volume * 0x4000));

    return { 0xf0, 0x7f, 0x7f, 0x04, 0x01, vol & 0x7f, vol >> 7, 0xf7 };
}

//==============================================================================
bool MidiMessage::isSysEx() const noexcept
{
    return *getRawData() == 0xf0;
}

MidiMessage MidiMessage::createSysExMessage (const void* sysexData, const int dataSize)
{
    HeapBlock<uint8> m (dataSize + 2);

    m[0] = 0xf0;
    memcpy (m + 1, sysexData, (size_t) dataSize);
    m[dataSize + 1] = 0xf7;

    return MidiMessage (m, dataSize + 2);
}

const uint8* MidiMessage::getSysExData() const noexcept
{
    return isSysEx() ? getRawData() + 1 : nullptr;
}

int MidiMessage::getSysExDataSize() const noexcept
{
    return isSysEx() ? size - 2 : 0;
}

//==============================================================================
bool MidiMessage::isMetaEvent() const noexcept      { return *getRawData() == 0xff; }
bool MidiMessage::isActiveSense() const noexcept    { return *getRawData() == 0xfe; }

int MidiMessage::getMetaEventType() const noexcept
{
    auto data = getRawData();
    return *data != 0xff ? -1 : data[1];
}

int MidiMessage::getMetaEventLength() const noexcept
{
    auto data = getRawData();

    if (*data == 0xff)
    {
        int n;
        return jmin (size - 2, readVariableLengthVal (data + 2, n));
    }

    return 0;
}

const uint8* MidiMessage::getMetaEventData() const noexcept
{
    jassert (isMetaEvent());

    int n;
    auto d = getRawData() + 2;
    readVariableLengthVal (d, n);
    return d + n;
}

bool MidiMessage::isTrackMetaEvent() const noexcept         { return getMetaEventType() == 0; }
bool MidiMessage::isEndOfTrackMetaEvent() const noexcept    { return getMetaEventType() == 47; }

bool MidiMessage::isTextMetaEvent() const noexcept
{
    auto t = getMetaEventType();
    return t > 0 && t < 16;
}

String MidiMessage::getTextFromTextMetaEvent() const
{
    auto textData = reinterpret_cast<const char*> (getMetaEventData());

    return String (CharPointer_UTF8 (textData),
                   CharPointer_UTF8 (textData + getMetaEventLength()));
}

MidiMessage MidiMessage::textMetaEvent (int type, StringRef text)
{
    jassert (type > 0 && type < 16);

    MidiMessage result;

    const size_t textSize = text.text.sizeInBytes() - 1;

    uint8 header[8];
    size_t n = sizeof (header);

    header[--n] = (uint8) (textSize & 0x7f);

    for (size_t i = textSize; (i >>= 7) != 0;)
        header[--n] = (uint8) ((i & 0x7f) | 0x80);

    header[--n] = (uint8) type;
    header[--n] = 0xff;

    const size_t headerLen = sizeof (header) - n;
    const int totalSize = (int) (headerLen + textSize);

    auto dest = result.allocateSpace (totalSize);
    result.size = totalSize;

    memcpy (dest, header + n, headerLen);
    memcpy (dest + headerLen, text.text.getAddress(), textSize);

    return result;
}

bool MidiMessage::isTrackNameEvent() const noexcept         { auto data = getRawData(); return (data[1] == 3)    && (*data == 0xff); }
bool MidiMessage::isTempoMetaEvent() const noexcept         { auto data = getRawData(); return (data[1] == 81)   && (*data == 0xff); }
bool MidiMessage::isMidiChannelMetaEvent() const noexcept   { auto data = getRawData(); return (data[1] == 0x20) && (*data == 0xff) && (data[2] == 1); }

int MidiMessage::getMidiChannelMetaEventChannel() const noexcept
{
    jassert (isMidiChannelMetaEvent());
    return getRawData()[3] + 1;
}

double MidiMessage::getTempoSecondsPerQuarterNote() const noexcept
{
    if (! isTempoMetaEvent())
        return 0.0;

    auto d = getMetaEventData();

    return (((unsigned int) d[0] << 16)
             | ((unsigned int) d[1] << 8)
             | d[2])
            / 1000000.0;
}

double MidiMessage::getTempoMetaEventTickLength (const short timeFormat) const noexcept
{
    if (timeFormat > 0)
    {
        if (! isTempoMetaEvent())
            return 0.5 / timeFormat;

        return getTempoSecondsPerQuarterNote() / timeFormat;
    }

    const int frameCode = (-timeFormat) >> 8;
    double framesPerSecond;

    switch (frameCode)
    {
        case 24: framesPerSecond = 24.0;   break;
        case 25: framesPerSecond = 25.0;   break;
        case 29: framesPerSecond = 30.0 * 1000.0 / 1001.0;  break;
        case 30: framesPerSecond = 30.0;   break;
        default: framesPerSecond = 30.0;   break;
    }

    return (1.0 / framesPerSecond) / (timeFormat & 0xff);
}

MidiMessage MidiMessage::tempoMetaEvent (int microsecondsPerQuarterNote) noexcept
{
    return { 0xff, 81, 3,
             (uint8) (microsecondsPerQuarterNote >> 16),
             (uint8) (microsecondsPerQuarterNote >> 8),
             (uint8) microsecondsPerQuarterNote };
}

bool MidiMessage::isTimeSignatureMetaEvent() const noexcept
{
    auto data = getRawData();
    return (data[1] == 0x58) && (*data == (uint8) 0xff);
}

void MidiMessage::getTimeSignatureInfo (int& numerator, int& denominator) const noexcept
{
    if (isTimeSignatureMetaEvent())
    {
        auto d = getMetaEventData();
        numerator = d[0];
        denominator = 1 << d[1];
    }
    else
    {
        numerator = 4;
        denominator = 4;
    }
}

MidiMessage MidiMessage::timeSignatureMetaEvent (const int numerator, const int denominator)
{
    int n = 1;
    int powerOfTwo = 0;

    while (n < denominator)
    {
        n <<= 1;
        ++powerOfTwo;
    }

    return { 0xff, 0x58, 0x04, numerator, powerOfTwo, 1, 96 };
}

MidiMessage MidiMessage::midiChannelMetaEvent (const int channel) noexcept
{
    return { 0xff, 0x20, 0x01, jlimit (0, 0xff, channel - 1) };
}

bool MidiMessage::isKeySignatureMetaEvent() const noexcept
{
    return getMetaEventType() == 0x59;
}

int MidiMessage::getKeySignatureNumberOfSharpsOrFlats() const noexcept
{
    return (int) (int8) getMetaEventData()[0];
}

bool MidiMessage::isKeySignatureMajorKey() const noexcept
{
    return getMetaEventData()[1] == 0;
}

MidiMessage MidiMessage::keySignatureMetaEvent (int numberOfSharpsOrFlats, bool isMinorKey)
{
    jassert (numberOfSharpsOrFlats >= -7 && numberOfSharpsOrFlats <= 7);

    return { 0xff, 0x59, 0x02, numberOfSharpsOrFlats, isMinorKey ? 1 : 0 };
}

MidiMessage MidiMessage::endOfTrack() noexcept
{
    return { 0xff, 0x2f, 0x00 };
}

//==============================================================================
bool MidiMessage::isSongPositionPointer() const noexcept         { return *getRawData() == 0xf2; }
int MidiMessage::getSongPositionPointerMidiBeat() const noexcept { auto data = getRawData(); return data[1] | (data[2] << 7); }

MidiMessage MidiMessage::songPositionPointer (const int positionInMidiBeats) noexcept
{
    return { 0xf2,
             positionInMidiBeats & 127,
             (positionInMidiBeats >> 7) & 127 };
}

bool MidiMessage::isMidiStart() const noexcept                  { return *getRawData() == 0xfa; }
MidiMessage MidiMessage::midiStart() noexcept                   { return MidiMessage (0xfa); }

bool MidiMessage::isMidiContinue() const noexcept               { return *getRawData() == 0xfb; }
MidiMessage MidiMessage::midiContinue() noexcept                { return MidiMessage (0xfb); }

bool MidiMessage::isMidiStop() const noexcept                   { return *getRawData() == 0xfc; }
MidiMessage MidiMessage::midiStop() noexcept                    { return MidiMessage (0xfc); }

bool MidiMessage::isMidiClock() const noexcept                  { return *getRawData() == 0xf8; }
MidiMessage MidiMessage::midiClock() noexcept                   { return MidiMessage (0xf8); }

bool MidiMessage::isQuarterFrame() const noexcept               { return *getRawData() == 0xf1; }
int MidiMessage::getQuarterFrameSequenceNumber() const noexcept { return ((int) getRawData()[1]) >> 4; }
int MidiMessage::getQuarterFrameValue() const noexcept          { return ((int) getRawData()[1]) & 0x0f; }

MidiMessage MidiMessage::quarterFrame (const int sequenceNumber, const int value) noexcept
{
    return MidiMessage (0xf1, (sequenceNumber << 4) | value);
}

bool MidiMessage::isFullFrame() const noexcept
{
    auto data = getRawData();

    return data[0] == 0xf0
            && data[1] == 0x7f
            && size >= 10
            && data[3] == 0x01
            && data[4] == 0x01;
}

void MidiMessage::getFullFrameParameters (int& hours, int& minutes, int& seconds, int& frames,
                                          MidiMessage::SmpteTimecodeType& timecodeType) const noexcept
{
    jassert (isFullFrame());

    auto data = getRawData();
    timecodeType = (SmpteTimecodeType) (data[5] >> 5);
    hours   = data[5] & 0x1f;
    minutes = data[6];
    seconds = data[7];
    frames  = data[8];
}

MidiMessage MidiMessage::fullFrame (int hours, int minutes, int seconds, int frames,
                                    MidiMessage::SmpteTimecodeType timecodeType)
{
    return { 0xf0, 0x7f, 0x7f, 0x01, 0x01,
             (hours & 0x01f) | (timecodeType << 5),
             minutes, seconds, frames,
             0xf7 };
}

bool MidiMessage::isMidiMachineControlMessage() const noexcept
{
    auto data = getRawData();

    return data[0] == 0xf0
        && data[1] == 0x7f
        && data[3] == 0x06
        && size > 5;
}

MidiMessage::MidiMachineControlCommand MidiMessage::getMidiMachineControlCommand() const noexcept
{
    jassert (isMidiMachineControlMessage());

    return (MidiMachineControlCommand) getRawData()[4];
}

MidiMessage MidiMessage::midiMachineControlCommand (MidiMessage::MidiMachineControlCommand command)
{
    return { 0xf0, 0x7f, 0, 6, command, 0xf7 };
}

//==============================================================================
bool MidiMessage::isMidiMachineControlGoto (int& hours, int& minutes, int& seconds, int& frames) const noexcept
{
    auto data = getRawData();

    if (size >= 12
         && data[0] == 0xf0
         && data[1] == 0x7f
         && data[3] == 0x06
         && data[4] == 0x44
         && data[5] == 0x06
         && data[6] == 0x01)
    {
        hours = data[7] % 24;   // (that some machines send out hours > 24)
        minutes = data[8];
        seconds = data[9];
        frames  = data[10];

        return true;
    }

    return false;
}

MidiMessage MidiMessage::midiMachineControlGoto (int hours, int minutes, int seconds, int frames)
{
    return { 0xf0, 0x7f, 0, 6, 0x44, 6, 1, hours, minutes, seconds, frames, 0xf7 };
}

//==============================================================================
String MidiMessage::getMidiNoteName (int note, bool useSharps, bool includeOctaveNumber, int octaveNumForMiddleC)
{
    static const char* const sharpNoteNames[] = { "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" };
    static const char* const flatNoteNames[]  = { "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B" };

    if (isPositiveAndBelow (note, 128))
    {
        String s (useSharps ? sharpNoteNames[note % 12]
                            : flatNoteNames [note % 12]);

        if (includeOctaveNumber)
            s << (note / 12 + (octaveNumForMiddleC - 5));

        return s;
    }

    return {};
}

double MidiMessage::getMidiNoteInHertz (const int noteNumber, const double frequencyOfA) noexcept
{
    return frequencyOfA * std::pow (2.0, (noteNumber - 69) / 12.0);
}

bool MidiMessage::isMidiNoteBlack (int noteNumber) noexcept
{
    return ((1 << (noteNumber % 12)) & 0x054a) != 0;
}

const char* MidiMessage::getGMInstrumentName (const int n)
{
    static const char* names[] =
    {
        NEEDS_TRANS("Acoustic Grand Piano"),    NEEDS_TRANS("Bright Acoustic Piano"),   NEEDS_TRANS("Electric Grand Piano"),    NEEDS_TRANS("Honky-tonk Piano"),
        NEEDS_TRANS("Electric Piano 1"),        NEEDS_TRANS("Electric Piano 2"),        NEEDS_TRANS("Harpsichord"),             NEEDS_TRANS("Clavinet"),
        NEEDS_TRANS("Celesta"),                 NEEDS_TRANS("Glockenspiel"),            NEEDS_TRANS("Music Box"),               NEEDS_TRANS("Vibraphone"),
        NEEDS_TRANS("Marimba"),                 NEEDS_TRANS("Xylophone"),               NEEDS_TRANS("Tubular Bells"),           NEEDS_TRANS("Dulcimer"),
        NEEDS_TRANS("Drawbar Organ"),           NEEDS_TRANS("Percussive Organ"),        NEEDS_TRANS("Rock Organ"),              NEEDS_TRANS("Church Organ"),
        NEEDS_TRANS("Reed Organ"),              NEEDS_TRANS("Accordion"),               NEEDS_TRANS("Harmonica"),               NEEDS_TRANS("Tango Accordion"),
        NEEDS_TRANS("Acoustic Guitar (nylon)"), NEEDS_TRANS("Acoustic Guitar (steel)"), NEEDS_TRANS("Electric Guitar (jazz)"),  NEEDS_TRANS("Electric Guitar (clean)"),
        NEEDS_TRANS("Electric Guitar (mute)"),  NEEDS_TRANS("Overdriven Guitar"),       NEEDS_TRANS("Distortion Guitar"),       NEEDS_TRANS("Guitar Harmonics"),
        NEEDS_TRANS("Acoustic Bass"),           NEEDS_TRANS("Electric Bass (finger)"),  NEEDS_TRANS("Electric Bass (pick)"),    NEEDS_TRANS("Fretless Bass"),
        NEEDS_TRANS("Slap Bass 1"),             NEEDS_TRANS("Slap Bass 2"),             NEEDS_TRANS("Synth Bass 1"),            NEEDS_TRANS("Synth Bass 2"),
        NEEDS_TRANS("Violin"),                  NEEDS_TRANS("Viola"),                   NEEDS_TRANS("Cello"),                   NEEDS_TRANS("Contrabass"),
        NEEDS_TRANS("Tremolo Strings"),         NEEDS_TRANS("Pizzicato Strings"),       NEEDS_TRANS("Orchestral Harp"),         NEEDS_TRANS("Timpani"),
        NEEDS_TRANS("String Ensemble 1"),       NEEDS_TRANS("String Ensemble 2"),       NEEDS_TRANS("SynthStrings 1"),          NEEDS_TRANS("SynthStrings 2"),
        NEEDS_TRANS("Choir Aahs"),              NEEDS_TRANS("Voice Oohs"),              NEEDS_TRANS("Synth Voice"),             NEEDS_TRANS("Orchestra Hit"),
        NEEDS_TRANS("Trumpet"),                 NEEDS_TRANS("Trombone"),                NEEDS_TRANS("Tuba"),                    NEEDS_TRANS("Muted Trumpet"),
        NEEDS_TRANS("French Horn"),             NEEDS_TRANS("Brass Section"),           NEEDS_TRANS("SynthBrass 1"),            NEEDS_TRANS("SynthBrass 2"),
        NEEDS_TRANS("Soprano Sax"),             NEEDS_TRANS("Alto Sax"),                NEEDS_TRANS("Tenor Sax"),               NEEDS_TRANS("Baritone Sax"),
        NEEDS_TRANS("Oboe"),                    NEEDS_TRANS("English Horn"),            NEEDS_TRANS("Bassoon"),                 NEEDS_TRANS("Clarinet"),
        NEEDS_TRANS("Piccolo"),                 NEEDS_TRANS("Flute"),                   NEEDS_TRANS("Recorder"),                NEEDS_TRANS("Pan Flute"),
        NEEDS_TRANS("Blown Bottle"),            NEEDS_TRANS("Shakuhachi"),              NEEDS_TRANS("Whistle"),                 NEEDS_TRANS("Ocarina"),
        NEEDS_TRANS("Lead 1 (square)"),         NEEDS_TRANS("Lead 2 (sawtooth)"),       NEEDS_TRANS("Lead 3 (calliope)"),       NEEDS_TRANS("Lead 4 (chiff)"),
        NEEDS_TRANS("Lead 5 (charang)"),        NEEDS_TRANS("Lead 6 (voice)"),          NEEDS_TRANS("Lead 7 (fifths)"),         NEEDS_TRANS("Lead 8 (bass+lead)"),
        NEEDS_TRANS("Pad 1 (new age)"),         NEEDS_TRANS("Pad 2 (warm)"),            NEEDS_TRANS("Pad 3 (polysynth)"),       NEEDS_TRANS("Pad 4 (choir)"),
        NEEDS_TRANS("Pad 5 (bowed)"),           NEEDS_TRANS("Pad 6 (metallic)"),        NEEDS_TRANS("Pad 7 (halo)"),            NEEDS_TRANS("Pad 8 (sweep)"),
        NEEDS_TRANS("FX 1 (rain)"),             NEEDS_TRANS("FX 2 (soundtrack)"),       NEEDS_TRANS("FX 3 (crystal)"),          NEEDS_TRANS("FX 4 (atmosphere)"),
        NEEDS_TRANS("FX 5 (brightness)"),       NEEDS_TRANS("FX 6 (goblins)"),          NEEDS_TRANS("FX 7 (echoes)"),           NEEDS_TRANS("FX 8 (sci-fi)"),
        NEEDS_TRANS("Sitar"),                   NEEDS_TRANS("Banjo"),                   NEEDS_TRANS("Shamisen"),                NEEDS_TRANS("Koto"),
        NEEDS_TRANS("Kalimba"),                 NEEDS_TRANS("Bag pipe"),                NEEDS_TRANS("Fiddle"),                  NEEDS_TRANS("Shanai"),
        NEEDS_TRANS("Tinkle Bell"),             NEEDS_TRANS("Agogo"),                   NEEDS_TRANS("Steel Drums"),             NEEDS_TRANS("Woodblock"),
        NEEDS_TRANS("Taiko Drum"),              NEEDS_TRANS("Melodic Tom"),             NEEDS_TRANS("Synth Drum"),              NEEDS_TRANS("Reverse Cymbal"),
        NEEDS_TRANS("Guitar Fret Noise"),       NEEDS_TRANS("Breath Noise"),            NEEDS_TRANS("Seashore"),                NEEDS_TRANS("Bird Tweet"),
        NEEDS_TRANS("Telephone Ring"),          NEEDS_TRANS("Helicopter"),              NEEDS_TRANS("Applause"),                NEEDS_TRANS("Gunshot")
    };

    return isPositiveAndBelow (n, numElementsInArray (names)) ? names[n] : nullptr;
}

const char* MidiMessage::getGMInstrumentBankName (const int n)
{
    static const char* names[] =
    {
        NEEDS_TRANS("Piano"),           NEEDS_TRANS("Chromatic Percussion"),    NEEDS_TRANS("Organ"),       NEEDS_TRANS("Guitar"),
        NEEDS_TRANS("Bass"),            NEEDS_TRANS("Strings"),                 NEEDS_TRANS("Ensemble"),    NEEDS_TRANS("Brass"),
        NEEDS_TRANS("Reed"),            NEEDS_TRANS("Pipe"),                    NEEDS_TRANS("Synth Lead"),  NEEDS_TRANS("Synth Pad"),
        NEEDS_TRANS("Synth Effects"),   NEEDS_TRANS("Ethnic"),                  NEEDS_TRANS("Percussive"),  NEEDS_TRANS("Sound Effects")
    };

    return isPositiveAndBelow (n, numElementsInArray (names)) ? names[n] : nullptr;
}

const char* MidiMessage::getRhythmInstrumentName (const int n)
{
    static const char* names[] =
    {
        NEEDS_TRANS("Acoustic Bass Drum"),  NEEDS_TRANS("Bass Drum 1"),     NEEDS_TRANS("Side Stick"),      NEEDS_TRANS("Acoustic Snare"),
        NEEDS_TRANS("Hand Clap"),           NEEDS_TRANS("Electric Snare"),  NEEDS_TRANS("Low Floor Tom"),   NEEDS_TRANS("Closed Hi-Hat"),
        NEEDS_TRANS("High Floor Tom"),      NEEDS_TRANS("Pedal Hi-Hat"),    NEEDS_TRANS("Low Tom"),         NEEDS_TRANS("Open Hi-Hat"),
        NEEDS_TRANS("Low-Mid Tom"),         NEEDS_TRANS("Hi-Mid Tom"),      NEEDS_TRANS("Crash Cymbal 1"),  NEEDS_TRANS("High Tom"),
        NEEDS_TRANS("Ride Cymbal 1"),       NEEDS_TRANS("Chinese Cymbal"),  NEEDS_TRANS("Ride Bell"),       NEEDS_TRANS("Tambourine"),
        NEEDS_TRANS("Splash Cymbal"),       NEEDS_TRANS("Cowbell"),         NEEDS_TRANS("Crash Cymbal 2"),  NEEDS_TRANS("Vibraslap"),
        NEEDS_TRANS("Ride Cymbal 2"),       NEEDS_TRANS("Hi Bongo"),        NEEDS_TRANS("Low Bongo"),       NEEDS_TRANS("Mute Hi Conga"),
        NEEDS_TRANS("Open Hi Conga"),       NEEDS_TRANS("Low Conga"),       NEEDS_TRANS("High Timbale"),    NEEDS_TRANS("Low Timbale"),
        NEEDS_TRANS("High Agogo"),          NEEDS_TRANS("Low Agogo"),       NEEDS_TRANS("Cabasa"),          NEEDS_TRANS("Maracas"),
        NEEDS_TRANS("Short Whistle"),       NEEDS_TRANS("Long Whistle"),    NEEDS_TRANS("Short Guiro"),     NEEDS_TRANS("Long Guiro"),
        NEEDS_TRANS("Claves"),              NEEDS_TRANS("Hi Wood Block"),   NEEDS_TRANS("Low Wood Block"),  NEEDS_TRANS("Mute Cuica"),
        NEEDS_TRANS("Open Cuica"),          NEEDS_TRANS("Mute Triangle"),   NEEDS_TRANS("Open Triangle")
    };

    return (n >= 35 && n <= 81) ? names[n - 35] : nullptr;
}

const char* MidiMessage::getControllerName (const int n)
{
    static const char* names[] =
    {
        NEEDS_TRANS("Bank Select"), NEEDS_TRANS("Modulation Wheel (coarse)"), NEEDS_TRANS("Breath controller (coarse)"),
        nullptr,
        NEEDS_TRANS("Foot Pedal (coarse)"), NEEDS_TRANS("Portamento Time (coarse)"), NEEDS_TRANS("Data Entry (coarse)"),
        NEEDS_TRANS("Volume (coarse)"), NEEDS_TRANS("Balance (coarse)"),
        nullptr,
        NEEDS_TRANS("Pan position (coarse)"), NEEDS_TRANS("Expression (coarse)"), NEEDS_TRANS("Effect Control 1 (coarse)"),
        NEEDS_TRANS("Effect Control 2 (coarse)"),
        nullptr, nullptr,
        NEEDS_TRANS("General Purpose Slider 1"), NEEDS_TRANS("General Purpose Slider 2"),
        NEEDS_TRANS("General Purpose Slider 3"), NEEDS_TRANS("General Purpose Slider 4"),
        nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
        NEEDS_TRANS("Bank Select (fine)"), NEEDS_TRANS("Modulation Wheel (fine)"), NEEDS_TRANS("Breath controller (fine)"),
        nullptr,
        NEEDS_TRANS("Foot Pedal (fine)"), NEEDS_TRANS("Portamento Time (fine)"), NEEDS_TRANS("Data Entry (fine)"), NEEDS_TRANS("Volume (fine)"),
        NEEDS_TRANS("Balance (fine)"), nullptr, NEEDS_TRANS("Pan position (fine)"), NEEDS_TRANS("Expression (fine)"),
        NEEDS_TRANS("Effect Control 1 (fine)"), NEEDS_TRANS("Effect Control 2 (fine)"),
        nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
        nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
        NEEDS_TRANS("Hold Pedal (on/off)"), NEEDS_TRANS("Portamento (on/off)"), NEEDS_TRANS("Sustenuto Pedal (on/off)"), NEEDS_TRANS("Soft Pedal (on/off)"),
        NEEDS_TRANS("Legato Pedal (on/off)"), NEEDS_TRANS("Hold 2 Pedal (on/off)"), NEEDS_TRANS("Sound Variation"), NEEDS_TRANS("Sound Timbre"),
        NEEDS_TRANS("Sound Release Time"), NEEDS_TRANS("Sound Attack Time"), NEEDS_TRANS("Sound Brightness"), NEEDS_TRANS("Sound Control 6"),
        NEEDS_TRANS("Sound Control 7"), NEEDS_TRANS("Sound Control 8"), NEEDS_TRANS("Sound Control 9"), NEEDS_TRANS("Sound Control 10"),
        NEEDS_TRANS("General Purpose Button 1 (on/off)"), NEEDS_TRANS("General Purpose Button 2 (on/off)"),
        NEEDS_TRANS("General Purpose Button 3 (on/off)"), NEEDS_TRANS("General Purpose Button 4 (on/off)"),
        nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
        NEEDS_TRANS("Reverb Level"), NEEDS_TRANS("Tremolo Level"), NEEDS_TRANS("Chorus Level"), NEEDS_TRANS("Celeste Level"),
        NEEDS_TRANS("Phaser Level"), NEEDS_TRANS("Data Button increment"), NEEDS_TRANS("Data Button decrement"), NEEDS_TRANS("Non-registered Parameter (fine)"),
        NEEDS_TRANS("Non-registered Parameter (coarse)"), NEEDS_TRANS("Registered Parameter (fine)"), NEEDS_TRANS("Registered Parameter (coarse)"),
        nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
        nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
        NEEDS_TRANS("All Sound Off"), NEEDS_TRANS("All Controllers Off"), NEEDS_TRANS("Local Keyboard (on/off)"), NEEDS_TRANS("All Notes Off"),
        NEEDS_TRANS("Omni Mode Off"), NEEDS_TRANS("Omni Mode On"), NEEDS_TRANS("Mono Operation"), NEEDS_TRANS("Poly Operation")
    };

    return isPositiveAndBelow (n, numElementsInArray (names)) ? names[n] : nullptr;
}

} // namespace juce
