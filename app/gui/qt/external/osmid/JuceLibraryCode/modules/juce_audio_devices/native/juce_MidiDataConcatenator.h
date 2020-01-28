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

//==============================================================================
/**
    Helper class that takes chunks of incoming midi bytes, packages them into
    messages, and dispatches them to a midi callback.
*/
class MidiDataConcatenator
{
public:
    //==============================================================================
    MidiDataConcatenator (int initialBufferSize)
        : pendingData ((size_t) initialBufferSize)
    {
    }

    void reset()
    {
        pendingBytes = 0;
        runningStatus = 0;
        pendingDataTime = 0;
    }

    template <typename UserDataType, typename CallbackType>
    void pushMidiData (const void* inputData, int numBytes, double time,
                       UserDataType* input, CallbackType& callback)
    {
        const uint8* d = static_cast<const uint8*> (inputData);

        while (numBytes > 0)
        {
            if (pendingBytes > 0 || d[0] == 0xf0)
            {
                processSysex (d, numBytes, time, input, callback);
                runningStatus = 0;
            }
            else
            {
                int len = 0;
                uint8 data[3];

                while (numBytes > 0)
                {
                    // If there's a realtime message embedded in the middle of
                    // the normal message, handle it now..
                    if (*d >= 0xf8 && *d <= 0xfe)
                    {
                        callback.handleIncomingMidiMessage (input, MidiMessage (*d++, time));
                        --numBytes;
                    }
                    else
                    {
                        if (len == 0 && *d < 0x80 && runningStatus >= 0x80)
                            data[len++] = runningStatus;

                        data[len++] = *d++;
                        --numBytes;

                        const uint8 firstByte = data[0];

                        if (firstByte < 0x80 || firstByte == 0xf7)
                        {
                            len = 0;
                            break;   // ignore this malformed MIDI message..
                        }

                        if (len >= MidiMessage::getMessageLengthFromFirstByte (firstByte))
                            break;
                    }
                }

                if (len > 0)
                {
                    int used = 0;
                    const MidiMessage m (data, len, used, 0, time);

                    if (used <= 0)
                        break; // malformed message..

                    jassert (used == len);
                    callback.handleIncomingMidiMessage (input, m);
                    runningStatus = data[0];
                }
            }
        }
    }

private:
    template <typename UserDataType, typename CallbackType>
    void processSysex (const uint8*& d, int& numBytes, double time,
                       UserDataType* input, CallbackType& callback)
    {
        if (*d == 0xf0)
        {
            pendingBytes = 0;
            pendingDataTime = time;
        }

        pendingData.ensureSize ((size_t) (pendingBytes + numBytes), false);
        uint8* totalMessage = static_cast<uint8*> (pendingData.getData());
        uint8* dest = totalMessage + pendingBytes;

        do
        {
            if (pendingBytes > 0 && *d >= 0x80)
            {
                if (*d == 0xf7)
                {
                    *dest++ = *d++;
                    ++pendingBytes;
                    --numBytes;
                    break;
                }

                if (*d >= 0xfa || *d == 0xf8)
                {
                    callback.handleIncomingMidiMessage (input, MidiMessage (*d, time));
                    ++d;
                    --numBytes;
                }
                else
                {
                    pendingBytes = 0;
                    int used = 0;
                    const MidiMessage m (d, numBytes, used, 0, time);

                    if (used > 0)
                    {
                        callback.handleIncomingMidiMessage (input, m);
                        numBytes -= used;
                        d += used;
                    }

                    break;
                }
            }
            else
            {
                *dest++ = *d++;
                ++pendingBytes;
                --numBytes;
            }
        }
        while (numBytes > 0);

        if (pendingBytes > 0)
        {
            if (totalMessage [pendingBytes - 1] == 0xf7)
            {
                callback.handleIncomingMidiMessage (input, MidiMessage (totalMessage, pendingBytes, pendingDataTime));
                pendingBytes = 0;
            }
            else
            {
                callback.handlePartialSysexMessage (input, totalMessage, pendingBytes, pendingDataTime);
            }
        }
    }

    MemoryBlock pendingData;
    double pendingDataTime = 0;
    int pendingBytes = 0;
    uint8 runningStatus = 0;

    JUCE_DECLARE_NON_COPYABLE (MidiDataConcatenator)
};

} // namespace juce
