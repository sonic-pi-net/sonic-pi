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

MidiRPNDetector::MidiRPNDetector() noexcept
{
}

MidiRPNDetector::~MidiRPNDetector() noexcept
{
}

bool MidiRPNDetector::parseControllerMessage (int midiChannel,
                                              int controllerNumber,
                                              int controllerValue,
                                              MidiRPNMessage& result) noexcept
{
    jassert (midiChannel >= 1 && midiChannel <= 16);
    jassert (controllerNumber >= 0 && controllerNumber < 128);
    jassert (controllerValue >= 0 && controllerValue < 128);

    return states[midiChannel - 1].handleController (midiChannel, controllerNumber, controllerValue, result);
}

void MidiRPNDetector::reset() noexcept
{
    for (int i = 0; i < 16; ++i)
    {
        states[i].parameterMSB = 0xff;
        states[i].parameterLSB = 0xff;
        states[i].resetValue();
        states[i].isNRPN = false;
    }
}

//==============================================================================
MidiRPNDetector::ChannelState::ChannelState() noexcept
    : parameterMSB (0xff), parameterLSB (0xff), valueMSB (0xff), valueLSB (0xff), isNRPN (false)
{
}

bool MidiRPNDetector::ChannelState::handleController (int channel,
                                                      int controllerNumber,
                                                      int value,
                                                      MidiRPNMessage& result) noexcept
{
    switch (controllerNumber)
    {
        case 0x62:  parameterLSB = uint8 (value); resetValue(); isNRPN = true;  break;
        case 0x63:  parameterMSB = uint8 (value); resetValue(); isNRPN = true;  break;

        case 0x64:  parameterLSB = uint8 (value); resetValue(); isNRPN = false; break;
        case 0x65:  parameterMSB = uint8 (value); resetValue(); isNRPN = false; break;

        case 0x06:  valueMSB = uint8 (value); return sendIfReady (channel, result);
        case 0x26:  valueLSB = uint8 (value); break;

        default:  break;
    }

    return false;
}

void MidiRPNDetector::ChannelState::resetValue() noexcept
{
    valueMSB = 0xff;
    valueLSB = 0xff;
}

//==============================================================================
bool MidiRPNDetector::ChannelState::sendIfReady (int channel, MidiRPNMessage& result) noexcept
{
    if (parameterMSB < 0x80 && parameterLSB < 0x80)
    {
        if (valueMSB < 0x80)
        {
            result.channel = channel;
            result.parameterNumber = (parameterMSB << 7) + parameterLSB;
            result.isNRPN = isNRPN;

            if (valueLSB < 0x80)
            {
                result.value = (valueMSB << 7) + valueLSB;
                result.is14BitValue = true;
            }
            else
            {
                result.value = valueMSB;
                result.is14BitValue = false;
            }

            return true;
        }
    }

    return false;
}

//==============================================================================
MidiBuffer MidiRPNGenerator::generate (MidiRPNMessage message)
{
    return generate (message.channel,
                     message.parameterNumber,
                     message.value,
                     message.isNRPN,
                     message.is14BitValue);
}

MidiBuffer MidiRPNGenerator::generate (int midiChannel,
                                       int parameterNumber,
                                       int value,
                                       bool isNRPN,
                                       bool use14BitValue)
{
    jassert (midiChannel > 0 && midiChannel <= 16);
    jassert (parameterNumber >= 0 && parameterNumber < 16384);
    jassert (value >= 0 && value < (use14BitValue ? 16384 : 128));

    uint8 parameterLSB = uint8 (parameterNumber & 0x0000007f);
    uint8 parameterMSB = uint8 (parameterNumber >> 7);

    uint8 valueLSB = use14BitValue ? uint8 (value & 0x0000007f) : 0x00;
    uint8 valueMSB = use14BitValue ? uint8 (value >> 7) : uint8 (value);

    uint8 channelByte = uint8 (0xb0 + midiChannel - 1);

    MidiBuffer buffer;

    buffer.addEvent (MidiMessage (channelByte, isNRPN ? 0x62 : 0x64, parameterLSB),  0);
    buffer.addEvent (MidiMessage (channelByte, isNRPN ? 0x63 : 0x65, parameterMSB),  0);

    // sending the value LSB is optional, but must come before sending the value MSB:
    if (use14BitValue)
        buffer.addEvent (MidiMessage (channelByte, 0x26, valueLSB), 0);

    buffer.addEvent (MidiMessage (channelByte, 0x06, valueMSB), 0);

    return buffer;
}


//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

class MidiRPNDetectorTests   : public UnitTest
{
public:
    MidiRPNDetectorTests()
        : UnitTest ("MidiRPNDetector class", UnitTestCategories::midi)
    {}

    void runTest() override
    {
        beginTest ("7-bit RPN");
        {
            MidiRPNDetector detector;
            MidiRPNMessage rpn;
            expect (! detector.parseControllerMessage (2, 101, 0,  rpn));
            expect (! detector.parseControllerMessage (2, 100, 7,  rpn));
            expect (detector.parseControllerMessage   (2, 6,   42, rpn));

            expectEquals (rpn.channel, 2);
            expectEquals (rpn.parameterNumber, 7);
            expectEquals (rpn.value, 42);
            expect (! rpn.isNRPN);
            expect (! rpn.is14BitValue);
        }

        beginTest ("14-bit RPN");
        {
            MidiRPNDetector detector;
            MidiRPNMessage rpn;
            expect (! detector.parseControllerMessage (1, 100, 44, rpn));
            expect (! detector.parseControllerMessage (1, 101, 2,  rpn));
            expect (! detector.parseControllerMessage (1, 38,  94, rpn));
            expect (detector.parseControllerMessage   (1, 6,   1,  rpn));

            expectEquals (rpn.channel, 1);
            expectEquals (rpn.parameterNumber, 300);
            expectEquals (rpn.value, 222);
            expect (! rpn.isNRPN);
            expect (rpn.is14BitValue);
        }

        beginTest ("RPNs on multiple channels simultaneously");
        {
            MidiRPNDetector detector;
            MidiRPNMessage rpn;
            expect (! detector.parseControllerMessage (1, 100, 44, rpn));
            expect (! detector.parseControllerMessage (2, 101, 0,  rpn));
            expect (! detector.parseControllerMessage (1, 101, 2,  rpn));
            expect (! detector.parseControllerMessage (2, 100, 7,  rpn));
            expect (! detector.parseControllerMessage (1, 38,  94, rpn));
            expect (detector.parseControllerMessage   (2, 6,   42, rpn));

            expectEquals (rpn.channel, 2);
            expectEquals (rpn.parameterNumber, 7);
            expectEquals (rpn.value, 42);
            expect (! rpn.isNRPN);
            expect (! rpn.is14BitValue);

            expect (detector.parseControllerMessage   (1, 6,   1,  rpn));

            expectEquals (rpn.channel, 1);
            expectEquals (rpn.parameterNumber, 300);
            expectEquals (rpn.value, 222);
            expect (! rpn.isNRPN);
            expect (rpn.is14BitValue);
        }

        beginTest ("14-bit RPN with value within 7-bit range");
        {
            MidiRPNDetector detector;
            MidiRPNMessage rpn;
            expect (! detector.parseControllerMessage (16, 100, 0 , rpn));
            expect (! detector.parseControllerMessage (16, 101, 0,  rpn));
            expect (! detector.parseControllerMessage (16, 38,  3,  rpn));
            expect (detector.parseControllerMessage   (16, 6,   0,  rpn));

            expectEquals (rpn.channel, 16);
            expectEquals (rpn.parameterNumber, 0);
            expectEquals (rpn.value, 3);
            expect (! rpn.isNRPN);
            expect (rpn.is14BitValue);
        }

        beginTest ("invalid RPN (wrong order)");
        {
            MidiRPNDetector detector;
            MidiRPNMessage rpn;
            expect (! detector.parseControllerMessage (2, 6,   42, rpn));
            expect (! detector.parseControllerMessage (2, 101, 0,  rpn));
            expect (! detector.parseControllerMessage (2, 100, 7,  rpn));
        }

        beginTest ("14-bit RPN interspersed with unrelated CC messages");
        {
            MidiRPNDetector detector;
            MidiRPNMessage rpn;
            expect (! detector.parseControllerMessage (16, 3,   80, rpn));
            expect (! detector.parseControllerMessage (16, 100, 0 , rpn));
            expect (! detector.parseControllerMessage (16, 4,   81, rpn));
            expect (! detector.parseControllerMessage (16, 101, 0,  rpn));
            expect (! detector.parseControllerMessage (16, 5,   82, rpn));
            expect (! detector.parseControllerMessage (16, 5,   83, rpn));
            expect (! detector.parseControllerMessage (16, 38,  3,  rpn));
            expect (! detector.parseControllerMessage (16, 4,   84, rpn));
            expect (! detector.parseControllerMessage (16, 3,   85, rpn));
            expect (detector.parseControllerMessage   (16, 6,   0,  rpn));

            expectEquals (rpn.channel, 16);
            expectEquals (rpn.parameterNumber, 0);
            expectEquals (rpn.value, 3);
            expect (! rpn.isNRPN);
            expect (rpn.is14BitValue);
        }

        beginTest ("14-bit NRPN");
        {
            MidiRPNDetector detector;
            MidiRPNMessage rpn;
            expect (! detector.parseControllerMessage (1, 98,  44, rpn));
            expect (! detector.parseControllerMessage (1, 99 , 2,  rpn));
            expect (! detector.parseControllerMessage (1, 38,  94, rpn));
            expect (detector.parseControllerMessage   (1, 6,   1,  rpn));

            expectEquals (rpn.channel, 1);
            expectEquals (rpn.parameterNumber, 300);
            expectEquals (rpn.value, 222);
            expect (rpn.isNRPN);
            expect (rpn.is14BitValue);
        }

        beginTest ("reset");
        {
            MidiRPNDetector detector;
            MidiRPNMessage rpn;
            expect (! detector.parseControllerMessage (2, 101, 0,  rpn));
            detector.reset();
            expect (! detector.parseControllerMessage (2, 100, 7,  rpn));
            expect (! detector.parseControllerMessage (2, 6,   42, rpn));
        }
    }
};

static MidiRPNDetectorTests MidiRPNDetectorUnitTests;

//==============================================================================
class MidiRPNGeneratorTests   : public UnitTest
{
public:
    MidiRPNGeneratorTests()
        : UnitTest ("MidiRPNGenerator class", UnitTestCategories::midi)
    {}

    void runTest() override
    {
        beginTest ("generating RPN/NRPN");
        {
            {
                MidiBuffer buffer = MidiRPNGenerator::generate (1, 23, 1337, true, true);
                expectContainsRPN (buffer, 1, 23, 1337, true, true);
            }
            {
                MidiBuffer buffer = MidiRPNGenerator::generate (16, 101, 34, false, false);
                expectContainsRPN (buffer, 16, 101, 34, false, false);
            }
            {
                MidiRPNMessage message = { 16, 101, 34, false, false };
                MidiBuffer buffer = MidiRPNGenerator::generate (message);
                expectContainsRPN (buffer, message);
            }
        }
    }

private:
    //==============================================================================
    void expectContainsRPN (const MidiBuffer& midiBuffer,
                            int channel,
                            int parameterNumber,
                            int value,
                            bool isNRPN,
                            bool is14BitValue)
    {
        MidiRPNMessage expected = { channel, parameterNumber, value, isNRPN, is14BitValue };
        expectContainsRPN (midiBuffer, expected);
    }

    //==============================================================================
    void expectContainsRPN (const MidiBuffer& midiBuffer, MidiRPNMessage expected)
    {
        MidiRPNMessage result = MidiRPNMessage();
        MidiRPNDetector detector;

        for (const auto metadata : midiBuffer)
        {
            const auto midiMessage = metadata.getMessage();

            if (detector.parseControllerMessage (midiMessage.getChannel(),
                                                 midiMessage.getControllerNumber(),
                                                 midiMessage.getControllerValue(),
                                                 result))
                break;
        }

        expectEquals (result.channel, expected.channel);
        expectEquals (result.parameterNumber, expected.parameterNumber);
        expectEquals (result.value, expected.value);
        expect (result.isNRPN == expected.isNRPN);
        expect (result.is14BitValue == expected.is14BitValue);
    }
};

static MidiRPNGeneratorTests MidiRPNGeneratorUnitTests;

#endif

} // namespace juce
