/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   JUCE is an open source library subject to commercial or open-source
   licensing.

   By using JUCE, you agree to the terms of both the JUCE 5 End-User License
   Agreement and JUCE 5 Privacy Policy (both updated and effective as of the
   27th April 2017).

   End User License Agreement: www.juce.com/juce-5-licence
   Privacy Policy: www.juce.com/juce-5-privacy-policy

   Or: You may also use this code under the terms of the GPL v3 (see
   www.gnu.org/licenses).

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

namespace juce
{

const OSCTimeTag OSCTimeTag::immediately;

static const uint64 millisecondsBetweenOscAndJuceEpochs = 2208988800000ULL;
static const uint64 rawTimeTagRepresentingImmediately = 0x0000000000000001ULL;

//==============================================================================
OSCTimeTag::OSCTimeTag() noexcept  : rawTimeTag (rawTimeTagRepresentingImmediately)
{
}

OSCTimeTag::OSCTimeTag (uint64 t) noexcept  : rawTimeTag (t)
{
}

OSCTimeTag::OSCTimeTag (Time time) noexcept
{
    const uint64 milliseconds = (uint64) time.toMilliseconds() + millisecondsBetweenOscAndJuceEpochs;

    uint64 seconds = milliseconds / 1000;
    uint32 fractionalPart = uint32 (4294967.296 * (milliseconds % 1000));

    rawTimeTag = (seconds << 32) + fractionalPart;
}

//==============================================================================
Time OSCTimeTag::toTime() const noexcept
{
    const uint64 seconds = rawTimeTag >> 32;
    const uint32 fractionalPart = (rawTimeTag & 0x00000000FFFFFFFFULL);

    const double fractionalPartInMillis = (double) fractionalPart / 4294967.296;

    // now using signed integer, because this is allowed to become negative:
    const int64 juceTimeInMillis = int64 ((seconds * 1000)
                                           + (uint64) roundToInt(fractionalPartInMillis)
                                           - millisecondsBetweenOscAndJuceEpochs);

    return Time (juceTimeInMillis);
}

//==============================================================================
bool OSCTimeTag::isImmediately() const noexcept
{
    return rawTimeTag == rawTimeTagRepresentingImmediately;
}

//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

class OSCTimeTagTests  : public UnitTest
{
public:
    OSCTimeTagTests() : UnitTest ("OSCTimeTag class", "OSC") {}

    void runTest()
    {
        beginTest ("Basics");

        {
            OSCTimeTag tag;
            expect (tag.isImmediately());
        }
        {
            OSCTimeTag tag (3535653);
            expect (! tag.isImmediately());

            OSCTimeTag otherTag;
            otherTag = tag;
            expect (! otherTag.isImmediately());

            OSCTimeTag copyTag (tag);
            expect (! copyTag.isImmediately());
        }

        beginTest ("Conversion to/from Juce Time");

        {
            Time time;
            OSCTimeTag tag (time);
            expect (! tag.isImmediately());
        }
        {
            OSCTimeTag tag;
            Time time = tag.toTime();
            expect (time < Time::getCurrentTime());
        }
        {
            Time currentTime (Time::currentTimeMillis());
            double deltaInSeconds = 1.234;
            RelativeTime delta (deltaInSeconds);
            Time laterTime = currentTime + delta;

            OSCTimeTag currentTimeTag (currentTime);
            OSCTimeTag laterTimeTag (laterTime);

            uint64 currentTimeTagRaw = currentTimeTag.getRawTimeTag();
            uint64 laterTimeTagRaw = laterTimeTag.getRawTimeTag();

            // in the raw time tag, the most significant 32 bits are seconds,
            // so let's verify that the difference is right:
            uint64 diff = laterTimeTagRaw - currentTimeTagRaw;
            double acceptableErrorInSeconds = 0.000001; // definitely not audible anymore.

            expect (diff / float (1ULL << 32) < deltaInSeconds + acceptableErrorInSeconds );
            expect (diff / float (1ULL << 32) > deltaInSeconds - acceptableErrorInSeconds );

            // round trip:

            Time currentTime2 = currentTimeTag.toTime();
            Time laterTime2 = laterTimeTag.toTime();
            RelativeTime delta2 = laterTime2 - currentTime2;

            expect (currentTime2 == currentTime);
            expect (laterTime2 == laterTime);
            expect (delta2 == delta);
        }
    }
};

static OSCTimeTagTests OSCTimeTagUnitTests;

#endif

} // namespace juce
