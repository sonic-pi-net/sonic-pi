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

void AudioDataConverters::convertFloatToInt16LE (const float* source, void* dest, int numSamples, int destBytesPerSample)
{
    auto maxVal = (double) 0x7fff;
    auto intData = static_cast<char*> (dest);

    if (dest != (void*) source || destBytesPerSample <= 4)
    {
        for (int i = 0; i < numSamples; ++i)
        {
            *reinterpret_cast<uint16*> (intData) = ByteOrder::swapIfBigEndian ((uint16) (short) roundToInt (jlimit (-maxVal, maxVal, maxVal * source[i])));
            intData += destBytesPerSample;
        }
    }
    else
    {
        intData += destBytesPerSample * numSamples;

        for (int i = numSamples; --i >= 0;)
        {
            intData -= destBytesPerSample;
            *reinterpret_cast<uint16*> (intData) = ByteOrder::swapIfBigEndian ((uint16) (short) roundToInt (jlimit (-maxVal, maxVal, maxVal * source[i])));
        }
    }
}

void AudioDataConverters::convertFloatToInt16BE (const float* source, void* dest, int numSamples, int destBytesPerSample)
{
    auto maxVal = (double) 0x7fff;
    auto intData = static_cast<char*> (dest);

    if (dest != (void*) source || destBytesPerSample <= 4)
    {
        for (int i = 0; i < numSamples; ++i)
        {
            *reinterpret_cast<uint16*> (intData) = ByteOrder::swapIfLittleEndian ((uint16) (short) roundToInt (jlimit (-maxVal, maxVal, maxVal * source[i])));
            intData += destBytesPerSample;
        }
    }
    else
    {
        intData += destBytesPerSample * numSamples;

        for (int i = numSamples; --i >= 0;)
        {
            intData -= destBytesPerSample;
            *reinterpret_cast<uint16*> (intData) = ByteOrder::swapIfLittleEndian ((uint16) (short) roundToInt (jlimit (-maxVal, maxVal, maxVal * source[i])));
        }
    }
}

void AudioDataConverters::convertFloatToInt24LE (const float* source, void* dest, int numSamples, int destBytesPerSample)
{
    auto maxVal = (double) 0x7fffff;
    auto intData = static_cast<char*> (dest);

    if (dest != (void*) source || destBytesPerSample <= 4)
    {
        for (int i = 0; i < numSamples; ++i)
        {
            ByteOrder::littleEndian24BitToChars (roundToInt (jlimit (-maxVal, maxVal, maxVal * source[i])), intData);
            intData += destBytesPerSample;
        }
    }
    else
    {
        intData += destBytesPerSample * numSamples;

        for (int i = numSamples; --i >= 0;)
        {
            intData -= destBytesPerSample;
            ByteOrder::littleEndian24BitToChars (roundToInt (jlimit (-maxVal, maxVal, maxVal * source[i])), intData);
        }
    }
}

void AudioDataConverters::convertFloatToInt24BE (const float* source, void* dest, int numSamples, int destBytesPerSample)
{
    auto maxVal = (double) 0x7fffff;
    auto intData = static_cast<char*> (dest);

    if (dest != (void*) source || destBytesPerSample <= 4)
    {
        for (int i = 0; i < numSamples; ++i)
        {
            ByteOrder::bigEndian24BitToChars (roundToInt (jlimit (-maxVal, maxVal, maxVal * source[i])), intData);
            intData += destBytesPerSample;
        }
    }
    else
    {
        intData += destBytesPerSample * numSamples;

        for (int i = numSamples; --i >= 0;)
        {
            intData -= destBytesPerSample;
            ByteOrder::bigEndian24BitToChars (roundToInt (jlimit (-maxVal, maxVal, maxVal * source[i])), intData);
        }
    }
}

void AudioDataConverters::convertFloatToInt32LE (const float* source, void* dest, int numSamples, int destBytesPerSample)
{
    auto maxVal = (double) 0x7fffffff;
    auto intData = static_cast<char*> (dest);

    if (dest != (void*) source || destBytesPerSample <= 4)
    {
        for (int i = 0; i < numSamples; ++i)
        {
            *reinterpret_cast<uint32*> (intData) = ByteOrder::swapIfBigEndian ((uint32) roundToInt (jlimit (-maxVal, maxVal, maxVal * source[i])));
            intData += destBytesPerSample;
        }
    }
    else
    {
        intData += destBytesPerSample * numSamples;

        for (int i = numSamples; --i >= 0;)
        {
            intData -= destBytesPerSample;
            *reinterpret_cast<uint32*> (intData) = ByteOrder::swapIfBigEndian ((uint32) roundToInt (jlimit (-maxVal, maxVal, maxVal * source[i])));
        }
    }
}

void AudioDataConverters::convertFloatToInt32BE (const float* source, void* dest, int numSamples, int destBytesPerSample)
{
    auto maxVal = (double) 0x7fffffff;
    auto intData = static_cast<char*> (dest);

    if (dest != (void*) source || destBytesPerSample <= 4)
    {
        for (int i = 0; i < numSamples; ++i)
        {
            *reinterpret_cast<uint32*> (intData) = ByteOrder::swapIfLittleEndian ((uint32) roundToInt (jlimit (-maxVal, maxVal, maxVal * source[i])));
            intData += destBytesPerSample;
        }
    }
    else
    {
        intData += destBytesPerSample * numSamples;

        for (int i = numSamples; --i >= 0;)
        {
            intData -= destBytesPerSample;
            *reinterpret_cast<uint32*> (intData) = ByteOrder::swapIfLittleEndian ((uint32) roundToInt (jlimit (-maxVal, maxVal, maxVal * source[i])));
        }
    }
}

void AudioDataConverters::convertFloatToFloat32LE (const float* source, void* dest, int numSamples, int destBytesPerSample)
{
    jassert (dest != (void*) source || destBytesPerSample <= 4); // This op can't be performed on in-place data!

    char* d = static_cast<char*> (dest);

    for (int i = 0; i < numSamples; ++i)
    {
        *reinterpret_cast<float*> (d) = source[i];

       #if JUCE_BIG_ENDIAN
        *reinterpret_cast<uint32*> (d) = ByteOrder::swap (*reinterpret_cast<uint32*> (d));
       #endif

        d += destBytesPerSample;
    }
}

void AudioDataConverters::convertFloatToFloat32BE (const float* source, void* dest, int numSamples, int destBytesPerSample)
{
    jassert (dest != (void*) source || destBytesPerSample <= 4); // This op can't be performed on in-place data!

    auto d = static_cast<char*> (dest);

    for (int i = 0; i < numSamples; ++i)
    {
        *reinterpret_cast<float*> (d) = source[i];

       #if JUCE_LITTLE_ENDIAN
        *reinterpret_cast<uint32*> (d) = ByteOrder::swap (*reinterpret_cast<uint32*> (d));
       #endif

        d += destBytesPerSample;
    }
}

//==============================================================================
void AudioDataConverters::convertInt16LEToFloat (const void* source, float* dest, int numSamples, int srcBytesPerSample)
{
    const float scale = 1.0f / 0x7fff;
    auto intData = static_cast<const char*> (source);

    if (source != (void*) dest || srcBytesPerSample >= 4)
    {
        for (int i = 0; i < numSamples; ++i)
        {
            dest[i] = scale * (short) ByteOrder::swapIfBigEndian (*reinterpret_cast<const uint16*> (intData));
            intData += srcBytesPerSample;
        }
    }
    else
    {
        intData += srcBytesPerSample * numSamples;

        for (int i = numSamples; --i >= 0;)
        {
            intData -= srcBytesPerSample;
            dest[i] = scale * (short) ByteOrder::swapIfBigEndian (*reinterpret_cast<const uint16*> (intData));
        }
    }
}

void AudioDataConverters::convertInt16BEToFloat (const void* source, float* dest, int numSamples, int srcBytesPerSample)
{
    const float scale = 1.0f / 0x7fff;
    auto intData = static_cast<const char*> (source);

    if (source != (void*) dest || srcBytesPerSample >= 4)
    {
        for (int i = 0; i < numSamples; ++i)
        {
            dest[i] = scale * (short) ByteOrder::swapIfLittleEndian (*reinterpret_cast<const uint16*> (intData));
            intData += srcBytesPerSample;
        }
    }
    else
    {
        intData += srcBytesPerSample * numSamples;

        for (int i = numSamples; --i >= 0;)
        {
            intData -= srcBytesPerSample;
            dest[i] = scale * (short) ByteOrder::swapIfLittleEndian (*reinterpret_cast<const uint16*> (intData));
        }
    }
}

void AudioDataConverters::convertInt24LEToFloat (const void* source, float* dest, int numSamples, int srcBytesPerSample)
{
    const float scale = 1.0f / 0x7fffff;
    auto intData = static_cast<const char*> (source);

    if (source != (void*) dest || srcBytesPerSample >= 4)
    {
        for (int i = 0; i < numSamples; ++i)
        {
            dest[i] = scale * (short) ByteOrder::littleEndian24Bit (intData);
            intData += srcBytesPerSample;
        }
    }
    else
    {
        intData += srcBytesPerSample * numSamples;

        for (int i = numSamples; --i >= 0;)
        {
            intData -= srcBytesPerSample;
            dest[i] = scale * (short) ByteOrder::littleEndian24Bit (intData);
        }
    }
}

void AudioDataConverters::convertInt24BEToFloat (const void* source, float* dest, int numSamples, int srcBytesPerSample)
{
    const float scale = 1.0f / 0x7fffff;
    auto intData = static_cast<const char*> (source);

    if (source != (void*) dest || srcBytesPerSample >= 4)
    {
        for (int i = 0; i < numSamples; ++i)
        {
            dest[i] = scale * (short) ByteOrder::bigEndian24Bit (intData);
            intData += srcBytesPerSample;
        }
    }
    else
    {
        intData += srcBytesPerSample * numSamples;

        for (int i = numSamples; --i >= 0;)
        {
            intData -= srcBytesPerSample;
            dest[i] = scale * (short) ByteOrder::bigEndian24Bit (intData);
        }
    }
}

void AudioDataConverters::convertInt32LEToFloat (const void* source, float* dest, int numSamples, int srcBytesPerSample)
{
    const float scale = 1.0f / (float) 0x7fffffff;
    auto intData = static_cast<const char*> (source);

    if (source != (void*) dest || srcBytesPerSample >= 4)
    {
        for (int i = 0; i < numSamples; ++i)
        {
            dest[i] = scale * (float) ByteOrder::swapIfBigEndian (*reinterpret_cast<const uint32*> (intData));
            intData += srcBytesPerSample;
        }
    }
    else
    {
        intData += srcBytesPerSample * numSamples;

        for (int i = numSamples; --i >= 0;)
        {
            intData -= srcBytesPerSample;
            dest[i] = scale * (float) ByteOrder::swapIfBigEndian (*reinterpret_cast<const uint32*> (intData));
        }
    }
}

void AudioDataConverters::convertInt32BEToFloat (const void* source, float* dest, int numSamples, int srcBytesPerSample)
{
    const float scale = 1.0f / (float) 0x7fffffff;
    auto intData = static_cast<const char*> (source);

    if (source != (void*) dest || srcBytesPerSample >= 4)
    {
        for (int i = 0; i < numSamples; ++i)
        {
            dest[i] = scale * (float) ByteOrder::swapIfLittleEndian (*reinterpret_cast<const uint32*> (intData));
            intData += srcBytesPerSample;
        }
    }
    else
    {
        intData += srcBytesPerSample * numSamples;

        for (int i = numSamples; --i >= 0;)
        {
            intData -= srcBytesPerSample;
            dest[i] = scale * (float) ByteOrder::swapIfLittleEndian (*reinterpret_cast<const uint32*> (intData));
        }
    }
}

void AudioDataConverters::convertFloat32LEToFloat (const void* source, float* dest, int numSamples, int srcBytesPerSample)
{
    auto s = static_cast<const char*> (source);

    for (int i = 0; i < numSamples; ++i)
    {
        dest[i] = *reinterpret_cast<const float*> (s);

       #if JUCE_BIG_ENDIAN
        auto d = reinterpret_cast<uint32*> (dest + i);
        *d = ByteOrder::swap (*d);
       #endif

        s += srcBytesPerSample;
    }
}

void AudioDataConverters::convertFloat32BEToFloat (const void* source, float* dest, int numSamples, int srcBytesPerSample)
{
    auto s = static_cast<const char*> (source);

    for (int i = 0; i < numSamples; ++i)
    {
        dest[i] = *reinterpret_cast<const float*> (s);

       #if JUCE_LITTLE_ENDIAN
        auto d = reinterpret_cast<uint32*> (dest + i);
        *d = ByteOrder::swap (*d);
       #endif

        s += srcBytesPerSample;
    }
}


//==============================================================================
void AudioDataConverters::convertFloatToFormat (DataFormat destFormat, const float* source, void* dest, int numSamples)
{
    switch (destFormat)
    {
        case int16LE:       convertFloatToInt16LE   (source, dest, numSamples); break;
        case int16BE:       convertFloatToInt16BE   (source, dest, numSamples); break;
        case int24LE:       convertFloatToInt24LE   (source, dest, numSamples); break;
        case int24BE:       convertFloatToInt24BE   (source, dest, numSamples); break;
        case int32LE:       convertFloatToInt32LE   (source, dest, numSamples); break;
        case int32BE:       convertFloatToInt32BE   (source, dest, numSamples); break;
        case float32LE:     convertFloatToFloat32LE (source, dest, numSamples); break;
        case float32BE:     convertFloatToFloat32BE (source, dest, numSamples); break;
        default:            jassertfalse; break;
    }
}

void AudioDataConverters::convertFormatToFloat (DataFormat sourceFormat, const void* source, float* dest, int numSamples)
{
    switch (sourceFormat)
    {
        case int16LE:       convertInt16LEToFloat   (source, dest, numSamples); break;
        case int16BE:       convertInt16BEToFloat   (source, dest, numSamples); break;
        case int24LE:       convertInt24LEToFloat   (source, dest, numSamples); break;
        case int24BE:       convertInt24BEToFloat   (source, dest, numSamples); break;
        case int32LE:       convertInt32LEToFloat   (source, dest, numSamples); break;
        case int32BE:       convertInt32BEToFloat   (source, dest, numSamples); break;
        case float32LE:     convertFloat32LEToFloat (source, dest, numSamples); break;
        case float32BE:     convertFloat32BEToFloat (source, dest, numSamples); break;
        default:            jassertfalse; break;
    }
}

//==============================================================================
void AudioDataConverters::interleaveSamples (const float** source, float* dest, int numSamples, int numChannels)
{
    for (int chan = 0; chan < numChannels; ++chan)
    {
        auto i = chan;
        auto src = source [chan];

        for (int j = 0; j < numSamples; ++j)
        {
            dest [i] = src [j];
            i += numChannels;
        }
    }
}

void AudioDataConverters::deinterleaveSamples (const float* source, float** dest, int numSamples, int numChannels)
{
    for (int chan = 0; chan < numChannels; ++chan)
    {
        auto i = chan;
        auto dst = dest [chan];

        for (int j = 0; j < numSamples; ++j)
        {
            dst [j] = source [i];
            i += numChannels;
        }
    }
}


//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

class AudioConversionTests  : public UnitTest
{
public:
    AudioConversionTests()
        : UnitTest ("Audio data conversion", UnitTestCategories::audio)
    {}

    template <class F1, class E1, class F2, class E2>
    struct Test5
    {
        static void test (UnitTest& unitTest, Random& r)
        {
            test (unitTest, false, r);
            test (unitTest, true, r);
        }

        static void test (UnitTest& unitTest, bool inPlace, Random& r)
        {
            const int numSamples = 2048;
            int32 original [(size_t) numSamples],
                  converted[(size_t) numSamples],
                  reversed [(size_t) numSamples];

            {
                AudioData::Pointer<F1, E1, AudioData::NonInterleaved, AudioData::NonConst> d (original);
                bool clippingFailed = false;

                for (int i = 0; i < numSamples / 2; ++i)
                {
                    d.setAsFloat (r.nextFloat() * 2.2f - 1.1f);

                    if (! d.isFloatingPoint())
                        clippingFailed = d.getAsFloat() > 1.0f || d.getAsFloat() < -1.0f || clippingFailed;

                    ++d;
                    d.setAsInt32 (r.nextInt());
                    ++d;
                }

                unitTest.expect (! clippingFailed);
            }

            // convert data from the source to dest format..
            std::unique_ptr<AudioData::Converter> conv (new AudioData::ConverterInstance<AudioData::Pointer<F1, E1, AudioData::NonInterleaved, AudioData::Const>,
                                                                                         AudioData::Pointer<F2, E2, AudioData::NonInterleaved, AudioData::NonConst>>());
            conv->convertSamples (inPlace ? reversed : converted, original, numSamples);

            // ..and back again..
            conv.reset (new AudioData::ConverterInstance<AudioData::Pointer<F2, E2, AudioData::NonInterleaved, AudioData::Const>,
                                                         AudioData::Pointer<F1, E1, AudioData::NonInterleaved, AudioData::NonConst>>());
            if (! inPlace)
                zeromem (reversed, sizeof (reversed));

            conv->convertSamples (reversed, inPlace ? reversed : converted, numSamples);

            {
                int biggestDiff = 0;
                AudioData::Pointer<F1, E1, AudioData::NonInterleaved, AudioData::Const> d1 (original);
                AudioData::Pointer<F1, E1, AudioData::NonInterleaved, AudioData::Const> d2 (reversed);

                const int errorMargin = 2 * AudioData::Pointer<F1, E1, AudioData::NonInterleaved, AudioData::Const>::get32BitResolution()
                                          + AudioData::Pointer<F2, E2, AudioData::NonInterleaved, AudioData::Const>::get32BitResolution();

                for (int i = 0; i < numSamples; ++i)
                {
                    biggestDiff = jmax (biggestDiff, std::abs (d1.getAsInt32() - d2.getAsInt32()));
                    ++d1;
                    ++d2;
                }

                unitTest.expect (biggestDiff <= errorMargin);
            }
        }
    };

    template <class F1, class E1, class FormatType>
    struct Test3
    {
        static void test (UnitTest& unitTest, Random& r)
        {
            Test5 <F1, E1, FormatType, AudioData::BigEndian>::test (unitTest, r);
            Test5 <F1, E1, FormatType, AudioData::LittleEndian>::test (unitTest, r);
        }
    };

    template <class FormatType, class Endianness>
    struct Test2
    {
        static void test (UnitTest& unitTest, Random& r)
        {
            Test3 <FormatType, Endianness, AudioData::Int8>::test (unitTest, r);
            Test3 <FormatType, Endianness, AudioData::UInt8>::test (unitTest, r);
            Test3 <FormatType, Endianness, AudioData::Int16>::test (unitTest, r);
            Test3 <FormatType, Endianness, AudioData::Int24>::test (unitTest, r);
            Test3 <FormatType, Endianness, AudioData::Int32>::test (unitTest, r);
            Test3 <FormatType, Endianness, AudioData::Float32>::test (unitTest, r);
        }
    };

    template <class FormatType>
    struct Test1
    {
        static void test (UnitTest& unitTest, Random& r)
        {
            Test2 <FormatType, AudioData::BigEndian>::test (unitTest, r);
            Test2 <FormatType, AudioData::LittleEndian>::test (unitTest, r);
        }
    };

    void runTest() override
    {
        auto r = getRandom();
        beginTest ("Round-trip conversion: Int8");
        Test1 <AudioData::Int8>::test (*this, r);
        beginTest ("Round-trip conversion: Int16");
        Test1 <AudioData::Int16>::test (*this, r);
        beginTest ("Round-trip conversion: Int24");
        Test1 <AudioData::Int24>::test (*this, r);
        beginTest ("Round-trip conversion: Int32");
        Test1 <AudioData::Int32>::test (*this, r);
        beginTest ("Round-trip conversion: Float32");
        Test1 <AudioData::Float32>::test (*this, r);
    }
};

static AudioConversionTests audioConversionUnitTests;

#endif

} // namespace juce
