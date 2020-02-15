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

#undef WINDOWS

/* The ASIO SDK *should* declare its callback functions as being __cdecl, but different versions seem
   to be pretty random about whether or not they do this. If you hit an error using these functions
   it'll be because you're trying to build using __stdcall, in which case you'd need to either get hold of
   an ASIO SDK which correctly specifies __cdecl, or add the __cdecl keyword to its functions yourself.
*/
#define JUCE_ASIOCALLBACK __cdecl

//==============================================================================
namespace ASIODebugging
{
   #if JUCE_ASIO_DEBUGGING
    #define JUCE_ASIO_LOG(msg)               ASIODebugging::logMessage (msg)
    #define JUCE_ASIO_LOG_ERROR(msg, errNum) ASIODebugging::logError ((msg), (errNum))

    static void logMessage (String message)
    {
        message = "ASIO: " + message;
        DBG (message);
        Logger::writeToLog (message);
    }

    static void logError (const String& context, long error)
    {
        const char* err = "Unknown error";

        switch (error)
        {
            case ASE_OK:               return;
            case ASE_NotPresent:       err = "Not Present"; break;
            case ASE_HWMalfunction:    err = "Hardware Malfunction"; break;
            case ASE_InvalidParameter: err = "Invalid Parameter"; break;
            case ASE_InvalidMode:      err = "Invalid Mode"; break;
            case ASE_SPNotAdvancing:   err = "Sample position not advancing"; break;
            case ASE_NoClock:          err = "No Clock"; break;
            case ASE_NoMemory:         err = "Out of memory"; break;
            default:                   break;
        }

        logMessage ("error: " + context + " - " + err);
    }
   #else
    static void dummyLog() {}
    #define JUCE_ASIO_LOG(msg)               ASIODebugging::dummyLog()
    #define JUCE_ASIO_LOG_ERROR(msg, errNum) ignoreUnused (errNum); ASIODebugging::dummyLog()
   #endif
}

//==============================================================================
struct ASIOSampleFormat
{
    ASIOSampleFormat() noexcept {}

    ASIOSampleFormat (const long type) noexcept
        : bitDepth (24),
          littleEndian (true),
          formatIsFloat (false),
          byteStride (4)
    {
        switch (type)
        {
            case ASIOSTInt16MSB:    byteStride = 2; littleEndian = false; bitDepth = 16; break;
            case ASIOSTInt24MSB:    byteStride = 3; littleEndian = false; break;
            case ASIOSTInt32MSB:    bitDepth = 32; littleEndian = false; break;
            case ASIOSTFloat32MSB:  bitDepth = 32; littleEndian = false; formatIsFloat = true; break;
            case ASIOSTFloat64MSB:  bitDepth = 64; byteStride = 8; littleEndian = false; break;
            case ASIOSTInt32MSB16:  bitDepth = 16; littleEndian = false; break;
            case ASIOSTInt32MSB18:  littleEndian = false; break;
            case ASIOSTInt32MSB20:  littleEndian = false; break;
            case ASIOSTInt32MSB24:  littleEndian = false; break;
            case ASIOSTInt16LSB:    byteStride = 2; bitDepth = 16; break;
            case ASIOSTInt24LSB:    byteStride = 3; break;
            case ASIOSTInt32LSB:    bitDepth = 32; break;
            case ASIOSTFloat32LSB:  bitDepth = 32; formatIsFloat = true; break;
            case ASIOSTFloat64LSB:  bitDepth = 64; byteStride = 8; break;
            case ASIOSTInt32LSB16:  bitDepth = 16; break;
            case ASIOSTInt32LSB18:  break; // (unhandled)
            case ASIOSTInt32LSB20:  break; // (unhandled)
            case ASIOSTInt32LSB24:  break;

            case ASIOSTDSDInt8LSB1: break; // (unhandled)
            case ASIOSTDSDInt8MSB1: break; // (unhandled)
            case ASIOSTDSDInt8NER8: break; // (unhandled)

            default:
                jassertfalse;  // (not a valid format code..)
                break;
        }
    }

    void convertToFloat (const void* const src, float* const dst, const int samps) const noexcept
    {
        if (formatIsFloat)
        {
            memcpy (dst, src, samps * sizeof (float));
        }
        else
        {
            switch (bitDepth)
            {
                case 16: convertInt16ToFloat (static_cast<const char*> (src), dst, byteStride, samps, littleEndian); break;
                case 24: convertInt24ToFloat (static_cast<const char*> (src), dst, byteStride, samps, littleEndian); break;
                case 32: convertInt32ToFloat (static_cast<const char*> (src), dst, byteStride, samps, littleEndian); break;
                default: jassertfalse; break;
            }
        }
    }

    void convertFromFloat (const float* const src, void* const dst, const int samps) const noexcept
    {
        if (formatIsFloat)
        {
            memcpy (dst, src, samps * sizeof (float));
        }
        else
        {
            switch (bitDepth)
            {
                case 16: convertFloatToInt16 (src, static_cast<char*> (dst), byteStride, samps, littleEndian); break;
                case 24: convertFloatToInt24 (src, static_cast<char*> (dst), byteStride, samps, littleEndian); break;
                case 32: convertFloatToInt32 (src, static_cast<char*> (dst), byteStride, samps, littleEndian); break;
                default: jassertfalse; break;
            }
        }
    }

    void clear (void* dst, const int numSamps) noexcept
    {
        if (dst != nullptr)
            zeromem (dst, numSamps * byteStride);
    }

    int bitDepth, byteStride;
    bool formatIsFloat, littleEndian;

private:
    static void convertInt16ToFloat (const char* src, float* dest, const int srcStrideBytes,
                                     int numSamples, const bool littleEndian) noexcept
    {
        const double g = 1.0 / 32768.0;

        if (littleEndian)
        {
            while (--numSamples >= 0)
            {
                *dest++ = (float) (g * (short) ByteOrder::littleEndianShort (src));
                src += srcStrideBytes;
            }
        }
        else
        {
            while (--numSamples >= 0)
            {
                *dest++ = (float) (g * (short) ByteOrder::bigEndianShort (src));
                src += srcStrideBytes;
            }
        }
    }

    static void convertFloatToInt16 (const float* src, char* dest, const int dstStrideBytes,
                                     int numSamples, const bool littleEndian) noexcept
    {
        const double maxVal = (double) 0x7fff;

        if (littleEndian)
        {
            while (--numSamples >= 0)
            {
                *(uint16*) dest = ByteOrder::swapIfBigEndian ((uint16) (short) roundToInt (jlimit (-maxVal, maxVal, maxVal * *src++)));
                dest += dstStrideBytes;
            }
        }
        else
        {
            while (--numSamples >= 0)
            {
                *(uint16*) dest = ByteOrder::swapIfLittleEndian ((uint16) (short) roundToInt (jlimit (-maxVal, maxVal, maxVal * *src++)));
                dest += dstStrideBytes;
            }
        }
    }

    static void convertInt24ToFloat (const char* src, float* dest, const int srcStrideBytes,
                                     int numSamples, const bool littleEndian) noexcept
    {
        const double g = 1.0 / 0x7fffff;

        if (littleEndian)
        {
            while (--numSamples >= 0)
            {
                *dest++ = (float) (g * ByteOrder::littleEndian24Bit (src));
                src += srcStrideBytes;
            }
        }
        else
        {
            while (--numSamples >= 0)
            {
                *dest++ = (float) (g * ByteOrder::bigEndian24Bit (src));
                src += srcStrideBytes;
            }
        }
    }

    static void convertFloatToInt24 (const float* src, char* dest, const int dstStrideBytes,
                                     int numSamples, const bool littleEndian) noexcept
    {
        const double maxVal = (double) 0x7fffff;

        if (littleEndian)
        {
            while (--numSamples >= 0)
            {
                ByteOrder::littleEndian24BitToChars ((uint32) roundToInt (jlimit (-maxVal, maxVal, maxVal * *src++)), dest);
                dest += dstStrideBytes;
            }
        }
        else
        {
            while (--numSamples >= 0)
            {
                ByteOrder::bigEndian24BitToChars ((uint32) roundToInt (jlimit (-maxVal, maxVal, maxVal * *src++)), dest);
                dest += dstStrideBytes;
            }
        }
    }

    static void convertInt32ToFloat (const char* src, float* dest, const int srcStrideBytes,
                                     int numSamples, const bool littleEndian) noexcept
    {
        const double g = 1.0 / 0x7fffffff;

        if (littleEndian)
        {
            while (--numSamples >= 0)
            {
                *dest++ = (float) (g * (int) ByteOrder::littleEndianInt (src));
                src += srcStrideBytes;
            }
        }
        else
        {
            while (--numSamples >= 0)
            {
                *dest++ = (float) (g * (int) ByteOrder::bigEndianInt (src));
                src += srcStrideBytes;
            }
        }
    }

    static void convertFloatToInt32 (const float* src, char* dest, const int dstStrideBytes,
                                     int numSamples, const bool littleEndian) noexcept
    {
        const double maxVal = (double) 0x7fffffff;

        if (littleEndian)
        {
            while (--numSamples >= 0)
            {
                *(uint32*) dest = ByteOrder::swapIfBigEndian ((uint32) roundToInt (jlimit (-maxVal, maxVal, maxVal * *src++)));
                dest += dstStrideBytes;
            }
        }
        else
        {
            while (--numSamples >= 0)
            {
                *(uint32*) dest = ByteOrder::swapIfLittleEndian ((uint32) roundToInt (jlimit (-maxVal, maxVal, maxVal * *src++)));
                dest += dstStrideBytes;
            }
        }
    }
};

//==============================================================================
class ASIOAudioIODevice;
static ASIOAudioIODevice* volatile currentASIODev[16] = { 0 };

extern HWND juce_messageWindowHandle;

class ASIOAudioIODeviceType;
static void sendASIODeviceChangeToListeners (ASIOAudioIODeviceType*);

//==============================================================================
class ASIOAudioIODevice  : public AudioIODevice,
                           private Timer
{
public:
    ASIOAudioIODevice (ASIOAudioIODeviceType* ownerType, const String& devName,
                       const CLSID clsID, const int slotNumber)
       : AudioIODevice (devName, "ASIO"),
         owner (ownerType),
         asioObject (nullptr),
         classId (clsID),
         inputLatency (0),
         outputLatency (0),
         minBufferSize (0), maxBufferSize (0),
         preferredBufferSize (0),
         bufferGranularity (0),
         numClockSources (0),
         currentBlockSizeSamples (0),
         currentBitDepth (16),
         currentSampleRate (0),
         currentCallback (nullptr),
         bufferIndex (0),
         numActiveInputChans (0),
         numActiveOutputChans (0),
         deviceIsOpen (false),
         isStarted (false),
         buffersCreated (false),
         calledback (false),
         littleEndian (false),
         postOutput (true),
         needToReset (false),
         insideControlPanelModalLoop (false),
         shouldUsePreferredSize (false)
    {
        ::CoInitialize (nullptr);

        name = devName;
        inBuffers.calloc (4);
        outBuffers.calloc (4);

        jassert (currentASIODev [slotNumber] == nullptr);
        currentASIODev [slotNumber] = this;

        openDevice();
    }

    ~ASIOAudioIODevice()
    {
        for (int i = 0; i < numElementsInArray (currentASIODev); ++i)
            if (currentASIODev[i] == this)
                currentASIODev[i] = nullptr;

        close();
        JUCE_ASIO_LOG ("closed");
        removeCurrentDriver();
    }

    void updateSampleRates()
    {
        // find a list of sample rates..
        Array<double> newRates;

        if (asioObject != nullptr)
        {
            const int possibleSampleRates[] = { 32000, 44100, 48000, 88200, 96000, 176400, 192000, 352800, 384000 };

            for (int index = 0; index < numElementsInArray (possibleSampleRates); ++index)
                if (asioObject->canSampleRate ((double) possibleSampleRates[index]) == 0)
                    newRates.add ((double) possibleSampleRates[index]);
        }

        if (newRates.isEmpty())
        {
            double cr = getSampleRate();
            JUCE_ASIO_LOG ("No sample rates supported - current rate: " + String ((int) cr));

            if (cr > 0)
                newRates.add ((int) cr);
        }

        if (sampleRates != newRates)
        {
            sampleRates.swapWith (newRates);

           #if JUCE_ASIO_DEBUGGING
            StringArray s;
            for (int i = 0; i < sampleRates.size(); ++i)
                s.add (String (sampleRates.getUnchecked(i)));

            JUCE_ASIO_LOG ("Rates: " + s.joinIntoString (" "));
           #endif
        }
    }

    StringArray getOutputChannelNames() override        { return outputChannelNames; }
    StringArray getInputChannelNames() override         { return inputChannelNames; }

    Array<double> getAvailableSampleRates() override    { return sampleRates; }
    Array<int> getAvailableBufferSizes() override       { return bufferSizes; }
    int getDefaultBufferSize() override                 { return preferredBufferSize; }

    int getXRunCount() const noexcept override          { return xruns; }

    String open (const BigInteger& inputChannels,
                 const BigInteger& outputChannels,
                 double sr, int bufferSizeSamples) override
    {
        if (isOpen())
            close();

        jassert (currentCallback == nullptr);

        if (bufferSizeSamples < 8 || bufferSizeSamples > 16384)
            shouldUsePreferredSize = true;

        if (asioObject == nullptr)
        {
            const String openingError (openDevice());

            if (asioObject == nullptr)
                return openingError;
        }

        isStarted = false;
        bufferIndex = -1;

        long err = asioObject->getChannels (&totalNumInputChans, &totalNumOutputChans);
        jassert (err == ASE_OK);

        bufferSizeSamples = readBufferSizes (bufferSizeSamples);

        double sampleRate = sr;
        currentSampleRate = sampleRate;
        currentBlockSizeSamples = bufferSizeSamples;
        currentChansOut.clear();
        currentChansIn.clear();

        updateSampleRates();

        if (sampleRate == 0 || (sampleRates.size() > 0 && ! sampleRates.contains (sampleRate)))
            sampleRate = sampleRates[0];

        jassert (sampleRate != 0);
        if (sampleRate == 0)
            sampleRate = 44100.0;

        updateClockSources();
        currentSampleRate = getSampleRate();

        error.clear();
        buffersCreated = false;

        setSampleRate (sampleRate);

        // (need to get this again in case a sample rate change affected the channel count)
        err = asioObject->getChannels (&totalNumInputChans, &totalNumOutputChans);
        jassert (err == ASE_OK);

        if (asioObject->future (kAsioCanReportOverload, nullptr) != ASE_OK)
            xruns = -1;

        inBuffers.calloc (totalNumInputChans + 8);
        outBuffers.calloc (totalNumOutputChans + 8);

        if (needToReset)
        {
            JUCE_ASIO_LOG (" Resetting");
            removeCurrentDriver();

            loadDriver();
            String initError = initDriver();

            if (initError.isNotEmpty())
                JUCE_ASIO_LOG ("ASIOInit: " + initError);

            needToReset = false;
        }

        const int totalBuffers = resetBuffers (inputChannels, outputChannels);

        setCallbackFunctions();

        JUCE_ASIO_LOG ("disposing buffers");
        err = asioObject->disposeBuffers();

        JUCE_ASIO_LOG ("creating buffers: " + String (totalBuffers) + ", " + String (currentBlockSizeSamples));
        err = asioObject->createBuffers (bufferInfos, totalBuffers, currentBlockSizeSamples, &callbacks);

        if (err != ASE_OK)
        {
            currentBlockSizeSamples = preferredBufferSize;
            JUCE_ASIO_LOG_ERROR ("create buffers 2", err);

            asioObject->disposeBuffers();
            err = asioObject->createBuffers (bufferInfos, totalBuffers, currentBlockSizeSamples, &callbacks);
        }

        if (err == ASE_OK)
        {
            buffersCreated = true;
            tempBuffer.calloc (totalBuffers * currentBlockSizeSamples + 32);

            int n = 0;
            Array<int> types;
            currentBitDepth = 16;

            for (int i = 0; i < (int) totalNumInputChans; ++i)
            {
                if (inputChannels[i])
                {
                    inBuffers[n] = tempBuffer + (currentBlockSizeSamples * n);

                    ASIOChannelInfo channelInfo = { 0 };
                    channelInfo.channel = i;
                    channelInfo.isInput = 1;
                    asioObject->getChannelInfo (&channelInfo);

                    types.addIfNotAlreadyThere (channelInfo.type);
                    inputFormat[n] = ASIOSampleFormat (channelInfo.type);

                    currentBitDepth = jmax (currentBitDepth, inputFormat[n].bitDepth);
                    ++n;
                }
            }

            jassert (numActiveInputChans == n);
            n = 0;

            for (int i = 0; i < (int) totalNumOutputChans; ++i)
            {
                if (outputChannels[i])
                {
                    outBuffers[n] = tempBuffer + (currentBlockSizeSamples * (numActiveInputChans + n));

                    ASIOChannelInfo channelInfo = { 0 };
                    channelInfo.channel = i;
                    channelInfo.isInput = 0;
                    asioObject->getChannelInfo (&channelInfo);

                    types.addIfNotAlreadyThere (channelInfo.type);
                    outputFormat[n] = ASIOSampleFormat (channelInfo.type);

                    currentBitDepth = jmax (currentBitDepth, outputFormat[n].bitDepth);
                    ++n;
                }
            }

            jassert (numActiveOutputChans == n);

            for (int i = types.size(); --i >= 0;)
                JUCE_ASIO_LOG ("channel format: " + String (types[i]));

            jassert (n <= totalBuffers);

            for (int i = 0; i < numActiveOutputChans; ++i)
            {
                outputFormat[i].clear (bufferInfos [numActiveInputChans + i].buffers[0], currentBlockSizeSamples);
                outputFormat[i].clear (bufferInfos [numActiveInputChans + i].buffers[1], currentBlockSizeSamples);
            }

            readLatencies();
            refreshBufferSizes();
            deviceIsOpen = true;

            JUCE_ASIO_LOG ("starting");
            calledback = false;
            err = asioObject->start();

            if (err != 0)
            {
                deviceIsOpen = false;
                JUCE_ASIO_LOG ("stop on failure");
                Thread::sleep (10);
                asioObject->stop();
                error = "Can't start device";
                Thread::sleep (10);
            }
            else
            {
                int count = 300;
                while (--count > 0 && ! calledback)
                    Thread::sleep (10);

                isStarted = true;

                if (! calledback)
                {
                    error = "Device didn't start correctly";
                    JUCE_ASIO_LOG ("no callbacks - stopping..");
                    asioObject->stop();
                }
            }
        }
        else
        {
            error = "Can't create i/o buffers";
        }

        if (error.isNotEmpty())
        {
            JUCE_ASIO_LOG_ERROR (error, err);
            disposeBuffers();

            Thread::sleep (20);
            isStarted = false;
            deviceIsOpen = false;

            const String errorCopy (error);
            close(); // (this resets the error string)
            error = errorCopy;
        }

        needToReset = false;
        return error;
    }

    void close() override
    {
        error.clear();
        stopTimer();
        stop();

        if (asioObject != nullptr && deviceIsOpen)
        {
            const ScopedLock sl (callbackLock);

            deviceIsOpen = false;
            isStarted = false;
            needToReset = false;

            JUCE_ASIO_LOG ("stopping");

            if (asioObject != nullptr)
            {
                Thread::sleep (20);
                asioObject->stop();
                Thread::sleep (10);
                disposeBuffers();
            }

            Thread::sleep (10);
        }
    }

    bool isOpen() override                       { return deviceIsOpen || insideControlPanelModalLoop; }
    bool isPlaying() override                    { return asioObject != nullptr && currentCallback != nullptr; }

    int getCurrentBufferSizeSamples() override   { return currentBlockSizeSamples; }
    double getCurrentSampleRate() override       { return currentSampleRate; }
    int getCurrentBitDepth() override            { return currentBitDepth; }

    BigInteger getActiveOutputChannels() const override    { return currentChansOut; }
    BigInteger getActiveInputChannels() const override     { return currentChansIn; }

    int getOutputLatencyInSamples() override     { return outputLatency + currentBlockSizeSamples / 4; }
    int getInputLatencyInSamples() override      { return inputLatency + currentBlockSizeSamples / 4; }

    void start (AudioIODeviceCallback* callback) override
    {
        if (callback != nullptr)
        {
            callback->audioDeviceAboutToStart (this);

            const ScopedLock sl (callbackLock);
            currentCallback = callback;
        }
    }

    void stop() override
    {
        AudioIODeviceCallback* const lastCallback = currentCallback;

        {
            const ScopedLock sl (callbackLock);
            currentCallback = nullptr;
        }

        if (lastCallback != nullptr)
            lastCallback->audioDeviceStopped();
    }

    String getLastError()           { return error; }
    bool hasControlPanel() const    { return true; }

    bool showControlPanel()
    {
        JUCE_ASIO_LOG ("showing control panel");

        bool done = false;
        insideControlPanelModalLoop = true;

        const uint32 started = Time::getMillisecondCounter();

        if (asioObject != nullptr)
        {
            asioObject->controlPanel();

            const int spent = (int) Time::getMillisecondCounter() - (int) started;

            JUCE_ASIO_LOG ("spent: " + String (spent));

            if (spent > 300)
            {
                shouldUsePreferredSize = true;
                done = true;
            }
        }

        insideControlPanelModalLoop = false;
        return done;
    }

    void resetRequest() noexcept
    {
        startTimer (500);
    }

    void timerCallback() override
    {
        if (! insideControlPanelModalLoop)
        {
            stopTimer();

            JUCE_ASIO_LOG ("restart request!");

            AudioIODeviceCallback* const oldCallback = currentCallback;

            close();

            needToReset = true;
            open (BigInteger (currentChansIn), BigInteger (currentChansOut),
                  currentSampleRate, currentBlockSizeSamples);

            reloadChannelNames();

            if (oldCallback != nullptr)
                start (oldCallback);

            sendASIODeviceChangeToListeners (owner);
        }
        else
        {
            startTimer (100);
        }
    }

private:
    //==============================================================================
    WeakReference<ASIOAudioIODeviceType> owner;
    IASIO* volatile asioObject;
    ASIOCallbacks callbacks;

    CLSID classId;
    String error;

    long totalNumInputChans, totalNumOutputChans;
    StringArray inputChannelNames, outputChannelNames;

    Array<double> sampleRates;
    Array<int> bufferSizes;
    long inputLatency, outputLatency;
    long minBufferSize, maxBufferSize, preferredBufferSize, bufferGranularity;
    ASIOClockSource clocks[32];
    int numClockSources;

    int volatile currentBlockSizeSamples;
    int volatile currentBitDepth;
    double volatile currentSampleRate;
    BigInteger currentChansOut, currentChansIn;
    AudioIODeviceCallback* volatile currentCallback;
    CriticalSection callbackLock;

    HeapBlock<ASIOBufferInfo> bufferInfos;
    HeapBlock<float*> inBuffers, outBuffers;
    HeapBlock<ASIOSampleFormat> inputFormat, outputFormat;

    WaitableEvent event1;
    HeapBlock<float> tempBuffer;
    int volatile bufferIndex, numActiveInputChans, numActiveOutputChans;

    bool deviceIsOpen, isStarted, buffersCreated;
    bool volatile calledback;
    bool volatile littleEndian, postOutput, needToReset;
    bool volatile insideControlPanelModalLoop;
    bool volatile shouldUsePreferredSize;
    int xruns = 0;

    //==============================================================================
    static String convertASIOString (char* const text, int length)
    {
        if (CharPointer_UTF8::isValidString (text, length))
            return String::fromUTF8 (text, length);

        WCHAR wideVersion [64] = { 0 };
        MultiByteToWideChar (CP_ACP, 0, text, length, wideVersion, numElementsInArray (wideVersion));
        return wideVersion;
    }

    String getChannelName (int index, bool isInput) const
    {
        ASIOChannelInfo channelInfo = { 0 };
        channelInfo.channel = index;
        channelInfo.isInput = isInput ? 1 : 0;
        asioObject->getChannelInfo (&channelInfo);

        return convertASIOString (channelInfo.name, sizeof (channelInfo.name));
    }

    void reloadChannelNames()
    {
        if (asioObject != nullptr
             && asioObject->getChannels (&totalNumInputChans, &totalNumOutputChans) == ASE_OK)
        {
            inputChannelNames.clear();
            outputChannelNames.clear();

            for (int i = 0; i < totalNumInputChans; ++i)
                inputChannelNames.add (getChannelName (i, true));

            for (int i = 0; i < totalNumOutputChans; ++i)
                outputChannelNames.add (getChannelName (i, false));

            outputChannelNames.trim();
            inputChannelNames.trim();
            outputChannelNames.appendNumbersToDuplicates (false, true);
            inputChannelNames.appendNumbersToDuplicates (false, true);
        }
    }

    long refreshBufferSizes()
    {
        return asioObject->getBufferSize (&minBufferSize, &maxBufferSize, &preferredBufferSize, &bufferGranularity);
    }

    int readBufferSizes (int bufferSizeSamples)
    {
        minBufferSize = 0;
        maxBufferSize = 0;
        bufferGranularity = 0;
        long newPreferredSize = 0;

        if (asioObject->getBufferSize (&minBufferSize, &maxBufferSize, &newPreferredSize, &bufferGranularity) == ASE_OK)
        {
            if (preferredBufferSize != 0 && newPreferredSize != 0 && newPreferredSize != preferredBufferSize)
                shouldUsePreferredSize = true;

            if (bufferSizeSamples < minBufferSize || bufferSizeSamples > maxBufferSize)
                shouldUsePreferredSize = true;

            preferredBufferSize = newPreferredSize;
        }

        // unfortunate workaround for certain drivers which crash if you make
        // dynamic changes to the buffer size...
        shouldUsePreferredSize = shouldUsePreferredSize || getName().containsIgnoreCase ("Digidesign");

        if (shouldUsePreferredSize)
        {
            JUCE_ASIO_LOG ("Using preferred size for buffer..");
            long err = refreshBufferSizes();

            if (err == ASE_OK)
            {
                bufferSizeSamples = (int) preferredBufferSize;
            }
            else
            {
                bufferSizeSamples = 1024;
                JUCE_ASIO_LOG_ERROR ("getBufferSize1", err);
            }

            shouldUsePreferredSize = false;
        }

        return bufferSizeSamples;
    }

    int resetBuffers (const BigInteger& inputChannels,
                      const BigInteger& outputChannels)
    {
        numActiveInputChans = 0;
        numActiveOutputChans = 0;

        ASIOBufferInfo* info = bufferInfos;
        for (int i = 0; i < totalNumInputChans; ++i)
        {
            if (inputChannels[i])
            {
                currentChansIn.setBit (i);
                info->isInput = 1;
                info->channelNum = i;
                info->buffers[0] = info->buffers[1] = nullptr;
                ++info;
                ++numActiveInputChans;
            }
        }

        for (int i = 0; i < totalNumOutputChans; ++i)
        {
            if (outputChannels[i])
            {
                currentChansOut.setBit (i);
                info->isInput = 0;
                info->channelNum = i;
                info->buffers[0] = info->buffers[1] = nullptr;
                ++info;
                ++numActiveOutputChans;
            }
        }

        return numActiveInputChans + numActiveOutputChans;
    }

    void addBufferSizes (long minSize, long maxSize, long preferredSize, long granularity)
    {
        // find a list of buffer sizes..
        JUCE_ASIO_LOG (String ((int) minSize) + "->" + String ((int) maxSize) + ", "
                        + String ((int) preferredSize) + ", " + String ((int) granularity));

        if (granularity >= 0)
        {
            granularity = jmax (16, (int) granularity);

            for (int i = jmax ((int) (minSize + 15) & ~15, (int) granularity); i < jmin (6400, (int) maxSize); i += granularity)
                bufferSizes.addIfNotAlreadyThere (granularity * (i / granularity));
        }
        else if (granularity < 0)
        {
            for (int i = 0; i < 18; ++i)
            {
                const int s = (1 << i);

                if (s >= minSize && s <= maxSize)
                    bufferSizes.add (s);
            }
        }

        bufferSizes.addIfNotAlreadyThere (preferredSize);
        bufferSizes.sort();
    }

    double getSampleRate() const
    {
        double cr = 0;
        long err = asioObject->getSampleRate (&cr);
        JUCE_ASIO_LOG_ERROR ("getSampleRate", err);
        return cr;
    }

    void setSampleRate (double newRate)
    {
        if (currentSampleRate != newRate)
        {
            JUCE_ASIO_LOG ("rate change: " + String (currentSampleRate) + " to " + String (newRate));
            long err = asioObject->setSampleRate (newRate);

            if (err == ASE_NoClock && numClockSources > 0)
            {
                JUCE_ASIO_LOG ("trying to set a clock source..");
                Thread::sleep (10);
                err = asioObject->setClockSource (clocks[0].index);
                JUCE_ASIO_LOG_ERROR ("setClockSource2", err);

                Thread::sleep (10);
                err = asioObject->setSampleRate (newRate);
            }

            if (err == 0)
                currentSampleRate = newRate;

            // on fail, ignore the attempt to change rate, and run with the current one..
        }
    }

    void updateClockSources()
    {
        zeromem (clocks, sizeof (clocks));
        long numSources = numElementsInArray (clocks);
        asioObject->getClockSources (clocks, &numSources);
        numClockSources = (int) numSources;

        bool isSourceSet = false;

        // careful not to remove this loop because it does more than just logging!
        for (int i = 0; i < numClockSources; ++i)
        {
            String s ("clock: ");
            s += clocks[i].name;

            if (clocks[i].isCurrentSource)
            {
                isSourceSet = true;
                s << " (cur)";
            }

            JUCE_ASIO_LOG (s);
        }

        if (numClockSources > 1 && ! isSourceSet)
        {
            JUCE_ASIO_LOG ("setting clock source");
            long err = asioObject->setClockSource (clocks[0].index);
            JUCE_ASIO_LOG_ERROR ("setClockSource1", err);
            Thread::sleep (20);
        }
        else
        {
            if (numClockSources == 0)
                JUCE_ASIO_LOG ("no clock sources!");
        }
    }

    void readLatencies()
    {
        inputLatency = outputLatency = 0;

        if (asioObject->getLatencies (&inputLatency, &outputLatency) != 0)
            JUCE_ASIO_LOG ("getLatencies() failed");
        else
            JUCE_ASIO_LOG ("Latencies: in = " + String ((int) inputLatency) + ", out = " + String ((int) outputLatency));
    }

    void createDummyBuffers (long preferredSize)
    {
        numActiveInputChans = 0;
        numActiveOutputChans = 0;

        ASIOBufferInfo* info = bufferInfos;
        int numChans = 0;

        for (int i = 0; i < jmin (2, (int) totalNumInputChans); ++i)
        {
            info->isInput = 1;
            info->channelNum = i;
            info->buffers[0] = info->buffers[1] = nullptr;
            ++info;
            ++numChans;
        }

        const int outputBufferIndex = numChans;

        for (int i = 0; i < jmin (2, (int) totalNumOutputChans); ++i)
        {
            info->isInput = 0;
            info->channelNum = i;
            info->buffers[0] = info->buffers[1] = nullptr;
            ++info;
            ++numChans;
        }

        setCallbackFunctions();

        JUCE_ASIO_LOG ("creating buffers (dummy): " + String (numChans) + ", " + String ((int) preferredSize));

        if (preferredSize > 0)
        {
            long err = asioObject->createBuffers (bufferInfos, numChans, preferredSize, &callbacks);
            JUCE_ASIO_LOG_ERROR ("dummy buffers", err);
        }

        long newInps = 0, newOuts = 0;
        asioObject->getChannels (&newInps, &newOuts);

        if (totalNumInputChans != newInps || totalNumOutputChans != newOuts)
        {
            totalNumInputChans = newInps;
            totalNumOutputChans = newOuts;

            JUCE_ASIO_LOG (String ((int) totalNumInputChans) + " in; " + String ((int) totalNumOutputChans) + " out");
        }

        updateSampleRates();
        reloadChannelNames();

        for (int i = 0; i < totalNumOutputChans; ++i)
        {
            ASIOChannelInfo channelInfo = { 0 };
            channelInfo.channel = i;
            channelInfo.isInput = 0;
            asioObject->getChannelInfo (&channelInfo);

            outputFormat[i] = ASIOSampleFormat (channelInfo.type);

            if (i < 2)
            {
                // clear the channels that are used with the dummy stuff
                outputFormat[i].clear (bufferInfos [outputBufferIndex + i].buffers[0], preferredBufferSize);
                outputFormat[i].clear (bufferInfos [outputBufferIndex + i].buffers[1], preferredBufferSize);
            }
        }
    }

    void removeCurrentDriver()
    {
        if (asioObject != nullptr)
        {
            asioObject->Release();
            asioObject = nullptr;
        }
    }

    bool loadDriver()
    {
        removeCurrentDriver();

        bool crashed = false;
        bool ok = tryCreatingDriver (crashed);

        if (crashed)
            JUCE_ASIO_LOG ("** Driver crashed while being opened");

        return ok;
    }

    bool tryCreatingDriver (bool& crashed)
    {
       #if ! JUCE_MINGW
        __try
       #endif
        {
            return CoCreateInstance (classId, 0, CLSCTX_INPROC_SERVER,
                                     classId, (void**) &asioObject) == S_OK;
        }
       #if ! JUCE_MINGW
        __except (EXCEPTION_EXECUTE_HANDLER) { crashed = true; }
        return false;
       #endif
    }

    String getLastDriverError() const
    {
        jassert (asioObject != nullptr);
        char buffer [512] = { 0 };
        asioObject->getErrorMessage (buffer);
        return String (buffer, sizeof (buffer) - 1);
    }

    String initDriver()
    {
        if (asioObject == nullptr)
            return "No Driver";

        const bool initOk = !! asioObject->init (juce_messageWindowHandle);
        String driverError;

        // Get error message if init() failed, or if it's a buggy Denon driver,
        // which returns true from init() even when it fails.
        if ((! initOk) || getName().containsIgnoreCase ("denon dj"))
            driverError = getLastDriverError();

        if ((! initOk) && driverError.isEmpty())
            driverError = "Driver failed to initialise";

        if (driverError.isEmpty())
        {
            char buffer [512];
            asioObject->getDriverName (buffer); // just in case any flimsy drivers expect this to be called..
        }

        return driverError;
    }

    String openDevice()
    {
        // open the device and get its info..
        JUCE_ASIO_LOG ("opening device: " + getName());

        needToReset = false;
        outputChannelNames.clear();
        inputChannelNames.clear();
        bufferSizes.clear();
        sampleRates.clear();
        deviceIsOpen = false;
        totalNumInputChans = 0;
        totalNumOutputChans = 0;
        numActiveInputChans = 0;
        numActiveOutputChans = 0;
        xruns = 0;
        currentCallback = nullptr;

        error.clear();

        if (getName().isEmpty())
            return error;

        long err = 0;

        if (loadDriver())
        {
            if ((error = initDriver()).isEmpty())
            {
                numActiveInputChans = 0;
                numActiveOutputChans = 0;
                totalNumInputChans = 0;
                totalNumOutputChans = 0;

                if (asioObject != nullptr
                     && (err = asioObject->getChannels (&totalNumInputChans, &totalNumOutputChans)) == 0)
                {
                    JUCE_ASIO_LOG (String ((int) totalNumInputChans) + " in, " + String ((int) totalNumOutputChans) + " out");

                    const int chansToAllocate = totalNumInputChans + totalNumOutputChans + 4;
                    bufferInfos.calloc (chansToAllocate);
                    inBuffers.calloc (chansToAllocate);
                    outBuffers.calloc (chansToAllocate);
                    inputFormat.calloc (chansToAllocate);
                    outputFormat.calloc (chansToAllocate);

                    if ((err = refreshBufferSizes()) == 0)
                    {
                        addBufferSizes (minBufferSize, maxBufferSize, preferredBufferSize, bufferGranularity);

                        double currentRate = getSampleRate();

                        if (currentRate < 1.0 || currentRate > 192001.0)
                        {
                            JUCE_ASIO_LOG ("setting default sample rate");
                            err = asioObject->setSampleRate (44100.0);
                            JUCE_ASIO_LOG_ERROR ("setting sample rate", err);

                            currentRate = getSampleRate();
                        }

                        currentSampleRate = currentRate;

                        postOutput = (asioObject->outputReady() == 0);
                        if (postOutput)
                            JUCE_ASIO_LOG ("outputReady true");

                        updateSampleRates();

                        readLatencies();                          // ..doing these steps because cubase does so at this stage
                        createDummyBuffers (preferredBufferSize); // in initialisation, and some devices fail if we don't.
                        readLatencies();

                        // start and stop because cubase does it..
                        err = asioObject->start();
                        // ignore an error here, as it might start later after setting other stuff up
                        JUCE_ASIO_LOG_ERROR ("start", err);

                        Thread::sleep (80);
                        asioObject->stop();
                    }
                    else
                    {
                        error = "Can't detect buffer sizes";
                    }
                }
                else
                {
                    error = "Can't detect asio channels";
                }
            }
        }
        else
        {
            error = "No such device";
        }

        if (error.isNotEmpty())
        {
            JUCE_ASIO_LOG_ERROR (error, err);
            disposeBuffers();
            removeCurrentDriver();
        }
        else
        {
            JUCE_ASIO_LOG ("device open");
        }

        deviceIsOpen = false;
        needToReset = false;
        stopTimer();
        return error;
    }

    void disposeBuffers()
    {
        if (asioObject != nullptr && buffersCreated)
        {
            buffersCreated = false;
            asioObject->disposeBuffers();
        }
    }

    //==============================================================================
    void JUCE_ASIOCALLBACK callback (const long index)
    {
        if (isStarted)
        {
            bufferIndex = index;
            processBuffer();
        }
        else
        {
            if (postOutput && (asioObject != nullptr))
                asioObject->outputReady();
        }

        calledback = true;
    }

    void processBuffer()
    {
        const ASIOBufferInfo* const infos = bufferInfos;
        const int bi = bufferIndex;

        const ScopedLock sl (callbackLock);

        if (bi >= 0)
        {
            const int samps = currentBlockSizeSamples;

            if (currentCallback != nullptr)
            {
                for (int i = 0; i < numActiveInputChans; ++i)
                {
                    jassert (inBuffers[i] != nullptr);
                    inputFormat[i].convertToFloat (infos[i].buffers[bi], inBuffers[i], samps);
                }

                currentCallback->audioDeviceIOCallback (const_cast<const float**> (inBuffers.getData()), numActiveInputChans,
                                                        outBuffers, numActiveOutputChans, samps);

                for (int i = 0; i < numActiveOutputChans; ++i)
                {
                    jassert (outBuffers[i] != nullptr);
                    outputFormat[i].convertFromFloat (outBuffers[i], infos [numActiveInputChans + i].buffers[bi], samps);
                }
            }
            else
            {
                for (int i = 0; i < numActiveOutputChans; ++i)
                     outputFormat[i].clear (infos[numActiveInputChans + i].buffers[bi], samps);
            }
        }

        if (postOutput)
            asioObject->outputReady();
    }

    long asioMessagesCallback (long selector, long value)
    {
        switch (selector)
        {
            case kAsioSelectorSupported:
                if (value == kAsioResetRequest || value == kAsioEngineVersion || value == kAsioResyncRequest
                     || value == kAsioLatenciesChanged || value == kAsioSupportsInputMonitor || value == kAsioOverload)
                    return 1;
                break;

            case kAsioBufferSizeChange: JUCE_ASIO_LOG ("kAsioBufferSizeChange"); resetRequest(); return 1;
            case kAsioResetRequest:     JUCE_ASIO_LOG ("kAsioResetRequest");     resetRequest(); return 1;
            case kAsioResyncRequest:    JUCE_ASIO_LOG ("kAsioResyncRequest");    resetRequest(); return 1;
            case kAsioLatenciesChanged: JUCE_ASIO_LOG ("kAsioLatenciesChanged"); return 1;
            case kAsioEngineVersion:    return 2;

            case kAsioSupportsTimeInfo:
            case kAsioSupportsTimeCode:  return 0;
            case kAsioOverload: xruns++; return 1;
        }

        return 0;
    }

    //==============================================================================
    template <int deviceIndex>
    struct ASIOCallbackFunctions
    {
        static ASIOTime* JUCE_ASIOCALLBACK bufferSwitchTimeInfoCallback (ASIOTime*, long index, long)
        {
            if (currentASIODev[deviceIndex] != nullptr)
                currentASIODev[deviceIndex]->callback (index);

            return nullptr;
        }

        static void JUCE_ASIOCALLBACK bufferSwitchCallback (long index, long)
        {
            if (currentASIODev[deviceIndex] != nullptr)
                currentASIODev[deviceIndex]->callback (index);
        }

        static long JUCE_ASIOCALLBACK asioMessagesCallback (long selector, long value, void*, double*)
        {
            return currentASIODev[deviceIndex] != nullptr
                     ? currentASIODev[deviceIndex]->asioMessagesCallback (selector, value)
                     : 0;
        }

        static void JUCE_ASIOCALLBACK sampleRateChangedCallback (ASIOSampleRate)
        {
            if (currentASIODev[deviceIndex] != nullptr)
                currentASIODev[deviceIndex]->resetRequest();
        }

        static void setCallbacks (ASIOCallbacks& callbacks) noexcept
        {
            callbacks.bufferSwitch          = &bufferSwitchCallback;
            callbacks.asioMessage           = &asioMessagesCallback;
            callbacks.bufferSwitchTimeInfo  = &bufferSwitchTimeInfoCallback;
            callbacks.sampleRateDidChange   = &sampleRateChangedCallback;
        }

        static void setCallbacksForDevice (ASIOCallbacks& callbacks, ASIOAudioIODevice* device) noexcept
        {
            if (currentASIODev[deviceIndex] == device)
                setCallbacks (callbacks);
            else
                ASIOCallbackFunctions<deviceIndex + 1>::setCallbacksForDevice (callbacks, device);
        }
    };

    void setCallbackFunctions() noexcept
    {
        ASIOCallbackFunctions<0>::setCallbacksForDevice (callbacks, this);
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ASIOAudioIODevice)
};

template <>
struct ASIOAudioIODevice::ASIOCallbackFunctions <sizeof(currentASIODev) / sizeof(currentASIODev[0])>
{
    static void setCallbacksForDevice (ASIOCallbacks&, ASIOAudioIODevice*) noexcept {}
};

//==============================================================================
class ASIOAudioIODeviceType  : public AudioIODeviceType
{
public:
    ASIOAudioIODeviceType() : AudioIODeviceType ("ASIO") {}

    //==============================================================================
    void scanForDevices()
    {
        hasScanned = true;

        deviceNames.clear();
        classIds.clear();

        HKEY hk = 0;
        int index = 0;

        if (RegOpenKey (HKEY_LOCAL_MACHINE, _T("software\\asio"), &hk) == ERROR_SUCCESS)
        {
            TCHAR name [256];

            while (RegEnumKey (hk, index++, name, numElementsInArray (name)) == ERROR_SUCCESS)
                addDriverInfo (name, hk);

            RegCloseKey (hk);
        }
    }

    StringArray getDeviceNames (bool /*wantInputNames*/) const
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        return deviceNames;
    }

    int getDefaultDeviceIndex (bool) const
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        for (int i = deviceNames.size(); --i >= 0;)
            if (deviceNames[i].containsIgnoreCase ("asio4all"))
                return i; // asio4all is a safe choice for a default..

       #if JUCE_DEBUG
        if (deviceNames.size() > 1 && deviceNames[0].containsIgnoreCase ("digidesign"))
            return 1; // (the digi m-box driver crashes the app when you run
                      // it in the debugger, which can be a bit annoying)
       #endif

        return 0;
    }

    static int findFreeSlot()
    {
        for (int i = 0; i < numElementsInArray (currentASIODev); ++i)
            if (currentASIODev[i] == 0)
                return i;

        jassertfalse;  // unfortunately you can only have a finite number
                       // of ASIO devices open at the same time..
        return -1;
    }

    int getIndexOfDevice (AudioIODevice* d, bool /*asInput*/) const
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        return d == nullptr ? -1 : deviceNames.indexOf (d->getName());
    }

    bool hasSeparateInputsAndOutputs() const    { return false; }

    AudioIODevice* createDevice (const String& outputDeviceName,
                                 const String& inputDeviceName)
    {
        // ASIO can't open two different devices for input and output - they must be the same one.
        jassert (inputDeviceName == outputDeviceName || outputDeviceName.isEmpty() || inputDeviceName.isEmpty());
        jassert (hasScanned); // need to call scanForDevices() before doing this

        const String deviceName (outputDeviceName.isNotEmpty() ? outputDeviceName
                                                               : inputDeviceName);
        const int index = deviceNames.indexOf (deviceName);

        if (index >= 0)
        {
            const int freeSlot = findFreeSlot();

            if (freeSlot >= 0)
                return new ASIOAudioIODevice (this, deviceName,
                                              classIds.getReference (index), freeSlot);
        }

        return nullptr;
    }

    void sendDeviceChangeToListeners()
    {
        callDeviceChangeListeners();
    }

    JUCE_DECLARE_WEAK_REFERENCEABLE (ASIOAudioIODeviceType)

private:
    StringArray deviceNames;
    Array<CLSID> classIds;

    bool hasScanned = false;

    //==============================================================================
    static bool checkClassIsOk (const String& classId)
    {
        HKEY hk = 0;
        bool ok = false;

        if (RegOpenKey (HKEY_CLASSES_ROOT, _T("clsid"), &hk) == ERROR_SUCCESS)
        {
            int index = 0;
            TCHAR name [512];

            while (RegEnumKey (hk, index++, name, numElementsInArray (name)) == ERROR_SUCCESS)
            {
                if (classId.equalsIgnoreCase (name))
                {
                    HKEY subKey, pathKey;

                    if (RegOpenKeyEx (hk, name, 0, KEY_READ, &subKey) == ERROR_SUCCESS)
                    {
                        if (RegOpenKeyEx (subKey, _T("InprocServer32"), 0, KEY_READ, &pathKey) == ERROR_SUCCESS)
                        {
                            TCHAR pathName [1024] = { 0 };
                            DWORD dtype = REG_SZ;
                            DWORD dsize = sizeof (pathName);

                            if (RegQueryValueEx (pathKey, 0, 0, &dtype, (LPBYTE) pathName, &dsize) == ERROR_SUCCESS)
                                // In older code, this used to check for the existence of the file, but there are situations
                                // where our process doesn't have access to it, but where the driver still loads ok..
                                ok = (pathName[0] != 0);

                            RegCloseKey (pathKey);
                        }

                        RegCloseKey (subKey);
                    }

                    break;
                }
            }

            RegCloseKey (hk);
        }

        return ok;
    }

    //==============================================================================
    void addDriverInfo (const String& keyName, HKEY hk)
    {
        HKEY subKey;

        if (RegOpenKeyEx (hk, keyName.toWideCharPointer(), 0, KEY_READ, &subKey) == ERROR_SUCCESS)
        {
            TCHAR buf [256] = { 0 };
            DWORD dtype = REG_SZ;
            DWORD dsize = sizeof (buf);

            if (RegQueryValueEx (subKey, _T("clsid"), 0, &dtype, (LPBYTE) buf, &dsize) == ERROR_SUCCESS)
            {
                if (dsize > 0 && checkClassIsOk (buf))
                {
                    CLSID classId;
                    if (CLSIDFromString ((LPOLESTR) buf, &classId) == S_OK)
                    {
                        dtype = REG_SZ;
                        dsize = sizeof (buf);
                        String deviceName;

                        if (RegQueryValueEx (subKey, _T("description"), 0, &dtype, (LPBYTE) buf, &dsize) == ERROR_SUCCESS)
                            deviceName = buf;
                        else
                            deviceName = keyName;

                        JUCE_ASIO_LOG ("found " + deviceName);
                        deviceNames.add (deviceName);
                        classIds.add (classId);
                    }
                }

                RegCloseKey (subKey);
            }
        }
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ASIOAudioIODeviceType)
};

void sendASIODeviceChangeToListeners (ASIOAudioIODeviceType* type)
{
    if (type != nullptr)
        type->sendDeviceChangeToListeners();
}

AudioIODeviceType* AudioIODeviceType::createAudioIODeviceType_ASIO()
{
    return new ASIOAudioIODeviceType();
}

} // namespace juce
