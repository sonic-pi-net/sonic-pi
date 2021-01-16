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

extern "C"
{
    // Declare just the minimum number of interfaces for the DSound objects that we need..
    struct DSBUFFERDESC
    {
        DWORD dwSize;
        DWORD dwFlags;
        DWORD dwBufferBytes;
        DWORD dwReserved;
        LPWAVEFORMATEX lpwfxFormat;
        GUID guid3DAlgorithm;
    };

    struct IDirectSoundBuffer;

    #undef INTERFACE
    #define INTERFACE IDirectSound
    DECLARE_INTERFACE_(IDirectSound, IUnknown)
    {
        STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID*) PURE;
        STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
        STDMETHOD_(ULONG,Release)       (THIS) PURE;
        STDMETHOD(CreateSoundBuffer)    (THIS_ DSBUFFERDESC*, IDirectSoundBuffer**, LPUNKNOWN) PURE;
        STDMETHOD(GetCaps)              (THIS_ void*) PURE;
        STDMETHOD(DuplicateSoundBuffer) (THIS_ IDirectSoundBuffer*, IDirectSoundBuffer**) PURE;
        STDMETHOD(SetCooperativeLevel)  (THIS_ HWND, DWORD) PURE;
        STDMETHOD(Compact)              (THIS) PURE;
        STDMETHOD(GetSpeakerConfig)     (THIS_ LPDWORD) PURE;
        STDMETHOD(SetSpeakerConfig)     (THIS_ DWORD) PURE;
        STDMETHOD(Initialize)           (THIS_ const GUID*) PURE;
    };

    #undef INTERFACE
    #define INTERFACE IDirectSoundBuffer
    DECLARE_INTERFACE_(IDirectSoundBuffer, IUnknown)
    {
        STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID*) PURE;
        STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
        STDMETHOD_(ULONG,Release)       (THIS) PURE;
        STDMETHOD(GetCaps)              (THIS_ void*) PURE;
        STDMETHOD(GetCurrentPosition)   (THIS_ LPDWORD, LPDWORD) PURE;
        STDMETHOD(GetFormat)            (THIS_ LPWAVEFORMATEX, DWORD, LPDWORD) PURE;
        STDMETHOD(GetVolume)            (THIS_ LPLONG) PURE;
        STDMETHOD(GetPan)               (THIS_ LPLONG) PURE;
        STDMETHOD(GetFrequency)         (THIS_ LPDWORD) PURE;
        STDMETHOD(GetStatus)            (THIS_ LPDWORD) PURE;
        STDMETHOD(Initialize)           (THIS_ IDirectSound*, DSBUFFERDESC*) PURE;
        STDMETHOD(Lock)                 (THIS_ DWORD, DWORD, LPVOID*, LPDWORD, LPVOID*, LPDWORD, DWORD) PURE;
        STDMETHOD(Play)                 (THIS_ DWORD, DWORD, DWORD) PURE;
        STDMETHOD(SetCurrentPosition)   (THIS_ DWORD) PURE;
        STDMETHOD(SetFormat)            (THIS_ const WAVEFORMATEX*) PURE;
        STDMETHOD(SetVolume)            (THIS_ LONG) PURE;
        STDMETHOD(SetPan)               (THIS_ LONG) PURE;
        STDMETHOD(SetFrequency)         (THIS_ DWORD) PURE;
        STDMETHOD(Stop)                 (THIS) PURE;
        STDMETHOD(Unlock)               (THIS_ LPVOID, DWORD, LPVOID, DWORD) PURE;
        STDMETHOD(Restore)              (THIS) PURE;
    };

    //==============================================================================
    struct DSCBUFFERDESC
    {
        DWORD dwSize;
        DWORD dwFlags;
        DWORD dwBufferBytes;
        DWORD dwReserved;
        LPWAVEFORMATEX lpwfxFormat;
    };

    struct IDirectSoundCaptureBuffer;

    #undef INTERFACE
    #define INTERFACE IDirectSoundCapture
    DECLARE_INTERFACE_(IDirectSoundCapture, IUnknown)
    {
        STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID*) PURE;
        STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
        STDMETHOD_(ULONG,Release)       (THIS) PURE;
        STDMETHOD(CreateCaptureBuffer)  (THIS_ DSCBUFFERDESC*, IDirectSoundCaptureBuffer**, LPUNKNOWN) PURE;
        STDMETHOD(GetCaps)              (THIS_ void*) PURE;
        STDMETHOD(Initialize)           (THIS_ const GUID*) PURE;
    };

    #undef INTERFACE
    #define INTERFACE IDirectSoundCaptureBuffer
    DECLARE_INTERFACE_(IDirectSoundCaptureBuffer, IUnknown)
    {
        STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID*) PURE;
        STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
        STDMETHOD_(ULONG,Release)       (THIS) PURE;
        STDMETHOD(GetCaps)              (THIS_ void*) PURE;
        STDMETHOD(GetCurrentPosition)   (THIS_ LPDWORD, LPDWORD) PURE;
        STDMETHOD(GetFormat)            (THIS_ LPWAVEFORMATEX, DWORD, LPDWORD) PURE;
        STDMETHOD(GetStatus)            (THIS_ LPDWORD) PURE;
        STDMETHOD(Initialize)           (THIS_ IDirectSoundCapture*, DSCBUFFERDESC*) PURE;
        STDMETHOD(Lock)                 (THIS_ DWORD, DWORD, LPVOID*, LPDWORD, LPVOID*, LPDWORD, DWORD) PURE;
        STDMETHOD(Start)                (THIS_ DWORD) PURE;
        STDMETHOD(Stop)                 (THIS) PURE;
        STDMETHOD(Unlock)               (THIS_ LPVOID, DWORD, LPVOID, DWORD) PURE;
    };

    #undef INTERFACE
}

namespace juce
{

//==============================================================================
namespace DSoundLogging
{
    String getErrorMessage (HRESULT hr)
    {
        const char* result = nullptr;

        switch (hr)
        {
            case MAKE_HRESULT(1, 0x878, 10):    result = "Device already allocated"; break;
            case MAKE_HRESULT(1, 0x878, 30):    result = "Control unavailable"; break;
            case E_INVALIDARG:                  result = "Invalid parameter"; break;
            case MAKE_HRESULT(1, 0x878, 50):    result = "Invalid call"; break;
            case E_FAIL:                        result = "Generic error"; break;
            case MAKE_HRESULT(1, 0x878, 70):    result = "Priority level error"; break;
            case E_OUTOFMEMORY:                 result = "Out of memory"; break;
            case MAKE_HRESULT(1, 0x878, 100):   result = "Bad format"; break;
            case E_NOTIMPL:                     result = "Unsupported function"; break;
            case MAKE_HRESULT(1, 0x878, 120):   result = "No driver"; break;
            case MAKE_HRESULT(1, 0x878, 130):   result = "Already initialised"; break;
            case CLASS_E_NOAGGREGATION:         result = "No aggregation"; break;
            case MAKE_HRESULT(1, 0x878, 150):   result = "Buffer lost"; break;
            case MAKE_HRESULT(1, 0x878, 160):   result = "Another app has priority"; break;
            case MAKE_HRESULT(1, 0x878, 170):   result = "Uninitialised"; break;
            case E_NOINTERFACE:                 result = "No interface"; break;
            case S_OK:                          result = "No error"; break;
            default:                            return "Unknown error: " + String ((int) hr);
        }

        return result;
    }

    //==============================================================================
   #if JUCE_DIRECTSOUND_LOGGING
    static void logMessage (String message)
    {
        message = "DSOUND: " + message;
        DBG (message);
        Logger::writeToLog (message);
    }

    static void logError (HRESULT hr, int lineNum)
    {
        if (FAILED (hr))
        {
            String error ("Error at line ");
            error << lineNum << ": " << getErrorMessage (hr);
            logMessage (error);
        }
    }

    #define JUCE_DS_LOG(a)        DSoundLogging::logMessage(a);
    #define JUCE_DS_LOG_ERROR(a)  DSoundLogging::logError(a, __LINE__);
   #else
    #define JUCE_DS_LOG(a)
    #define JUCE_DS_LOG_ERROR(a)
   #endif
}

//==============================================================================
namespace
{
    #define DSOUND_FUNCTION(functionName, params) \
        typedef HRESULT (WINAPI *type##functionName) params; \
        static type##functionName ds##functionName = nullptr;

    #define DSOUND_FUNCTION_LOAD(functionName) \
        ds##functionName = (type##functionName) GetProcAddress (h, #functionName);  \
        jassert (ds##functionName != nullptr);

    typedef BOOL (CALLBACK *LPDSENUMCALLBACKW) (LPGUID, LPCWSTR, LPCWSTR, LPVOID);
    typedef BOOL (CALLBACK *LPDSENUMCALLBACKA) (LPGUID, LPCSTR, LPCSTR, LPVOID);

    DSOUND_FUNCTION (DirectSoundCreate, (const GUID*, IDirectSound**, LPUNKNOWN))
    DSOUND_FUNCTION (DirectSoundCaptureCreate, (const GUID*, IDirectSoundCapture**, LPUNKNOWN))
    DSOUND_FUNCTION (DirectSoundEnumerateW, (LPDSENUMCALLBACKW, LPVOID))
    DSOUND_FUNCTION (DirectSoundCaptureEnumerateW, (LPDSENUMCALLBACKW, LPVOID))

    void initialiseDSoundFunctions()
    {
        if (dsDirectSoundCreate == nullptr)
        {
            HMODULE h = LoadLibraryA ("dsound.dll");

            DSOUND_FUNCTION_LOAD (DirectSoundCreate)
            DSOUND_FUNCTION_LOAD (DirectSoundCaptureCreate)
            DSOUND_FUNCTION_LOAD (DirectSoundEnumerateW)
            DSOUND_FUNCTION_LOAD (DirectSoundCaptureEnumerateW)
        }
    }

    // the overall size of buffer used is this value x the block size
    enum { blocksPerOverallBuffer = 16 };
}

//==============================================================================
class DSoundInternalOutChannel
{
public:
    DSoundInternalOutChannel (const String& name_, const GUID& guid_, int rate,
                              int bufferSize, float* left, float* right)
        : bitDepth (16), name (name_), guid (guid_), sampleRate (rate),
          bufferSizeSamples (bufferSize), leftBuffer (left), rightBuffer (right),
          pDirectSound (nullptr), pOutputBuffer (nullptr)
    {
    }

    ~DSoundInternalOutChannel()
    {
        close();
    }

    void close()
    {
        if (pOutputBuffer != nullptr)
        {
            JUCE_DS_LOG ("closing output: " + name);
            HRESULT hr = pOutputBuffer->Stop();
            JUCE_DS_LOG_ERROR (hr); ignoreUnused (hr);

            pOutputBuffer->Release();
            pOutputBuffer = nullptr;
        }

        if (pDirectSound != nullptr)
        {
            pDirectSound->Release();
            pDirectSound = nullptr;
        }
    }

    String open()
    {
        JUCE_DS_LOG ("opening output: " + name + "  rate=" + String (sampleRate)
                       + " bits=" + String (bitDepth) + " buf=" + String (bufferSizeSamples));

        pDirectSound = nullptr;
        pOutputBuffer = nullptr;
        writeOffset = 0;
        xruns = 0;

        firstPlayTime = true;
        lastPlayTime = 0;

        String error;
        HRESULT hr = E_NOINTERFACE;

        if (dsDirectSoundCreate != nullptr)
            hr = dsDirectSoundCreate (&guid, &pDirectSound, nullptr);

        if (SUCCEEDED (hr))
        {
            bytesPerBuffer = (bufferSizeSamples * (bitDepth >> 2)) & ~15;
            ticksPerBuffer = bytesPerBuffer * Time::getHighResolutionTicksPerSecond() / (sampleRate * (bitDepth >> 2));
            totalBytesPerBuffer = (blocksPerOverallBuffer * bytesPerBuffer) & ~15;
            const int numChannels = 2;

            hr = pDirectSound->SetCooperativeLevel (GetDesktopWindow(), 2 /* DSSCL_PRIORITY */);
            JUCE_DS_LOG_ERROR (hr);

            if (SUCCEEDED (hr))
            {
                IDirectSoundBuffer* pPrimaryBuffer;

                DSBUFFERDESC primaryDesc = {};
                primaryDesc.dwSize = sizeof (DSBUFFERDESC);
                primaryDesc.dwFlags = 1 /* DSBCAPS_PRIMARYBUFFER */;
                primaryDesc.dwBufferBytes = 0;
                primaryDesc.lpwfxFormat = 0;

                JUCE_DS_LOG ("co-op level set");
                hr = pDirectSound->CreateSoundBuffer (&primaryDesc, &pPrimaryBuffer, 0);
                JUCE_DS_LOG_ERROR (hr);

                if (SUCCEEDED (hr))
                {
                    WAVEFORMATEX wfFormat;
                    wfFormat.wFormatTag       = WAVE_FORMAT_PCM;
                    wfFormat.nChannels        = (unsigned short) numChannels;
                    wfFormat.nSamplesPerSec   = (DWORD) sampleRate;
                    wfFormat.wBitsPerSample   = (unsigned short) bitDepth;
                    wfFormat.nBlockAlign      = (unsigned short) (wfFormat.nChannels * wfFormat.wBitsPerSample / 8);
                    wfFormat.nAvgBytesPerSec  = wfFormat.nSamplesPerSec * wfFormat.nBlockAlign;
                    wfFormat.cbSize = 0;

                    hr = pPrimaryBuffer->SetFormat (&wfFormat);
                    JUCE_DS_LOG_ERROR (hr);

                    if (SUCCEEDED (hr))
                    {
                        DSBUFFERDESC secondaryDesc = {};
                        secondaryDesc.dwSize = sizeof (DSBUFFERDESC);
                        secondaryDesc.dwFlags =  0x8000 /* DSBCAPS_GLOBALFOCUS */
                                                  | 0x10000 /* DSBCAPS_GETCURRENTPOSITION2 */;
                        secondaryDesc.dwBufferBytes = (DWORD) totalBytesPerBuffer;
                        secondaryDesc.lpwfxFormat = &wfFormat;

                        hr = pDirectSound->CreateSoundBuffer (&secondaryDesc, &pOutputBuffer, 0);
                        JUCE_DS_LOG_ERROR (hr);

                        if (SUCCEEDED (hr))
                        {
                            JUCE_DS_LOG ("buffer created");

                            DWORD dwDataLen;
                            unsigned char* pDSBuffData;

                            hr = pOutputBuffer->Lock (0, (DWORD) totalBytesPerBuffer,
                                                      (LPVOID*) &pDSBuffData, &dwDataLen, 0, 0, 0);
                            JUCE_DS_LOG_ERROR (hr);

                            if (SUCCEEDED (hr))
                            {
                                zeromem (pDSBuffData, dwDataLen);

                                hr = pOutputBuffer->Unlock (pDSBuffData, dwDataLen, 0, 0);

                                if (SUCCEEDED (hr))
                                {
                                    hr = pOutputBuffer->SetCurrentPosition (0);

                                    if (SUCCEEDED (hr))
                                    {
                                        hr = pOutputBuffer->Play (0, 0, 1 /* DSBPLAY_LOOPING */);

                                        if (SUCCEEDED (hr))
                                            return {};
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        error = DSoundLogging::getErrorMessage (hr);
        close();
        return error;
    }

    void synchronisePosition()
    {
        if (pOutputBuffer != nullptr)
        {
            DWORD playCursor;
            pOutputBuffer->GetCurrentPosition (&playCursor, &writeOffset);
        }
    }

    bool service()
    {
        if (pOutputBuffer == 0)
            return true;

        DWORD playCursor, writeCursor;

        for (;;)
        {
            HRESULT hr = pOutputBuffer->GetCurrentPosition (&playCursor, &writeCursor);

            if (hr == MAKE_HRESULT (1, 0x878, 150)) // DSERR_BUFFERLOST
            {
                pOutputBuffer->Restore();
                continue;
            }

            if (SUCCEEDED (hr))
                break;

            JUCE_DS_LOG_ERROR (hr);
            jassertfalse;
            return true;
        }

        auto currentPlayTime = Time::getHighResolutionTicks();
        if (! firstPlayTime)
        {
            auto expectedBuffers = (currentPlayTime - lastPlayTime) / ticksPerBuffer;

            playCursor += static_cast<DWORD> (expectedBuffers * bytesPerBuffer);
        }
        else
            firstPlayTime = false;

        lastPlayTime = currentPlayTime;

        int playWriteGap = (int) (writeCursor - playCursor);
        if (playWriteGap < 0)
            playWriteGap += totalBytesPerBuffer;

        int bytesEmpty = (int) (playCursor - writeOffset);
        if (bytesEmpty < 0)
            bytesEmpty += totalBytesPerBuffer;

        if (bytesEmpty > (totalBytesPerBuffer - playWriteGap))
        {
            writeOffset = writeCursor;
            bytesEmpty = totalBytesPerBuffer - playWriteGap;

            // buffer underflow
            xruns++;
        }

        if (bytesEmpty >= bytesPerBuffer)
        {
            int* buf1 = nullptr;
            int* buf2 = nullptr;
            DWORD dwSize1 = 0;
            DWORD dwSize2 = 0;

            HRESULT hr = pOutputBuffer->Lock (writeOffset, (DWORD) bytesPerBuffer,
                                              (void**) &buf1, &dwSize1,
                                              (void**) &buf2, &dwSize2, 0);

            if (hr == MAKE_HRESULT (1, 0x878, 150)) // DSERR_BUFFERLOST
            {
                pOutputBuffer->Restore();

                hr = pOutputBuffer->Lock (writeOffset, (DWORD) bytesPerBuffer,
                                          (void**) &buf1, &dwSize1,
                                          (void**) &buf2, &dwSize2, 0);
            }

            if (SUCCEEDED (hr))
            {
                if (bitDepth == 16)
                {
                    const float* left = leftBuffer;
                    const float* right = rightBuffer;
                    int samples1 = (int) (dwSize1 >> 2);
                    int samples2 = (int) (dwSize2 >> 2);

                    if (left == nullptr)
                    {
                        for (int* dest = buf1; --samples1 >= 0;)  *dest++ = convertInputValues (0, *right++);
                        for (int* dest = buf2; --samples2 >= 0;)  *dest++ = convertInputValues (0, *right++);
                    }
                    else if (right == nullptr)
                    {
                        for (int* dest = buf1; --samples1 >= 0;)  *dest++ = convertInputValues (*left++, 0);
                        for (int* dest = buf2; --samples2 >= 0;)  *dest++ = convertInputValues (*left++, 0);
                    }
                    else
                    {
                        for (int* dest = buf1; --samples1 >= 0;)  *dest++ = convertInputValues (*left++, *right++);
                        for (int* dest = buf2; --samples2 >= 0;)  *dest++ = convertInputValues (*left++, *right++);
                    }
                }
                else
                {
                    jassertfalse;
                }

                writeOffset = (writeOffset + dwSize1 + dwSize2) % totalBytesPerBuffer;

                pOutputBuffer->Unlock (buf1, dwSize1, buf2, dwSize2);
            }
            else
            {
                jassertfalse;
                JUCE_DS_LOG_ERROR (hr);
            }

            bytesEmpty -= bytesPerBuffer;
            return true;
        }
        else
        {
            return false;
        }
    }

    int bitDepth, xruns;
    bool doneFlag;

private:
    String name;
    GUID guid;
    int sampleRate, bufferSizeSamples;
    float* leftBuffer;
    float* rightBuffer;

    IDirectSound* pDirectSound;
    IDirectSoundBuffer* pOutputBuffer;
    DWORD writeOffset;
    int totalBytesPerBuffer, bytesPerBuffer;

    bool firstPlayTime;
    int64 lastPlayTime, ticksPerBuffer;

    static int convertInputValues (const float l, const float r) noexcept
    {
        return jlimit (-32768, 32767, roundToInt (32767.0f * r)) << 16
                | (0xffff & jlimit (-32768, 32767, roundToInt (32767.0f * l)));
    }

    JUCE_DECLARE_NON_COPYABLE (DSoundInternalOutChannel)
};

//==============================================================================
struct DSoundInternalInChannel
{
public:
    DSoundInternalInChannel (const String& name_, const GUID& guid_, int rate,
                             int bufferSize, float* left, float* right)
        : name (name_), guid (guid_), sampleRate (rate),
          bufferSizeSamples (bufferSize), leftBuffer (left), rightBuffer (right)
    {
    }

    ~DSoundInternalInChannel()
    {
        close();
    }

    void close()
    {
        if (pInputBuffer != nullptr)
        {
            JUCE_DS_LOG ("closing input: " + name);
            HRESULT hr = pInputBuffer->Stop();
            JUCE_DS_LOG_ERROR (hr); ignoreUnused (hr);

            pInputBuffer->Release();
            pInputBuffer = nullptr;
        }

        if (pDirectSoundCapture != nullptr)
        {
            pDirectSoundCapture->Release();
            pDirectSoundCapture = nullptr;
        }

        if (pDirectSound != nullptr)
        {
            pDirectSound->Release();
            pDirectSound = nullptr;
        }
    }

    String open()
    {
        JUCE_DS_LOG ("opening input: " + name
                       + "  rate=" + String (sampleRate) + " bits=" + String (bitDepth) + " buf=" + String (bufferSizeSamples));

        pDirectSound = nullptr;
        pDirectSoundCapture = nullptr;
        pInputBuffer = nullptr;
        readOffset = 0;
        totalBytesPerBuffer = 0;

        HRESULT hr = dsDirectSoundCaptureCreate != nullptr
                        ? dsDirectSoundCaptureCreate (&guid, &pDirectSoundCapture, nullptr)
                        : E_NOINTERFACE;

        if (SUCCEEDED (hr))
        {
            const int numChannels = 2;
            bytesPerBuffer = (bufferSizeSamples * (bitDepth >> 2)) & ~15;
            totalBytesPerBuffer = (blocksPerOverallBuffer * bytesPerBuffer) & ~15;

            WAVEFORMATEX wfFormat;
            wfFormat.wFormatTag       = WAVE_FORMAT_PCM;
            wfFormat.nChannels        = (unsigned short)numChannels;
            wfFormat.nSamplesPerSec   = (DWORD) sampleRate;
            wfFormat.wBitsPerSample   = (unsigned short) bitDepth;
            wfFormat.nBlockAlign      = (unsigned short) (wfFormat.nChannels * (wfFormat.wBitsPerSample / 8));
            wfFormat.nAvgBytesPerSec  = wfFormat.nSamplesPerSec * wfFormat.nBlockAlign;
            wfFormat.cbSize = 0;

            DSCBUFFERDESC captureDesc = {};
            captureDesc.dwSize = sizeof (DSCBUFFERDESC);
            captureDesc.dwFlags = 0;
            captureDesc.dwBufferBytes = (DWORD) totalBytesPerBuffer;
            captureDesc.lpwfxFormat = &wfFormat;

            JUCE_DS_LOG ("object created");
            hr = pDirectSoundCapture->CreateCaptureBuffer (&captureDesc, &pInputBuffer, 0);

            if (SUCCEEDED (hr))
            {
                hr = pInputBuffer->Start (1 /* DSCBSTART_LOOPING */);

                if (SUCCEEDED (hr))
                    return {};
            }
        }

        JUCE_DS_LOG_ERROR (hr);
        const String error (DSoundLogging::getErrorMessage (hr));
        close();

        return error;
    }

    void synchronisePosition()
    {
        if (pInputBuffer != nullptr)
        {
            DWORD capturePos;
            pInputBuffer->GetCurrentPosition (&capturePos, (DWORD*) &readOffset);
        }
    }

    bool service()
    {
        if (pInputBuffer == 0)
            return true;

        DWORD capturePos, readPos;
        HRESULT hr = pInputBuffer->GetCurrentPosition (&capturePos, &readPos);
        JUCE_DS_LOG_ERROR (hr);

        if (FAILED (hr))
            return true;

        int bytesFilled = (int) (readPos - readOffset);

        if (bytesFilled < 0)
            bytesFilled += totalBytesPerBuffer;

        if (bytesFilled >= bytesPerBuffer)
        {
            short* buf1 = nullptr;
            short* buf2 = nullptr;
            DWORD dwsize1 = 0;
            DWORD dwsize2 = 0;

            hr = pInputBuffer->Lock ((DWORD) readOffset, (DWORD) bytesPerBuffer,
                                             (void**) &buf1, &dwsize1,
                                             (void**) &buf2, &dwsize2, 0);

            if (SUCCEEDED (hr))
            {
                if (bitDepth == 16)
                {
                    const float g = 1.0f / 32768.0f;

                    float* destL = leftBuffer;
                    float* destR = rightBuffer;
                    int samples1 = (int) (dwsize1 >> 2);
                    int samples2 = (int) (dwsize2 >> 2);

                    if (destL == nullptr)
                    {
                        for (const short* src = buf1; --samples1 >= 0;) { ++src; *destR++ = *src++ * g; }
                        for (const short* src = buf2; --samples2 >= 0;) { ++src; *destR++ = *src++ * g; }
                    }
                    else if (destR == nullptr)
                    {
                        for (const short* src = buf1; --samples1 >= 0;) { *destL++ = *src++ * g; ++src; }
                        for (const short* src = buf2; --samples2 >= 0;) { *destL++ = *src++ * g; ++src; }
                    }
                    else
                    {
                        for (const short* src = buf1; --samples1 >= 0;) { *destL++ = *src++ * g; *destR++ = *src++ * g; }
                        for (const short* src = buf2; --samples2 >= 0;) { *destL++ = *src++ * g; *destR++ = *src++ * g; }
                    }
                }
                else
                {
                    jassertfalse;
                }

                readOffset = (readOffset + dwsize1 + dwsize2) % totalBytesPerBuffer;

                pInputBuffer->Unlock (buf1, dwsize1, buf2, dwsize2);
            }
            else
            {
                JUCE_DS_LOG_ERROR (hr);
                jassertfalse;
            }

            bytesFilled -= bytesPerBuffer;

            return true;
        }
        else
        {
            return false;
        }
    }

    unsigned int readOffset;
    int bytesPerBuffer, totalBytesPerBuffer;
    int bitDepth = 16;
    bool doneFlag;

private:
    String name;
    GUID guid;
    int sampleRate, bufferSizeSamples;
    float* leftBuffer;
    float* rightBuffer;

    IDirectSound* pDirectSound = nullptr;
    IDirectSoundCapture* pDirectSoundCapture = nullptr;
    IDirectSoundCaptureBuffer* pInputBuffer = nullptr;

    JUCE_DECLARE_NON_COPYABLE (DSoundInternalInChannel)
};

//==============================================================================
class DSoundAudioIODevice  : public AudioIODevice,
                             public Thread
{
public:
    DSoundAudioIODevice (const String& deviceName,
                         const int outputDeviceIndex_,
                         const int inputDeviceIndex_)
        : AudioIODevice (deviceName, "DirectSound"),
          Thread ("JUCE DSound"),
          outputDeviceIndex (outputDeviceIndex_),
          inputDeviceIndex (inputDeviceIndex_)
    {
        if (outputDeviceIndex_ >= 0)
        {
            outChannels.add (TRANS("Left"));
            outChannels.add (TRANS("Right"));
        }

        if (inputDeviceIndex_ >= 0)
        {
            inChannels.add (TRANS("Left"));
            inChannels.add (TRANS("Right"));
        }
    }

    ~DSoundAudioIODevice()
    {
        close();
    }

    String open (const BigInteger& inputChannels,
                 const BigInteger& outputChannels,
                 double newSampleRate, int newBufferSize) override
    {
        lastError = openDevice (inputChannels, outputChannels, newSampleRate, newBufferSize);
        isOpen_ = lastError.isEmpty();

        return lastError;
    }

    void close() override
    {
        stop();

        if (isOpen_)
        {
            closeDevice();
            isOpen_ = false;
        }
    }

    bool isOpen() override                              { return isOpen_ && isThreadRunning(); }
    int getCurrentBufferSizeSamples() override          { return bufferSizeSamples; }
    double getCurrentSampleRate() override              { return sampleRate; }
    BigInteger getActiveOutputChannels() const override { return enabledOutputs; }
    BigInteger getActiveInputChannels() const override  { return enabledInputs; }
    int getOutputLatencyInSamples() override            { return (int) (getCurrentBufferSizeSamples() * 1.5); }
    int getInputLatencyInSamples() override             { return getOutputLatencyInSamples(); }
    StringArray getOutputChannelNames() override        { return outChannels; }
    StringArray getInputChannelNames() override         { return inChannels; }

    Array<double> getAvailableSampleRates() override
    {
        return { 44100.0, 48000.0, 88200.0, 96000.0 };
    }

    Array<int> getAvailableBufferSizes() override
    {
        Array<int> r;
        int n = 64;

        for (int i = 0; i < 50; ++i)
        {
            r.add (n);
            n += (n < 512) ? 32
                           : ((n < 1024) ? 64
                                         : ((n < 2048) ? 128 : 256));
        }

        return r;
    }

    int getDefaultBufferSize() override                 { return 2560; }

    int getCurrentBitDepth() override
    {
        int bits = 256;

        for (int i = inChans.size(); --i >= 0;)
            bits = jmin (bits, inChans[i]->bitDepth);

        for (int i = outChans.size(); --i >= 0;)
            bits = jmin (bits, outChans[i]->bitDepth);

        if (bits > 32)
            bits = 16;

        return bits;
    }

    void start (AudioIODeviceCallback* call) override
    {
        if (isOpen_ && call != nullptr && ! isStarted)
        {
            if (! isThreadRunning())
            {
                // something gone wrong and the thread's stopped..
                isOpen_ = false;
                return;
            }

            call->audioDeviceAboutToStart (this);

            const ScopedLock sl (startStopLock);
            callback = call;
            isStarted = true;
        }
    }

    void stop() override
    {
        if (isStarted)
        {
            auto* callbackLocal = callback;

            {
                const ScopedLock sl (startStopLock);
                isStarted = false;
            }

            if (callbackLocal != nullptr)
                callbackLocal->audioDeviceStopped();
        }
    }

    bool isPlaying() override            { return isStarted && isOpen_ && isThreadRunning(); }
    String getLastError() override       { return lastError; }

    int getXRunCount() const noexcept override
    {
        return outChans[0] != nullptr ? outChans[0]->xruns : -1;
    }

    //==============================================================================
    StringArray inChannels, outChannels;
    int outputDeviceIndex, inputDeviceIndex;

private:
    bool isOpen_ = false;
    bool isStarted = false;
    String lastError;

    OwnedArray<DSoundInternalInChannel> inChans;
    OwnedArray<DSoundInternalOutChannel> outChans;
    WaitableEvent startEvent;

    int bufferSizeSamples = 0;
    double sampleRate = 0;
    BigInteger enabledInputs, enabledOutputs;
    AudioBuffer<float> inputBuffers, outputBuffers;

    AudioIODeviceCallback* callback = nullptr;
    CriticalSection startStopLock;

    String openDevice (const BigInteger& inputChannels,
                       const BigInteger& outputChannels,
                       double sampleRate_, int bufferSizeSamples_);

    void closeDevice()
    {
        isStarted = false;
        stopThread (5000);

        inChans.clear();
        outChans.clear();
    }

    void resync()
    {
        if (! threadShouldExit())
        {
            sleep (5);

            for (int i = 0; i < outChans.size(); ++i)
                outChans.getUnchecked(i)->synchronisePosition();

            for (int i = 0; i < inChans.size(); ++i)
                inChans.getUnchecked(i)->synchronisePosition();
        }
    }

public:
    void run() override
    {
        while (! threadShouldExit())
        {
            if (wait (100))
                break;
        }

        const int latencyMs = (int) (bufferSizeSamples * 1000.0 / sampleRate);
        const int maxTimeMS = jmax (5, 3 * latencyMs);

        while (! threadShouldExit())
        {
            int numToDo = 0;
            uint32 startTime = Time::getMillisecondCounter();

            for (int i = inChans.size(); --i >= 0;)
            {
                inChans.getUnchecked(i)->doneFlag = false;
                ++numToDo;
            }

            for (int i = outChans.size(); --i >= 0;)
            {
                outChans.getUnchecked(i)->doneFlag = false;
                ++numToDo;
            }

            if (numToDo > 0)
            {
                const int maxCount = 3;
                int count = maxCount;

                for (;;)
                {
                    for (int i = inChans.size(); --i >= 0;)
                    {
                        DSoundInternalInChannel* const in = inChans.getUnchecked(i);

                        if ((! in->doneFlag) && in->service())
                        {
                            in->doneFlag = true;
                            --numToDo;
                        }
                    }

                    for (int i = outChans.size(); --i >= 0;)
                    {
                        DSoundInternalOutChannel* const out = outChans.getUnchecked(i);

                        if ((! out->doneFlag) && out->service())
                        {
                            out->doneFlag = true;
                            --numToDo;
                        }
                    }

                    if (numToDo <= 0)
                        break;

                    if (Time::getMillisecondCounter() > startTime + maxTimeMS)
                    {
                        resync();
                        break;
                    }

                    if (--count <= 0)
                    {
                        Sleep (1);
                        count = maxCount;
                    }

                    if (threadShouldExit())
                        return;
                }
            }
            else
            {
                sleep (1);
            }

            const ScopedLock sl (startStopLock);

            if (isStarted)
            {
                callback->audioDeviceIOCallback (inputBuffers.getArrayOfReadPointers(), inputBuffers.getNumChannels(),
                                                 outputBuffers.getArrayOfWritePointers(), outputBuffers.getNumChannels(),
                                                 bufferSizeSamples);
            }
            else
            {
                outputBuffers.clear();
                sleep (1);
            }
        }
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (DSoundAudioIODevice)
};

//==============================================================================
struct DSoundDeviceList
{
    StringArray outputDeviceNames, inputDeviceNames;
    Array<GUID> outputGuids, inputGuids;

    void scan()
    {
        outputDeviceNames.clear();
        inputDeviceNames.clear();
        outputGuids.clear();
        inputGuids.clear();

        if (dsDirectSoundEnumerateW != 0)
        {
            dsDirectSoundEnumerateW (outputEnumProcW, this);
            dsDirectSoundCaptureEnumerateW (inputEnumProcW, this);
        }
    }

    bool operator!= (const DSoundDeviceList& other) const noexcept
    {
        return outputDeviceNames != other.outputDeviceNames
            || inputDeviceNames != other.inputDeviceNames
            || outputGuids != other.outputGuids
            || inputGuids != other.inputGuids;
    }

private:
    static BOOL enumProc (LPGUID lpGUID, String desc, StringArray& names, Array<GUID>& guids)
    {
        desc = desc.trim();

        if (desc.isNotEmpty())
        {
            const String origDesc (desc);

            int n = 2;
            while (names.contains (desc))
                desc = origDesc + " (" + String (n++) + ")";

            names.add (desc);
            guids.add (lpGUID != nullptr ? *lpGUID : GUID());
        }

        return TRUE;
    }

    BOOL outputEnumProc (LPGUID guid, LPCWSTR desc)  { return enumProc (guid, desc, outputDeviceNames, outputGuids); }
    BOOL inputEnumProc  (LPGUID guid, LPCWSTR desc)  { return enumProc (guid, desc, inputDeviceNames,  inputGuids); }

    static BOOL CALLBACK outputEnumProcW (LPGUID lpGUID, LPCWSTR description, LPCWSTR, LPVOID object)
    {
        return static_cast<DSoundDeviceList*> (object)->outputEnumProc (lpGUID, description);
    }

    static BOOL CALLBACK inputEnumProcW (LPGUID lpGUID, LPCWSTR description, LPCWSTR, LPVOID object)
    {
        return static_cast<DSoundDeviceList*> (object)->inputEnumProc (lpGUID, description);
    }
};

//==============================================================================
String DSoundAudioIODevice::openDevice (const BigInteger& inputChannels,
                                        const BigInteger& outputChannels,
                                        double sampleRate_, int bufferSizeSamples_)
{
    closeDevice();

    sampleRate = sampleRate_ > 0.0 ? sampleRate_ : 44100.0;

    if (bufferSizeSamples_ <= 0)
        bufferSizeSamples_ = 960; // use as a default size if none is set.

    bufferSizeSamples = bufferSizeSamples_ & ~7;

    DSoundDeviceList dlh;
    dlh.scan();

    enabledInputs = inputChannels;
    enabledInputs.setRange (inChannels.size(),
                            enabledInputs.getHighestBit() + 1 - inChannels.size(),
                            false);

    inputBuffers.setSize (enabledInputs.countNumberOfSetBits(), bufferSizeSamples);
    inputBuffers.clear();
    int numIns = 0;

    for (int i = 0; i <= enabledInputs.getHighestBit(); i += 2)
    {
        float* left  = enabledInputs[i]     ? inputBuffers.getWritePointer (numIns++) : nullptr;
        float* right = enabledInputs[i + 1] ? inputBuffers.getWritePointer (numIns++) : nullptr;

        if (left != nullptr || right != nullptr)
            inChans.add (new DSoundInternalInChannel (dlh.inputDeviceNames [inputDeviceIndex],
                                                      dlh.inputGuids [inputDeviceIndex],
                                                      (int) sampleRate, bufferSizeSamples,
                                                      left, right));
    }

    enabledOutputs = outputChannels;
    enabledOutputs.setRange (outChannels.size(),
                             enabledOutputs.getHighestBit() + 1 - outChannels.size(),
                             false);

    outputBuffers.setSize (enabledOutputs.countNumberOfSetBits(), bufferSizeSamples);
    outputBuffers.clear();
    int numOuts = 0;

    for (int i = 0; i <= enabledOutputs.getHighestBit(); i += 2)
    {
        float* left  = enabledOutputs[i]     ? outputBuffers.getWritePointer (numOuts++) : nullptr;
        float* right = enabledOutputs[i + 1] ? outputBuffers.getWritePointer (numOuts++) : nullptr;

        if (left != nullptr || right != nullptr)
            outChans.add (new DSoundInternalOutChannel (dlh.outputDeviceNames[outputDeviceIndex],
                                                        dlh.outputGuids [outputDeviceIndex],
                                                        (int) sampleRate, bufferSizeSamples,
                                                        left, right));
    }

    String error;

    // boost our priority while opening the devices to try to get better sync between them
    const int oldThreadPri = GetThreadPriority (GetCurrentThread());
    const DWORD oldProcPri = GetPriorityClass (GetCurrentProcess());
    SetThreadPriority (GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL);
    SetPriorityClass (GetCurrentProcess(), REALTIME_PRIORITY_CLASS);

    for (int i = 0; i < outChans.size(); ++i)
    {
        error = outChans[i]->open();

        if (error.isNotEmpty())
        {
            error = "Error opening " + dlh.outputDeviceNames[i] + ": \"" + error + "\"";
            break;
        }
    }

    if (error.isEmpty())
    {
        for (int i = 0; i < inChans.size(); ++i)
        {
            error = inChans[i]->open();

            if (error.isNotEmpty())
            {
                error = "Error opening " + dlh.inputDeviceNames[i] + ": \"" + error + "\"";
                break;
            }
        }
    }

    if (error.isEmpty())
    {
        for (int i = 0; i < outChans.size(); ++i)
            outChans.getUnchecked(i)->synchronisePosition();

        for (int i = 0; i < inChans.size(); ++i)
            inChans.getUnchecked(i)->synchronisePosition();

        startThread (9);
        sleep (10);

        notify();
    }
    else
    {
        JUCE_DS_LOG ("Opening failed: " + error);
    }

    SetThreadPriority (GetCurrentThread(), oldThreadPri);
    SetPriorityClass (GetCurrentProcess(), oldProcPri);

    return error;
}

//==============================================================================
class DSoundAudioIODeviceType  : public AudioIODeviceType,
                                 private DeviceChangeDetector
{
public:
    DSoundAudioIODeviceType()
        : AudioIODeviceType ("DirectSound"),
          DeviceChangeDetector (L"DirectSound")
    {
        initialiseDSoundFunctions();
    }

    void scanForDevices() override
    {
        hasScanned = true;
        deviceList.scan();
    }

    StringArray getDeviceNames (bool wantInputNames) const override
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        return wantInputNames ? deviceList.inputDeviceNames
                              : deviceList.outputDeviceNames;
    }

    int getDefaultDeviceIndex (bool /*forInput*/) const override
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this
        return 0;
    }

    int getIndexOfDevice (AudioIODevice* device, bool asInput) const override
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        if (DSoundAudioIODevice* const d = dynamic_cast<DSoundAudioIODevice*> (device))
            return asInput ? d->inputDeviceIndex
                           : d->outputDeviceIndex;

        return -1;
    }

    bool hasSeparateInputsAndOutputs() const override   { return true; }

    AudioIODevice* createDevice (const String& outputDeviceName,
                                 const String& inputDeviceName) override
    {
        jassert (hasScanned); // need to call scanForDevices() before doing this

        const int outputIndex = deviceList.outputDeviceNames.indexOf (outputDeviceName);
        const int inputIndex = deviceList.inputDeviceNames.indexOf (inputDeviceName);

        if (outputIndex >= 0 || inputIndex >= 0)
            return new DSoundAudioIODevice (outputDeviceName.isNotEmpty() ? outputDeviceName
                                                                          : inputDeviceName,
                                            outputIndex, inputIndex);

        return nullptr;
    }

private:
    DSoundDeviceList deviceList;
    bool hasScanned = false;

    void systemDeviceChanged() override
    {
        DSoundDeviceList newList;
        newList.scan();

        if (newList != deviceList)
        {
            deviceList = newList;
            callDeviceChangeListeners();
        }
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (DSoundAudioIODeviceType)
};

//==============================================================================
AudioIODeviceType* AudioIODeviceType::createAudioIODeviceType_DirectSound()
{
    return new DSoundAudioIODeviceType();
}

} // namespace juce
