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
#ifndef SL_ANDROID_DATAFORMAT_PCM_EX
 #define SL_ANDROID_DATAFORMAT_PCM_EX                   ((SLuint32) 0x00000004)
#endif

#ifndef SL_ANDROID_PCM_REPRESENTATION_FLOAT
 #define SL_ANDROID_PCM_REPRESENTATION_FLOAT            ((SLuint32) 0x00000003)
#endif

#ifndef SL_ANDROID_RECORDING_PRESET_UNPROCESSED
 #define SL_ANDROID_RECORDING_PRESET_UNPROCESSED        ((SLuint32) 0x00000005)
#endif

//==============================================================================
struct PCMDataFormatEx : SLDataFormat_PCM
{
    SLuint32 representation;
};

//==============================================================================
template <typename T> struct IntfIID;
template <> struct IntfIID<SLObjectItf_>                   { static SLInterfaceID_ iid; };
template <> struct IntfIID<SLEngineItf_>                   { static SLInterfaceID_ iid; };
template <> struct IntfIID<SLOutputMixItf_>                { static SLInterfaceID_ iid; };
template <> struct IntfIID<SLPlayItf_>                     { static SLInterfaceID_ iid; };
template <> struct IntfIID<SLRecordItf_>                   { static SLInterfaceID_ iid; };
template <> struct IntfIID<SLAndroidSimpleBufferQueueItf_> { static SLInterfaceID_ iid; };
template <> struct IntfIID<SLAndroidConfigurationItf_>     { static SLInterfaceID_ iid; };

SLInterfaceID_ IntfIID<SLObjectItf_>::iid                   = { 0x79216360, 0xddd7, 0x11db, 0xac16, {0x00, 0x02, 0xa5, 0xd5, 0xc5, 0x1b} };
SLInterfaceID_ IntfIID<SLEngineItf_>::iid                   = { 0x8d97c260, 0xddd4, 0x11db, 0x958f, {0x00, 0x02, 0xa5, 0xd5, 0xc5, 0x1b} };
SLInterfaceID_ IntfIID<SLOutputMixItf_>::iid                = { 0x97750f60, 0xddd7, 0x11db, 0x92b1, {0x00, 0x02, 0xa5, 0xd5, 0xc5, 0x1b} };
SLInterfaceID_ IntfIID<SLPlayItf_>::iid                     = { 0xef0bd9c0, 0xddd7, 0x11db, 0xbf49, {0x00, 0x02, 0xa5, 0xd5, 0xc5, 0x1b} };
SLInterfaceID_ IntfIID<SLRecordItf_>::iid                   = { 0xc5657aa0, 0xdddb, 0x11db, 0x82f7, {0x00, 0x02, 0xa5, 0xd5, 0xc5, 0x1b} };
SLInterfaceID_ IntfIID<SLAndroidSimpleBufferQueueItf_>::iid = { 0x198e4940, 0xc5d7, 0x11df, 0xa2a6, {0x00, 0x02, 0xa5, 0xd5, 0xc5, 0x1b} };
SLInterfaceID_ IntfIID<SLAndroidConfigurationItf_>::iid     = { 0x89f6a7e0, 0xbeac, 0x11df, 0x8b5c, {0x00, 0x02, 0xa5, 0xd5, 0xc5, 0x1b} };

//==============================================================================
// Some life-time and type management of OpenSL objects
class SlObjectRef
{
public:
    //==============================================================================
    SlObjectRef() noexcept {}
    SlObjectRef (const SlObjectRef&  obj) noexcept : cb (obj.cb) {}
    SlObjectRef (SlObjectRef&& obj) noexcept : cb (static_cast<ReferenceCountedObjectPtr<ControlBlock>&&> (obj.cb)) { obj.cb = nullptr; }
    explicit SlObjectRef (SLObjectItf o) : cb (new ControlBlock (o)) {}

    //==============================================================================
    SlObjectRef& operator=(const SlObjectRef& r)   noexcept { cb = r.cb; return *this; }
    SlObjectRef& operator=(SlObjectRef&& r)        noexcept { cb = static_cast<ReferenceCountedObjectPtr<ControlBlock>&&> (r.cb); r.cb = nullptr; return *this; }
    SlObjectRef& operator=(std::nullptr_t) noexcept   { cb = nullptr; return *this; }

    //==============================================================================
    const SLObjectItf_* const  operator*()  noexcept  { return *cb->ptr.get(); }
    SLObjectItf operator->() noexcept  { return (cb == nullptr ? nullptr :  cb->ptr.get()); }
    operator SLObjectItf()   noexcept  { return (cb == nullptr ? nullptr :  cb->ptr.get()); }

    //==============================================================================
    bool operator== (nullptr_t) const noexcept  { return (cb == nullptr || cb->ptr == nullptr); }
    bool operator!= (nullptr_t) const noexcept  { return (cb != nullptr && cb->ptr != nullptr); }
private:

    //==============================================================================
    struct ControlBlock : ReferenceCountedObject { ScopedPointer<const SLObjectItf_* const> ptr; ControlBlock() {} ControlBlock (SLObjectItf o) : ptr (o) {} };
    ReferenceCountedObjectPtr<ControlBlock> cb;
};

template <typename T>
class SlRef : public SlObjectRef
{
public:
    //==============================================================================
    SlRef() noexcept : type (nullptr) {}
    SlRef (SlRef& r)  noexcept : SlObjectRef (r), type (r.type) {}
    SlRef (SlRef&& r) noexcept : SlObjectRef (static_cast<SlRef&&> (r)), type (r.type) { r.type = nullptr; }

    //==============================================================================
    SlRef& operator= (const SlRef& r)  noexcept { SlObjectRef::operator= (r); type = r.type; return *this; }
    SlRef& operator= (SlRef&& r) noexcept { SlObjectRef::operator= (static_cast<SlObjectRef&&> (r)); type = r.type; r.type = nullptr; return *this; }
    SlRef& operator= (std::nullptr_t) noexcept { SlObjectRef::operator= (nullptr); type = nullptr; return *this; }

    //==============================================================================
    T* const operator*()  noexcept { return *type; }
    T* const * operator->() noexcept { return type; }
    operator T* const *()   noexcept { return type; }

    //==============================================================================
    static SlRef cast (SlObjectRef&  base) { return SlRef (base); }
    static SlRef cast (SlObjectRef&& base) { return SlRef (static_cast<SlObjectRef&&> (base)); }
private:
    //==============================================================================
    SlRef (SlObjectRef& base) : SlObjectRef (base)
    {
        SLObjectItf obj = SlObjectRef::operator->();
        SLresult err = (*obj)->GetInterface (obj, &IntfIID<T>::iid, &type);
        if (type == nullptr || err != SL_RESULT_SUCCESS)
            *this = nullptr;
    }

    SlRef (SlObjectRef&& base) : SlObjectRef (static_cast<SlObjectRef&&> (base))
    {
        SLObjectItf obj = SlObjectRef::operator->();
        SLresult err = (*obj)->GetInterface (obj, &IntfIID<T>::iid, &type);
        base = nullptr;

        if (type == nullptr || err != SL_RESULT_SUCCESS)
            *this = nullptr;
    }
    T* const * type;
};

template <>
struct ContainerDeletePolicy<const SLObjectItf_* const>
{
    static void destroy (SLObjectItf object)
    {
        if (object != nullptr)
            (*object)->Destroy (object);
    }
};

//==============================================================================
template <typename T> struct BufferHelpers {};

template <>
struct BufferHelpers<int16>
{
    enum { isFloatingPoint = 0 };

    static void initPCMDataFormat (PCMDataFormatEx& dataFormat, int numChannels, double sampleRate)
    {
        dataFormat.formatType     = SL_DATAFORMAT_PCM;
        dataFormat.numChannels    = (SLuint32) numChannels;
        dataFormat.samplesPerSec  = (SLuint32) (sampleRate * 1000);
        dataFormat.bitsPerSample  = SL_PCMSAMPLEFORMAT_FIXED_16;
        dataFormat.containerSize  = SL_PCMSAMPLEFORMAT_FIXED_16;
        dataFormat.channelMask    = (numChannels == 1) ? SL_SPEAKER_FRONT_CENTER :
                                                        (SL_SPEAKER_FRONT_LEFT | SL_SPEAKER_FRONT_RIGHT);
        dataFormat.endianness     = SL_BYTEORDER_LITTLEENDIAN;
        dataFormat.representation = 0;
    }

    static void prepareCallbackBuffer (AudioSampleBuffer&, int16*) {}

    static void convertFromOpenSL (const int16* srcInterleaved, AudioSampleBuffer& audioBuffer)
    {
        for (int i = 0; i < audioBuffer.getNumChannels(); ++i)
        {
            typedef AudioData::Pointer<AudioData::Float32, AudioData::NativeEndian, AudioData::NonInterleaved, AudioData::NonConst> DstSampleType;
            typedef AudioData::Pointer<AudioData::Int16,   AudioData::LittleEndian, AudioData::Interleaved,    AudioData::Const>    SrcSampleType;

            DstSampleType dstData (audioBuffer.getWritePointer (i));
            SrcSampleType srcData (srcInterleaved + i, audioBuffer.getNumChannels());
            dstData.convertSamples (srcData, audioBuffer.getNumSamples());
        }
    }

    static void convertToOpenSL (const AudioSampleBuffer& audioBuffer, int16* dstInterleaved)
    {
        for (int i = 0; i < audioBuffer.getNumChannels(); ++i)
        {
            typedef AudioData::Pointer<AudioData::Int16,   AudioData::LittleEndian, AudioData::Interleaved, AudioData::NonConst> DstSampleType;
            typedef AudioData::Pointer<AudioData::Float32, AudioData::NativeEndian, AudioData::NonInterleaved, AudioData::Const> SrcSampleType;

            DstSampleType dstData (dstInterleaved + i, audioBuffer.getNumChannels());
            SrcSampleType srcData (audioBuffer.getReadPointer (i));

            dstData.convertSamples (srcData, audioBuffer.getNumSamples());
        }
    }

};

template <>
struct BufferHelpers<float>
{
    enum { isFloatingPoint = 1 };

    static void initPCMDataFormat (PCMDataFormatEx& dataFormat, int numChannels, double sampleRate)
    {
        dataFormat.formatType     = SL_ANDROID_DATAFORMAT_PCM_EX;
        dataFormat.numChannels    = (SLuint32) numChannels;
        dataFormat.samplesPerSec  = (SLuint32) (sampleRate * 1000);
        dataFormat.bitsPerSample  = 32;
        dataFormat.containerSize  = 32;
        dataFormat.channelMask    = (numChannels == 1) ? SL_SPEAKER_FRONT_CENTER :
                                                        (SL_SPEAKER_FRONT_LEFT | SL_SPEAKER_FRONT_RIGHT);
        dataFormat.endianness     = SL_BYTEORDER_LITTLEENDIAN;
        dataFormat.representation = SL_ANDROID_PCM_REPRESENTATION_FLOAT;
    }

    static void prepareCallbackBuffer (AudioSampleBuffer& audioBuffer, float* native)
    {
        if (audioBuffer.getNumChannels() == 1)
            audioBuffer.setDataToReferTo (&native, 1, audioBuffer.getNumSamples());
    }

    static void convertFromOpenSL (const float* srcInterleaved, AudioSampleBuffer& audioBuffer)
    {
        if (audioBuffer.getNumChannels() == 1)
        {
            jassert (srcInterleaved == audioBuffer.getWritePointer (0));
            return;
        }

        for (int i = 0; i < audioBuffer.getNumChannels(); ++i)
        {
            typedef AudioData::Pointer<AudioData::Float32, AudioData::NativeEndian, AudioData::NonInterleaved, AudioData::NonConst> DstSampleType;
            typedef AudioData::Pointer<AudioData::Float32, AudioData::LittleEndian, AudioData::Interleaved,    AudioData::Const>    SrcSampleType;

            DstSampleType dstData (audioBuffer.getWritePointer (i));
            SrcSampleType srcData (srcInterleaved + i, audioBuffer.getNumChannels());
            dstData.convertSamples (srcData, audioBuffer.getNumSamples());
        }
    }

    static void convertToOpenSL (const AudioSampleBuffer& audioBuffer, float* dstInterleaved)
    {
        if (audioBuffer.getNumChannels() == 1)
        {
            jassert (dstInterleaved == audioBuffer.getReadPointer (0));
            return;
        }

        for (int i = 0; i < audioBuffer.getNumChannels(); ++i)
        {
            typedef AudioData::Pointer<AudioData::Float32, AudioData::LittleEndian, AudioData::Interleaved,    AudioData::NonConst> DstSampleType;
            typedef AudioData::Pointer<AudioData::Float32, AudioData::NativeEndian, AudioData::NonInterleaved, AudioData::Const>    SrcSampleType;

            DstSampleType dstData (dstInterleaved + i, audioBuffer.getNumChannels());
            SrcSampleType srcData (audioBuffer.getReadPointer (i));

            dstData.convertSamples (srcData, audioBuffer.getNumSamples());
        }
    }
};

class SLRealtimeThread;

//==============================================================================
class OpenSLAudioIODevice  : public AudioIODevice
{
public:
    //==============================================================================
    template <typename T>
    class OpenSLSessionT;

    //==============================================================================
    // CRTP
    template <typename T, class Child, typename RunnerObjectType>
    struct OpenSLQueueRunner
    {
        OpenSLQueueRunner (OpenSLSessionT<T>& sessionToUse, int numChannelsToUse)
            : owner (sessionToUse),
              numChannels (numChannelsToUse),
              nativeBuffer (static_cast<size_t> (numChannels * owner.bufferSize * owner.numBuffers)),
              scratchBuffer (numChannelsToUse, owner.bufferSize),
              sampleBuffer (scratchBuffer.getArrayOfWritePointers(), numChannelsToUse, owner.bufferSize),
              nextBlock (0), numBlocksOut (0)
        {}

        ~OpenSLQueueRunner()
        {
            if (config != nullptr && javaProxy != nullptr)
            {
                javaProxy.clear();
                (*config)->ReleaseJavaProxy (config, /*SL_ANDROID_JAVA_PROXY_ROUTING*/1);
            }
        }

        bool init()
        {
            runner = crtp().createPlayerOrRecorder();
            if (runner == nullptr)
                return false;

            const bool supportsJavaProxy = (getEnv()->GetStaticIntField (AndroidBuildVersion, AndroidBuildVersion.SDK_INT) >= 24);

            if (supportsJavaProxy)
            {
                // may return nullptr on some platforms - that's ok
                config = SlRef<SLAndroidConfigurationItf_>::cast (runner);

                if (config != nullptr)
                {
                    jobject audioRoutingJni;
                    auto status = (*config)->AcquireJavaProxy (config, /*SL_ANDROID_JAVA_PROXY_ROUTING*/1,
                                                               &audioRoutingJni);

                    if (status == SL_RESULT_SUCCESS && audioRoutingJni != 0)
                        javaProxy = GlobalRef (audioRoutingJni);
                }
            }

            queue = SlRef<SLAndroidSimpleBufferQueueItf_>::cast (runner);
            if (queue == nullptr)
                return false;

            return ((*queue)->RegisterCallback (queue, staticFinished, this) == SL_RESULT_SUCCESS);
        }


        void clear()
        {
            nextBlock.set (0);
            numBlocksOut.set (0);

            zeromem (nativeBuffer.get(), static_cast<size_t> (owner.bufferSize * numChannels * owner.numBuffers) * sizeof (T));
            scratchBuffer.clear();
            (*queue)->Clear (queue);
        }

        void enqueueBuffer()
        {
            (*queue)->Enqueue (queue, getCurrentBuffer(), static_cast<SLuint32> (getBufferSizeInSamples() * sizeof (T)));
            ++numBlocksOut;
        }

        bool isBufferAvailable() const         { return (numBlocksOut.get() < owner.numBuffers); }
        T* getNextBuffer()                     { nextBlock.set((nextBlock.get() + 1) % owner.numBuffers); return getCurrentBuffer(); }
        T* getCurrentBuffer()                  { return nativeBuffer.get() + (static_cast<size_t> (nextBlock.get()) * getBufferSizeInSamples()); }
        size_t getBufferSizeInSamples() const  { return static_cast<size_t> (owner.bufferSize * numChannels); }

        void finished (SLAndroidSimpleBufferQueueItf)
        {
            attachAndroidJNI();

            --numBlocksOut;
            owner.doSomeWorkOnAudioThread();
        }

        static void staticFinished (SLAndroidSimpleBufferQueueItf caller, void *pContext)
        {
            reinterpret_cast<OpenSLQueueRunner*> (pContext)->finished (caller);
        }

        // get the "this" pointer for CRTP
        Child&       crtp()       { return * ((Child*) this); }
        const Child& crtp() const { return * ((Child*) this); }

        OpenSLSessionT<T>& owner;

        SlRef<RunnerObjectType> runner;
        SlRef<SLAndroidSimpleBufferQueueItf_> queue;
        SlRef<SLAndroidConfigurationItf_> config;
        GlobalRef javaProxy;

        int numChannels;

        HeapBlock<T> nativeBuffer;
        AudioSampleBuffer scratchBuffer, sampleBuffer;

        Atomic<int> nextBlock, numBlocksOut;
    };

    //==============================================================================
    template <typename T>
    struct OpenSLQueueRunnerPlayer      : OpenSLQueueRunner<T, OpenSLQueueRunnerPlayer<T>, SLPlayItf_>
    {
        typedef OpenSLQueueRunner<T, OpenSLQueueRunnerPlayer<T>, SLPlayItf_> Base;

        enum { isPlayer = 1 };

        OpenSLQueueRunnerPlayer (OpenSLSessionT<T>& sessionToUse, int numChannelsToUse)
            : Base (sessionToUse, numChannelsToUse)
        {}

        SlRef<SLPlayItf_> createPlayerOrRecorder()
        {
            SLDataLocator_AndroidSimpleBufferQueue queueLocator = {SL_DATALOCATOR_ANDROIDSIMPLEBUFFERQUEUE, static_cast<SLuint32> (Base::owner.numBuffers)};
            SLDataLocator_OutputMix outputMix = {SL_DATALOCATOR_OUTPUTMIX, Base::owner.outputMix};

            PCMDataFormatEx dataFormat;
            BufferHelpers<T>::initPCMDataFormat (dataFormat, Base::numChannels, Base::owner.sampleRate);

            SLDataSource source = {&queueLocator, &dataFormat};
            SLDataSink   sink   = {&outputMix,    nullptr};

            SLInterfaceID queueInterfaces[] = { &IntfIID<SLAndroidSimpleBufferQueueItf_>::iid, &IntfIID<SLAndroidConfigurationItf_>::iid };
            SLboolean interfaceRequired[] = {SL_BOOLEAN_TRUE, SL_BOOLEAN_FALSE};

            SLObjectItf obj = nullptr;

            SLresult status = (*Base::owner.engine)->CreateAudioPlayer (Base::owner.engine, &obj, &source, &sink, 2, queueInterfaces, interfaceRequired);
            if (status != SL_RESULT_SUCCESS || obj == nullptr || (*obj)->Realize (obj, 0) != SL_RESULT_SUCCESS)
            {
                if (obj != nullptr)
                    (*obj)->Destroy (obj);

                return SlRef<SLPlayItf_>();
            }

            return SlRef<SLPlayItf_>::cast (SlObjectRef (obj));
        }

        void setState (bool running)    { (*Base::runner)->SetPlayState (Base::runner, running ? SL_PLAYSTATE_PLAYING : SL_PLAYSTATE_STOPPED); }
    };

    template <typename T>
    struct OpenSLQueueRunnerRecorder      : OpenSLQueueRunner<T, OpenSLQueueRunnerRecorder<T>, SLRecordItf_>
    {
        typedef OpenSLQueueRunner<T, OpenSLQueueRunnerRecorder<T>, SLRecordItf_> Base;

        enum { isPlayer = 0 };

        OpenSLQueueRunnerRecorder (OpenSLSessionT<T>& sessionToUse, int numChannelsToUse)
            : Base (sessionToUse, numChannelsToUse)
        {}

        SlRef<SLRecordItf_> createPlayerOrRecorder()
        {
            SLDataLocator_IODevice ioDeviceLocator = {SL_DATALOCATOR_IODEVICE, SL_IODEVICE_AUDIOINPUT, SL_DEFAULTDEVICEID_AUDIOINPUT, nullptr};
            SLDataLocator_AndroidSimpleBufferQueue queueLocator = {SL_DATALOCATOR_ANDROIDSIMPLEBUFFERQUEUE, static_cast<SLuint32> (Base::owner.numBuffers)};

            PCMDataFormatEx dataFormat;
            BufferHelpers<T>::initPCMDataFormat (dataFormat, Base::numChannels, Base::owner.sampleRate);

            SLDataSource source = {&ioDeviceLocator, nullptr};
            SLDataSink   sink   = {&queueLocator,    &dataFormat};

            SLInterfaceID queueInterfaces[] = { &IntfIID<SLAndroidSimpleBufferQueueItf_>::iid, &IntfIID<SLAndroidConfigurationItf_>::iid };
            SLboolean interfaceRequired[] = {SL_BOOLEAN_TRUE, SL_BOOLEAN_FALSE};

            SLObjectItf obj = nullptr;

            SLresult status = (*Base::owner.engine)->CreateAudioRecorder (Base::owner.engine, &obj, &source, &sink, 2, queueInterfaces, interfaceRequired);
            if (status != SL_RESULT_SUCCESS || obj == nullptr || (*obj)->Realize (obj, 0) != SL_RESULT_SUCCESS)
            {
                if (obj != nullptr)
                    (*obj)->Destroy (obj);

                return SlRef<SLRecordItf_>();
            }

            SlRef<SLRecordItf_> recorder = SlRef<SLRecordItf_>::cast (SlObjectRef (obj));

            return recorder;
        }

        bool setAudioPreprocessingEnabled (bool shouldEnable)
        {
            if (Base::config != nullptr)
            {
                const bool supportsUnprocessed = (getEnv()->GetStaticIntField (AndroidBuildVersion, AndroidBuildVersion.SDK_INT) >= 25);
                const SLuint32 recordingPresetValue
                    = (shouldEnable ? SL_ANDROID_RECORDING_PRESET_GENERIC
                                    : (supportsUnprocessed ? SL_ANDROID_RECORDING_PRESET_UNPROCESSED
                                                           : SL_ANDROID_RECORDING_PRESET_VOICE_RECOGNITION));

                SLresult status = (*Base::config)->SetConfiguration (Base::config, SL_ANDROID_KEY_RECORDING_PRESET,
                                                                     &recordingPresetValue, sizeof (recordingPresetValue));

                return (status == SL_RESULT_SUCCESS);
            }

            return false;
        }

        void setState (bool running)    { (*Base::runner)->SetRecordState (Base::runner, running ? SL_RECORDSTATE_RECORDING : SL_RECORDSTATE_STOPPED); }
    };

    //==============================================================================
    class OpenSLSession
    {
    public:
        OpenSLSession (DynamicLibrary& slLibraryToUse,
                       int numInputChannels, int numOutputChannels,
                       double samleRateToUse, int bufferSizeToUse,
                       int numBuffersToUse)
            : inputChannels (numInputChannels), outputChannels (numOutputChannels),
              sampleRate (samleRateToUse), bufferSize (bufferSizeToUse), numBuffers (numBuffersToUse),
              running (false), audioProcessingEnabled (true), callback (nullptr)
        {
            jassert (numInputChannels > 0 || numOutputChannels > 0);

            if (CreateEngineFunc createEngine = (CreateEngineFunc) slLibraryToUse.getFunction ("slCreateEngine"))
            {
                SLObjectItf obj = nullptr;

                SLresult err = createEngine (&obj, 0, nullptr, 0, nullptr, nullptr);
                if (err != SL_RESULT_SUCCESS || obj == nullptr || (*obj)->Realize (obj, 0) != SL_RESULT_SUCCESS)
                {
                    if (obj != nullptr)
                        (*obj)->Destroy (obj);

                    return;
                }

                engine = SlRef<SLEngineItf_>::cast (SlObjectRef (obj));
            }

            if (outputChannels > 0)
            {
                SLObjectItf obj = nullptr;

                SLresult err = (*engine)->CreateOutputMix (engine, &obj, 0, nullptr, nullptr);
                if (err != SL_RESULT_SUCCESS || obj == nullptr || (*obj)->Realize (obj, 0) != SL_RESULT_SUCCESS)
                {
                    if (obj != nullptr)
                        (*obj)->Destroy (obj);

                    return;
                }

                outputMix = SlRef<SLOutputMixItf_>::cast (SlObjectRef (obj));
            }
        }

        virtual ~OpenSLSession() {}

        virtual bool openedOK() const    { return (engine != nullptr && (outputChannels == 0 || (outputMix != nullptr))); }
        virtual void start()             { stop(); jassert (callback.get() != nullptr); running = true; }
        virtual void stop()              { running = false; }
        virtual bool setAudioPreprocessingEnabled (bool shouldEnable) = 0;
        virtual bool supportsFloatingPoint() const noexcept = 0;
        virtual int getXRunCount() const noexcept = 0;

        void setCallback (AudioIODeviceCallback* callbackToUse)
        {
            if (! running)
            {
                callback.set (callbackToUse);
                return;
            }

            // don't set callback to null! stop the playback instead!
            jassert (callbackToUse != nullptr);

            // spin-lock until we can set the callback
            while (true)
            {
                AudioIODeviceCallback* old = callback.get();
                if (old == callbackToUse)
                    break;

                if (callback.compareAndSetBool (callbackToUse, old))
                    break;

                Thread::sleep (1);
            }
        }

        void process (const float** inputChannelData, float** outputChannelData)
        {
            if (AudioIODeviceCallback* cb = callback.exchange(nullptr))
            {
                cb->audioDeviceIOCallback (inputChannelData, inputChannels, outputChannelData, outputChannels, bufferSize);
                callback.set (cb);
            }
            else
            {
                for (int i = 0; i < outputChannels; ++i)
                    zeromem (outputChannelData[i], sizeof(float) * static_cast<size_t> (bufferSize));
            }
        }

        static OpenSLSession* create (DynamicLibrary& slLibrary,
                                      int numInputChannels, int numOutputChannels,
                                      double samleRateToUse, int bufferSizeToUse,
                                      int numBuffersToUse);

        //==============================================================================
        typedef SLresult (*CreateEngineFunc)(SLObjectItf*,SLuint32,const SLEngineOption*,SLuint32,const SLInterfaceID*,const SLboolean*);

        //==============================================================================
        int inputChannels, outputChannels;
        double sampleRate;
        int bufferSize, numBuffers;

        bool running, audioProcessingEnabled;

        SlRef<SLEngineItf_> engine;
        SlRef<SLOutputMixItf_> outputMix;

        Atomic<AudioIODeviceCallback*> callback;
    };

    template <typename T>
    class OpenSLSessionT : public OpenSLSession
    {
    public:
        OpenSLSessionT (DynamicLibrary& slLibraryToUse,
                        int numInputChannels, int numOutputChannels,
                        double samleRateToUse, int bufferSizeToUse,
                        int numBuffersToUse)
            : OpenSLSession (slLibraryToUse, numInputChannels, numOutputChannels, samleRateToUse, bufferSizeToUse, numBuffersToUse)
        {
            jassert (numInputChannels > 0 || numOutputChannels > 0);

            if (OpenSLSession::openedOK())
            {
                if (inputChannels > 0)
                {
                    recorder = new OpenSLQueueRunnerRecorder<T>(*this, inputChannels);

                    if (! recorder->init())
                    {
                        recorder = nullptr;
                        return;
                    }
                }

                if (outputChannels > 0)
                {
                    player = new OpenSLQueueRunnerPlayer<T>(*this, outputChannels);

                    if (! player->init())
                    {
                        player = nullptr;
                        return;
                    }

                    const bool supportsUnderrunCount = (getEnv()->GetStaticIntField (AndroidBuildVersion, AndroidBuildVersion.SDK_INT) >= 24);
                    getUnderrunCount = supportsUnderrunCount ? getEnv()->GetMethodID (AudioTrack, "getUnderrunCount", "()I") : 0;
                }
            }
        }

        bool openedOK() const override
        {
            return (OpenSLSession::openedOK() && (inputChannels == 0  || recorder != nullptr)
                                              && (outputChannels == 0 || player   != nullptr));
        }

        void start() override
        {
            OpenSLSession::start();

            guard.set (0);

            if (inputChannels > 0)
                recorder->clear();

            if (outputChannels > 0)
                player->clear();

            // first enqueue all buffers
            for (int i = 0; i < numBuffers; ++i)
                doSomeWorkOnAudioThread();

            if (inputChannels > 0)
                recorder->setState (true);

            if (outputChannels > 0)
                player->setState (true);
        }

        void stop() override
        {
            OpenSLSession::stop();

            while (! guard.compareAndSetBool (1, 0))
                Thread::sleep (1);

            if (inputChannels > 0)
                recorder->setState (false);

            if (outputChannels > 0)
                player->setState (false);

            guard.set (0);
        }

        bool setAudioPreprocessingEnabled (bool shouldEnable) override
        {
            if (shouldEnable != audioProcessingEnabled)
            {
                audioProcessingEnabled = shouldEnable;

                if (recorder != nullptr)
                    return recorder->setAudioPreprocessingEnabled (audioProcessingEnabled);
            }

            return true;
        }

        int getXRunCount() const noexcept override
        {
            if (player != nullptr && player->javaProxy != nullptr && getUnderrunCount != 0)
                return getEnv()->CallIntMethod (player->javaProxy, getUnderrunCount);

            return -1;
        }

        bool supportsFloatingPoint() const noexcept override          { return (BufferHelpers<T>::isFloatingPoint != 0); }

        void doSomeWorkOnAudioThread()
        {
            // only the player or the recorder should enter this section at any time
            if (guard.compareAndSetBool (1, 0))
            {
                // are there enough buffers avaialable to process some audio
                if ((inputChannels == 0 || recorder->isBufferAvailable()) && (outputChannels == 0 || player->isBufferAvailable()))
                {
                    T* recorderBuffer = (inputChannels  > 0 ? recorder->getNextBuffer() : nullptr);
                    T* playerBuffer   = (outputChannels > 0 ? player->getNextBuffer()   : nullptr);

                    const float** inputChannelData = nullptr;
                    float** outputChannelData = nullptr;

                    if (recorderBuffer != nullptr)
                    {
                        BufferHelpers<T>::prepareCallbackBuffer (recorder->sampleBuffer, recorderBuffer);
                        BufferHelpers<T>::convertFromOpenSL (recorderBuffer, recorder->sampleBuffer);

                        inputChannelData = recorder->sampleBuffer.getArrayOfReadPointers();
                    }

                    if (playerBuffer != nullptr)
                    {
                        BufferHelpers<T>::prepareCallbackBuffer (player->sampleBuffer, playerBuffer);
                        outputChannelData = player->sampleBuffer.getArrayOfWritePointers();
                    }

                    process (inputChannelData, outputChannelData);

                    if (recorderBuffer != nullptr)
                        recorder->enqueueBuffer();

                    if (playerBuffer != nullptr)
                    {
                        BufferHelpers<T>::convertToOpenSL (player->sampleBuffer, playerBuffer);
                        player->enqueueBuffer();
                    }
                }

                guard.set (0);
            }
        }

        //==============================================================================
        ScopedPointer<OpenSLQueueRunnerPlayer<T>> player;
        ScopedPointer<OpenSLQueueRunnerRecorder<T>> recorder;
        Atomic<int> guard;
        jmethodID getUnderrunCount = 0;
    };

    //==============================================================================
    OpenSLAudioIODevice (const String& deviceName)
        : AudioIODevice (deviceName, openSLTypeName),
          actualBufferSize (0), sampleRate (0),
          audioProcessingEnabled (true),
          callback (nullptr)
    {
        // OpenSL has piss-poor support for determining latency, so the only way I can find to
        // get a number for this is by asking the AudioTrack/AudioRecord classes..
        AndroidAudioIODevice javaDevice (deviceName);

        // this is a total guess about how to calculate the latency, but seems to vaguely agree
        // with the devices I've tested.. YMMV
        inputLatency  = (javaDevice.minBufferSizeIn  * 2) / 3;
        outputLatency = (javaDevice.minBufferSizeOut * 2) / 3;

        const int64 longestLatency = jmax (inputLatency, outputLatency);
        const int64 totalLatency = inputLatency + outputLatency;
        inputLatency  = (int) ((longestLatency * inputLatency)  / totalLatency) & ~15;
        outputLatency = (int) ((longestLatency * outputLatency) / totalLatency) & ~15;

        bool success = slLibrary.open ("libOpenSLES.so");

        // You can only create this class if you are sure that your hardware supports OpenSL
        jassert (success);
        ignoreUnused (success);
    }

    ~OpenSLAudioIODevice()
    {
        close();
    }

    bool openedOk() const       { return session != nullptr; }

    StringArray getOutputChannelNames() override
    {
        StringArray s;
        s.add ("Left");
        s.add ("Right");
        return s;
    }

    StringArray getInputChannelNames() override
    {
        StringArray s;
        s.add ("Audio Input");
        return s;
    }

    Array<double> getAvailableSampleRates() override
    {
        //see https://developer.android.com/ndk/guides/audio/opensl-for-android.html

        static const double rates[] = { 8000.0, 11025.0, 12000.0, 16000.0,
                                        22050.0, 24000.0, 32000.0, 44100.0, 48000.0 };
        Array<double> retval (rates, numElementsInArray (rates));

        // make sure the native sample rate is pafrt of the list
        double native = getNativeSampleRate();
        if (native != 0.0 && ! retval.contains (native))
            retval.add (native);

        return retval;
    }

    Array<int> getAvailableBufferSizes() override
    {
        // we need to offer the lowest possible buffer size which
        // is the native buffer size
        const int defaultNumMultiples = 8;
        const int nativeBufferSize = getNativeBufferSize();

        Array<int> retval;
        for (int i = 1; i < defaultNumMultiples; ++i)
            retval.add (i * nativeBufferSize);

        return retval;
    }

    String open (const BigInteger& inputChannels,
                 const BigInteger& outputChannels,
                 double requestedSampleRate,
                 int bufferSize) override
    {
        close();

        lastError.clear();
        sampleRate = (int) requestedSampleRate;

        int preferredBufferSize = (bufferSize <= 0) ? getDefaultBufferSize() : bufferSize;

        activeOutputChans = outputChannels;
        activeOutputChans.setRange (2, activeOutputChans.getHighestBit(), false);
        int numOutputChannels = activeOutputChans.countNumberOfSetBits();

        activeInputChans = inputChannels;
        activeInputChans.setRange (1, activeInputChans.getHighestBit(), false);
        int numInputChannels = activeInputChans.countNumberOfSetBits();

        actualBufferSize = preferredBufferSize;

        const int audioBuffersToEnqueue = hasLowLatencyAudioPath() ? buffersToEnqueueForLowLatency
                                                                   : buffersToEnqueueSlowAudio;

        if (numInputChannels > 0 && (! RuntimePermissions::isGranted (RuntimePermissions::recordAudio)))
        {
            // If you hit this assert, you probably forgot to get RuntimePermissions::recordAudio
            // before trying to open an audio input device. This is not going to work!
            jassertfalse;
            lastError = "Error opening OpenSL input device: the app was not granted android.permission.RECORD_AUDIO";
        }

        session = OpenSLSession::create (slLibrary, numInputChannels, numOutputChannels,
                                         sampleRate, actualBufferSize, audioBuffersToEnqueue);
        if (session != nullptr)
            session->setAudioPreprocessingEnabled (audioProcessingEnabled);
        else
        {
            if (numInputChannels > 0 && numOutputChannels > 0 && RuntimePermissions::isGranted (RuntimePermissions::recordAudio))
            {
                // New versions of the Android emulator do not seem to support audio input anymore on OS X
                activeInputChans = BigInteger(0);
                numInputChannels = 0;

                session = OpenSLSession::create(slLibrary, numInputChannels, numOutputChannels,
                                                sampleRate, actualBufferSize, audioBuffersToEnqueue);
            }
        }

        DBG ("OpenSL: numInputChannels = " << numInputChannels
             << ", numOutputChannels = " << numOutputChannels
             << ", nativeBufferSize = " << getNativeBufferSize()
             << ", nativeSampleRate = " << getNativeSampleRate()
             << ", actualBufferSize = " << actualBufferSize
             << ", audioBuffersToEnqueue = " << audioBuffersToEnqueue
             << ", sampleRate = " << sampleRate
             << ", supportsFloatingPoint = " << (session != nullptr && session->supportsFloatingPoint() ? "true" : "false"));

        if (session == nullptr)
            lastError = "Unknown error initializing opensl session";

        deviceOpen = (session != nullptr);
        return lastError;
    }

    void close() override
    {
        stop();
        session = nullptr;
        callback = nullptr;
    }

    int getOutputLatencyInSamples() override            { return outputLatency; }
    int getInputLatencyInSamples() override             { return inputLatency; }
    bool isOpen() override                              { return deviceOpen; }
    int getCurrentBufferSizeSamples() override          { return actualBufferSize; }
    int getCurrentBitDepth() override                   { return (session != nullptr && session->supportsFloatingPoint() ? 32 : 16); }
    BigInteger getActiveOutputChannels() const override { return activeOutputChans; }
    BigInteger getActiveInputChannels() const override  { return activeInputChans; }
    String getLastError() override                      { return lastError; }
    bool isPlaying() override                           { return callback != nullptr; }
    int getXRunCount() const noexcept override          { return (session != nullptr ? session->getXRunCount() : -1); }

    int getDefaultBufferSize() override
    {
        // Only on a Pro-Audio device will we set the lowest possible buffer size
        // by default. We need to be more conservative on other devices
        // as they may be low-latency, but still have a crappy CPU.
        return (isProAudioDevice() ? 1 : 6)
                 * defaultBufferSizeIsMultipleOfNative * getNativeBufferSize();
    }

    double getCurrentSampleRate() override
    {
        return (sampleRate == 0.0 ? getNativeSampleRate() : sampleRate);
    }

    void start (AudioIODeviceCallback* newCallback) override
    {
        if (session != nullptr && callback != newCallback)
        {
            AudioIODeviceCallback* oldCallback = callback;

            if (newCallback != nullptr)
                newCallback->audioDeviceAboutToStart (this);

            if (oldCallback != nullptr)
            {
                // already running
                if (newCallback == nullptr)
                    stop();
                else
                    session->setCallback (newCallback);

                oldCallback->audioDeviceStopped();
            }
            else
            {
                jassert (newCallback != nullptr);

                // session hasn't started yet
                session->setCallback (newCallback);
                session->start();
            }

            callback = newCallback;
        }
    }

    void stop() override
    {
        if (session != nullptr && callback != nullptr)
        {
            callback = nullptr;
            session->stop();
            session->setCallback (nullptr);
        }
    }

    bool setAudioPreprocessingEnabled (bool shouldAudioProcessingBeEnabled) override
    {
        audioProcessingEnabled = shouldAudioProcessingBeEnabled;

        if (session != nullptr)
            session->setAudioPreprocessingEnabled (audioProcessingEnabled);

        return true;
    }

    static const char* const openSLTypeName;

private:
    //==============================================================================
    friend class SLRealtimeThread;

    //==============================================================================
    DynamicLibrary slLibrary;
    int actualBufferSize, sampleRate;
    int inputLatency, outputLatency;
    bool deviceOpen, audioProcessingEnabled;
    String lastError;
    BigInteger activeOutputChans, activeInputChans;
    AudioIODeviceCallback* callback;

    ScopedPointer<OpenSLSession> session;

    enum
    {
        // The number of buffers to enqueue needs to be at least two for the audio to use the low-latency
        // audio path (see "Performance" section in ndk/docs/Additional_library_docs/opensles/index.html)
        buffersToEnqueueForLowLatency = 4,
        buffersToEnqueueSlowAudio = 8,
        defaultBufferSizeIsMultipleOfNative = 1
    };

    //==============================================================================
    static String audioManagerGetProperty (const String& property)
    {
        const LocalRef<jstring> jProperty (javaString (property));
        const LocalRef<jstring> text ((jstring) android.activity.callObjectMethod (JuceAppActivity.audioManagerGetProperty,
                                                                                   jProperty.get()));
        if (text.get() != 0)
            return juceString (text);

        return {};
    }

    static bool androidHasSystemFeature (const String& property)
    {
        const LocalRef<jstring> jProperty (javaString (property));
        return android.activity.callBooleanMethod (JuceAppActivity.hasSystemFeature, jProperty.get());
    }

    static double getNativeSampleRate()
    {
        return audioManagerGetProperty ("android.media.property.OUTPUT_SAMPLE_RATE").getDoubleValue();
    }

    static int getNativeBufferSize()
    {
        const int val = audioManagerGetProperty ("android.media.property.OUTPUT_FRAMES_PER_BUFFER").getIntValue();
        return val > 0 ? val : 512;
    }

    static bool isProAudioDevice()
    {
        return androidHasSystemFeature ("android.hardware.audio.pro");
    }

    static bool hasLowLatencyAudioPath()
    {
        return androidHasSystemFeature ("android.hardware.audio.low_latency");
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OpenSLAudioIODevice)
};

OpenSLAudioIODevice::OpenSLSession* OpenSLAudioIODevice::OpenSLSession::create (DynamicLibrary& slLibrary,
                                                                                int numInputChannels, int numOutputChannels,
                                                                                double samleRateToUse, int bufferSizeToUse,
                                                                                int numBuffersToUse)
{
    ScopedPointer<OpenSLSession> retval;
    auto sdkVersion = getEnv()->GetStaticIntField (AndroidBuildVersion, AndroidBuildVersion.SDK_INT);

    // SDK versions 21 and higher should natively support floating point...
    if (sdkVersion >= 21)
    {
        retval = new OpenSLSessionT<float> (slLibrary, numInputChannels, numOutputChannels, samleRateToUse,
                                            bufferSizeToUse, numBuffersToUse);

        // ...however, some devices lie so re-try without floating point
        if (retval != nullptr && (! retval->openedOK()))
            retval = nullptr;
    }

    if (retval == nullptr)
    {
        retval = new OpenSLSessionT<int16> (slLibrary, numInputChannels, numOutputChannels, samleRateToUse,
                                            bufferSizeToUse, numBuffersToUse);

        if (retval != nullptr && (! retval->openedOK()))
            retval = nullptr;
    }

    return retval.release();
}

//==============================================================================
class OpenSLAudioDeviceType  : public AudioIODeviceType
{
public:
    OpenSLAudioDeviceType()  : AudioIODeviceType (OpenSLAudioIODevice::openSLTypeName) {}

    //==============================================================================
    void scanForDevices() override {}

    StringArray getDeviceNames (bool) const override                             { return StringArray (OpenSLAudioIODevice::openSLTypeName); }
    int getDefaultDeviceIndex (bool) const override                              { return 0; }
    int getIndexOfDevice (AudioIODevice* device, bool) const override            { return device != nullptr ? 0 : -1; }
    bool hasSeparateInputsAndOutputs() const override                            { return false; }

    AudioIODevice* createDevice (const String& outputDeviceName,
                                 const String& inputDeviceName) override
    {
        ScopedPointer<OpenSLAudioIODevice> dev;

        if (outputDeviceName.isNotEmpty() || inputDeviceName.isNotEmpty())
            dev = new OpenSLAudioIODevice (outputDeviceName.isNotEmpty() ? outputDeviceName
                                                                         : inputDeviceName);

        return dev.release();
    }

    static bool isOpenSLAvailable()
    {
        DynamicLibrary library;
        return library.open ("libOpenSLES.so");
    }

private:


    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OpenSLAudioDeviceType)
};

const char* const OpenSLAudioIODevice::openSLTypeName = "Android OpenSL";


//==============================================================================
bool isOpenSLAvailable()  { return OpenSLAudioDeviceType::isOpenSLAvailable(); }

AudioIODeviceType* AudioIODeviceType::createAudioIODeviceType_OpenSLES()
{
    return isOpenSLAvailable() ? new OpenSLAudioDeviceType() : nullptr;
}

//==============================================================================
class SLRealtimeThread
{
public:
    static constexpr int numBuffers = 4;

    SLRealtimeThread()
    {
        if (auto createEngine = (OpenSLAudioIODevice::OpenSLSession::CreateEngineFunc) slLibrary.getFunction ("slCreateEngine"))
        {
            SLObjectItf obj = nullptr;
            auto err = createEngine (&obj, 0, nullptr, 0, nullptr, nullptr);

            if (err != SL_RESULT_SUCCESS || obj == nullptr)
                return;

            if ((*obj)->Realize (obj, 0) != SL_RESULT_SUCCESS)
            {
                (*obj)->Destroy (obj);
                return;
            }

            engine = SlRef<SLEngineItf_>::cast (SlObjectRef (obj));

            if (engine == nullptr)
            {
                (*obj)->Destroy (obj);
                return;
            }

            obj = nullptr;
            err = (*engine)->CreateOutputMix (engine, &obj, 0, nullptr, nullptr);

            if (err != SL_RESULT_SUCCESS || obj == nullptr || (*obj)->Realize (obj, 0) != SL_RESULT_SUCCESS)
            {
                (*obj)->Destroy (obj);
                return;
            }

            outputMix = SlRef<SLOutputMixItf_>::cast (SlObjectRef (obj));

            if (outputMix == nullptr)
            {
                (*obj)->Destroy (obj);
                return;
            }

            SLDataLocator_AndroidSimpleBufferQueue queueLocator = {SL_DATALOCATOR_ANDROIDSIMPLEBUFFERQUEUE, static_cast<SLuint32> (numBuffers)};
            SLDataLocator_OutputMix outputMixLocator = {SL_DATALOCATOR_OUTPUTMIX, outputMix};

            PCMDataFormatEx dataFormat;
            BufferHelpers<int16>::initPCMDataFormat (dataFormat, 1, OpenSLAudioIODevice::getNativeSampleRate());

            SLDataSource source = { &queueLocator, &dataFormat };
            SLDataSink   sink   = { &outputMixLocator, nullptr };

            SLInterfaceID queueInterfaces[] = { &IntfIID<SLAndroidSimpleBufferQueueItf_>::iid };
            SLboolean trueFlag = SL_BOOLEAN_TRUE;

            obj = nullptr;
            err = (*engine)->CreateAudioPlayer (engine, &obj, &source, &sink, 1, queueInterfaces, &trueFlag);

            if (err != SL_RESULT_SUCCESS || obj == nullptr)
                return;

            if ((*obj)->Realize (obj, 0) != SL_RESULT_SUCCESS)
            {
                (*obj)->Destroy (obj);
                return;
            }

            player = SlRef<SLPlayItf_>::cast (SlObjectRef (obj));

            if (player == nullptr)
            {
                (*obj)->Destroy (obj);
                return;
            }

            queue = SlRef<SLAndroidSimpleBufferQueueItf_>::cast (player);
            if (queue == nullptr)
                return;

            if ((*queue)->RegisterCallback (queue, staticFinished, this) != SL_RESULT_SUCCESS)
            {
                queue = nullptr;
                return;
            }

            pthread_cond_init (&threadReady, nullptr);
            pthread_mutex_init (&threadReadyMutex, nullptr);
        }
    }

    bool isOK() const      { return queue != nullptr; }

    pthread_t startThread (void* (*entry) (void*), void* userPtr)
    {
        memset (buffer.get(), 0, static_cast<size_t> (sizeof (int16) * static_cast<size_t> (bufferSize * numBuffers)));

        for (int i = 0; i < numBuffers; ++i)
        {
            int16* dst = buffer.get() + (bufferSize * i);
            (*queue)->Enqueue (queue, dst, static_cast<SLuint32> (static_cast<size_t> (bufferSize) * sizeof (int16)));
        }

        pthread_mutex_lock (&threadReadyMutex);

        threadEntryProc = entry;
        threadUserPtr  = userPtr;

        (*player)->SetPlayState (player, SL_PLAYSTATE_PLAYING);

        pthread_cond_wait (&threadReady, &threadReadyMutex);
        pthread_mutex_unlock (&threadReadyMutex);

        return threadID;
    }

    void finished()
    {
        if (threadEntryProc != nullptr)
        {
            pthread_mutex_lock (&threadReadyMutex);

            threadID = pthread_self();

            pthread_cond_signal (&threadReady);
            pthread_mutex_unlock (&threadReadyMutex);

            threadEntryProc (threadUserPtr);
            threadEntryProc = nullptr;

            (*player)->SetPlayState (player, SL_PLAYSTATE_STOPPED);
            MessageManager::callAsync ([this] () { delete this; });
        }
    }

private:
    //=============================================================================
    static void staticFinished (SLAndroidSimpleBufferQueueItf, void* context)
    {
        static_cast<SLRealtimeThread*> (context)->finished();
    }

    //=============================================================================
    DynamicLibrary slLibrary { "libOpenSLES.so" };

    SlRef<SLEngineItf_>    engine;
    SlRef<SLOutputMixItf_> outputMix;
    SlRef<SLPlayItf_>      player;
    SlRef<SLAndroidSimpleBufferQueueItf_> queue;

    int bufferSize = OpenSLAudioIODevice::getNativeBufferSize();
    HeapBlock<int16> buffer { HeapBlock<int16> (static_cast<size_t> (1 * bufferSize * numBuffers)) };

    void* (*threadEntryProc) (void*) = nullptr;
    void* threadUserPtr              = nullptr;

    pthread_cond_t  threadReady;
    pthread_mutex_t threadReadyMutex;
    pthread_t       threadID;
};

pthread_t juce_createRealtimeAudioThread (void* (*entry) (void*), void* userPtr)
{
    ScopedPointer<SLRealtimeThread> thread (new SLRealtimeThread);

    if (! thread->isOK())
        return 0;

    pthread_t threadID = thread->startThread (entry, userPtr);

    // the thread will de-allocate itself
    thread.release();

    return threadID;
}

} // namespace juce
