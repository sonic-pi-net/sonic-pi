/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2015 - ROLI Ltd.

   Permission is granted to use this software under the terms of either:
   a) the GPL v2 (or any later version)
   b) the Affero GPL v3

   Details of these licenses can be found at: www.gnu.org/licenses

   JUCE is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

   ------------------------------------------------------------------------------

   To release a closed-source product which uses JUCE, commercial licenses are
   available: visit www.juce.com for more information.

  ==============================================================================
*/

namespace CDBurnerHelpers
{
    IDiscRecorder* enumCDBurners (StringArray* list, int indexToOpen, IDiscMaster** master)
    {
        CoInitialize (0);

        IDiscMaster* dm;
        IDiscRecorder* result = nullptr;

        if (SUCCEEDED (CoCreateInstance (CLSID_MSDiscMasterObj, 0,
                                         CLSCTX_INPROC_SERVER | CLSCTX_LOCAL_SERVER,
                                         IID_IDiscMaster,
                                         (void**) &dm)))
        {
            if (SUCCEEDED (dm->Open()))
            {
                IEnumDiscRecorders* drEnum = nullptr;

                if (SUCCEEDED (dm->EnumDiscRecorders (&drEnum)))
                {
                    IDiscRecorder* dr = nullptr;
                    DWORD dummy;
                    int index = 0;

                    while (drEnum->Next (1, &dr, &dummy) == S_OK)
                    {
                        if (indexToOpen == index)
                        {
                            result = dr;
                            break;
                        }
                        else if (list != nullptr)
                        {
                            BSTR path;

                            if (SUCCEEDED (dr->GetPath (&path)))
                                list->add ((const WCHAR*) path);
                        }

                        ++index;
                        dr->Release();
                    }

                    drEnum->Release();
                }

                if (master == 0)
                    dm->Close();
            }

            if (master != nullptr)
                *master = dm;
            else
                dm->Release();
        }

        return result;
    }
}

//==============================================================================
class AudioCDBurner::Pimpl  : public ComBaseClassHelper <IDiscMasterProgressEvents>,
                              public Timer
{
public:
    Pimpl (AudioCDBurner& owner_, IDiscMaster* discMaster_, IDiscRecorder* discRecorder_)
      : owner (owner_), discMaster (discMaster_), discRecorder (discRecorder_), redbook (0),
        listener (0), progress (0), shouldCancel (false)
    {
        HRESULT hr = discMaster->SetActiveDiscMasterFormat (IID_IRedbookDiscMaster, (void**) &redbook);
        jassert (SUCCEEDED (hr));
        hr = discMaster->SetActiveDiscRecorder (discRecorder);
        //jassert (SUCCEEDED (hr));

        lastState = getDiskState();
        startTimer (2000);
    }

    ~Pimpl()  {}

    void releaseObjects()
    {
        discRecorder->Close();
        if (redbook != nullptr)
            redbook->Release();
        discRecorder->Release();
        discMaster->Release();
        Release();
    }

    JUCE_COMRESULT QueryCancel (boolean* pbCancel)
    {
        if (listener != nullptr && ! shouldCancel)
            shouldCancel = listener->audioCDBurnProgress (progress);

        *pbCancel = shouldCancel;

        return S_OK;
    }

    JUCE_COMRESULT NotifyBlockProgress (long nCompleted, long nTotal)
    {
        progress = nCompleted / (float) nTotal;
        shouldCancel = listener != nullptr && listener->audioCDBurnProgress (progress);

        return E_NOTIMPL;
    }

    JUCE_COMRESULT NotifyPnPActivity (void)                              { return E_NOTIMPL; }
    JUCE_COMRESULT NotifyAddProgress (long /*nCompletedSteps*/, long /*nTotalSteps*/)    { return E_NOTIMPL; }
    JUCE_COMRESULT NotifyTrackProgress (long /*nCurrentTrack*/, long /*nTotalTracks*/)   { return E_NOTIMPL; }
    JUCE_COMRESULT NotifyPreparingBurn (long /*nEstimatedSeconds*/)      { return E_NOTIMPL; }
    JUCE_COMRESULT NotifyClosingDisc (long /*nEstimatedSeconds*/)        { return E_NOTIMPL; }
    JUCE_COMRESULT NotifyBurnComplete (HRESULT /*status*/)               { return E_NOTIMPL; }
    JUCE_COMRESULT NotifyEraseComplete (HRESULT /*status*/)              { return E_NOTIMPL; }

    class ScopedDiscOpener
    {
    public:
        ScopedDiscOpener (Pimpl& p) : pimpl (p) { pimpl.discRecorder->OpenExclusive(); }
        ~ScopedDiscOpener()                     { pimpl.discRecorder->Close(); }

    private:
        Pimpl& pimpl;

        JUCE_DECLARE_NON_COPYABLE (ScopedDiscOpener)
    };

    DiskState getDiskState()
    {
        const ScopedDiscOpener opener (*this);

        long type, flags;
        HRESULT hr = discRecorder->QueryMediaType (&type, &flags);

        if (FAILED (hr))
            return unknown;

        if (type != 0 && (flags & MEDIA_WRITABLE) != 0)
            return writableDiskPresent;

        if (type == 0)
            return noDisc;

        return readOnlyDiskPresent;
    }

    int getIntProperty (const LPOLESTR name, const int defaultReturn) const
    {
        ComSmartPtr<IPropertyStorage> prop;
        if (FAILED (discRecorder->GetRecorderProperties (prop.resetAndGetPointerAddress())))
            return defaultReturn;

        PROPSPEC iPropSpec;
        iPropSpec.ulKind = PRSPEC_LPWSTR;
        iPropSpec.lpwstr = name;

        PROPVARIANT iPropVariant;
        return FAILED (prop->ReadMultiple (1, &iPropSpec, &iPropVariant))
                   ? defaultReturn : (int) iPropVariant.lVal;
    }

    bool setIntProperty (const LPOLESTR name, const int value) const
    {
        ComSmartPtr<IPropertyStorage> prop;
        if (FAILED (discRecorder->GetRecorderProperties (prop.resetAndGetPointerAddress())))
            return false;

        PROPSPEC iPropSpec;
        iPropSpec.ulKind = PRSPEC_LPWSTR;
        iPropSpec.lpwstr = name;

        PROPVARIANT iPropVariant;
        if (FAILED (prop->ReadMultiple (1, &iPropSpec, &iPropVariant)))
            return false;

        iPropVariant.lVal = (long) value;
        return SUCCEEDED (prop->WriteMultiple (1, &iPropSpec, &iPropVariant, iPropVariant.vt))
                && SUCCEEDED (discRecorder->SetRecorderProperties (prop));
    }

    void timerCallback() override
    {
        const DiskState state = getDiskState();

        if (state != lastState)
        {
            lastState = state;
            owner.sendChangeMessage();
        }
    }

    AudioCDBurner& owner;
    DiskState lastState;
    IDiscMaster* discMaster;
    IDiscRecorder* discRecorder;
    IRedbookDiscMaster* redbook;
    AudioCDBurner::BurnProgressListener* listener;
    float progress;
    bool shouldCancel;
};

//==============================================================================
AudioCDBurner::AudioCDBurner (const int deviceIndex)
{
    IDiscMaster* discMaster = nullptr;
    IDiscRecorder* discRecorder = CDBurnerHelpers::enumCDBurners (0, deviceIndex, &discMaster);

    if (discRecorder != nullptr)
        pimpl = new Pimpl (*this, discMaster, discRecorder);
}

AudioCDBurner::~AudioCDBurner()
{
    if (pimpl != nullptr)
        pimpl.release()->releaseObjects();
}

StringArray AudioCDBurner::findAvailableDevices()
{
    StringArray devs;
    CDBurnerHelpers::enumCDBurners (&devs, -1, 0);
    return devs;
}

AudioCDBurner* AudioCDBurner::openDevice (const int deviceIndex)
{
    ScopedPointer<AudioCDBurner> b (new AudioCDBurner (deviceIndex));

    if (b->pimpl == 0)
        b = nullptr;

    return b.release();
}

AudioCDBurner::DiskState AudioCDBurner::getDiskState() const
{
    return pimpl->getDiskState();
}

bool AudioCDBurner::isDiskPresent() const
{
    return getDiskState() == writableDiskPresent;
}

bool AudioCDBurner::openTray()
{
    const Pimpl::ScopedDiscOpener opener (*pimpl);
    return SUCCEEDED (pimpl->discRecorder->Eject());
}

AudioCDBurner::DiskState AudioCDBurner::waitUntilStateChange (int timeOutMilliseconds)
{
    const int64 timeout = Time::currentTimeMillis() + timeOutMilliseconds;
    DiskState oldState = getDiskState();
    DiskState newState = oldState;

    while (newState == oldState && Time::currentTimeMillis() < timeout)
    {
        newState = getDiskState();
        Thread::sleep (jmin (250, (int) (timeout - Time::currentTimeMillis())));
    }

    return newState;
}

Array<int> AudioCDBurner::getAvailableWriteSpeeds() const
{
    Array<int> results;
    const int maxSpeed = pimpl->getIntProperty (L"MaxWriteSpeed", 1);
    const int speeds[] = { 1, 2, 4, 8, 12, 16, 20, 24, 32, 40, 64, 80 };

    for (int i = 0; i < numElementsInArray (speeds); ++i)
        if (speeds[i] <= maxSpeed)
            results.add (speeds[i]);

    results.addIfNotAlreadyThere (maxSpeed);
    return results;
}

bool AudioCDBurner::setBufferUnderrunProtection (const bool shouldBeEnabled)
{
    if (pimpl->getIntProperty (L"BufferUnderrunFreeCapable", 0) == 0)
        return false;

    pimpl->setIntProperty (L"EnableBufferUnderrunFree", shouldBeEnabled ? -1 : 0);
    return pimpl->getIntProperty (L"EnableBufferUnderrunFree", 0) != 0;
}

int AudioCDBurner::getNumAvailableAudioBlocks() const
{
    long blocksFree = 0;
    pimpl->redbook->GetAvailableAudioTrackBlocks (&blocksFree);
    return blocksFree;
}

String AudioCDBurner::burn (AudioCDBurner::BurnProgressListener* listener, bool ejectDiscAfterwards,
                            bool performFakeBurnForTesting, int writeSpeed)
{
    pimpl->setIntProperty (L"WriteSpeed", writeSpeed > 0 ? writeSpeed : -1);

    pimpl->listener = listener;
    pimpl->progress = 0;
    pimpl->shouldCancel = false;

    UINT_PTR cookie;
    HRESULT hr = pimpl->discMaster->ProgressAdvise ((AudioCDBurner::Pimpl*) pimpl, &cookie);

    hr = pimpl->discMaster->RecordDisc (performFakeBurnForTesting,
                                        ejectDiscAfterwards);

    String error;
    if (hr != S_OK)
    {
        const char* e = "Couldn't open or write to the CD device";

        if (hr == IMAPI_E_USERABORT)
            e = "User cancelled the write operation";
        else if (hr == IMAPI_E_MEDIUM_NOTPRESENT || hr == IMAPI_E_TRACKOPEN)
            e = "No Disk present";

        error = e;
    }

    pimpl->discMaster->ProgressUnadvise (cookie);
    pimpl->listener = 0;

    return error;
}

bool AudioCDBurner::addAudioTrack (AudioSource* audioSource, int numSamples)
{
    if (audioSource == 0)
        return false;

    ScopedPointer<AudioSource> source (audioSource);

    long bytesPerBlock;
    HRESULT hr = pimpl->redbook->GetAudioBlockSize (&bytesPerBlock);

    const int samplesPerBlock = bytesPerBlock / 4;
    bool ok = true;

    hr = pimpl->redbook->CreateAudioTrack ((long) numSamples / (bytesPerBlock * 4));

    HeapBlock<byte> buffer (bytesPerBlock);
    AudioSampleBuffer sourceBuffer (2, samplesPerBlock);
    int samplesDone = 0;

    source->prepareToPlay (samplesPerBlock, 44100.0);

    while (ok)
    {
        {
            AudioSourceChannelInfo info (&sourceBuffer, 0, samplesPerBlock);
            sourceBuffer.clear();

            source->getNextAudioBlock (info);
        }

        buffer.clear (bytesPerBlock);

        typedef AudioData::Pointer <AudioData::Int16, AudioData::LittleEndian,
                                    AudioData::Interleaved, AudioData::NonConst> CDSampleFormat;

        typedef AudioData::Pointer <AudioData::Float32, AudioData::NativeEndian,
                                    AudioData::NonInterleaved, AudioData::Const> SourceSampleFormat;

        CDSampleFormat left (buffer, 2);
        left.convertSamples (SourceSampleFormat (sourceBuffer.getReadPointer (0)), samplesPerBlock);
        CDSampleFormat right (buffer + 2, 2);
        right.convertSamples (SourceSampleFormat (sourceBuffer.getReadPointer (1)), samplesPerBlock);

        hr = pimpl->redbook->AddAudioTrackBlocks (buffer, bytesPerBlock);

        if (FAILED (hr))
            ok = false;

        samplesDone += samplesPerBlock;

        if (samplesDone >= numSamples)
            break;
    }

    hr = pimpl->redbook->CloseAudioTrack();
    return ok && hr == S_OK;
}
