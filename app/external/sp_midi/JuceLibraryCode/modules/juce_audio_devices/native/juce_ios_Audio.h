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

class iOSAudioIODeviceType;

class iOSAudioIODevice : public AudioIODevice
{
public:
    //==============================================================================
    String open (const BigInteger&, const BigInteger&, double, int) override;
    void close() override;

    void start (AudioIODeviceCallback*) override;
    void stop() override;

    Array<double> getAvailableSampleRates() override;
    Array<int> getAvailableBufferSizes() override;

    bool setAudioPreprocessingEnabled (bool) override;

    //==============================================================================
    bool isPlaying() override;
    bool isOpen() override;
    String getLastError() override;

    //==============================================================================
    StringArray getOutputChannelNames() override;
    StringArray getInputChannelNames() override;

    int getDefaultBufferSize() override;
    int getCurrentBufferSizeSamples() override;

    double getCurrentSampleRate() override;

    int getCurrentBitDepth() override;

    BigInteger getActiveOutputChannels() const override;
    BigInteger getActiveInputChannels() const override;

    int getOutputLatencyInSamples() override;
    int getInputLatencyInSamples() override;

    int getXRunCount() const noexcept override;

    //==============================================================================
    void setMidiMessageCollector (MidiMessageCollector*);
    AudioPlayHead* getAudioPlayHead() const;

    //==============================================================================
    bool isInterAppAudioConnected() const;
   #if JUCE_MODULE_AVAILABLE_juce_graphics
    Image getIcon (int size);
   #endif
    void switchApplication();

private:
    //==============================================================================
    iOSAudioIODevice (iOSAudioIODeviceType*, const String&, const String&);

    //==============================================================================
    friend class iOSAudioIODeviceType;
    friend struct AudioSessionHolder;

    struct Pimpl;
    friend struct Pimpl;
    std::unique_ptr<Pimpl> pimpl;

    JUCE_DECLARE_NON_COPYABLE (iOSAudioIODevice)
};

} // namespace juce
