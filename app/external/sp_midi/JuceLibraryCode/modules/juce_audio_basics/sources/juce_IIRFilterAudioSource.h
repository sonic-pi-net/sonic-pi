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

//==============================================================================
/**
    An AudioSource that performs an IIR filter on another source.

    @tags{Audio}
*/
class JUCE_API  IIRFilterAudioSource  : public AudioSource
{
public:
    //==============================================================================
    /** Creates a IIRFilterAudioSource for a given input source.

        @param inputSource              the input source to read from - this must not be null
        @param deleteInputWhenDeleted   if true, the input source will be deleted when
                                        this object is deleted
    */
    IIRFilterAudioSource (AudioSource* inputSource,
                          bool deleteInputWhenDeleted);

    /** Destructor. */
    ~IIRFilterAudioSource() override;

    //==============================================================================
    /** Changes the filter to use the same parameters as the one being passed in. */
    void setCoefficients (const IIRCoefficients& newCoefficients);

    /** Calls IIRFilter::makeInactive() on all the filters being used internally. */
    void makeInactive();

    //==============================================================================
    void prepareToPlay (int samplesPerBlockExpected, double sampleRate) override;
    void releaseResources() override;
    void getNextAudioBlock (const AudioSourceChannelInfo&) override;

private:
    //==============================================================================
    OptionalScopedPointer<AudioSource> input;
    OwnedArray<IIRFilter> iirFilters;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (IIRFilterAudioSource)
};

} // namespace juce
