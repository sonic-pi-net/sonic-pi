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
    A type of AudioSource which can be repositioned.

    The basic AudioSource just streams continuously with no idea of a current
    time or length, so the PositionableAudioSource is used for a finite stream
    that has a current read position.

    @see AudioSource, AudioTransportSource

    @tags{Audio}
*/
class JUCE_API  PositionableAudioSource  : public AudioSource
{
protected:
    //==============================================================================
    /** Creates the PositionableAudioSource. */
    PositionableAudioSource() = default;

public:
    /** Destructor */
    ~PositionableAudioSource() override = default;

    //==============================================================================
    /** Tells the stream to move to a new position.

        Calling this indicates that the next call to AudioSource::getNextAudioBlock()
        should return samples from this position.

        Note that this may be called on a different thread to getNextAudioBlock(),
        so the subclass should make sure it's synchronised.
    */
    virtual void setNextReadPosition (int64 newPosition) = 0;

    /** Returns the position from which the next block will be returned.

        @see setNextReadPosition
    */
    virtual int64 getNextReadPosition() const = 0;

    /** Returns the total length of the stream (in samples). */
    virtual int64 getTotalLength() const = 0;

    /** Returns true if this source is actually playing in a loop. */
    virtual bool isLooping() const = 0;

    /** Tells the source whether you'd like it to play in a loop. */
    virtual void setLooping (bool shouldLoop)       { ignoreUnused (shouldLoop); }
};

} // namespace juce
