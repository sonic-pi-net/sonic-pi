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
    Maintains an ongoing measurement of the proportion of time which is being
    spent inside an audio callback.

    @tags{Audio}
*/
class JUCE_API  AudioProcessLoadMeasurer
{
public:
    /** */
    AudioProcessLoadMeasurer();

    /** Destructor. */
    ~AudioProcessLoadMeasurer();

    //==============================================================================
    /** Resets the state. */
    void reset();

    /** Resets the counter, in preparation for use with the given sample rate and block size. */
    void reset (double sampleRate, int blockSize);

    /** Returns the current load as a proportion 0 to 1.0 */
    double getLoadAsProportion() const;

    /** Returns the current load as a percentage 0 to 100.0 */
    double getLoadAsPercentage() const;

    /** Returns the number of over- (or under-) runs recorded since the state was reset. */
    int getXRunCount() const;

    //==============================================================================
    /** This class measures the time between its construction and destruction and
        adds it to an AudioProcessLoadMeasurer.

        e.g.
        @code
        {
            AudioProcessLoadMeasurer::ScopedTimer timer (myProcessLoadMeasurer);
            myCallback->doTheCallback();
        }
        @endcode

        @tags{Audio}
    */
    struct JUCE_API  ScopedTimer
    {
        ScopedTimer (AudioProcessLoadMeasurer&);
        ~ScopedTimer();

    private:
        AudioProcessLoadMeasurer& owner;
        double startTime;

        JUCE_DECLARE_NON_COPYABLE (ScopedTimer)
    };

    /** Can be called manually to add the time of a callback to the stats.
        Normally you probably would never call this - it's simpler and more robust to
        use a ScopedTimer to measure the time using an RAII pattern.
    */
    void registerBlockRenderTime (double millisecondsTaken);

private:
    double cpuUsageMs = 0, timeToCpuScale = 0, msPerBlock = 0;
    int xruns = 0;
};


} // namespace juce
