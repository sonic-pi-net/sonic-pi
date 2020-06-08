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
