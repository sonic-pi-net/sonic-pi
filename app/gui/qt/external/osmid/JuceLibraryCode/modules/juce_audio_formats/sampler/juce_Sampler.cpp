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

SamplerSound::SamplerSound (const String& soundName,
                            AudioFormatReader& source,
                            const BigInteger& notes,
                            int midiNoteForNormalPitch,
                            double attackTimeSecs,
                            double releaseTimeSecs,
                            double maxSampleLengthSeconds)
    : name (soundName),
      sourceSampleRate (source.sampleRate),
      midiNotes (notes),
      midiRootNote (midiNoteForNormalPitch)
{
    if (sourceSampleRate > 0 && source.lengthInSamples > 0)
    {
        length = jmin ((int) source.lengthInSamples,
                       (int) (maxSampleLengthSeconds * sourceSampleRate));

        data = new AudioSampleBuffer (jmin (2, (int) source.numChannels), length + 4);

        source.read (data, 0, length + 4, 0, true, true);

        attackSamples  = roundToInt (attackTimeSecs  * sourceSampleRate);
        releaseSamples = roundToInt (releaseTimeSecs * sourceSampleRate);
    }
}

SamplerSound::~SamplerSound()
{
}

bool SamplerSound::appliesToNote (int midiNoteNumber)
{
    return midiNotes[midiNoteNumber];
}

bool SamplerSound::appliesToChannel (int /*midiChannel*/)
{
    return true;
}

//==============================================================================
SamplerVoice::SamplerVoice() {}
SamplerVoice::~SamplerVoice() {}

bool SamplerVoice::canPlaySound (SynthesiserSound* sound)
{
    return dynamic_cast<const SamplerSound*> (sound) != nullptr;
}

void SamplerVoice::startNote (int midiNoteNumber, float velocity, SynthesiserSound* s, int /*currentPitchWheelPosition*/)
{
    if (auto* sound = dynamic_cast<const SamplerSound*> (s))
    {
        pitchRatio = std::pow (2.0, (midiNoteNumber - sound->midiRootNote) / 12.0)
                        * sound->sourceSampleRate / getSampleRate();

        sourceSamplePosition = 0.0;
        lgain = velocity;
        rgain = velocity;

        isInAttack = (sound->attackSamples > 0);
        isInRelease = false;

        if (isInAttack)
        {
            attackReleaseLevel = 0.0f;
            attackDelta = (float) (pitchRatio / sound->attackSamples);
        }
        else
        {
            attackReleaseLevel = 1.0f;
            attackDelta = 0.0f;
        }

        if (sound->releaseSamples > 0)
            releaseDelta = (float) (-pitchRatio / sound->releaseSamples);
        else
            releaseDelta = -1.0f;
    }
    else
    {
        jassertfalse; // this object can only play SamplerSounds!
    }
}

void SamplerVoice::stopNote (float /*velocity*/, bool allowTailOff)
{
    if (allowTailOff)
    {
        isInAttack = false;
        isInRelease = true;
    }
    else
    {
        clearCurrentNote();
    }
}

void SamplerVoice::pitchWheelMoved (int /*newValue*/) {}
void SamplerVoice::controllerMoved (int /*controllerNumber*/, int /*newValue*/) {}

//==============================================================================
void SamplerVoice::renderNextBlock (AudioSampleBuffer& outputBuffer, int startSample, int numSamples)
{
    if (auto* playingSound = static_cast<SamplerSound*> (getCurrentlyPlayingSound().get()))
    {
        auto& data = *playingSound->data;
        const float* const inL = data.getReadPointer (0);
        const float* const inR = data.getNumChannels() > 1 ? data.getReadPointer (1) : nullptr;

        float* outL = outputBuffer.getWritePointer (0, startSample);
        float* outR = outputBuffer.getNumChannels() > 1 ? outputBuffer.getWritePointer (1, startSample) : nullptr;

        while (--numSamples >= 0)
        {
            auto pos = (int) sourceSamplePosition;
            auto alpha = (float) (sourceSamplePosition - pos);
            auto invAlpha = 1.0f - alpha;

            // just using a very simple linear interpolation here..
            float l = (inL[pos] * invAlpha + inL[pos + 1] * alpha);
            float r = (inR != nullptr) ? (inR[pos] * invAlpha + inR[pos + 1] * alpha)
                                       : l;

            l *= lgain;
            r *= rgain;

            if (isInAttack)
            {
                l *= attackReleaseLevel;
                r *= attackReleaseLevel;

                attackReleaseLevel += attackDelta;

                if (attackReleaseLevel >= 1.0f)
                {
                    attackReleaseLevel = 1.0f;
                    isInAttack = false;
                }
            }
            else if (isInRelease)
            {
                l *= attackReleaseLevel;
                r *= attackReleaseLevel;

                attackReleaseLevel += releaseDelta;

                if (attackReleaseLevel <= 0.0f)
                {
                    stopNote (0.0f, false);
                    break;
                }
            }

            if (outR != nullptr)
            {
                *outL++ += l;
                *outR++ += r;
            }
            else
            {
                *outL++ += (l + r) * 0.5f;
            }

            sourceSamplePosition += pitchRatio;

            if (sourceSamplePosition > playingSound->length)
            {
                stopNote (0.0f, false);
                break;
            }
        }
    }
}

} // namespace juce
