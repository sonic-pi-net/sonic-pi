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

AudioChannelSet::AudioChannelSet (uint32 c) : channels (c) {}
AudioChannelSet::AudioChannelSet (const Array<ChannelType>& c)
{
    for (auto channel : c)
        addChannel (channel);
}

bool AudioChannelSet::operator== (const AudioChannelSet& other) const noexcept  { return channels == other.channels; }
bool AudioChannelSet::operator!= (const AudioChannelSet& other) const noexcept  { return channels != other.channels; }
bool AudioChannelSet::operator<  (const AudioChannelSet& other) const noexcept  { return channels <  other.channels; }

String AudioChannelSet::getChannelTypeName (AudioChannelSet::ChannelType type)
{
    if (type >= discreteChannel0)
        return "Discrete " + String (type - discreteChannel0 + 1);

    switch (type)
    {
        case left:                return NEEDS_TRANS("Left");
        case right:               return NEEDS_TRANS("Right");
        case centre:              return NEEDS_TRANS("Centre");
        case LFE:                 return NEEDS_TRANS("LFE");
        case leftSurround:        return NEEDS_TRANS("Left Surround");
        case rightSurround:       return NEEDS_TRANS("Right Surround");
        case leftCentre:          return NEEDS_TRANS("Left Centre");
        case rightCentre:         return NEEDS_TRANS("Right Centre");
        case centreSurround:      return NEEDS_TRANS("Centre Surround");
        case leftSurroundRear:    return NEEDS_TRANS("Left Surround Rear");
        case rightSurroundRear:   return NEEDS_TRANS("Right Surround Rear");
        case topMiddle:           return NEEDS_TRANS("Top Middle");
        case topFrontLeft:        return NEEDS_TRANS("Top Front Left");
        case topFrontCentre:      return NEEDS_TRANS("Top Front Centre");
        case topFrontRight:       return NEEDS_TRANS("Top Front Right");
        case topRearLeft:         return NEEDS_TRANS("Top Rear Left");
        case topRearCentre:       return NEEDS_TRANS("Top Rear Centre");
        case topRearRight:        return NEEDS_TRANS("Top Rear Right");
        case wideLeft:            return NEEDS_TRANS("Wide Left");
        case wideRight:           return NEEDS_TRANS("Wide Right");
        case LFE2:                return NEEDS_TRANS("LFE 2");
        case leftSurroundSide:    return NEEDS_TRANS("Left Surround Side");
        case rightSurroundSide:   return NEEDS_TRANS("Right Surround Side");
        case ambisonicW:          return NEEDS_TRANS("Ambisonic W");
        case ambisonicX:          return NEEDS_TRANS("Ambisonic X");
        case ambisonicY:          return NEEDS_TRANS("Ambisonic Y");
        case ambisonicZ:          return NEEDS_TRANS("Ambisonic Z");
        case topSideLeft:         return NEEDS_TRANS("Top Side Left");
        case topSideRight:        return NEEDS_TRANS("Top Side Right");
        default:                  break;
    }

    return "Unknown";
}

String AudioChannelSet::getAbbreviatedChannelTypeName (AudioChannelSet::ChannelType type)
{
    if (type >= discreteChannel0)
        return String (type - discreteChannel0 + 1);

    switch (type)
    {
        case left:                return "L";
        case right:               return "R";
        case centre:              return "C";
        case LFE:                 return "Lfe";
        case leftSurround:        return "Ls";
        case rightSurround:       return "Rs";
        case leftCentre:          return "Lc";
        case rightCentre:         return "Rc";
        case centreSurround:      return "Cs";
        case leftSurroundRear:    return "Lrs";
        case rightSurroundRear:   return "Rrs";
        case topMiddle:           return "Tm";
        case topFrontLeft:        return "Tfl";
        case topFrontCentre:      return "Tfc";
        case topFrontRight:       return "Tfr";
        case topRearLeft:         return "Trl";
        case topRearCentre:       return "Trc";
        case topRearRight:        return "Trr";
        case wideLeft:            return "Wl";
        case wideRight:           return "Wr";
        case LFE2:                return "Lfe2";
        case leftSurroundSide:    return "Lss";
        case rightSurroundSide:   return "Rss";
        case ambisonicW:          return "W";
        case ambisonicX:          return "X";
        case ambisonicY:          return "Y";
        case ambisonicZ:          return "Z";
        case topSideLeft:         return "Tsl";
        case topSideRight:        return "Tsr";
        default:                  break;
    }

    return {};
}

AudioChannelSet::ChannelType AudioChannelSet::getChannelTypeFromAbbreviation (const String& abbr)
{
    if (abbr.length() > 0 && (abbr[0] >= '0' && abbr[0] <= '9'))
        return static_cast<AudioChannelSet::ChannelType> (static_cast<int> (discreteChannel0)
                                                               + abbr.getIntValue() + 1);

    if (abbr == "L")    return left;
    if (abbr == "R")    return right;
    if (abbr == "C")    return centre;
    if (abbr == "Lfe")  return LFE;
    if (abbr == "Ls")   return leftSurround;
    if (abbr == "Rs")   return rightSurround;
    if (abbr == "Lc")   return leftCentre;
    if (abbr == "Rc")   return rightCentre;
    if (abbr == "Cs")   return centreSurround;
    if (abbr == "Lrs")  return leftSurroundRear;
    if (abbr == "Rrs")  return rightSurroundRear;
    if (abbr == "Tm")   return topMiddle;
    if (abbr == "Tfl")  return topFrontLeft;
    if (abbr == "Tfc")  return topFrontCentre;
    if (abbr == "Tfr")  return topFrontRight;
    if (abbr == "Trl")  return topRearLeft;
    if (abbr == "Trc")  return topRearCentre;
    if (abbr == "Trr")  return topRearRight;
    if (abbr == "Wl")   return wideLeft;
    if (abbr == "Wr")   return wideRight;
    if (abbr == "Lfe2") return LFE2;
    if (abbr == "Lss")  return leftSurroundSide;
    if (abbr == "Rss")  return rightSurroundSide;
    if (abbr == "W")    return ambisonicW;
    if (abbr == "X")    return ambisonicX;
    if (abbr == "Y")    return ambisonicY;
    if (abbr == "Z")    return ambisonicZ;
    if (abbr == "Tsl")  return topSideLeft;
    if (abbr == "Tsr")  return topSideRight;

    return unknown;
}

String AudioChannelSet::getSpeakerArrangementAsString() const
{
    StringArray speakerTypes;

    for (auto& speaker : getChannelTypes())
    {
        auto name = getAbbreviatedChannelTypeName (speaker);

        if (name.isNotEmpty())
            speakerTypes.add (name);
    }

    return speakerTypes.joinIntoString (" ");
}

AudioChannelSet AudioChannelSet::fromAbbreviatedString (const String& str)
{
    AudioChannelSet set;

    for (auto& abbr : StringArray::fromTokens (str, true))
    {
        auto type = getChannelTypeFromAbbreviation (abbr);

        if (type != unknown)
            set.addChannel (type);
    }

    return set;
}

String AudioChannelSet::getDescription() const
{
    if (isDiscreteLayout())            return "Discrete #" + String (size());
    if (*this == disabled())           return "Disabled";
    if (*this == mono())               return "Mono";
    if (*this == stereo())             return "Stereo";

    if (*this == createLCR())          return "LCR";
    if (*this == createLRS())          return "LRS";
    if (*this == createLCRS())         return "LCRS";

    if (*this == create5point0())       return "5.0 Surround";
    if (*this == create5point1())       return "5.1 Surround";
    if (*this == create6point0())       return "6.0 Surround";
    if (*this == create6point1())       return "6.1 Surround";
    if (*this == create6point0Music())  return "6.0 (Music) Surround";
    if (*this == create6point1Music())  return "6.1 (Music) Surround";
    if (*this == create7point0())       return "7.0 Surround";
    if (*this == create7point1())       return "7.1 Surround";
    if (*this == create7point0SDDS())   return "7.0 Surround SDDS";
    if (*this == create7point1SDDS())   return "7.1 Surround SDDS";
    if (*this == create7point0point2()) return "7.0.2 Surround";
    if (*this == create7point1point2()) return "7.1.2 Surround";

    if (*this == quadraphonic())       return "Quadraphonic";
    if (*this == pentagonal())         return "Pentagonal";
    if (*this == hexagonal())          return "Hexagonal";
    if (*this == octagonal())          return "Octagonal";
    if (*this == ambisonic())          return "Ambisonic";

    return "Unknown";
}

bool AudioChannelSet::isDiscreteLayout() const noexcept
{
    for (auto& speaker : getChannelTypes())
        if (speaker <= topSideRight)
            return false;

    return true;
}

int AudioChannelSet::size() const noexcept
{
    return channels.countNumberOfSetBits();
}

AudioChannelSet::ChannelType AudioChannelSet::getTypeOfChannel (int index) const noexcept
{
    int bit = channels.findNextSetBit(0);

    for (int i = 0; i < index && bit >= 0; ++i)
        bit = channels.findNextSetBit (bit + 1);

    return static_cast<ChannelType> (bit);
}

int AudioChannelSet::getChannelIndexForType (AudioChannelSet::ChannelType type) const noexcept
{
    int idx = 0;

    for (int bit = channels.findNextSetBit (0); bit >= 0; bit = channels.findNextSetBit (bit + 1))
    {
        if (static_cast<ChannelType> (bit) == type)
            return idx;

        idx++;
    }

    return -1;
}

Array<AudioChannelSet::ChannelType> AudioChannelSet::getChannelTypes() const
{
    Array<ChannelType> result;

    for (int bit = channels.findNextSetBit(0); bit >= 0; bit = channels.findNextSetBit (bit + 1))
        result.add (static_cast<ChannelType> (bit));

    return result;
}

void AudioChannelSet::addChannel (ChannelType newChannel)
{
    const int bit = static_cast<int> (newChannel);
    jassert (bit >= 0 && bit < 1024);
    channels.setBit (bit);
}

void AudioChannelSet::removeChannel (ChannelType newChannel)
{
    const int bit = static_cast<int> (newChannel);
    jassert (bit >= 0 && bit < 1024);
    channels.clearBit (bit);
}

AudioChannelSet AudioChannelSet::disabled()            { return {}; }
AudioChannelSet AudioChannelSet::mono()                { return AudioChannelSet (1u << centre); }
AudioChannelSet AudioChannelSet::stereo()              { return AudioChannelSet ((1u << left) | (1u << right)); }
AudioChannelSet AudioChannelSet::createLCR()           { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre)); }
AudioChannelSet AudioChannelSet::createLRS()           { return AudioChannelSet ((1u << left) | (1u << right) | (1u << surround)); }
AudioChannelSet AudioChannelSet::createLCRS()          { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << surround)); }
AudioChannelSet AudioChannelSet::create5point0()       { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << leftSurround) | (1u << rightSurround)); }
AudioChannelSet AudioChannelSet::create5point1()       { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << LFE) | (1u << leftSurround) | (1u << rightSurround)); }
AudioChannelSet AudioChannelSet::create6point0()       { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << leftSurround) | (1u << rightSurround) | (1u << centreSurround)); }
AudioChannelSet AudioChannelSet::create6point1()       { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << LFE) | (1u << leftSurround) | (1u << rightSurround) | (1u << centreSurround)); }
AudioChannelSet AudioChannelSet::create6point0Music()  { return AudioChannelSet ((1u << left) | (1u << right) | (1u << leftSurround) | (1u << rightSurround) | (1u << leftSurroundSide) | (1u << rightSurroundSide)); }
AudioChannelSet AudioChannelSet::create6point1Music()  { return AudioChannelSet ((1u << left) | (1u << right) | (1u << LFE) | (1u << leftSurround) | (1u << rightSurround) | (1u << leftSurroundSide) | (1u << rightSurroundSide)); }
AudioChannelSet AudioChannelSet::create7point0()       { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << leftSurroundSide) | (1u << rightSurroundSide) | (1u << leftSurroundRear) | (1u << rightSurroundRear)); }
AudioChannelSet AudioChannelSet::create7point0SDDS()   { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << leftSurround) | (1u << rightSurround) | (1u << leftCentre) | (1u << rightCentre)); }
AudioChannelSet AudioChannelSet::create7point1()       { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << LFE) | (1u << leftSurroundSide) | (1u << rightSurroundSide) | (1u << leftSurroundRear) | (1u << rightSurroundRear)); }
AudioChannelSet AudioChannelSet::create7point1SDDS()   { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << LFE) | (1u << leftSurround) | (1u << rightSurround) | (1u << leftCentre) | (1u << rightCentre)); }
AudioChannelSet AudioChannelSet::ambisonic()           { return AudioChannelSet ((1u << ambisonicW) | (1u << ambisonicX) | (1u << ambisonicY) | (1u << ambisonicZ)); }
AudioChannelSet AudioChannelSet::quadraphonic()        { return AudioChannelSet ((1u << left) | (1u << right) | (1u << leftSurround) | (1u << rightSurround)); }
AudioChannelSet AudioChannelSet::pentagonal()          { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << leftSurroundRear) | (1u << rightSurroundRear)); }
AudioChannelSet AudioChannelSet::hexagonal()           { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << centreSurround) | (1u << leftSurroundRear) | (1u << rightSurroundRear)); }
AudioChannelSet AudioChannelSet::octagonal()           { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << leftSurround) | (1u << rightSurround) | (1u << centreSurround) | (1u << wideLeft) | (1u << wideRight)); }
AudioChannelSet AudioChannelSet::create7point0point2() { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << leftSurroundSide) | (1u << rightSurroundSide) | (1u << leftSurroundRear) | (1u << rightSurroundRear) | (1u << topSideLeft) | (1u << topSideRight)); }
AudioChannelSet AudioChannelSet::create7point1point2() { return AudioChannelSet ((1u << left) | (1u << right) | (1u << centre) | (1u << LFE) | (1u << leftSurroundSide) | (1u << rightSurroundSide) | (1u << leftSurroundRear) | (1u << rightSurroundRear) | (1u << topSideLeft) | (1u << topSideRight)); }


AudioChannelSet AudioChannelSet::discreteChannels (int numChannels)
{
    AudioChannelSet s;
    s.channels.setRange (discreteChannel0, numChannels, true);
    return s;
}

AudioChannelSet AudioChannelSet::canonicalChannelSet (int numChannels)
{
    if (numChannels == 1)  return AudioChannelSet::mono();
    if (numChannels == 2)  return AudioChannelSet::stereo();
    if (numChannels == 3)  return AudioChannelSet::createLCR();
    if (numChannels == 4)  return AudioChannelSet::quadraphonic();
    if (numChannels == 5)  return AudioChannelSet::create5point0();
    if (numChannels == 6)  return AudioChannelSet::create5point1();
    if (numChannels == 7)  return AudioChannelSet::create7point0();
    if (numChannels == 8)  return AudioChannelSet::create7point1();

    return discreteChannels (numChannels);
}

AudioChannelSet AudioChannelSet::namedChannelSet (int numChannels)
{
    if (numChannels == 1)  return AudioChannelSet::mono();
    if (numChannels == 2)  return AudioChannelSet::stereo();
    if (numChannels == 3)  return AudioChannelSet::createLCR();
    if (numChannels == 4)  return AudioChannelSet::quadraphonic();
    if (numChannels == 5)  return AudioChannelSet::create5point0();
    if (numChannels == 6)  return AudioChannelSet::create5point1();
    if (numChannels == 7)  return AudioChannelSet::create7point0();
    if (numChannels == 8)  return AudioChannelSet::create7point1();

    return {};
}

Array<AudioChannelSet> AudioChannelSet::channelSetsWithNumberOfChannels (int numChannels)
{
    Array<AudioChannelSet> retval;

    if (numChannels != 0)
    {
        retval.add (AudioChannelSet::discreteChannels (numChannels));

        if      (numChannels == 1)
        {
            retval.add (AudioChannelSet::mono());
        }
        else if (numChannels == 2)
        {
            retval.add (AudioChannelSet::stereo());
        }
        else if (numChannels == 3)
        {
            retval.add (AudioChannelSet::createLCR());
            retval.add (AudioChannelSet::createLRS());
        }
        else if (numChannels == 4)
        {
            retval.add (AudioChannelSet::quadraphonic());
            retval.add (AudioChannelSet::createLCRS());
            retval.add (AudioChannelSet::ambisonic());
        }
        else if (numChannels == 5)
        {
            retval.add (AudioChannelSet::create5point0());
            retval.add (AudioChannelSet::pentagonal());
        }
        else if (numChannels == 6)
        {
            retval.add (AudioChannelSet::create5point1());
            retval.add (AudioChannelSet::create6point0());
            retval.add (AudioChannelSet::create6point0Music());
            retval.add (AudioChannelSet::hexagonal());
        }
        else if (numChannels == 7)
        {
            retval.add (AudioChannelSet::create7point0());
            retval.add (AudioChannelSet::create7point0SDDS());
            retval.add (AudioChannelSet::create6point1());
            retval.add (AudioChannelSet::create6point1Music());
        }
        else if (numChannels == 8)
        {
            retval.add (AudioChannelSet::create7point1());
            retval.add (AudioChannelSet::create7point1SDDS());
            retval.add (AudioChannelSet::octagonal());
        }
    }

    return retval;
}

AudioChannelSet JUCE_CALLTYPE AudioChannelSet::channelSetWithChannels (const Array<ChannelType>& channelArray)
{
    AudioChannelSet set;

    for (auto ch : channelArray)
    {
        jassert (! set.channels[static_cast<int> (ch)]);

        set.addChannel (ch);
    }

    return set;
}

//==============================================================================
AudioChannelSet JUCE_CALLTYPE AudioChannelSet::fromWaveChannelMask (int32 dwChannelMask)
{
    return AudioChannelSet (static_cast<uint32> ((dwChannelMask & ((1 << 18) - 1)) << 1));
}

int32 AudioChannelSet::getWaveChannelMask() const noexcept
{
    if (channels.getHighestBit() > topRearRight)
        return -1;

    return (channels.toInteger() >> 1);
}

} // namespace juce
