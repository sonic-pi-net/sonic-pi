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

#if ! DOXYGEN && (JUCE_MAC || JUCE_IOS)

struct CoreAudioLayouts
{
    //==============================================================================
    enum
    {
        coreAudioHOASN3DLayoutTag = (190U<<16) | 0 // kAudioChannelLayoutTag_HOA_ACN_SN3D
    };

    //==============================================================================
    /** Convert CoreAudio's native AudioChannelLayout to JUCE's AudioChannelSet.

        Note that this method cannot preserve the order of channels.
    */
    static AudioChannelSet fromCoreAudio (const AudioChannelLayout& layout)
    {
        return AudioChannelSet::channelSetWithChannels (getCoreAudioLayoutChannels (layout));
    }

    /** Convert CoreAudio's native AudioChannelLayoutTag to JUCE's AudioChannelSet.

        Note that this method cannot preserve the order of channels.
    */
    static AudioChannelSet fromCoreAudio (AudioChannelLayoutTag layoutTag)
    {
        return AudioChannelSet::channelSetWithChannels (getSpeakerLayoutForCoreAudioTag (layoutTag));
    }

    /** Convert JUCE's AudioChannelSet to CoreAudio's AudioChannelLayoutTag.

        Note that this method cannot preserve the order of channels.
    */
    static AudioChannelLayoutTag toCoreAudio (const AudioChannelSet& set)
    {
        if (set.getAmbisonicOrder() >= 0)
            return coreAudioHOASN3DLayoutTag | static_cast<unsigned> (set.size());

        for (auto* tbl = SpeakerLayoutTable::get(); tbl->tag != 0; ++tbl)
        {
            AudioChannelSet caSet;

            for (int i = 0; i < numElementsInArray (tbl->channelTypes)
                 && tbl->channelTypes[i] != AudioChannelSet::unknown; ++i)
                caSet.addChannel (tbl->channelTypes[i]);

            if (caSet == set)
                return tbl->tag;
        }

        return kAudioChannelLayoutTag_DiscreteInOrder | static_cast<AudioChannelLayoutTag> (set.size());
    }

    static const Array<AudioChannelLayoutTag>& getKnownCoreAudioTags()
    {
        static Array<AudioChannelLayoutTag> tags (createKnownCoreAudioTags());
        return tags;
    }

    //==============================================================================
    /** Convert CoreAudio's native AudioChannelLayout to an array of JUCE ChannelTypes. */
    static Array<AudioChannelSet::ChannelType> getCoreAudioLayoutChannels (const AudioChannelLayout& layout)
    {
        switch (layout.mChannelLayoutTag & 0xffff0000)
        {
            case kAudioChannelLayoutTag_UseChannelBitmap:
                return AudioChannelSet::fromWaveChannelMask (static_cast<int> (layout.mChannelBitmap)).getChannelTypes();
            case kAudioChannelLayoutTag_UseChannelDescriptions:
            {
                Array<AudioChannelSet::ChannelType> channels;

                for (UInt32 i = 0; i < layout.mNumberChannelDescriptions; ++i)
                    channels.addIfNotAlreadyThere (getChannelTypeFromAudioChannelLabel (layout.mChannelDescriptions[i].mChannelLabel));

                // different speaker mappings may point to the same JUCE speaker so fill up
                // this array with discrete channels
                for (int j = 0; channels.size() < static_cast<int> (layout.mNumberChannelDescriptions); ++j)
                    channels.addIfNotAlreadyThere (static_cast<AudioChannelSet::ChannelType> (AudioChannelSet::discreteChannel0 + j));

                return channels;
            }
            case kAudioChannelLayoutTag_DiscreteInOrder:
                return AudioChannelSet::discreteChannels (static_cast<int> (layout.mChannelLayoutTag) & 0xffff).getChannelTypes();
            default:
                break;
        }

        return getSpeakerLayoutForCoreAudioTag (layout.mChannelLayoutTag);
    }

    static Array<AudioChannelSet::ChannelType> getSpeakerLayoutForCoreAudioTag (AudioChannelLayoutTag tag)
    {
        // You need to specify the full AudioChannelLayout when using
        // the UseChannelBitmap and UseChannelDescriptions layout tag
        jassert (tag != kAudioChannelLayoutTag_UseChannelBitmap && tag != kAudioChannelLayoutTag_UseChannelDescriptions);

        Array<AudioChannelSet::ChannelType> speakers;

        for (auto* tbl = SpeakerLayoutTable::get(); tbl->tag != 0; ++tbl)
        {
            if (tag == tbl->tag)
            {
                for (int i = 0; i < numElementsInArray (tbl->channelTypes)
                                  && tbl->channelTypes[i] != AudioChannelSet::unknown; ++i)
                    speakers.add (tbl->channelTypes[i]);

                return speakers;
            }
        }

        auto numChannels = tag & 0xffff;
        if (tag >= coreAudioHOASN3DLayoutTag && tag <= (coreAudioHOASN3DLayoutTag | 0xffff))
        {
            auto sqrtMinusOne   = std::sqrt (static_cast<float> (numChannels)) - 1.0f;
            auto ambisonicOrder = jmax (0, static_cast<int> (std::floor (sqrtMinusOne)));

            if (static_cast<float> (ambisonicOrder) == sqrtMinusOne)
                return AudioChannelSet::ambisonic (ambisonicOrder).getChannelTypes();
        }

        for (UInt32 i = 0; i < numChannels; ++i)
            speakers.add (static_cast<AudioChannelSet::ChannelType> (AudioChannelSet::discreteChannel0 + i));

        return speakers;
    }

private:
    //==============================================================================
    struct LayoutTagSpeakerList
    {
        AudioChannelLayoutTag tag;
        AudioChannelSet::ChannelType channelTypes[16];
    };

    static Array<AudioChannelLayoutTag> createKnownCoreAudioTags()
    {
        Array<AudioChannelLayoutTag> tags;

        for (auto* tbl = SpeakerLayoutTable::get(); tbl->tag != 0; ++tbl)
            tags.addIfNotAlreadyThere (tbl->tag);

        for (unsigned order = 0; order <= 5; ++order)
            tags.addIfNotAlreadyThere (coreAudioHOASN3DLayoutTag | ((order + 1) * (order + 1)));

        return tags;
    }

    //==============================================================================
    // This list has been derived from https://pastebin.com/24dQ4BPJ
    // Apple channel labels have been replaced by JUCE channel names
    // This means that some layouts will be identical in JUCE but not in CoreAudio

    // In Apple's official definition the following tags exist with the same speaker layout and order
    // even when *not* represented in JUCE channels
    // kAudioChannelLayoutTag_Binaural = kAudioChannelLayoutTag_Stereo
    // kAudioChannelLayoutTag_MPEG_5_0_B = kAudioChannelLayoutTag_Pentagonal
    // kAudioChannelLayoutTag_ITU_2_2 = kAudioChannelLayoutTag_Quadraphonic
    // kAudioChannelLayoutTag_AudioUnit_6_0 = kAudioChannelLayoutTag_Hexagonal
    struct SpeakerLayoutTable : AudioChannelSet // save us some typing
    {
        static LayoutTagSpeakerList* get() noexcept
        {
            static LayoutTagSpeakerList tbl[] = {
                // list layouts for which there is a corresponding named AudioChannelSet first
                { kAudioChannelLayoutTag_Mono, { centre } },
                { kAudioChannelLayoutTag_Stereo, { left, right } },
                { kAudioChannelLayoutTag_MPEG_3_0_A, { left, right, centre } },
                { kAudioChannelLayoutTag_ITU_2_1, { left, right, centreSurround } },
                { kAudioChannelLayoutTag_MPEG_4_0_A, { left, right, centre, centreSurround } },
                { kAudioChannelLayoutTag_MPEG_5_0_A, { left, right, centre, leftSurround, rightSurround } },
                { kAudioChannelLayoutTag_MPEG_5_1_A, { left, right, centre, LFE, leftSurround, rightSurround } },
                { kAudioChannelLayoutTag_AudioUnit_6_0, { left, right, leftSurround, rightSurround, centre, centreSurround } },
                { kAudioChannelLayoutTag_MPEG_6_1_A, { left, right, centre, LFE, leftSurround, rightSurround, centreSurround } },
                { kAudioChannelLayoutTag_DTS_6_0_A, { leftSurroundSide, rightSurroundSide, left, right, leftSurround, rightSurround } },
                { kAudioChannelLayoutTag_DTS_6_1_A, { leftSurroundSide, rightSurroundSide, left, right, leftSurround, rightSurround, LFE } },
                { kAudioChannelLayoutTag_AudioUnit_7_0, { left, right, leftSurroundSide, rightSurroundSide, centre, leftSurroundRear, rightSurroundRear } },
                { kAudioChannelLayoutTag_AudioUnit_7_0_Front, { left, right, leftSurround, rightSurround, centre, leftCentre, rightCentre } },
                { kAudioChannelLayoutTag_MPEG_7_1_C, { left, right, centre, LFE, leftSurroundSide, rightSurroundSide, leftSurroundRear, rightSurroundRear } },
                { kAudioChannelLayoutTag_MPEG_7_1_A, { left, right, centre, LFE, leftSurround, rightSurround, leftCentre, rightCentre } },
                { kAudioChannelLayoutTag_Ambisonic_B_Format, { ambisonicW, ambisonicX, ambisonicY, ambisonicZ } },
                { kAudioChannelLayoutTag_Quadraphonic, { left, right, leftSurround, rightSurround } },
                { kAudioChannelLayoutTag_Pentagonal, { left, right, leftSurroundRear, rightSurroundRear, centre } },
                { kAudioChannelLayoutTag_Hexagonal, { left, right, leftSurroundRear, rightSurroundRear, centre, centreSurround } },
                { kAudioChannelLayoutTag_Octagonal, { left, right, leftSurround, rightSurround, centre, centreSurround, wideLeft, wideRight } },

                // more uncommon layouts
                { kAudioChannelLayoutTag_StereoHeadphones, { left, right } },
                { kAudioChannelLayoutTag_MatrixStereo, { left, right } },
                { kAudioChannelLayoutTag_MidSide, { centre, discreteChannel0 } },
                { kAudioChannelLayoutTag_XY, { ambisonicX, ambisonicY } },
                { kAudioChannelLayoutTag_Binaural, { left, right } },
                { kAudioChannelLayoutTag_Cube, { left, right, leftSurround, rightSurround, topFrontLeft, topFrontRight, topRearLeft, topRearRight } },
                { kAudioChannelLayoutTag_MPEG_3_0_B, { centre, left, right } },
                { kAudioChannelLayoutTag_MPEG_4_0_B, { centre, left, right, centreSurround } },
                { kAudioChannelLayoutTag_MPEG_5_0_B, { left, right, leftSurround, rightSurround, centre } },
                { kAudioChannelLayoutTag_MPEG_5_0_C, { left, centre, right, leftSurround, rightSurround } },
                { kAudioChannelLayoutTag_MPEG_5_0_D, { centre, left, right, leftSurround, rightSurround } },
                { kAudioChannelLayoutTag_MPEG_5_1_B, { left, right, leftSurround, rightSurround, centre, LFE } },
                { kAudioChannelLayoutTag_MPEG_5_1_C, { left, centre, right, leftSurround, rightSurround, LFE } },
                { kAudioChannelLayoutTag_MPEG_5_1_D, { centre, left, right, leftSurround, rightSurround, LFE } },
                { kAudioChannelLayoutTag_MPEG_7_1_B, { centre, leftCentre, rightCentre, left, right, leftSurround, rightSurround, LFE } },
                { kAudioChannelLayoutTag_Emagic_Default_7_1, { left, right, leftSurround, rightSurround, centre, LFE, leftCentre, rightCentre } },
                { kAudioChannelLayoutTag_SMPTE_DTV, { left, right, centre, LFE, leftSurround, rightSurround, discreteChannel0 /* leftMatrixTotal */, (ChannelType) (discreteChannel0 + 1) /* rightMatrixTotal */} },
                { kAudioChannelLayoutTag_ITU_2_2, { left, right, leftSurround, rightSurround } },
                { kAudioChannelLayoutTag_DVD_4, { left, right, LFE } },
                { kAudioChannelLayoutTag_DVD_5, { left, right, LFE, centreSurround } },
                { kAudioChannelLayoutTag_DVD_6, { left, right, LFE, leftSurround, rightSurround } },
                { kAudioChannelLayoutTag_DVD_10, { left, right, centre, LFE } },
                { kAudioChannelLayoutTag_DVD_11, { left, right, centre, LFE, centreSurround } },
                { kAudioChannelLayoutTag_DVD_18, { left, right, leftSurround, rightSurround, LFE } },
                { kAudioChannelLayoutTag_AAC_6_0, { centre, left, right, leftSurround, rightSurround, centreSurround } },
                { kAudioChannelLayoutTag_AAC_6_1, { centre, left, right, leftSurround, rightSurround, centreSurround, LFE } },
                { kAudioChannelLayoutTag_AAC_7_0, { centre, left, right, leftSurround, rightSurround, leftSurroundRear, rightSurroundRear } },
                { kAudioChannelLayoutTag_AAC_7_1_B, { centre, left, right, leftSurround, rightSurround, leftSurroundRear, rightSurroundRear, LFE } },
                { kAudioChannelLayoutTag_AAC_7_1_C, { centre, left, right, leftSurround, rightSurround, LFE, topFrontLeft, topFrontRight } },
                { kAudioChannelLayoutTag_AAC_Octagonal, { centre, left, right, leftSurround, rightSurround, leftSurroundRear, rightSurroundRear, centreSurround } },
                { kAudioChannelLayoutTag_TMH_10_2_std, { left, right, centre, topFrontCentre, leftSurroundSide, rightSurroundSide, leftSurround, rightSurround, topFrontLeft, topFrontRight, wideLeft, wideRight, topRearCentre, centreSurround, LFE, LFE2 } },
                { kAudioChannelLayoutTag_AC3_1_0_1, { centre, LFE } },
                { kAudioChannelLayoutTag_AC3_3_0, { left, centre, right } },
                { kAudioChannelLayoutTag_AC3_3_1, { left, centre, right, centreSurround } },
                { kAudioChannelLayoutTag_AC3_3_0_1, { left, centre, right, LFE } },
                { kAudioChannelLayoutTag_AC3_2_1_1, { left, right, centreSurround, LFE } },
                { kAudioChannelLayoutTag_AC3_3_1_1, { left, centre, right, centreSurround, LFE } },
                { kAudioChannelLayoutTag_EAC_6_0_A, { left, centre, right, leftSurround, rightSurround, centreSurround } },
                { kAudioChannelLayoutTag_EAC_7_0_A, { left, centre, right, leftSurround, rightSurround, leftSurroundRear, rightSurroundRear } },
                { kAudioChannelLayoutTag_EAC3_6_1_A, { left, centre, right, leftSurround, rightSurround, LFE, centreSurround } },
                { kAudioChannelLayoutTag_EAC3_6_1_B, { left, centre, right, leftSurround, rightSurround, LFE, centreSurround } },
                { kAudioChannelLayoutTag_EAC3_6_1_C, { left, centre, right, leftSurround, rightSurround, LFE, topFrontCentre } },
                { kAudioChannelLayoutTag_EAC3_7_1_A, { left, centre, right, leftSurround, rightSurround, LFE, leftSurroundRear, rightSurroundRear } },
                { kAudioChannelLayoutTag_EAC3_7_1_B, { left, centre, right, leftSurround, rightSurround, LFE, leftCentre, rightCentre } },
                { kAudioChannelLayoutTag_EAC3_7_1_C, { left, centre, right, leftSurround, rightSurround, LFE, leftSurroundSide, rightSurroundSide } },
                { kAudioChannelLayoutTag_EAC3_7_1_D, { left, centre, right, leftSurround, rightSurround, LFE, wideLeft, wideRight } },
                { kAudioChannelLayoutTag_EAC3_7_1_E, { left, centre, right, leftSurround, rightSurround, LFE, topFrontLeft, topFrontRight } },
                { kAudioChannelLayoutTag_EAC3_7_1_F, { left, centre, right, leftSurround, rightSurround, LFE, centreSurround, topMiddle } },
                { kAudioChannelLayoutTag_EAC3_7_1_G, { left, centre, right, leftSurround, rightSurround, LFE, centreSurround, topFrontCentre } },
                { kAudioChannelLayoutTag_EAC3_7_1_H, { left, centre, right, leftSurround, rightSurround, LFE, centreSurround, topFrontCentre } },
                { kAudioChannelLayoutTag_DTS_3_1, { centre, left, right, LFE } },
                { kAudioChannelLayoutTag_DTS_4_1, { centre, left, right, centreSurround, LFE } },
                { kAudioChannelLayoutTag_DTS_6_0_B, { centre, left, right, leftSurroundRear, rightSurroundRear, centreSurround } },
                { kAudioChannelLayoutTag_DTS_6_0_C, { centre, centreSurround, left, right, leftSurroundRear, rightSurroundRear } },
                { kAudioChannelLayoutTag_DTS_6_1_B, { centre, left, right, leftSurroundRear, rightSurroundRear, centreSurround, LFE } },
                { kAudioChannelLayoutTag_DTS_6_1_C, { centre, centreSurround, left, right, leftSurroundRear, rightSurroundRear, LFE } },
                { kAudioChannelLayoutTag_DTS_6_1_D, { centre, left, right, leftSurround, rightSurround, LFE, centreSurround } },
                { kAudioChannelLayoutTag_DTS_7_0, { leftCentre, centre, rightCentre, left, right, leftSurround, rightSurround } },
                { kAudioChannelLayoutTag_DTS_7_1, { leftCentre, centre, rightCentre, left, right, leftSurround, rightSurround, LFE } },
                { kAudioChannelLayoutTag_DTS_8_0_A, { leftCentre, rightCentre, left, right, leftSurround, rightSurround, leftSurroundRear, rightSurroundRear } },
                { kAudioChannelLayoutTag_DTS_8_0_B, { leftCentre, centre, rightCentre, left, right, leftSurround, centreSurround, rightSurround } },
                { kAudioChannelLayoutTag_DTS_8_1_A, { leftCentre, rightCentre, left, right, leftSurround, rightSurround, leftSurroundRear, rightSurroundRear, LFE } },
                { kAudioChannelLayoutTag_DTS_8_1_B, { leftCentre, centre, rightCentre, left, right, leftSurround, centreSurround, rightSurround, LFE } },
                { 0, {} }
            };

            return tbl;
        }
    };

    //==============================================================================
    static AudioChannelSet::ChannelType getChannelTypeFromAudioChannelLabel (AudioChannelLabel label) noexcept
    {
        if (label >= kAudioChannelLabel_Discrete_0 && label <= kAudioChannelLabel_Discrete_65535)
        {
            const unsigned int discreteChannelNum = label - kAudioChannelLabel_Discrete_0;
            return static_cast<AudioChannelSet::ChannelType> (AudioChannelSet::discreteChannel0 + discreteChannelNum);
        }

        switch (label)
        {
            case kAudioChannelLabel_Center:
            case kAudioChannelLabel_Mono:                   return AudioChannelSet::centre;
            case kAudioChannelLabel_Left:
            case kAudioChannelLabel_HeadphonesLeft:         return AudioChannelSet::left;
            case kAudioChannelLabel_Right:
            case kAudioChannelLabel_HeadphonesRight:        return AudioChannelSet::right;
            case kAudioChannelLabel_LFEScreen:              return AudioChannelSet::LFE;
            case kAudioChannelLabel_LeftSurround:           return AudioChannelSet::leftSurround;
            case kAudioChannelLabel_RightSurround:          return AudioChannelSet::rightSurround;
            case kAudioChannelLabel_LeftCenter:             return AudioChannelSet::leftCentre;
            case kAudioChannelLabel_RightCenter:            return AudioChannelSet::rightCentre;
            case kAudioChannelLabel_CenterSurround:         return AudioChannelSet::surround;
            case kAudioChannelLabel_LeftSurroundDirect:     return AudioChannelSet::leftSurroundSide;
            case kAudioChannelLabel_RightSurroundDirect:    return AudioChannelSet::rightSurroundSide;
            case kAudioChannelLabel_TopCenterSurround:      return AudioChannelSet::topMiddle;
            case kAudioChannelLabel_VerticalHeightLeft:     return AudioChannelSet::topFrontLeft;
            case kAudioChannelLabel_VerticalHeightRight:    return AudioChannelSet::topFrontRight;
            case kAudioChannelLabel_VerticalHeightCenter:   return AudioChannelSet::topFrontCentre;
            case kAudioChannelLabel_TopBackLeft:            return AudioChannelSet::topRearLeft;
            case kAudioChannelLabel_RearSurroundLeft:       return AudioChannelSet::leftSurroundRear;
            case kAudioChannelLabel_TopBackRight:           return AudioChannelSet::topRearRight;
            case kAudioChannelLabel_RearSurroundRight:      return AudioChannelSet::rightSurroundRear;
            case kAudioChannelLabel_TopBackCenter:          return AudioChannelSet::topRearCentre;
            case kAudioChannelLabel_LFE2:                   return AudioChannelSet::LFE2;
            case kAudioChannelLabel_LeftWide:               return AudioChannelSet::wideLeft;
            case kAudioChannelLabel_RightWide:              return AudioChannelSet::wideRight;
            case kAudioChannelLabel_Ambisonic_W:            return AudioChannelSet::ambisonicW;
            case kAudioChannelLabel_Ambisonic_X:            return AudioChannelSet::ambisonicX;
            case kAudioChannelLabel_Ambisonic_Y:            return AudioChannelSet::ambisonicY;
            case kAudioChannelLabel_Ambisonic_Z:            return AudioChannelSet::ambisonicZ;
            default:                                        return AudioChannelSet::unknown;
        }
    }
};

#endif

} // namespace juce
