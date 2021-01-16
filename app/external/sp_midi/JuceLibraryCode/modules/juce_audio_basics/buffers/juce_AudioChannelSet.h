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
    Represents a set of audio channel types.

    For example, you might have a set of left + right channels, which is a stereo
    channel set. It is a collection of values from the AudioChannelSet::ChannelType
    enum, where each type may only occur once within the set.

    The documentation below lists which AudioChannelSet corresponds to which native
    layouts used by AAX, VST2/VST3 and CoreAudio/AU. The layout tags in CoreAudio
    are particularly confusing. For example, the layout which is labeled as "7.1 SDDS"
    in Logic Pro, corresponds to CoreAudio/AU's kAudioChannelLayoutTag_DTS_7_0 tag, whereas
    AAX's DTS 7.1 Layout corresponds to CoreAudio/AU's
    kAudioChannelLayoutTag_MPEG_7_1_A format, etc. Please do not use the CoreAudio tag
    as an indication to the actual layout of the speakers.

    @see Bus

    @tags{Audio}
*/
class JUCE_API  AudioChannelSet
{
public:
    /** Creates an empty channel set.
        You can call addChannel to add channels to the set.
    */
    AudioChannelSet() = default;

    /** Creates a zero-channel set which can be used to indicate that a
        bus is disabled. */
    static AudioChannelSet JUCE_CALLTYPE disabled();

    //==============================================================================
    /** Creates a one-channel mono set (centre).

        Is equivalent to: kMonoAAX (VST), AAX_eStemFormat_Mono (AAX), kAudioChannelLayoutTag_Mono (CoreAudio)
    */
    static AudioChannelSet JUCE_CALLTYPE mono();


    /** Creates a set containing a stereo set (left, right).

        Is equivalent to: kStereo (VST), AAX_eStemFormat_Stereo (AAX), kAudioChannelLayoutTag_Stereo (CoreAudio)
    */
    static AudioChannelSet JUCE_CALLTYPE stereo();


    //==============================================================================
    /** Creates a set containing an LCR set (left, right, centre).

        Is equivalent to: k30Cine (VST), AAX_eStemFormat_LCR (AAX), kAudioChannelLayoutTag_MPEG_3_0_A (CoreAudio)

        This format is referred to as "LRC" in Cubase.
        This format is referred to as "LCR" in Pro Tools.
    */
    static AudioChannelSet JUCE_CALLTYPE createLCR();


    /** Creates a set containing an LRS set (left, right, surround).

        Is equivalent to: k30Music (VST), n/a (AAX), kAudioChannelLayoutTag_ITU_2_1 (CoreAudio)

        This format is referred to as "LRS" in Cubase.
    */
    static AudioChannelSet JUCE_CALLTYPE createLRS();


    /** Creates a set containing an LCRS set (left, right, centre, surround).

        Is equivalent to: k40Cine (VST), AAX_eStemFormat_LCRS (AAX), kAudioChannelLayoutTag_MPEG_4_0_A (CoreAudio)

        This format is referred to as "LCRS (Pro Logic)" in Logic Pro.
        This format is referred to as "LRCS" in Cubase.
        This format is referred to as "LCRS" in Pro Tools.
    */
    static AudioChannelSet JUCE_CALLTYPE createLCRS();


    //==============================================================================
    /** Creates a set for a 5.0 surround setup (left, right, centre, leftSurround, rightSurround).

        Is equivalent to: k50 (VST), AAX_eStemFormat_5_0 (AAX), kAudioChannelLayoutTag_MPEG_5_0_A (CoreAudio)

        This format is referred to as "5.0" in Cubase.
        This format is referred to as "5.0" in Pro Tools.
    */
    static AudioChannelSet JUCE_CALLTYPE create5point0();


    /** Creates a set for a 5.1 surround setup (left, right, centre, leftSurround, rightSurround, LFE).

        Is equivalent to: k51 (VST), AAX_eStemFormat_5_1 (AAX), kAudioChannelLayoutTag_MPEG_5_1_A (CoreAudio)

        This format is referred to as "5.1 (ITU 775)" in Logic Pro.
        This format is referred to as "5.1" in Cubase.
        This format is referred to as "5.1" in Pro Tools.
    */
    static AudioChannelSet JUCE_CALLTYPE create5point1();


    /** Creates a set for a 6.0 Cine surround setup (left, right, centre, leftSurround, rightSurround, centreSurround).

        Is equivalent to: k60Cine (VST), AAX_eStemFormat_6_0 (AAX), kAudioChannelLayoutTag_AudioUnit_6_0 (CoreAudio)

        Logic Pro incorrectly uses this for the surround format labeled "6.1 (ES/EX)".
        This format is referred to as "6.0 Cine" in Cubase.
        This format is referred to as "6.0" in Pro Tools.
    */
    static AudioChannelSet JUCE_CALLTYPE create6point0();


    /** Creates a set for a 6.1 Cine surround setup (left, right, centre, leftSurround, rightSurround, centreSurround, LFE).

         Is equivalent to: k61Cine (VST), AAX_eStemFormat_6_1 (AAX), kAudioChannelLayoutTag_MPEG_6_1_A (CoreAudio)

         This format is referred to as "6.1" in Pro Tools.
     */
    static AudioChannelSet JUCE_CALLTYPE create6point1();


    /** Creates a set for a 6.0 Music surround setup (left, right, leftSurround, rightSurround, leftSurroundSide, rightSurroundSide).

        Is equivalent to: k60Music (VST), n/a (AAX), kAudioChannelLayoutTag_DTS_6_0_A (CoreAudio)

        This format is referred to as "6.0 Music" in Cubase.
    */
    static AudioChannelSet JUCE_CALLTYPE create6point0Music();


    /** Creates a set for a 6.0 Music surround setup (left, right, leftSurround, rightSurround, leftSurroundSide, rightSurroundSide, LFE).

        Is equivalent to: k61Music (VST), n/a (AAX), kAudioChannelLayoutTag_DTS_6_1_A (CoreAudio)
    */
    static AudioChannelSet JUCE_CALLTYPE create6point1Music();


    /** Creates a set for a DTS 7.0 surround setup (left, right, centre, leftSurroundSide, rightSurroundSide, leftSurroundRear, rightSurroundRear).

        Is equivalent to: k70Music (VST), AAX_eStemFormat_7_0_DTS (AAX), kAudioChannelLayoutTag_AudioUnit_7_0 (CoreAudio)

        This format is referred to as "7.0" in Pro Tools.
    */
    static AudioChannelSet JUCE_CALLTYPE create7point0();


    /** Creates a set for a SDDS 7.0 surround setup (left, right, centre, leftSurround, rightSurround, leftCentre, rightCentre).

        Is equivalent to: k70Cine (VST), AAX_eStemFormat_7_0_SDDS (AAX), kAudioChannelLayoutTag_AudioUnit_7_0_Front (CoreAudio)

        This format is referred to as "7.0 SDDS" in Pro Tools.
     */
    static AudioChannelSet JUCE_CALLTYPE create7point0SDDS();


    /** Creates a set for a DTS 7.1 surround setup (left, right, centre, leftSurroundSide, rightSurroundSide, leftSurroundRear, rightSurroundRear, LFE).

        Is equivalent to: k71CineSideFill (VST), AAX_eStemFormat_7_1_DTS (AAX), kAudioChannelLayoutTag_MPEG_7_1_C/kAudioChannelLayoutTag_ITU_3_4_1 (CoreAudio)

        This format is referred to as "7.1 (3/4.1)" in Logic Pro.
        This format is referred to as "7.1" in Pro Tools.
    */
    static AudioChannelSet JUCE_CALLTYPE create7point1();


    /** Creates a set for a 7.1 surround setup (left, right, centre, leftSurround, rightSurround, leftCentre, rightCentre, LFE).

        Is equivalent to: k71Cine (VST), AAX_eStemFormat_7_1_SDDS (AAX), kAudioChannelLayoutTag_MPEG_7_1_A (CoreAudio)

        This format is referred to as "7.1 (SDDS)" in Logic Pro.
        This format is referred to as "7.1 SDDS" in Pro Tools.
    */
    static AudioChannelSet JUCE_CALLTYPE create7point1SDDS();

    /** Creates a set for Dolby Atmos 7.0.2 surround setup (left, right, centre, leftSurroundSide, rightSurroundSide, leftSurroundRear, rightSurroundRear, topSideLeft, topSideRight).

        Is equivalent to: n/a (VST), AAX_eStemFormat_7_0_2 (AAX), n/a (CoreAudio)
    */
    static AudioChannelSet JUCE_CALLTYPE create7point0point2();

    /** Creates a set for Dolby Atmos 7.1.2 surround setup (left, right, centre, leftSurroundSide, rightSurroundSide, leftSurroundRear, rightSurroundRear, LFE, topSideLeft, topSideRight).

        Is equivalent to: k71_2 (VST), AAX_eStemFormat_7_1_2 (AAX), n/a (CoreAudio)
    */
    static AudioChannelSet JUCE_CALLTYPE create7point1point2();


    //==============================================================================
    /** Creates a set for quadraphonic surround setup (left, right, leftSurround, rightSurround)

        Is equivalent to: k40Music (VST), AAX_eStemFormat_Quad (AAX), kAudioChannelLayoutTag_Quadraphonic (CoreAudio)

        This format is referred to as "Quadraphonic" in Logic Pro.
        This format is referred to as "Quadro" in Cubase.
        This format is referred to as "Quad" in Pro Tools.
     */
    static AudioChannelSet JUCE_CALLTYPE quadraphonic();


    /** Creates a set for pentagonal surround setup (left, right, centre, leftSurroundRear, rightSurroundRear).

        Is equivalent to: n/a (VST), n/a (AAX), kAudioChannelLayoutTag_Pentagonal (CoreAudio)
    */
    static AudioChannelSet JUCE_CALLTYPE pentagonal();


    /** Creates a set for hexagonal surround setup (left, right, leftSurroundRear, rightSurroundRear, centre, surroundCentre).

        Is equivalent to: n/a (VST), n/a (AAX), kAudioChannelLayoutTag_Hexagonal (CoreAudio)
    */
    static AudioChannelSet JUCE_CALLTYPE hexagonal();


    /** Creates a set for octagonal surround setup (left, right, leftSurround, rightSurround, centre, centreSurround, wideLeft, wideRight).

        Is equivalent to: n/a (VST), n/a (AAX), kAudioChannelLayoutTag_Octagonal (CoreAudio)
    */
    static AudioChannelSet JUCE_CALLTYPE octagonal();

    //==============================================================================
    /** Creates a set for ACN, SN3D normalised ambisonic surround setups with a given order.

        Is equivalent to: kAmbiXXXOrderACN (VST), AAX_eStemFormat_Ambi_XXX_ACN (AAX), kAudioChannelLayoutTag_HOA_ACN_SN3D (CoreAudio)
    */
    static AudioChannelSet JUCE_CALLTYPE ambisonic (int order = 1);

    /** Returns the order of the ambisonic layout represented by this AudioChannelSet. If the
        AudioChannelSet is not an ambisonic layout, then this method will return -1.
    */
    int getAmbisonicOrder() const;

    //==============================================================================
    /** Creates a set of untyped discrete channels. */
    static AudioChannelSet JUCE_CALLTYPE discreteChannels (int numChannels);

    /** Create a canonical channel set for a given number of channels.
        For example, numChannels = 1 will return mono, numChannels = 2 will return stereo, etc. */
    static AudioChannelSet JUCE_CALLTYPE canonicalChannelSet (int numChannels);

    /** Create a channel set for a given number of channels which is non-discrete.
        If numChannels is larger than the number of channels of the surround format
        with the maximum amount of channels (currently 7.1 Surround), then this
        function returns an empty set.*/
    static AudioChannelSet JUCE_CALLTYPE namedChannelSet (int numChannels);

    /** Return an array of channel sets which have a given number of channels */
    static Array<AudioChannelSet> JUCE_CALLTYPE channelSetsWithNumberOfChannels (int numChannels);

    //==============================================================================
    /** Represents different audio channel types. */
    enum ChannelType
    {
        unknown             = 0, /**< Unknown channel type. */

        //==============================================================================
        left                = 1, /**< L channel. */
        right               = 2, /**< R channel. */
        centre              = 3, /**< C channel. (Sometimes M for mono) */

        //==============================================================================
        LFE                 = 4,              /**< LFE channel. */
        leftSurround        = 5,              /**< Ls channel.  */
        rightSurround       = 6,              /**< Rs channel.  */
        leftCentre          = 7,              /**< Lc (AAX/VST), Lc used as Lss in AU for most layouts. */
        rightCentre         = 8,              /**< Rc (AAX/VST), Rc used as Rss in AU for most layouts. */
        centreSurround      = 9,              /**< Cs/S channel. */
        surround            = centreSurround, /**< Same as Centre Surround channel. */
        leftSurroundSide    = 10,             /**< Lss (AXX), Side Left  "Sl" (VST), Left Centre  "LC" (AU) channel. */
        rightSurroundSide   = 11,             /**< Rss (AXX), Side right "Sr" (VST), Right Centre "Rc" (AU) channel. */
        topMiddle           = 12,             /**< Top Middle channel.       */
        topFrontLeft        = 13,             /**< Top Front Left channel.   */
        topFrontCentre      = 14,             /**< Top Front Centre channel. */
        topFrontRight       = 15,             /**< Top Front Right channel.  */
        topRearLeft         = 16,             /**< Top Rear Left channel.    */
        topRearCentre       = 17,             /**< Top Rear Centre channel.  */
        topRearRight        = 18,             /**< Top Rear Right channel.   */
        LFE2                = 19,             /**< Second LFE channel.       */
        leftSurroundRear    = 20,             /**< Lsr (AAX), Lcs (VST), Rls (AU) channel. */
        rightSurroundRear   = 21,             /**< Rsr (AAX), Rcs (VST), Rrs (AU) channel. */
        wideLeft            = 22,             /**< Wide Left channel.  */
        wideRight           = 23,             /**< Wide Right channel. */

        //==============================================================================
        // Used by Dolby Atmos 7.0.2 and 7.1.2
        topSideLeft         = 28, /**< Lts (AAX), Tsl (VST) channel for Dolby Atmos. */
        topSideRight        = 29, /**< Rts (AAX), Tsr (VST) channel for Dolby Atmos. */

        //==============================================================================
        // Ambisonic ACN formats - all channels are SN3D normalised

        // zero-th and first-order ambisonic ACN
        ambisonicACN0       = 24, /**< Zero-th ambisonic channel number 0.     */
        ambisonicACN1       = 25, /**< First-order ambisonic channel number 1. */
        ambisonicACN2       = 26, /**< First-order ambisonic channel number 2. */
        ambisonicACN3       = 27, /**< First-order ambisonic channel number 3. */

        // second-order ambisonic
        ambisonicACN4       = 30, /**< Second-order ambisonic channel number 4. */
        ambisonicACN5       = 31, /**< Second-order ambisonic channel number 5. */
        ambisonicACN6       = 32, /**< Second-order ambisonic channel number 6. */
        ambisonicACN7       = 33, /**< Second-order ambisonic channel number 7. */
        ambisonicACN8       = 34, /**< Second-order ambisonic channel number 8. */

        // third-order ambisonic
        ambisonicACN9       = 35, /**< Third-order ambisonic channel number 9.  */
        ambisonicACN10      = 36, /**< Third-order ambisonic channel number 10. */
        ambisonicACN11      = 37, /**< Third-order ambisonic channel number 11. */
        ambisonicACN12      = 38, /**< Third-order ambisonic channel number 12. */
        ambisonicACN13      = 39, /**< Third-order ambisonic channel number 13. */
        ambisonicACN14      = 40, /**< Third-order ambisonic channel number 14. */
        ambisonicACN15      = 41, /**< Third-order ambisonic channel number 15. */

        // fourth-order ambisonic
        ambisonicACN16      = 42, /**< Fourth-order ambisonic channel number 16. */
        ambisonicACN17      = 43, /**< Fourth-order ambisonic channel number 17. */
        ambisonicACN18      = 44, /**< Fourth-order ambisonic channel number 18. */
        ambisonicACN19      = 45, /**< Fourth-order ambisonic channel number 19. */
        ambisonicACN20      = 46, /**< Fourth-order ambisonic channel number 20. */
        ambisonicACN21      = 47, /**< Fourth-order ambisonic channel number 21. */
        ambisonicACN22      = 48, /**< Fourth-order ambisonic channel number 22. */
        ambisonicACN23      = 49, /**< Fourth-order ambisonic channel number 23. */
        ambisonicACN24      = 50, /**< Fourth-order ambisonic channel number 24. */

        // fifth-order ambisonic
        ambisonicACN25      = 51, /**< Fifth-order ambisonic channel number 25. */
        ambisonicACN26      = 52, /**< Fifth-order ambisonic channel number 26. */
        ambisonicACN27      = 53, /**< Fifth-order ambisonic channel number 27. */
        ambisonicACN28      = 54, /**< Fifth-order ambisonic channel number 28. */
        ambisonicACN29      = 55, /**< Fifth-order ambisonic channel number 29. */
        ambisonicACN30      = 56, /**< Fifth-order ambisonic channel number 30. */
        ambisonicACN31      = 57, /**< Fifth-order ambisonic channel number 31. */
        ambisonicACN32      = 58, /**< Fifth-order ambisonic channel number 32. */
        ambisonicACN33      = 59, /**< Fifth-order ambisonic channel number 33. */
        ambisonicACN34      = 60, /**< Fifth-order ambisonic channel number 34. */
        ambisonicACN35      = 61, /**< Fifth-order ambisonic channel number 35. */

        //==============================================================================
        ambisonicW          = ambisonicACN0, /**< Same as zero-th ambisonic channel number 0.     */
        ambisonicX          = ambisonicACN3, /**< Same as first-order ambisonic channel number 3. */
        ambisonicY          = ambisonicACN1, /**< Same as first-order ambisonic channel number 1. */
        ambisonicZ          = ambisonicACN2, /**< Same as first-order ambisonic channel number 2. */

        //==============================================================================
        bottomFrontLeft     = 62, /**< Bottom Front Left (Bfl)   */
        bottomFrontCentre   = 63, /**< Bottom Front Centre (Bfc) */
        bottomFrontRight    = 64, /**< Bottom Front Right (Bfr)  */

        proximityLeft       = 65, /**< Proximity Left (Pl)  */
        proximityRight      = 66, /**< Proximity Right (Pr) */

        bottomSideLeft      = 67, /**< Bottom Side Left (Bsl)   */
        bottomSideRight     = 68, /**< Bottom Side Right (Bsr)  */
        bottomRearLeft      = 69, /**< Bottom Rear Left (Brl)  */
        bottomRearCentre    = 70, /**< Bottom Rear Center (Brc)  */
        bottomRearRight     = 71, /**< Bottom Rear Right (Brr)  */

        //==============================================================================
        discreteChannel0    = 128  /**< Non-typed individual channels are indexed upwards from this value. */
    };

    /** Returns the name of a given channel type. For example, this method may return "Surround Left". */
    static String JUCE_CALLTYPE getChannelTypeName (ChannelType);

    /** Returns the abbreviated name of a channel type. For example, this method may return "Ls". */
    static String JUCE_CALLTYPE getAbbreviatedChannelTypeName (ChannelType);

    /** Returns the channel type from an abbreviated name. */
    static ChannelType JUCE_CALLTYPE getChannelTypeFromAbbreviation (const String& abbreviation);

    //==============================================================================
    enum
    {
        maxChannelsOfNamedLayout = 36
    };

    /** Adds a channel to the set. */
    void addChannel (ChannelType newChannelType);

    /** Removes a channel from the set. */
    void removeChannel (ChannelType newChannelType);

    /** Returns the number of channels in the set. */
    int size() const noexcept;

    /** Returns true if there are no channels in the set. */
    bool isDisabled() const noexcept                    { return size() == 0; }

    /** Returns an array of all the types in this channel set. */
    Array<ChannelType> getChannelTypes() const;

    /** Returns the type of one of the channels in the set, by index. */
    ChannelType getTypeOfChannel (int channelIndex) const noexcept;

    /** Returns the index for a particular channel-type.
        Will return -1 if the this set does not contain a channel of this type. */
    int getChannelIndexForType (ChannelType type) const noexcept;

    /** Returns a string containing a whitespace-separated list of speaker types
        corresponding to each channel. For example in a 5.1 arrangement,
        the string may be "L R C Lfe Ls Rs". If the speaker arrangement is unknown,
        the returned string will be empty.*/
    String getSpeakerArrangementAsString() const;

    /** Returns an AudioChannelSet from a string returned by getSpeakerArrangementAsString

        @see getSpeakerArrangementAsString */
    static AudioChannelSet fromAbbreviatedString (const String& set);

    /** Returns the description of the current layout. For example, this method may return
        "Quadraphonic". Note that the returned string may not be unique. */
    String getDescription() const;

    /** Returns if this is a channel layout made-up of discrete channels. */
    bool isDiscreteLayout() const noexcept;

    /** Intersect two channel layouts. */
    void intersect (const AudioChannelSet& other)      { channels &= other.channels; }

    /** Creates a channel set for a list of channel types. This function will assert
        if you supply a duplicate channel.

        Note that this method ignores the order in which the channels are given, i.e.
        two arrays with the same elements but in a different order will still result
        in the same channel set.
    */
    static AudioChannelSet JUCE_CALLTYPE channelSetWithChannels (const Array<ChannelType>&);

    //==============================================================================
    // Conversion between wave and juce channel layout identifiers

    /** Create an AudioChannelSet from a WAVEFORMATEXTENSIBLE channelMask (typically used
        in .wav files). */
    static AudioChannelSet JUCE_CALLTYPE fromWaveChannelMask (int32 dwChannelMask);

    /** Returns a WAVEFORMATEXTENSIBLE channelMask representation (typically used in .wav
        files) of the receiver.

        Returns -1 if the receiver cannot be represented in a WAVEFORMATEXTENSIBLE channelMask
        representation.
    */
    int32 getWaveChannelMask() const noexcept;

    //==============================================================================
    bool operator== (const AudioChannelSet&) const noexcept;
    bool operator!= (const AudioChannelSet&) const noexcept;
    bool operator<  (const AudioChannelSet&) const noexcept;

private:
    //==============================================================================
    BigInteger channels;

    //==============================================================================
    explicit AudioChannelSet (uint32);
    explicit AudioChannelSet (const Array<ChannelType>&);

    //==============================================================================
    static int JUCE_CALLTYPE getAmbisonicOrderForNumChannels (int);
};

} // namespace juce
