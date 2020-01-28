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
    Reads and Writes WAV format audio files.

    @see AudioFormat
*/
class JUCE_API  WavAudioFormat  : public AudioFormat
{
public:
    //==============================================================================
    /** Creates a format object. */
    WavAudioFormat();

    /** Destructor. */
    ~WavAudioFormat();

    //==============================================================================
    // BWAV chunk properties:

    static const char* const bwavDescription;       /**< Metadata property name used in BWAV chunks. */
    static const char* const bwavOriginator;        /**< Metadata property name used in BWAV chunks. */
    static const char* const bwavOriginatorRef;     /**< Metadata property name used in BWAV chunks. */
    static const char* const bwavOriginationDate;   /**< Metadata property name used in BWAV chunks. The format should be: yyyy-mm-dd */
    static const char* const bwavOriginationTime;   /**< Metadata property name used in BWAV chunks. The format should be: format is: hh-mm-ss */
    static const char* const bwavCodingHistory;     /**< Metadata property name used in BWAV chunks. */

    /** Metadata property name used in BWAV chunks.
        This is the number of samples from the start of an edit that the
        file is supposed to begin at. Seems like an obvious mistake to
        only allow a file to occur in an edit once, but that's the way
        it is..

        @see AudioFormatReader::metadataValues, createWriterFor
    */
    static const char* const bwavTimeReference;

    /** Utility function to fill out the appropriate metadata for a BWAV file.

        This just makes it easier than using the property names directly, and it
        fills out the time and date in the right format.
    */
    static StringPairArray createBWAVMetadata (const String& description,
                                               const String& originator,
                                               const String& originatorRef,
                                               Time dateAndTime,
                                               int64 timeReferenceSamples,
                                               const String& codingHistory);

    //==============================================================================
    // 'acid' chunk properties:

    static const char* const acidOneShot;           /**< Metadata property name used in acid chunks. */
    static const char* const acidRootSet;           /**< Metadata property name used in acid chunks. */
    static const char* const acidStretch;           /**< Metadata property name used in acid chunks. */
    static const char* const acidDiskBased;         /**< Metadata property name used in acid chunks. */
    static const char* const acidizerFlag;          /**< Metadata property name used in acid chunks. */
    static const char* const acidRootNote;          /**< Metadata property name used in acid chunks. */
    static const char* const acidBeats;             /**< Metadata property name used in acid chunks. */
    static const char* const acidDenominator;       /**< Metadata property name used in acid chunks. */
    static const char* const acidNumerator;         /**< Metadata property name used in acid chunks. */
    static const char* const acidTempo;             /**< Metadata property name used in acid chunks. */

    //==============================================================================
    // INFO chunk properties:

    static const char* const riffInfoArchivalLocation;      /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoArtist;                /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoBaseURL;               /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoCinematographer;       /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoComment;               /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoComment2;              /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoComments;              /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoCommissioned;          /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoCopyright;             /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoCostumeDesigner;       /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoCountry;               /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoCropped;               /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoDateCreated;           /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoDateTimeOriginal;      /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoDefaultAudioStream;    /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoDimension;             /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoDirectory;             /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoDistributedBy;         /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoDotsPerInch;           /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoEditedBy;              /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoEighthLanguage;        /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoEncodedBy;             /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoEndTimecode;           /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoEngineer;              /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoFifthLanguage;         /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoFirstLanguage;         /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoFourthLanguage;        /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoGenre;                 /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoKeywords;              /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoLanguage;              /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoLength;                /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoLightness;             /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoLocation;              /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoLogoIconURL;           /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoLogoURL;               /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoMedium;                /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoMoreInfoBannerImage;   /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoMoreInfoBannerURL;     /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoMoreInfoText;          /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoMoreInfoURL;           /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoMusicBy;               /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoNinthLanguage;         /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoNumberOfParts;         /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoOrganisation;          /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoPart;                  /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoProducedBy;            /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoProductName;           /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoProductionDesigner;    /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoProductionStudio;      /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoRate;                  /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoRated;                 /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoRating;                /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoRippedBy;              /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoSecondaryGenre;        /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoSecondLanguage;        /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoSeventhLanguage;       /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoSharpness;             /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoSixthLanguage;         /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoSoftware;              /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoSoundSchemeTitle;      /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoSource;                /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoSourceFrom;            /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoStarring_ISTR;         /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoStarring_STAR;         /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoStartTimecode;         /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoStatistics;            /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoSubject;               /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoTapeName;              /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoTechnician;            /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoThirdLanguage;         /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoTimeCode;              /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoTitle;                 /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoTrackNo;               /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoTrackNumber;           /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoURL;                   /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoVegasVersionMajor;     /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoVegasVersionMinor;     /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoVersion;               /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoWatermarkURL;          /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoWrittenBy;             /**< Metadata property name used in INFO chunks. */
    static const char* const riffInfoYear;                  /**< Metadata property name used in INFO chunks. */

    //==============================================================================
    /** Metadata property name used when reading an ISRC code from an AXML chunk. */
    static const char* const ISRC;

    /** Metadata property name used when reading a WAV file with a Tracktion chunk. */
    static const char* const tracktionLoopInfo;

    //==============================================================================
    Array<int> getPossibleSampleRates() override;
    Array<int> getPossibleBitDepths() override;
    bool canDoStereo() override;
    bool canDoMono() override;
    bool isChannelLayoutSupported (const AudioChannelSet& channelSet) override;

    //==============================================================================
    AudioFormatReader* createReaderFor (InputStream* sourceStream,
                                        bool deleteStreamIfOpeningFails) override;

    MemoryMappedAudioFormatReader* createMemoryMappedReader (const File&)      override;
    MemoryMappedAudioFormatReader* createMemoryMappedReader (FileInputStream*) override;

    AudioFormatWriter* createWriterFor (OutputStream* streamToWriteTo,
                                        double sampleRateToUse,
                                        unsigned int numberOfChannels,
                                        int bitsPerSample,
                                        const StringPairArray& metadataValues,
                                        int qualityOptionIndex) override;

    AudioFormatWriter* createWriterFor (OutputStream* streamToWriteTo,
                                        double sampleRateToUse,
                                        const AudioChannelSet& channelLayout,
                                        int bitsPerSample,
                                        const StringPairArray& metadataValues,
                                        int qualityOptionIndex) override;

    //==============================================================================
    /** Utility function to replace the metadata in a wav file with a new set of values.

        If possible, this cheats by overwriting just the metadata region of the file, rather
        than by copying the whole file again.
    */
    bool replaceMetadataInFile (const File& wavFile, const StringPairArray& newMetadata);


private:
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (WavAudioFormat)
};

} // namespace juce
