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

AudioFormatManager::AudioFormatManager() {}
AudioFormatManager::~AudioFormatManager() {}

//==============================================================================
void AudioFormatManager::registerFormat (AudioFormat* newFormat, bool makeThisTheDefaultFormat)
{
    jassert (newFormat != nullptr);

    if (newFormat != nullptr)
    {
       #if JUCE_DEBUG
        for (auto* af : knownFormats)
        {
            if (af->getFormatName() == newFormat->getFormatName())
                jassertfalse; // trying to add the same format twice!
        }
       #endif

        if (makeThisTheDefaultFormat)
            defaultFormatIndex = getNumKnownFormats();

        knownFormats.add (newFormat);
    }
}

void AudioFormatManager::registerBasicFormats()
{
    registerFormat (new WavAudioFormat(), true);
    registerFormat (new AiffAudioFormat(), false);

   #if JUCE_USE_FLAC
    registerFormat (new FlacAudioFormat(), false);
   #endif

   #if JUCE_USE_OGGVORBIS
    registerFormat (new OggVorbisAudioFormat(), false);
   #endif

   #if JUCE_MAC || JUCE_IOS
    registerFormat (new CoreAudioFormat(), false);
   #endif

   #if JUCE_USE_MP3AUDIOFORMAT
    registerFormat (new MP3AudioFormat(), false);
   #endif

   #if JUCE_USE_WINDOWS_MEDIA_FORMAT
    registerFormat (new WindowsMediaAudioFormat(), false);
   #endif
}

void AudioFormatManager::clearFormats()
{
    knownFormats.clear();
    defaultFormatIndex = 0;
}

int AudioFormatManager::getNumKnownFormats() const                  { return knownFormats.size(); }
AudioFormat* AudioFormatManager::getKnownFormat (int index) const   { return knownFormats[index]; }
AudioFormat* AudioFormatManager::getDefaultFormat() const           { return getKnownFormat (defaultFormatIndex); }

AudioFormat* AudioFormatManager::findFormatForFileExtension (const String& fileExtension) const
{
    if (! fileExtension.startsWithChar ('.'))
        return findFormatForFileExtension ("." + fileExtension);

    for (auto* af : knownFormats)
        if (af->getFileExtensions().contains (fileExtension, true))
            return af;

    return nullptr;
}

String AudioFormatManager::getWildcardForAllFormats() const
{
    StringArray extensions;

    for (auto* af : knownFormats)
        extensions.addArray (af->getFileExtensions());

    extensions.trim();
    extensions.removeEmptyStrings();

    for (auto& e : extensions)
        e = (e.startsWithChar ('.') ? "*" : "*.") + e;

    extensions.removeDuplicates (true);
    return extensions.joinIntoString (";");
}

//==============================================================================
AudioFormatReader* AudioFormatManager::createReaderFor (const File& file)
{
    // you need to actually register some formats before the manager can
    // use them to open a file!
    jassert (getNumKnownFormats() > 0);

    for (auto* af : knownFormats)
        if (af->canHandleFile (file))
            if (auto* in = file.createInputStream())
                if (auto* r = af->createReaderFor (in, true))
                    return r;

    return nullptr;
}

AudioFormatReader* AudioFormatManager::createReaderFor (InputStream* audioFileStream)
{
    // you need to actually register some formats before the manager can
    // use them to open a file!
    jassert (getNumKnownFormats() > 0);

    if (audioFileStream != nullptr)
    {
        ScopedPointer<InputStream> in (audioFileStream);
        auto originalStreamPos = in->getPosition();

        for (auto* af : knownFormats)
        {
            if (auto* r = af->createReaderFor (in, false))
            {
                in.release();
                return r;
            }

            in->setPosition (originalStreamPos);

            // the stream that is passed-in must be capable of being repositioned so
            // that all the formats can have a go at opening it.
            jassert (in->getPosition() == originalStreamPos);
        }
    }

    return nullptr;
}

} // namespace juce
