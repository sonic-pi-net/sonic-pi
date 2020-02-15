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

MPESynthesiser::MPESynthesiser()
{
}

MPESynthesiser::MPESynthesiser (MPEInstrument* mpeInstrument)  : MPESynthesiserBase (mpeInstrument)
{
}

MPESynthesiser::~MPESynthesiser()
{
}

//==============================================================================
void MPESynthesiser::startVoice (MPESynthesiserVoice* voice, MPENote noteToStart)
{
    jassert (voice != nullptr);
    voice->currentlyPlayingNote = noteToStart;
    voice->noteStarted();
}

void MPESynthesiser::stopVoice (MPESynthesiserVoice* voice, MPENote noteToStop, bool allowTailOff)
{
    jassert (voice != nullptr);
    voice->currentlyPlayingNote = noteToStop;
    voice->noteStopped (allowTailOff);
}

//==============================================================================
void MPESynthesiser::noteAdded (MPENote newNote)
{
    const ScopedLock sl (voicesLock);

    if (MPESynthesiserVoice* voice = findFreeVoice (newNote, shouldStealVoices))
        startVoice (voice, newNote);
}

void MPESynthesiser::notePressureChanged (MPENote changedNote)
{
    const ScopedLock sl (voicesLock);

    for (int i = 0; i < voices.size(); ++i)
    {
        MPESynthesiserVoice* voice = voices.getUnchecked (i);

        if (voice->isCurrentlyPlayingNote (changedNote))
        {
            voice->currentlyPlayingNote = changedNote;
            voice->notePressureChanged();
        }
    }
}

void MPESynthesiser::notePitchbendChanged (MPENote changedNote)
{
    const ScopedLock sl (voicesLock);

    for (int i = 0; i < voices.size(); ++i)
    {
        MPESynthesiserVoice* voice = voices.getUnchecked (i);

        if (voice->isCurrentlyPlayingNote (changedNote))
        {
            voice->currentlyPlayingNote = changedNote;
            voice->notePitchbendChanged();
        }
    }
}

void MPESynthesiser::noteTimbreChanged (MPENote changedNote)
{
    const ScopedLock sl (voicesLock);

    for (int i = 0; i < voices.size(); ++i)
    {
        MPESynthesiserVoice* voice = voices.getUnchecked (i);

        if (voice->isCurrentlyPlayingNote (changedNote))
        {
            voice->currentlyPlayingNote = changedNote;
            voice->noteTimbreChanged();
        }
    }
}

void MPESynthesiser::noteKeyStateChanged (MPENote changedNote)
{
    const ScopedLock sl (voicesLock);

    for (int i = 0; i < voices.size(); ++i)
    {
        MPESynthesiserVoice* voice = voices.getUnchecked (i);

        if (voice->isCurrentlyPlayingNote (changedNote))
        {
            voice->currentlyPlayingNote = changedNote;
            voice->noteKeyStateChanged();
        }
    }
}

void MPESynthesiser::noteReleased (MPENote finishedNote)
{
    const ScopedLock sl (voicesLock);

    for (int i = voices.size(); --i >= 0;)
    {
        MPESynthesiserVoice* const voice = voices.getUnchecked (i);

        if (voice->isCurrentlyPlayingNote(finishedNote))
            stopVoice (voice, finishedNote, true);
    }
}

void MPESynthesiser::setCurrentPlaybackSampleRate (const double newRate)
{
    MPESynthesiserBase::setCurrentPlaybackSampleRate (newRate);

    const ScopedLock sl (voicesLock);

    turnOffAllVoices (false);

    for (int i = voices.size(); --i >= 0;)
        voices.getUnchecked (i)->setCurrentSampleRate (newRate);
}

void MPESynthesiser::handleMidiEvent (const MidiMessage& m)
{
    if (m.isController())
        handleController (m.getChannel(), m.getControllerNumber(), m.getControllerValue());
    else if (m.isProgramChange())
        handleProgramChange (m.getChannel(), m.getProgramChangeNumber());

    MPESynthesiserBase::handleMidiEvent (m);
}

MPESynthesiserVoice* MPESynthesiser::findFreeVoice (MPENote noteToFindVoiceFor, bool stealIfNoneAvailable) const
{
    const ScopedLock sl (voicesLock);

    for (int i = 0; i < voices.size(); ++i)
    {
        MPESynthesiserVoice* const voice = voices.getUnchecked (i);

        if (! voice->isActive())
            return voice;
    }

    if (stealIfNoneAvailable)
        return findVoiceToSteal (noteToFindVoiceFor);

    return nullptr;
}

struct MPEVoiceAgeSorter
{
    static int compareElements (MPESynthesiserVoice* v1, MPESynthesiserVoice* v2) noexcept
    {
        return v1->wasStartedBefore (*v2) ? -1 : (v2->wasStartedBefore (*v1) ? 1 : 0);
    }
};

MPESynthesiserVoice* MPESynthesiser::findVoiceToSteal (MPENote noteToStealVoiceFor) const
{
    // This voice-stealing algorithm applies the following heuristics:
    // - Re-use the oldest notes first
    // - Protect the lowest & topmost notes, even if sustained, but not if they've been released.


    // apparently you are trying to render audio without having any voices...
    jassert (voices.size() > 0);

    // These are the voices we want to protect (ie: only steal if unavoidable)
    MPESynthesiserVoice* low = nullptr; // Lowest sounding note, might be sustained, but NOT in release phase
    MPESynthesiserVoice* top = nullptr; // Highest sounding note, might be sustained, but NOT in release phase

    // this is a list of voices we can steal, sorted by how long they've been running
    Array<MPESynthesiserVoice*> usableVoices;
    usableVoices.ensureStorageAllocated (voices.size());

    for (int i = 0; i < voices.size(); ++i)
    {
        MPESynthesiserVoice* const voice = voices.getUnchecked (i);
        jassert (voice->isActive()); // We wouldn't be here otherwise

        MPEVoiceAgeSorter sorter;
        usableVoices.addSorted (sorter, voice);

        if (! voice->isPlayingButReleased()) // Don't protect released notes
        {
            const int noteNumber = voice->getCurrentlyPlayingNote().initialNote;

            if (low == nullptr || noteNumber < low->getCurrentlyPlayingNote().initialNote)
                low = voice;

            if (top == nullptr || noteNumber > top->getCurrentlyPlayingNote().initialNote)
                top = voice;
        }
    }

    // Eliminate pathological cases (ie: only 1 note playing): we always give precedence to the lowest note(s)
    if (top == low)
        top = nullptr;

    const int numUsableVoices = usableVoices.size();

    // If we want to re-use the voice to trigger a new note,
    // then The oldest note that's playing the same note number is ideal.
    if (noteToStealVoiceFor.isValid())
    {
        for (int i = 0; i < numUsableVoices; ++i)
        {
            MPESynthesiserVoice* const voice = usableVoices.getUnchecked (i);

            if (voice->getCurrentlyPlayingNote().initialNote == noteToStealVoiceFor.initialNote)
                return voice;
        }
    }

    // Oldest voice that has been released (no finger on it and not held by sustain pedal)
    for (int i = 0; i < numUsableVoices; ++i)
    {
        MPESynthesiserVoice* const voice = usableVoices.getUnchecked (i);

        if (voice != low && voice != top && voice->isPlayingButReleased())
            return voice;
    }

    // Oldest voice that doesn't have a finger on it:
    for (int i = 0; i < numUsableVoices; ++i)
    {
        MPESynthesiserVoice* const voice = usableVoices.getUnchecked (i);

        if (voice != low && voice != top
             && voice->getCurrentlyPlayingNote().keyState != MPENote::keyDown
             && voice->getCurrentlyPlayingNote().keyState != MPENote::keyDownAndSustained)
            return voice;
    }

    // Oldest voice that isn't protected
    for (int i = 0; i < numUsableVoices; ++i)
    {
        MPESynthesiserVoice* const voice = usableVoices.getUnchecked (i);

        if (voice != low && voice != top)
            return voice;
    }

    // We've only got "protected" voices now: lowest note takes priority
    jassert (low != nullptr);

    // Duophonic synth: give priority to the bass note:
    if (top != nullptr)
        return top;

    return low;
}

//==============================================================================
void MPESynthesiser::addVoice (MPESynthesiserVoice* const newVoice)
{
    const ScopedLock sl (voicesLock);
    newVoice->setCurrentSampleRate (getSampleRate());
    voices.add (newVoice);
}

void MPESynthesiser::clearVoices()
{
    const ScopedLock sl (voicesLock);
    voices.clear();
}

MPESynthesiserVoice* MPESynthesiser::getVoice (const int index) const
{
    const ScopedLock sl (voicesLock);
    return voices [index];
}

void MPESynthesiser::removeVoice (const int index)
{
    const ScopedLock sl (voicesLock);
    voices.remove (index);
}

void MPESynthesiser::reduceNumVoices (const int newNumVoices)
{
    // we can't possibly get to a negative number of voices...
    jassert (newNumVoices >= 0);

    const ScopedLock sl (voicesLock);

    while (voices.size() > newNumVoices)
    {
        if (MPESynthesiserVoice* voice = findFreeVoice (MPENote(), true))
            voices.removeObject (voice);
        else
            voices.remove (0); // if there's no voice to steal, kill the oldest voice
    }
}

void MPESynthesiser::turnOffAllVoices (bool allowTailOff)
{
    // first turn off all voices (it's more efficient to do this immediately
    // rather than to go through the MPEInstrument for this).
    for (int i = voices.size(); --i >= 0;)
        voices.getUnchecked (i)->noteStopped (allowTailOff);

    // finally make sure the MPE Instrument also doesn't have any notes anymore.
    instrument->releaseAllNotes();
}

//==============================================================================
void MPESynthesiser::renderNextSubBlock (AudioBuffer<float>& buffer, int startSample, int numSamples)
{
    for (int i = voices.size(); --i >= 0;)
    {
        MPESynthesiserVoice* voice = voices.getUnchecked (i);

        if (voice->isActive())
            voice->renderNextBlock (buffer, startSample, numSamples);
    }
}

void MPESynthesiser::renderNextSubBlock (AudioBuffer<double>& buffer, int startSample, int numSamples)
{
    for (int i = voices.size(); --i >= 0;)
    {
        MPESynthesiserVoice* voice = voices.getUnchecked (i);

        if (voice->isActive())
            voice->renderNextBlock (buffer, startSample, numSamples);
    }
}

} // namespace juce
