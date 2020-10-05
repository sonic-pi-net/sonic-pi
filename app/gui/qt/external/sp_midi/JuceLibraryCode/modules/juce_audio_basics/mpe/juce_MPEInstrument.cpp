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

namespace
{
    const uint8 noLSBValueReceived = 0xff;
    const Range<int> allChannels { 1, 17 };
}

//==============================================================================
MPEInstrument::MPEInstrument() noexcept
{
    std::fill_n (lastPressureLowerBitReceivedOnChannel, 16, noLSBValueReceived);
    std::fill_n (lastTimbreLowerBitReceivedOnChannel, 16, noLSBValueReceived);
    std::fill_n (isMemberChannelSustained, 16, false);

    pitchbendDimension.value = &MPENote::pitchbend;
    pressureDimension.value = &MPENote::pressure;
    timbreDimension.value = &MPENote::timbre;

    // the default value for pressure is 0, for all other dimension it is centre (= default MPEValue)
    std::fill_n (pressureDimension.lastValueReceivedOnChannel, 16, MPEValue::minValue());

    legacyMode.isEnabled = false;
    legacyMode.pitchbendRange = 2;
    legacyMode.channelRange = allChannels;
}

MPEInstrument::~MPEInstrument()
{
}

//==============================================================================
MPEZoneLayout MPEInstrument::getZoneLayout() const noexcept
{
    return zoneLayout;
}

void MPEInstrument::setZoneLayout (MPEZoneLayout newLayout)
{
    releaseAllNotes();

    const ScopedLock sl (lock);
    legacyMode.isEnabled = false;
    zoneLayout = newLayout;
}

//==============================================================================
void MPEInstrument::enableLegacyMode (int pitchbendRange, Range<int> channelRange)
{
    releaseAllNotes();

    const ScopedLock sl (lock);
    legacyMode.isEnabled = true;
    legacyMode.pitchbendRange = pitchbendRange;
    legacyMode.channelRange = channelRange;
    zoneLayout.clearAllZones();
}

bool MPEInstrument::isLegacyModeEnabled() const noexcept
{
    return legacyMode.isEnabled;
}

Range<int> MPEInstrument::getLegacyModeChannelRange() const noexcept
{
    return legacyMode.channelRange;
}

void MPEInstrument::setLegacyModeChannelRange (Range<int> channelRange)
{
    jassert (allChannels.contains (channelRange));

    releaseAllNotes();
    const ScopedLock sl (lock);
    legacyMode.channelRange = channelRange;
}

int MPEInstrument::getLegacyModePitchbendRange() const noexcept
{
    return legacyMode.pitchbendRange;
}

void MPEInstrument::setLegacyModePitchbendRange (int pitchbendRange)
{
    jassert (pitchbendRange >= 0 && pitchbendRange <= 96);

    releaseAllNotes();
    const ScopedLock sl (lock);
    legacyMode.pitchbendRange = pitchbendRange;
}

//==============================================================================
void MPEInstrument::setPressureTrackingMode (TrackingMode modeToUse)
{
    pressureDimension.trackingMode = modeToUse;
}

void MPEInstrument::setPitchbendTrackingMode (TrackingMode modeToUse)
{
    pitchbendDimension.trackingMode = modeToUse;
}

void MPEInstrument::setTimbreTrackingMode (TrackingMode modeToUse)
{
    timbreDimension.trackingMode = modeToUse;
}

//==============================================================================
void MPEInstrument::addListener (Listener* listenerToAdd)
{
    listeners.add (listenerToAdd);
}

void MPEInstrument::removeListener (Listener* listenerToRemove)
{
    listeners.remove (listenerToRemove);
}

//==============================================================================
void MPEInstrument::processNextMidiEvent (const MidiMessage& message)
{
    zoneLayout.processNextMidiEvent (message);

    if (message.isNoteOn (true))              processMidiNoteOnMessage (message);
    else if (message.isNoteOff (false))       processMidiNoteOffMessage (message);
    else if (message.isResetAllControllers()
             || message.isAllNotesOff())      processMidiResetAllControllersMessage (message);
    else if (message.isPitchWheel())          processMidiPitchWheelMessage (message);
    else if (message.isChannelPressure())     processMidiChannelPressureMessage (message);
    else if (message.isController())          processMidiControllerMessage (message);
    else if (message.isAftertouch())          processMidiAfterTouchMessage (message);
}

//==============================================================================
void MPEInstrument::processMidiNoteOnMessage (const MidiMessage& message)
{
    // Note: If a note-on with velocity = 0 is used to convey a note-off,
    // then the actual note-off velocity is not known. In this case,
    // the MPE convention is to use note-off velocity = 64.

    if (message.getVelocity() == 0)
    {
        noteOff (message.getChannel(),
                 message.getNoteNumber(),
                 MPEValue::from7BitInt (64));
    }
    else
    {
        noteOn (message.getChannel(),
                message.getNoteNumber(),
                MPEValue::from7BitInt (message.getVelocity()));
    }
}

//==============================================================================
void MPEInstrument::processMidiNoteOffMessage (const MidiMessage& message)
{
    noteOff (message.getChannel(),
             message.getNoteNumber(),
             MPEValue::from7BitInt (message.getVelocity()));
}

//==============================================================================
void MPEInstrument::processMidiPitchWheelMessage (const MidiMessage& message)
{
    pitchbend (message.getChannel(),
               MPEValue::from14BitInt (message.getPitchWheelValue()));
}

//==============================================================================
void MPEInstrument::processMidiChannelPressureMessage (const MidiMessage& message)
{
    pressure (message.getChannel(),
              MPEValue::from7BitInt (message.getChannelPressureValue()));
}

//==============================================================================
void MPEInstrument::processMidiControllerMessage (const MidiMessage& message)
{
    switch (message.getControllerNumber())
    {
        case 64:    sustainPedal      (message.getChannel(), message.isSustainPedalOn());   break;
        case 66:    sostenutoPedal    (message.getChannel(), message.isSostenutoPedalOn()); break;
        case 70:    handlePressureMSB (message.getChannel(), message.getControllerValue()); break;
        case 74:    handleTimbreMSB   (message.getChannel(), message.getControllerValue()); break;
        case 102:   handlePressureLSB (message.getChannel(), message.getControllerValue()); break;
        case 106:   handleTimbreLSB   (message.getChannel(), message.getControllerValue()); break;
        default:    break;
    }
}

//==============================================================================
void MPEInstrument::processMidiResetAllControllersMessage (const MidiMessage& message)
{
    // in MPE mode, "reset all controllers" is per-zone and expected on the master channel;
    // in legacy mode, it is per MIDI channel (within the channel range used).

    if (legacyMode.isEnabled && legacyMode.channelRange.contains (message.getChannel()))
    {
        for (auto i = notes.size(); --i >= 0;)
        {
            auto& note = notes.getReference (i);

            if (note.midiChannel == message.getChannel())
            {
                note.keyState = MPENote::off;
                note.noteOffVelocity = MPEValue::from7BitInt (64); // some reasonable number
                listeners.call ([&] (Listener& l) { l.noteReleased (note); });
                notes.remove (i);
            }
        }
    }
    else if (isMasterChannel (message.getChannel()))
    {
        auto zone = (message.getChannel() == 1 ? zoneLayout.getLowerZone()
                                               : zoneLayout.getUpperZone());

        for (auto i = notes.size(); --i >= 0;)
        {
            auto& note = notes.getReference (i);

            if (zone.isUsing (note.midiChannel))
            {
                note.keyState = MPENote::off;
                note.noteOffVelocity = MPEValue::from7BitInt (64); // some reasonable number
                listeners.call ([&] (Listener& l) { l.noteReleased (note); });
                notes.remove (i);
            }
        }
    }
}

void MPEInstrument::processMidiAfterTouchMessage (const MidiMessage& message)
{
    if (! isMasterChannel (message.getChannel()))
        return;

    polyAftertouch (message.getChannel(), message.getNoteNumber(),
                    MPEValue::from7BitInt (message.getAfterTouchValue()));
}

//==============================================================================
void MPEInstrument::handlePressureMSB (int midiChannel, int value) noexcept
{
    auto lsb = lastPressureLowerBitReceivedOnChannel[midiChannel - 1];

    pressure (midiChannel, lsb == noLSBValueReceived ? MPEValue::from7BitInt (value)
                                                     : MPEValue::from14BitInt (lsb + (value << 7)));
}

void MPEInstrument::handlePressureLSB (int midiChannel, int value) noexcept
{
    lastPressureLowerBitReceivedOnChannel[midiChannel - 1] = uint8 (value);
}

void MPEInstrument::handleTimbreMSB (int midiChannel, int value) noexcept
{
    auto lsb = lastTimbreLowerBitReceivedOnChannel[midiChannel - 1];

    timbre (midiChannel, lsb == noLSBValueReceived ? MPEValue::from7BitInt (value)
                                                   : MPEValue::from14BitInt (lsb + (value << 7)));
}

void MPEInstrument::handleTimbreLSB (int midiChannel, int value) noexcept
{
    lastTimbreLowerBitReceivedOnChannel[midiChannel - 1] = uint8 (value);
}

//==============================================================================
void MPEInstrument::noteOn (int midiChannel,
                            int midiNoteNumber,
                            MPEValue midiNoteOnVelocity)
{
    if (! isUsingChannel (midiChannel))
        return;

    MPENote newNote (midiChannel,
                     midiNoteNumber,
                     midiNoteOnVelocity,
                     getInitialValueForNewNote (midiChannel, pitchbendDimension),
                     getInitialValueForNewNote (midiChannel, pressureDimension),
                     getInitialValueForNewNote (midiChannel, timbreDimension),
                     isMemberChannelSustained[midiChannel - 1] ? MPENote::keyDownAndSustained : MPENote::keyDown);

    const ScopedLock sl (lock);
    updateNoteTotalPitchbend (newNote);

    if (auto* alreadyPlayingNote = getNotePtr (midiChannel, midiNoteNumber))
    {
        // pathological case: second note-on received for same note -> retrigger it
        alreadyPlayingNote->keyState = MPENote::off;
        alreadyPlayingNote->noteOffVelocity = MPEValue::from7BitInt (64); // some reasonable number
        listeners.call ([=] (Listener& l) { l.noteReleased (*alreadyPlayingNote); });
        notes.remove (alreadyPlayingNote);
    }

    notes.add (newNote);
    listeners.call ([&] (Listener& l) { l.noteAdded (newNote); });
}

//==============================================================================
void MPEInstrument::noteOff (int midiChannel,
                             int midiNoteNumber,
                             MPEValue midiNoteOffVelocity)
{
    if (notes.isEmpty() || ! isUsingChannel (midiChannel))
        return;

    const ScopedLock sl (lock);

    if (auto* note = getNotePtr (midiChannel, midiNoteNumber))
    {
        note->keyState = (note->keyState == MPENote::keyDownAndSustained) ? MPENote::sustained : MPENote::off;
        note->noteOffVelocity = midiNoteOffVelocity;

        // If no more notes are playing on this channel in mpe mode, reset the dimension values
        if (! legacyMode.isEnabled && getLastNotePlayedPtr (midiChannel) == nullptr)
        {
            pressureDimension.lastValueReceivedOnChannel[midiChannel - 1] = MPEValue::minValue();
            pitchbendDimension.lastValueReceivedOnChannel[midiChannel - 1] = MPEValue::centreValue();
            timbreDimension.lastValueReceivedOnChannel[midiChannel - 1] = MPEValue::centreValue();
        }

        if (note->keyState == MPENote::off)
        {
            listeners.call ([=] (Listener& l) { l.noteReleased (*note); });
            notes.remove (note);
        }
        else
        {
            listeners.call ([=] (Listener& l) { l.noteKeyStateChanged (*note); });
        }
    }
}

//==============================================================================
void MPEInstrument::pitchbend (int midiChannel, MPEValue value)
{
    const ScopedLock sl (lock);
    updateDimension (midiChannel, pitchbendDimension, value);
}

void MPEInstrument::pressure (int midiChannel, MPEValue value)
{
    const ScopedLock sl (lock);
    updateDimension (midiChannel, pressureDimension, value);
}

void MPEInstrument::timbre (int midiChannel, MPEValue value)
{
    const ScopedLock sl (lock);
    updateDimension (midiChannel, timbreDimension, value);
}

void MPEInstrument::polyAftertouch (int midiChannel, int midiNoteNumber, MPEValue value)
{
    const ScopedLock sl (lock);

    for (auto i = notes.size(); --i >= 0;)
    {
        auto& note = notes.getReference (i);

        if (note.midiChannel == midiChannel
            && note.initialNote == midiNoteNumber
            && pressureDimension.getValue (note) != value)
        {
            pressureDimension.getValue (note) = value;
            callListenersDimensionChanged (note, pressureDimension);
        }
    }
}

MPEValue MPEInstrument::getInitialValueForNewNote (int midiChannel, MPEDimension& dimension) const
{
    if (! legacyMode.isEnabled && getLastNotePlayedPtr (midiChannel) != nullptr)
        return &dimension == &pressureDimension ? MPEValue::minValue() : MPEValue::centreValue();

    return dimension.lastValueReceivedOnChannel[midiChannel - 1];
}

//==============================================================================
void MPEInstrument::updateDimension (int midiChannel, MPEDimension& dimension, MPEValue value)
{
    dimension.lastValueReceivedOnChannel[midiChannel - 1] = value;

    if (notes.isEmpty())
        return;

    if (isMemberChannel (midiChannel))
    {
        if (dimension.trackingMode == allNotesOnChannel)
        {
            for (auto i = notes.size(); --i >= 0;)
            {
                auto& note = notes.getReference (i);

                if (note.midiChannel == midiChannel)
                    updateDimensionForNote (note, dimension, value);
            }
        }
        else
        {
            if (auto* note = getNotePtr (midiChannel, dimension.trackingMode))
                updateDimensionForNote (*note, dimension, value);
        }
    }
    else if (isMasterChannel (midiChannel))
    {
        updateDimensionMaster (midiChannel == 1, dimension, value);
    }
}

//==============================================================================
void MPEInstrument::updateDimensionMaster (bool isLowerZone, MPEDimension& dimension, MPEValue value)
{
    auto zone = (isLowerZone ? zoneLayout.getLowerZone()
                             : zoneLayout.getUpperZone());

    if (! zone.isActive())
        return;

    for (auto i = notes.size(); --i >= 0;)
    {
        auto& note = notes.getReference (i);

        if (! zone.isUsing (note.midiChannel))
            continue;

        if (&dimension == &pitchbendDimension)
        {
            // master pitchbend is a special case: we don't change the note's own pitchbend,
            // instead we have to update its total (master + note) pitchbend.
            updateNoteTotalPitchbend (note);
            listeners.call ([&] (Listener& l) { l.notePitchbendChanged (note); });
        }
        else if (dimension.getValue (note) != value)
        {
            dimension.getValue (note) = value;
            callListenersDimensionChanged (note, dimension);
        }
    }
}

//==============================================================================
void MPEInstrument::updateDimensionForNote (MPENote& note, MPEDimension& dimension, MPEValue value)
{
    if (dimension.getValue (note) != value)
    {
        dimension.getValue (note) = value;

        if (&dimension == &pitchbendDimension)
            updateNoteTotalPitchbend (note);

        callListenersDimensionChanged (note, dimension);
    }
}

//==============================================================================
void MPEInstrument::callListenersDimensionChanged (const MPENote& note, const MPEDimension& dimension)
{
    if (&dimension == &pressureDimension)  { listeners.call ([&] (Listener& l) { l.notePressureChanged  (note); }); return; }
    if (&dimension == &timbreDimension)    { listeners.call ([&] (Listener& l) { l.noteTimbreChanged    (note); }); return; }
    if (&dimension == &pitchbendDimension) { listeners.call ([&] (Listener& l) { l.notePitchbendChanged (note); }); return; }
}

//==============================================================================
void MPEInstrument::updateNoteTotalPitchbend (MPENote& note)
{
    if (legacyMode.isEnabled)
    {
        note.totalPitchbendInSemitones = note.pitchbend.asSignedFloat() * (float) legacyMode.pitchbendRange;
    }
    else
    {
        auto zone = zoneLayout.getLowerZone();

        if (! zone.isActive() || ! zone.isUsing (note.midiChannel))
        {
            auto upperZone = zoneLayout.getUpperZone();

            if (upperZone.isActive() && upperZone.isUsing (note.midiChannel))
            {
                zone = upperZone;
            }
            else
            {
                // this note doesn't belong to any zone!
                jassertfalse;
                return;
            }
        }

        auto notePitchbendInSemitones = 0.0f;

        if (zone.isUsingChannelAsMemberChannel (note.midiChannel))
            notePitchbendInSemitones = note.pitchbend.asSignedFloat() * (float) zone.perNotePitchbendRange;

        auto masterPitchbendInSemitones = pitchbendDimension.lastValueReceivedOnChannel[zone.getMasterChannel() - 1]
                                                            .asSignedFloat()
                                          * (float) zone.masterPitchbendRange;

        note.totalPitchbendInSemitones = notePitchbendInSemitones + masterPitchbendInSemitones;
    }
}

//==============================================================================
void MPEInstrument::sustainPedal (int midiChannel, bool isDown)
{
    const ScopedLock sl (lock);
    handleSustainOrSostenuto (midiChannel, isDown, false);
}

void MPEInstrument::sostenutoPedal (int midiChannel, bool isDown)
{
    const ScopedLock sl (lock);
    handleSustainOrSostenuto (midiChannel, isDown, true);
}

//==============================================================================
void MPEInstrument::handleSustainOrSostenuto (int midiChannel, bool isDown, bool isSostenuto)
{
    // in MPE mode, sustain/sostenuto is per-zone and expected on the master channel;
    // in legacy mode, sustain/sostenuto is per MIDI channel (within the channel range used).

    if (legacyMode.isEnabled ? (! legacyMode.channelRange.contains (midiChannel)) : (! isMasterChannel (midiChannel)))
        return;

    auto zone = (midiChannel == 1 ? zoneLayout.getLowerZone()
                                  : zoneLayout.getUpperZone());

    for (auto i = notes.size(); --i >= 0;)
    {
        auto& note = notes.getReference (i);

        if (legacyMode.isEnabled ? (note.midiChannel == midiChannel) : zone.isUsing (note.midiChannel))
        {
            if (note.keyState == MPENote::keyDown && isDown)
                note.keyState = MPENote::keyDownAndSustained;
            else if (note.keyState == MPENote::sustained && ! isDown)
                note.keyState = MPENote::off;
            else if (note.keyState == MPENote::keyDownAndSustained && ! isDown)
                note.keyState = MPENote::keyDown;

            if (note.keyState == MPENote::off)
            {
                listeners.call ([&] (Listener& l) { l.noteReleased (note); });
                notes.remove (i);
            }
            else
            {
                listeners.call ([&] (Listener& l) { l.noteKeyStateChanged (note); });
            }
        }
    }

    if (! isSostenuto)
    {
        isMemberChannelSustained[midiChannel - 1] = isDown;

        if (! legacyMode.isEnabled)
        {
            if (zone.isLowerZone())
                for (auto i = zone.getFirstMemberChannel(); i <= zone.getLastMemberChannel(); ++i)
                    isMemberChannelSustained[i - 1] = isDown;
            else
                for (auto i = zone.getFirstMemberChannel(); i >= zone.getLastMemberChannel(); --i)
                    isMemberChannelSustained[i - 1] = isDown;
        }
    }
}

//==============================================================================
bool MPEInstrument::isMemberChannel (int midiChannel) const noexcept
{
    if (legacyMode.isEnabled)
        return legacyMode.channelRange.contains (midiChannel);

    return zoneLayout.getLowerZone().isUsingChannelAsMemberChannel (midiChannel)
            || zoneLayout.getUpperZone().isUsingChannelAsMemberChannel (midiChannel);
}

bool MPEInstrument::isMasterChannel (int midiChannel) const noexcept
{
    if (legacyMode.isEnabled)
        return false;

    const auto lowerZone = zoneLayout.getLowerZone();
    const auto upperZone = zoneLayout.getUpperZone();

    return (lowerZone.isActive() && midiChannel == lowerZone.getMasterChannel())
            || (upperZone.isActive() && midiChannel == upperZone.getMasterChannel());
}

bool MPEInstrument::isUsingChannel (int midiChannel) const noexcept
{
    if (legacyMode.isEnabled)
        return legacyMode.channelRange.contains (midiChannel);

    return zoneLayout.getLowerZone().isUsing (midiChannel)
            || zoneLayout.getUpperZone().isUsing (midiChannel);
}

//==============================================================================
int MPEInstrument::getNumPlayingNotes() const noexcept
{
    return notes.size();
}

MPENote MPEInstrument::getNote (int midiChannel, int midiNoteNumber) const noexcept
{
    if (auto* note = getNotePtr (midiChannel, midiNoteNumber))
        return *note;

    return {};
}

MPENote MPEInstrument::getNote (int index) const noexcept
{
    return notes[index];
}

//==============================================================================
MPENote MPEInstrument::getMostRecentNote (int midiChannel) const noexcept
{
    if (auto* note = getLastNotePlayedPtr (midiChannel))
        return *note;

    return {};
}

MPENote MPEInstrument::getMostRecentNoteOtherThan (MPENote otherThanThisNote) const noexcept
{
    for (auto i = notes.size(); --i >= 0;)
    {
        auto& note = notes.getReference (i);

        if (note != otherThanThisNote)
            return note;
    }

    return {};
}

//==============================================================================
const MPENote* MPEInstrument::getNotePtr (int midiChannel, int midiNoteNumber) const noexcept
{
    for (int i = 0; i < notes.size(); ++i)
    {
        auto& note = notes.getReference (i);

        if (note.midiChannel == midiChannel && note.initialNote == midiNoteNumber)
            return &note;
    }

    return nullptr;
}

MPENote* MPEInstrument::getNotePtr (int midiChannel, int midiNoteNumber) noexcept
{
    return const_cast<MPENote*> (static_cast<const MPEInstrument&> (*this).getNotePtr (midiChannel, midiNoteNumber));
}

//==============================================================================
const MPENote* MPEInstrument::getNotePtr (int midiChannel, TrackingMode mode) const noexcept
{
    // for the "all notes" tracking mode, this method can never possibly
    // work because it returns 0 or 1 note but there might be more than one!
    jassert (mode != allNotesOnChannel);

    if (mode == lastNotePlayedOnChannel)  return getLastNotePlayedPtr (midiChannel);
    if (mode == lowestNoteOnChannel)      return getLowestNotePtr (midiChannel);
    if (mode == highestNoteOnChannel)     return getHighestNotePtr (midiChannel);

    return nullptr;
}

MPENote* MPEInstrument::getNotePtr (int midiChannel, TrackingMode mode) noexcept
{
    return const_cast<MPENote*> (static_cast<const MPEInstrument&> (*this).getNotePtr (midiChannel, mode));
}

//==============================================================================
const MPENote* MPEInstrument::getLastNotePlayedPtr (int midiChannel) const noexcept
{
    for (auto i = notes.size(); --i >= 0;)
    {
        auto& note = notes.getReference (i);

        if (note.midiChannel == midiChannel
             && (note.keyState == MPENote::keyDown || note.keyState == MPENote::keyDownAndSustained))
            return &note;
    }

    return nullptr;
}

MPENote* MPEInstrument::getLastNotePlayedPtr (int midiChannel) noexcept
{
    return const_cast<MPENote*> (static_cast<const MPEInstrument&> (*this).getLastNotePlayedPtr (midiChannel));
}

//==============================================================================
const MPENote* MPEInstrument::getHighestNotePtr (int midiChannel) const noexcept
{
    int initialNoteMax = -1;
    const MPENote* result = nullptr;

    for (auto i = notes.size(); --i >= 0;)
    {
        auto& note = notes.getReference (i);

        if (note.midiChannel == midiChannel
             && (note.keyState == MPENote::keyDown || note.keyState == MPENote::keyDownAndSustained)
             && note.initialNote > initialNoteMax)
        {
            result = &note;
            initialNoteMax = note.initialNote;
        }
    }

    return result;
}

MPENote* MPEInstrument::getHighestNotePtr (int midiChannel) noexcept
{
    return const_cast<MPENote*> (static_cast<const MPEInstrument&> (*this).getHighestNotePtr (midiChannel));
}

const MPENote* MPEInstrument::getLowestNotePtr (int midiChannel) const noexcept
{
    int initialNoteMin = 128;
    const MPENote* result = nullptr;

    for (auto i = notes.size(); --i >= 0;)
    {
        auto& note = notes.getReference (i);

        if (note.midiChannel == midiChannel
             && (note.keyState == MPENote::keyDown || note.keyState == MPENote::keyDownAndSustained)
             && note.initialNote < initialNoteMin)
        {
            result = &note;
            initialNoteMin = note.initialNote;
        }
    }

    return result;
}

MPENote* MPEInstrument::getLowestNotePtr (int midiChannel) noexcept
{
    return const_cast<MPENote*> (static_cast<const MPEInstrument&> (*this).getLowestNotePtr (midiChannel));
}

//==============================================================================
void MPEInstrument::releaseAllNotes()
{
    const ScopedLock sl (lock);

    for (auto i = notes.size(); --i >= 0;)
    {
        auto& note = notes.getReference (i);
        note.keyState = MPENote::off;
        note.noteOffVelocity = MPEValue::from7BitInt (64); // some reasonable number
        listeners.call ([&] (Listener& l) { l.noteReleased (note); });
    }

    notes.clear();
}


//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

class MPEInstrumentTests : public UnitTest
{
public:
    MPEInstrumentTests()
        : UnitTest ("MPEInstrument class", UnitTestCategories::midi)
    {
        // using lower and upper MPE zones with the following layout for testing
        //
        // 1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16
        // * ...................|             |........................ *

        testLayout.setLowerZone (5);
        testLayout.setUpperZone (6);
    }

    void runTest() override
    {
        beginTest ("initial zone layout");
        {
            MPEInstrument test;
            expect (! test.getZoneLayout().getLowerZone().isActive());
            expect (! test.getZoneLayout().getUpperZone().isActive());
        }

        beginTest ("get/setZoneLayout");
        {
            MPEInstrument test;
            test.setZoneLayout (testLayout);

            auto newLayout = test.getZoneLayout();

            expect (test.getZoneLayout().getLowerZone().isActive());
            expect (test.getZoneLayout().getUpperZone().isActive());
            expectEquals (newLayout.getLowerZone().getMasterChannel(), 1);
            expectEquals (newLayout.getLowerZone().numMemberChannels, 5);
            expectEquals (newLayout.getUpperZone().getMasterChannel(), 16);
            expectEquals (newLayout.getUpperZone().numMemberChannels, 6);
        }

        beginTest ("noteOn / noteOff");
        {
            {
                MPEInstrument test;
                test.setZoneLayout (testLayout);
                expectEquals (test.getNumPlayingNotes(), 0);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // note-on on unused channel - ignore
                test.noteOn (7, 60, MPEValue::from7BitInt (100));
                expectEquals (test.getNumPlayingNotes(), 0);
                expectEquals (test.noteAddedCallCounter, 0);

                // note-on on member channel - create new note
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                expectEquals (test.getNumPlayingNotes(), 1);
                expectEquals (test.noteAddedCallCounter, 1);
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);

                // note-off
                test.noteOff (3, 60, MPEValue::from7BitInt (33));
                expectEquals (test.getNumPlayingNotes(), 0);
                expectEquals (test.noteReleasedCallCounter, 1);
                expectHasFinishedNote (test, 3, 60, 33);


                // note-on on master channel - create new note
                test.noteOn (1, 62, MPEValue::from7BitInt (100));
                expectEquals (test.getNumPlayingNotes(), 1);
                expectEquals (test.noteAddedCallCounter, 2);
                expectNote (test.getNote (1, 62), 100, 0, 8192, 64, MPENote::keyDown);

                // note-off
                test.noteOff (1, 62, MPEValue::from7BitInt (33));
                expectEquals (test.getNumPlayingNotes(), 0);
                expectEquals (test.noteReleasedCallCounter, 2);
                expectHasFinishedNote (test, 1, 62, 33);
            }

            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));

                // note off with non-matching note number shouldn't do anything
                test.noteOff (3, 61, MPEValue::from7BitInt (33));
                expectEquals (test.getNumPlayingNotes(), 1);
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.noteReleasedCallCounter, 0);

                // note off with non-matching midi channel shouldn't do anything
                test.noteOff (2, 60, MPEValue::from7BitInt (33));
                expectEquals (test.getNumPlayingNotes(), 1);
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.noteReleasedCallCounter, 0);
            }

            {
                // can have multiple notes on the same channel
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);
                test.noteOn (3, 0, MPEValue::from7BitInt (100));
                test.noteOn (3, 1, MPEValue::from7BitInt (100));
                test.noteOn (3, 2, MPEValue::from7BitInt (100));
                expectEquals (test.getNumPlayingNotes(), 3);
                expectNote (test.getNote (3, 0), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 1), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 2), 100, 0, 8192, 64, MPENote::keyDown);
            }
            {
                // pathological case: second note-on for same note should retrigger it.
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);
                test.noteOn (3, 0, MPEValue::from7BitInt (100));
                test.noteOn (3, 0, MPEValue::from7BitInt (60));
                expectEquals (test.getNumPlayingNotes(), 1);
                expectNote (test.getNote (3, 0), 60, 0, 8192, 64, MPENote::keyDown);
            }
        }

        beginTest ("noteReleased after setZoneLayout");
        {
            UnitTestInstrument test;
            test.setZoneLayout (testLayout);

            test.noteOn (3, 60, MPEValue::from7BitInt (100));
            test.noteOn (3, 61, MPEValue::from7BitInt (100));
            test.noteOn (4, 61, MPEValue::from7BitInt (100));
            expectEquals (test.getNumPlayingNotes(), 3);
            expectEquals (test.noteReleasedCallCounter, 0);

            test.setZoneLayout (testLayout);
            expectEquals (test.getNumPlayingNotes(), 0);
            expectEquals (test.noteReleasedCallCounter, 3);
        }

        beginTest ("releaseAllNotes");
        {
            UnitTestInstrument test;
            test.setZoneLayout (testLayout);
            test.noteOn (3, 60, MPEValue::from7BitInt (100));
            test.noteOn (4, 61, MPEValue::from7BitInt (100));
            test.noteOn (15, 62, MPEValue::from7BitInt (100));
            expectEquals (test.getNumPlayingNotes(), 3);

            test.releaseAllNotes();
            expectEquals (test.getNumPlayingNotes(), 0);
        }

        beginTest ("sustainPedal");
        {
            UnitTestInstrument test;
            test.setZoneLayout (testLayout);
            test.noteOn (3, 60, MPEValue::from7BitInt (100));  // note in lower zone
            test.noteOn (10, 60, MPEValue::from7BitInt (100));  // note in upper zone

            // sustain pedal on per-note channel shouldn't do anything.
            test.sustainPedal (3, true);
            expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);

            expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectEquals (test.noteKeyStateChangedCallCounter, 0);

            // sustain pedal on non-zone channel shouldn't do anything either.
            test.sustainPedal (7, true);
            expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectEquals (test.noteKeyStateChangedCallCounter, 0);

            // sustain pedal on master channel should sustain notes on _that_ zone.
            test.sustainPedal (1, true);
            expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDownAndSustained);
            expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectEquals (test.noteKeyStateChangedCallCounter, 1);

            // release
            test.sustainPedal (1, false);
            expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectEquals (test.noteKeyStateChangedCallCounter, 2);

            // should also sustain new notes added after the press
            test.sustainPedal (1, true);
            expectEquals (test.noteKeyStateChangedCallCounter, 3);
            test.noteOn (4, 51, MPEValue::from7BitInt (100));
            expectNote (test.getNote (4, 51), 100, 0, 8192, 64, MPENote::keyDownAndSustained);
            expectEquals (test.noteKeyStateChangedCallCounter, 3);

            // ...but only if that sustain came on the master channel of that zone!
            test.sustainPedal (11, true);
            test.noteOn (11, 52, MPEValue::from7BitInt (100));
            expectNote (test.getNote (11, 52), 100, 0, 8192, 64, MPENote::keyDown);
            test.noteOff (11, 52, MPEValue::from7BitInt (100));
            expectEquals (test.noteReleasedCallCounter, 1);

            // note-off should not turn off sustained notes inside the same zone
            test.noteOff (3, 60, MPEValue::from7BitInt (100));
            test.noteOff (4, 51, MPEValue::from7BitInt (100));
            test.noteOff (10, 60, MPEValue::from7BitInt (100)); // not affected!
            expectEquals (test.getNumPlayingNotes(), 2);
            expectEquals (test.noteReleasedCallCounter, 2);
            expectEquals (test.noteKeyStateChangedCallCounter, 5);
            expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::sustained);
            expectNote (test.getNote (4, 51), 100, 0, 8192, 64, MPENote::sustained);

            // notes should be turned off when pedal is released
            test.sustainPedal (1, false);
            expectEquals (test.getNumPlayingNotes(), 0);
            expectEquals (test.noteReleasedCallCounter, 4);
        }

        beginTest ("sostenutoPedal");
        {
            UnitTestInstrument test;
            test.setZoneLayout (testLayout);
            test.noteOn (3, 60, MPEValue::from7BitInt (100));  // note in lower zone
            test.noteOn (10, 60, MPEValue::from7BitInt (100));  // note in upper zone

            // sostenuto pedal on per-note channel shouldn't do anything.
            test.sostenutoPedal (3, true);
            expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectEquals (test.noteKeyStateChangedCallCounter, 0);

            // sostenuto pedal on non-zone channel shouldn't do anything either.
            test.sostenutoPedal (9, true);
            expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectEquals (test.noteKeyStateChangedCallCounter, 0);

            // sostenuto pedal on master channel should sustain notes on *that* zone.
            test.sostenutoPedal (1, true);
            expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDownAndSustained);
            expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectEquals (test.noteKeyStateChangedCallCounter, 1);

            // release
            test.sostenutoPedal (1, false);
            expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectEquals (test.noteKeyStateChangedCallCounter, 2);

            // should only sustain notes turned on *before* the press (difference to sustain pedal)
            test.sostenutoPedal (1, true);
            expectEquals (test.noteKeyStateChangedCallCounter, 3);
            test.noteOn (4, 51, MPEValue::from7BitInt (100));
            expectEquals (test.getNumPlayingNotes(), 3);
            expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDownAndSustained);
            expectNote (test.getNote (4, 51), 100, 0, 8192, 64, MPENote::keyDown);
            expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
            expectEquals (test.noteKeyStateChangedCallCounter, 3);

            // note-off should not turn off sustained notes inside the same zone,
            // but only if they were turned on *before* the sostenuto pedal (difference to sustain pedal)
            test.noteOff (3, 60, MPEValue::from7BitInt (100));
            test.noteOff (4, 51, MPEValue::from7BitInt (100));
            test.noteOff (10, 60, MPEValue::from7BitInt (100)); // not affected!
            expectEquals (test.getNumPlayingNotes(), 1);
            expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::sustained);
            expectEquals (test.noteReleasedCallCounter, 2);
            expectEquals (test.noteKeyStateChangedCallCounter, 4);

            // notes should be turned off when pedal is released
            test.sustainPedal (1, false);
            expectEquals (test.getNumPlayingNotes(), 0);
            expectEquals (test.noteReleasedCallCounter, 3);
        }

        beginTest ("getMostRecentNote");
        {
            MPEInstrument test;
            test.setZoneLayout (testLayout);

            test.noteOn (3, 60, MPEValue::from7BitInt (100));
            test.noteOn (3, 61, MPEValue::from7BitInt (100));

            {
                auto note = test.getMostRecentNote (2);
                expect (! note.isValid());
            }
            {
                auto note = test.getMostRecentNote (3);
                expect (note.isValid());
                expectEquals (int (note.midiChannel), 3);
                expectEquals (int (note.initialNote), 61);
            }

            test.sustainPedal (1, true);
            test.noteOff (3, 61, MPEValue::from7BitInt (100));

            {
                auto note = test.getMostRecentNote (3);
                expect (note.isValid());
                expectEquals (int (note.midiChannel), 3);
                expectEquals (int (note.initialNote), 60);
            }

            test.sustainPedal (1, false);
            test.noteOff (3, 60, MPEValue::from7BitInt (100));

            {
                auto note = test.getMostRecentNote (3);
                expect (! note.isValid());
            }
        }

        beginTest ("getMostRecentNoteOtherThan");
        {
            MPENote testNote (3, 60,
                              MPEValue::centreValue(), MPEValue::centreValue(),
                              MPEValue::centreValue(), MPEValue::centreValue());

            {
                // case 1: the note to exclude is not the most recent one.

                MPEInstrument test;
                test.setZoneLayout (testLayout);
                expect (! test.getMostRecentNoteOtherThan (testNote).isValid());

                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                expect (! test.getMostRecentNoteOtherThan (testNote).isValid());

                test.noteOn (4, 61, MPEValue::from7BitInt (100));
                expect (test.getMostRecentNoteOtherThan (testNote).isValid());
                expect (test.getMostRecentNoteOtherThan (testNote).midiChannel == 4);
                expect (test.getMostRecentNoteOtherThan (testNote).initialNote == 61);
            }
            {
                // case 2: the note to exclude is the most recent one.

                MPEInstrument test;
                test.setZoneLayout (testLayout);
                expect (! test.getMostRecentNoteOtherThan (testNote).isValid());

                test.noteOn (4, 61, MPEValue::from7BitInt (100));
                expect (test.getMostRecentNoteOtherThan (testNote).isValid());
                expect (test.getMostRecentNoteOtherThan (testNote).midiChannel == 4);
                expect (test.getMostRecentNoteOtherThan (testNote).initialNote == 61);

                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                expect (test.getMostRecentNoteOtherThan (testNote).isValid());
                expect (test.getMostRecentNoteOtherThan (testNote).midiChannel == 4);
                expect (test.getMostRecentNoteOtherThan (testNote).initialNote == 61);
            }
        }

        beginTest ("pressure");
        {
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (4, 60, MPEValue::from7BitInt (100));
                test.noteOn (10, 60, MPEValue::from7BitInt (100));

                // applying pressure on a per-note channel should modulate one note
                test.pressure (3, MPEValue::from7BitInt (33));
                expectNote (test.getNote (3, 60), 100, 33, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (4, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.notePressureChangedCallCounter, 1);

                // applying pressure on a master channel should modulate all notes in this zone
                test.pressure (1, MPEValue::from7BitInt (44));
                expectNote (test.getNote (3, 60), 100, 44, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (4, 60), 100, 44, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.notePressureChangedCallCounter, 3);

                // applying pressure on an unrelated channel should be ignored
                test.pressure (8, MPEValue::from7BitInt (55));
                expectNote (test.getNote (3, 60), 100, 44, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (4, 60), 100, 44, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.notePressureChangedCallCounter, 3);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // two notes on same channel - only last added should be modulated
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.pressure (3, MPEValue::from7BitInt (66));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 66, 8192, 64, MPENote::keyDown);
                expectEquals (test.notePressureChangedCallCounter, 1);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // edge case: two notes on same channel, one gets released,
                // then the other should be modulated
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.noteOff (3, 61, MPEValue::from7BitInt (100));
                test.pressure (3, MPEValue::from7BitInt (77));
                expectEquals (test.getNumPlayingNotes(), 1);
                expectNote (test.getNote (3, 60), 100, 77, 8192, 64, MPENote::keyDown);
                expectEquals (test.notePressureChangedCallCounter, 1);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // if no pressure is sent before note-on, default = 0 should be used
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // if pressure is sent before note-on, use that
                test.pressure (3, MPEValue::from7BitInt (77));
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                expectNote (test.getNote (3, 60), 100, 77, 8192, 64, MPENote::keyDown);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // if pressure is sent before note-on, but it belonged to another note
                // on the same channel that has since been turned off, use default = 0
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.pressure (3, MPEValue::from7BitInt (77));
                test.noteOff (3, 61, MPEValue::from7BitInt (100));
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // edge case: two notes on the same channel simultaneously. the second one should use
                // pressure = 0 initially but then react to additional pressure messages
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.pressure (3, MPEValue::from7BitInt (77));
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
                test.pressure (3, MPEValue::from7BitInt (78));
                expectNote (test.getNote (3, 60), 100, 78, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 77, 8192, 64, MPENote::keyDown);
            }

            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // master channel will use poly-aftertouch for pressure
                test.noteOn (16, 60, MPEValue::from7BitInt (100));
                expectNote (test.getNote (16, 60), 100, 0, 8192, 64, MPENote::keyDown);
                test.aftertouch (16, 60, MPEValue::from7BitInt (27));
                expectNote (test.getNote (16, 60), 100, 27, 8192, 64, MPENote::keyDown);

                // member channels will not respond to poly-aftertouch
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.aftertouch (3, 60, MPEValue::from7BitInt (50));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
            }
        }

        beginTest ("pitchbend");
        {
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (4, 60, MPEValue::from7BitInt (100));
                test.noteOn (10, 60, MPEValue::from7BitInt (100));

                // applying pitchbend on a per-note channel should modulate one note
                test.pitchbend (3, MPEValue::from14BitInt (1111));
                expectNote (test.getNote (3, 60), 100, 0, 1111, 64, MPENote::keyDown);
                expectNote (test.getNote (4, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.notePitchbendChangedCallCounter, 1);

                // applying pitchbend on a master channel should be ignored for the
                // value of per-note pitchbend. Tests covering master pitchbend below.
                // Note: noteChanged will be called anyway for notes in that zone
                // because the total pitchbend for those notes has changed
                test.pitchbend (1, MPEValue::from14BitInt (2222));
                expectNote (test.getNote (3, 60), 100, 0, 1111, 64, MPENote::keyDown);
                expectNote (test.getNote (4, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.notePitchbendChangedCallCounter, 3);

                // applying pitchbend on an unrelated channel should do nothing.
                test.pitchbend (8, MPEValue::from14BitInt (3333));
                expectNote (test.getNote (3, 60), 100, 0, 1111, 64, MPENote::keyDown);
                expectNote (test.getNote (4, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.notePitchbendChangedCallCounter, 3);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // two notes on same channel - only last added should be bent
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.pitchbend (3, MPEValue::from14BitInt (4444));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 0, 4444, 64, MPENote::keyDown);
                expectEquals (test.notePitchbendChangedCallCounter, 1);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // edge case: two notes on same channel, one gets released,
                // then the other should be bent
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.noteOff (3, 61, MPEValue::from7BitInt (100));
                test.pitchbend (3, MPEValue::from14BitInt (5555));
                expectEquals (test.getNumPlayingNotes(), 1);
                expectNote (test.getNote (3, 60), 100, 0, 5555, 64, MPENote::keyDown);
                expectEquals (test.notePitchbendChangedCallCounter, 1);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // Richard's edge case:
                // - press one note
                // - press sustain (careful: must be sent on master channel)
                // - release first note (is still sustained!)
                // - press another note (happens to be on the same MIDI channel!)
                // - pitchbend that other note
                // - the first note should not be bent, only the second one.

                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.sustainPedal (1, true);
                test.noteOff (3, 60, MPEValue::from7BitInt (64));
                expectEquals (test.getNumPlayingNotes(), 1);
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::sustained);
                expectEquals (test.noteKeyStateChangedCallCounter, 2);

                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.pitchbend (3, MPEValue::from14BitInt (6666));
                expectEquals (test.getNumPlayingNotes(), 2);
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::sustained);
                expectNote (test.getNote (3, 61), 100, 0, 6666, 64, MPENote::keyDownAndSustained);
                expectEquals (test.notePitchbendChangedCallCounter, 1);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // Zsolt's edge case:
                // - press one note
                // - modulate pitchbend or timbre
                // - release the note
                // - press same note again without sending a pitchbend or timbre message before the note-on
                // - the note should be turned on with a default value for pitchbend/timbre,
                //   and *not* the last value received on channel.

                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.pitchbend (3, MPEValue::from14BitInt (5555));
                expectNote (test.getNote (3, 60), 100, 0, 5555, 64, MPENote::keyDown);

                test.noteOff (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
            }
            {
                // applying per-note pitchbend should set the note's totalPitchbendInSemitones
                // correctly depending on the per-note pitchbend range of the zone.
                UnitTestInstrument test;

                MPEZoneLayout layout = testLayout;
                test.setZoneLayout (layout);  // default should be +/- 48 semitones
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.pitchbend (3, MPEValue::from14BitInt (4096));
                expectDoubleWithinRelativeError (test.getMostRecentNote (3).totalPitchbendInSemitones, -24.0, 0.01);

                layout.setLowerZone (5, 96);
                test.setZoneLayout (layout);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.pitchbend (3, MPEValue::from14BitInt (0)); // -max
                expectDoubleWithinRelativeError (test.getMostRecentNote (3).totalPitchbendInSemitones, -96.0, 0.01);

                layout.setLowerZone (5, 1);
                test.setZoneLayout (layout);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.pitchbend (3, MPEValue::from14BitInt (16383)); // +max
                expectDoubleWithinRelativeError (test.getMostRecentNote (3).totalPitchbendInSemitones, 1.0, 0.01);

                layout.setLowerZone (5, 0); // pitchbendrange = 0 --> no pitchbend at all
                test.setZoneLayout (layout);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.pitchbend (3, MPEValue::from14BitInt (12345));
                expectDoubleWithinRelativeError (test.getMostRecentNote (3).totalPitchbendInSemitones, 0.0, 0.01);
            }
            {
                // applying master pitchbend should set the note's totalPitchbendInSemitones
                // correctly depending on the master pitchbend range of the zone.
                UnitTestInstrument test;

                MPEZoneLayout layout = testLayout;
                test.setZoneLayout (layout);  // default should be +/- 2 semitones
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.pitchbend (1, MPEValue::from14BitInt (4096)); //halfway between -max and centre
                expectDoubleWithinRelativeError (test.getMostRecentNote (3).totalPitchbendInSemitones, -1.0, 0.01);

                layout.setLowerZone (5, 48, 96);
                test.setZoneLayout (layout);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.pitchbend (1, MPEValue::from14BitInt (0)); // -max
                expectDoubleWithinRelativeError (test.getMostRecentNote (3).totalPitchbendInSemitones, -96.0, 0.01);

                layout.setLowerZone (5, 48, 1);
                test.setZoneLayout (layout);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.pitchbend (1, MPEValue::from14BitInt (16383)); // +max
                expectDoubleWithinRelativeError (test.getMostRecentNote (3).totalPitchbendInSemitones, 1.0, 0.01);

                layout.setLowerZone (5, 48, 0); // pitchbendrange = 0 --> no pitchbend at all
                test.setZoneLayout (layout);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.pitchbend (1, MPEValue::from14BitInt (12345));
                expectDoubleWithinRelativeError (test.getMostRecentNote (3).totalPitchbendInSemitones, 0.0, 0.01);
            }
            {
                // applying both per-note and master pitchbend simultaneously should set
                // the note's totalPitchbendInSemitones to the sum of both, correctly
                // weighted with the per-note and master pitchbend range, respectively.
                UnitTestInstrument test;

                MPEZoneLayout layout = testLayout;
                layout.setLowerZone (5, 12, 1);
                test.setZoneLayout (layout);

                test.pitchbend (1, MPEValue::from14BitInt (4096)); // master pitchbend 0.5 semitones down
                test.pitchbend (3, MPEValue::from14BitInt (0)); // per-note pitchbend 12 semitones down
                // additionally, note should react to both pitchbend messages
                // correctly even if they arrived before the note-on.
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                expectDoubleWithinRelativeError (test.getMostRecentNote (3).totalPitchbendInSemitones, -12.5, 0.01);
            }
        }

        beginTest ("timbre");
        {
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (4, 60, MPEValue::from7BitInt (100));
                test.noteOn (10, 60, MPEValue::from7BitInt (100));

                // modulating timbre on a per-note channel should modulate one note
                test.timbre (3, MPEValue::from7BitInt (33));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 33, MPENote::keyDown);
                expectNote (test.getNote (4, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.noteTimbreChangedCallCounter, 1);

                // modulating timbre on a master channel should modulate all notes in this zone
                test.timbre (1, MPEValue::from7BitInt (44));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 44, MPENote::keyDown);
                expectNote (test.getNote (4, 60), 100, 0, 8192, 44, MPENote::keyDown);
                expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.noteTimbreChangedCallCounter, 3);

                // modulating timbre on an unrelated channel should be ignored
                test.timbre (9, MPEValue::from7BitInt (55));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 44, MPENote::keyDown);
                expectNote (test.getNote (4, 60), 100, 0, 8192, 44, MPENote::keyDown);
                expectNote (test.getNote (10, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.noteTimbreChangedCallCounter, 3);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // two notes on same channel - only last added should be modulated
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.timbre (3, MPEValue::from7BitInt (66));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 0, 8192, 66, MPENote::keyDown);
                expectEquals (test.noteTimbreChangedCallCounter, 1);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // edge case: two notes on same channel, one gets released,
                // then the other should be modulated
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.noteOff (3, 61, MPEValue::from7BitInt (100));
                test.timbre (3, MPEValue::from7BitInt (77));
                expectEquals (test.getNumPlayingNotes(), 1);
                expectNote (test.getNote (3, 60), 100, 0, 8192, 77, MPENote::keyDown);
                expectEquals (test.noteTimbreChangedCallCounter, 1);
            }
            {
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                // Zsolt's edge case for timbre
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.timbre (3, MPEValue::from7BitInt (42));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 42, MPENote::keyDown);

                test.noteOff (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
            }
        }

        beginTest ("setPressureTrackingMode");
        {
            {
                // last note played (= default)
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.setPressureTrackingMode (MPEInstrument::lastNotePlayedOnChannel);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 62, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.pressure (3, MPEValue::from7BitInt (99));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 62), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 99,  8192, 64, MPENote::keyDown);
                expectEquals (test.notePressureChangedCallCounter, 1);
            }
            {
                // lowest note
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.setPressureTrackingMode (MPEInstrument::lowestNoteOnChannel);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 62, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.pressure (3, MPEValue::from7BitInt (99));
                expectNote (test.getNote (3, 60), 100, 99,  8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 62), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.notePressureChangedCallCounter, 1);
            }
            {
                // highest note
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.setPressureTrackingMode (MPEInstrument::highestNoteOnChannel);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 62, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.pressure (3, MPEValue::from7BitInt (99));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 62), 100, 99,  8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.notePressureChangedCallCounter, 1);
            }
            {
                // all notes
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.setPressureTrackingMode (MPEInstrument::allNotesOnChannel);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 62, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.pressure (3, MPEValue::from7BitInt (99));
                expectNote (test.getNote (3, 60), 100, 99, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 62), 100, 99, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 99, 8192, 64, MPENote::keyDown);
                expectEquals (test.notePressureChangedCallCounter, 3);
            }
        }

        beginTest ("setPitchbendTrackingMode");
        {
            {
                // last note played (= default)
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.setPitchbendTrackingMode (MPEInstrument::lastNotePlayedOnChannel);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 62, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.pitchbend (3, MPEValue::from14BitInt (9999));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 62), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 0, 9999, 64, MPENote::keyDown);
                expectEquals (test.notePitchbendChangedCallCounter, 1);
            }
            {
                // lowest note
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.setPitchbendTrackingMode (MPEInstrument::lowestNoteOnChannel);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 62, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.pitchbend (3, MPEValue::from14BitInt (9999));
                expectNote (test.getNote (3, 60), 100, 0, 9999, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 62), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.notePitchbendChangedCallCounter, 1);
            }
            {
                // highest note
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.setPitchbendTrackingMode (MPEInstrument::highestNoteOnChannel);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 62, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.pitchbend (3, MPEValue::from14BitInt (9999));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 62), 100, 0, 9999, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.notePitchbendChangedCallCounter, 1);
            }
            {
                // all notes
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.setPitchbendTrackingMode (MPEInstrument::allNotesOnChannel);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 62, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.pitchbend (3, MPEValue::from14BitInt (9999));
                expectNote (test.getNote (3, 60), 100, 0, 9999, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 62), 100, 0, 9999, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 0, 9999, 64, MPENote::keyDown);
                expectEquals (test.notePitchbendChangedCallCounter, 3);
            }
        }

        beginTest ("setTimbreTrackingMode");
        {
            {
                // last note played (= default)
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.setTimbreTrackingMode (MPEInstrument::lastNotePlayedOnChannel);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 62, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.timbre (3, MPEValue::from7BitInt (99));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 62), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 0, 8192, 99, MPENote::keyDown);
                expectEquals (test.noteTimbreChangedCallCounter, 1);
            }
            {
                // lowest note
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.setTimbreTrackingMode (MPEInstrument::lowestNoteOnChannel);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 62, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.timbre (3, MPEValue::from7BitInt (99));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 99, MPENote::keyDown);
                expectNote (test.getNote (3, 62), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.noteTimbreChangedCallCounter, 1);
            }
            {
                // highest note
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.setTimbreTrackingMode (MPEInstrument::highestNoteOnChannel);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 62, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.timbre (3, MPEValue::from7BitInt (99));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (3, 62), 100, 0, 8192, 99, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 0, 8192, 64, MPENote::keyDown);
                expectEquals (test.noteTimbreChangedCallCounter, 1);
            }
            {
                // all notes
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);

                test.setTimbreTrackingMode (MPEInstrument::allNotesOnChannel);
                test.noteOn (3, 60, MPEValue::from7BitInt (100));
                test.noteOn (3, 62, MPEValue::from7BitInt (100));
                test.noteOn (3, 61, MPEValue::from7BitInt (100));
                test.timbre (3, MPEValue::from7BitInt (99));
                expectNote (test.getNote (3, 60), 100, 0, 8192, 99, MPENote::keyDown);
                expectNote (test.getNote (3, 62), 100, 0, 8192, 99, MPENote::keyDown);
                expectNote (test.getNote (3, 61), 100, 0, 8192, 99, MPENote::keyDown);
                expectEquals (test.noteTimbreChangedCallCounter, 3);
            }
        }

        beginTest ("processNextMidiEvent");
        {
            UnitTestInstrument test;

            // note on should trigger noteOn method call

            test.processNextMidiEvent (MidiMessage::noteOn (3, 42, uint8 (92)));
            expectEquals (test.noteOnCallCounter, 1);
            expectEquals (test.lastMidiChannelReceived, 3);
            expectEquals (test.lastMidiNoteNumberReceived, 42);
            expectEquals (test.lastMPEValueReceived.as7BitInt(), 92);

            // note off should trigger noteOff method call

            test.processNextMidiEvent (MidiMessage::noteOff (4, 12, uint8 (33)));
            expectEquals (test.noteOffCallCounter, 1);
            expectEquals (test.lastMidiChannelReceived, 4);
            expectEquals (test.lastMidiNoteNumberReceived, 12);
            expectEquals (test.lastMPEValueReceived.as7BitInt(), 33);

            // note on with velocity = 0 should trigger noteOff method call
            // with a note off velocity of 64 (centre value)

            test.processNextMidiEvent (MidiMessage::noteOn (5, 11, uint8 (0)));
            expectEquals (test.noteOffCallCounter, 2);
            expectEquals (test.lastMidiChannelReceived, 5);
            expectEquals (test.lastMidiNoteNumberReceived, 11);
            expectEquals (test.lastMPEValueReceived.as7BitInt(), 64);

            // pitchwheel message should trigger pitchbend method call

            test.processNextMidiEvent (MidiMessage::pitchWheel (1, 3333));
            expectEquals (test.pitchbendCallCounter, 1);
            expectEquals (test.lastMidiChannelReceived, 1);
            expectEquals (test.lastMPEValueReceived.as14BitInt(), 3333);

            // pressure using channel pressure message (7-bit value) should
            // trigger pressure method call

            test.processNextMidiEvent (MidiMessage::channelPressureChange (10, 35));
            expectEquals (test.pressureCallCounter, 1);
            expectEquals (test.lastMidiChannelReceived, 10);
            expectEquals (test.lastMPEValueReceived.as7BitInt(), 35);

            // pressure using 14-bit value over CC70 and CC102 should trigger
            // pressure method call after the MSB is sent

            // a) sending only the MSB
            test.processNextMidiEvent (MidiMessage::controllerEvent (3, 70, 120));
            expectEquals (test.pressureCallCounter, 2);
            expectEquals (test.lastMidiChannelReceived, 3);
            expectEquals (test.lastMPEValueReceived.as7BitInt(), 120);

            // b) sending LSB and MSB (only the MSB should trigger the call) - per MIDI channel!
            test.processNextMidiEvent (MidiMessage::controllerEvent (4, 102, 121));
            expectEquals (test.pressureCallCounter, 2);
            test.processNextMidiEvent (MidiMessage::controllerEvent (5, 102, 122));
            expectEquals (test.pressureCallCounter, 2);
            test.processNextMidiEvent (MidiMessage::controllerEvent (4, 70, 123));
            expectEquals (test.pressureCallCounter, 3);
            expectEquals (test.lastMidiChannelReceived, 4);
            expectEquals (test.lastMPEValueReceived.as14BitInt(), 121 + (123 << 7));
            test.processNextMidiEvent (MidiMessage::controllerEvent (5, 70, 124));
            expectEquals (test.pressureCallCounter, 4);
            expectEquals (test.lastMidiChannelReceived, 5);
            expectEquals (test.lastMPEValueReceived.as14BitInt(), 122 + (124 << 7));
            test.processNextMidiEvent (MidiMessage::controllerEvent (5, 70, 64));
            expectEquals (test.pressureCallCounter, 5);
            expectEquals (test.lastMidiChannelReceived, 5);
            expectEquals (test.lastMPEValueReceived.as7BitInt(), 64);

            // same for timbre 14-bit value over CC74 and CC106
            test.processNextMidiEvent (MidiMessage::controllerEvent (3, 74, 120));
            expectEquals (test.timbreCallCounter, 1);
            expectEquals (test.lastMidiChannelReceived, 3);
            expectEquals (test.lastMPEValueReceived.as7BitInt(), 120);
            test.processNextMidiEvent (MidiMessage::controllerEvent (4, 106, 121));
            expectEquals (test.timbreCallCounter, 1);
            test.processNextMidiEvent (MidiMessage::controllerEvent (5, 106, 122));
            expectEquals (test.timbreCallCounter, 1);
            test.processNextMidiEvent (MidiMessage::controllerEvent (4, 74, 123));
            expectEquals (test.timbreCallCounter, 2);
            expectEquals (test.lastMidiChannelReceived, 4);
            expectEquals (test.lastMPEValueReceived.as14BitInt(), 121 + (123 << 7));
            test.processNextMidiEvent (MidiMessage::controllerEvent (5, 74, 124));
            expectEquals (test.timbreCallCounter, 3);
            expectEquals (test.lastMidiChannelReceived, 5);
            expectEquals (test.lastMPEValueReceived.as14BitInt(), 122 + (124 << 7));
            test.processNextMidiEvent (MidiMessage::controllerEvent (5, 74, 64));
            expectEquals (test.timbreCallCounter, 4);
            expectEquals (test.lastMidiChannelReceived, 5);
            expectEquals (test.lastMPEValueReceived.as7BitInt(), 64);

            // sustain pedal message (CC64) should trigger sustainPedal method call
            test.processNextMidiEvent (MidiMessage::controllerEvent (1, 64, 127));
            expectEquals (test.sustainPedalCallCounter, 1);
            expectEquals (test.lastMidiChannelReceived, 1);
            expect (test.lastSustainPedalValueReceived);
            test.processNextMidiEvent (MidiMessage::controllerEvent (16, 64, 0));
            expectEquals (test.sustainPedalCallCounter, 2);
            expectEquals (test.lastMidiChannelReceived, 16);
            expect (! test.lastSustainPedalValueReceived);

            // sostenuto pedal message (CC66) should trigger sostenutoPedal method call
            test.processNextMidiEvent (MidiMessage::controllerEvent (1, 66, 127));
            expectEquals (test.sostenutoPedalCallCounter, 1);
            expectEquals (test.lastMidiChannelReceived, 1);
            expect (test.lastSostenutoPedalValueReceived);
            test.processNextMidiEvent (MidiMessage::controllerEvent (16, 66, 0));
            expectEquals (test.sostenutoPedalCallCounter, 2);
            expectEquals (test.lastMidiChannelReceived, 16);
            expect (! test.lastSostenutoPedalValueReceived);
        }
        {
            // MIDI messages modifying the zone layout should be correctly
            // forwarded to the internal zone layout and modify it.
            // (testing the actual logic of the zone layout is done in the
            // MPEZoneLayout unit tests)
            MPEInstrument test;

            MidiBuffer buffer;
            buffer.addEvents (MPEMessages::setLowerZone (5), 0, -1, 0);
            buffer.addEvents (MPEMessages::setUpperZone (6), 0, -1, 0);

            for (const auto metadata : buffer)
                test.processNextMidiEvent (metadata.getMessage());

            expect (test.getZoneLayout().getLowerZone().isActive());
            expect (test.getZoneLayout().getUpperZone().isActive());
            expectEquals (test.getZoneLayout().getLowerZone().getMasterChannel(), 1);
            expectEquals (test.getZoneLayout().getLowerZone().numMemberChannels, 5);
            expectEquals (test.getZoneLayout().getUpperZone().getMasterChannel(), 16);
            expectEquals (test.getZoneLayout().getUpperZone().numMemberChannels, 6);
        }

        beginTest ("MIDI all notes off");
        {
            UnitTestInstrument test;
            test.setZoneLayout (testLayout);
            test.noteOn (3, 60, MPEValue::from7BitInt (100));
            test.noteOn (4, 61, MPEValue::from7BitInt (100));
            test.noteOn (15, 62, MPEValue::from7BitInt (100));
            test.noteOn (15, 63, MPEValue::from7BitInt (100));
            expectEquals (test.getNumPlayingNotes(), 4);

            // on note channel: ignore.
            test.processNextMidiEvent (MidiMessage::allControllersOff (3));
            expectEquals (test.getNumPlayingNotes(), 4);

            // on unused channel: ignore.
            test.processNextMidiEvent (MidiMessage::allControllersOff (9));
            expectEquals (test.getNumPlayingNotes(), 4);

            // on master channel: release notes in that zone only.
            test.processNextMidiEvent (MidiMessage::allControllersOff (1));
            expectEquals (test.getNumPlayingNotes(), 2);
            test.processNextMidiEvent (MidiMessage::allControllersOff (16));
            expectEquals (test.getNumPlayingNotes(), 0);
        }

        beginTest ("MIDI all notes off (legacy mode)");
        {
            UnitTestInstrument test;
            test.enableLegacyMode();
            test.noteOn (3, 60, MPEValue::from7BitInt (100));
            test.noteOn (4, 61, MPEValue::from7BitInt (100));
            test.noteOn (15, 62, MPEValue::from7BitInt (100));
            test.noteOn (15, 63, MPEValue::from7BitInt (100));
            expectEquals (test.getNumPlayingNotes(), 4);

            test.processNextMidiEvent (MidiMessage::allControllersOff (3));
            expectEquals (test.getNumPlayingNotes(), 3);

            test.processNextMidiEvent (MidiMessage::allControllersOff (15));
            expectEquals (test.getNumPlayingNotes(), 1);

            test.processNextMidiEvent (MidiMessage::allControllersOff (4));
            expectEquals (test.getNumPlayingNotes(), 0);
        }

        beginTest ("default initial values for pitchbend and timbre");
        {
            MPEInstrument test;
            test.setZoneLayout (testLayout);

            test.pitchbend (3, MPEValue::from14BitInt (3333));  // use for next note-on on ch. 3
            test.pitchbend (2, MPEValue::from14BitInt (4444));  // ignore
            test.pitchbend (2, MPEValue::from14BitInt (5555));  // ignore

            test.timbre (3, MPEValue::from7BitInt (66));  // use for next note-on on ch. 3
            test.timbre (2, MPEValue::from7BitInt (77));  // ignore
            test.timbre (2, MPEValue::from7BitInt (88));  // ignore

            test.noteOn (3, 60, MPEValue::from7BitInt (100));

            expectNote (test.getMostRecentNote (3), 100, 0, 3333, 66, MPENote::keyDown);
        }

        beginTest ("Legacy mode");
        {
            {
                // basic check
                MPEInstrument test;
                expect (! test.isLegacyModeEnabled());

                test.setZoneLayout (testLayout);
                expect (! test.isLegacyModeEnabled());

                test.enableLegacyMode();
                expect (test.isLegacyModeEnabled());

                test.setZoneLayout (testLayout);
                expect (! test.isLegacyModeEnabled());
            }
            {
                // constructor w/o default arguments
                 MPEInstrument test;
                test.enableLegacyMode (0, Range<int> (1, 11));
                expectEquals (test.getLegacyModePitchbendRange(), 0);
                expect (test.getLegacyModeChannelRange() == Range<int> (1, 11));
            }
            {
                // getters and setters
                MPEInstrument test;
                test.enableLegacyMode();

                expectEquals (test.getLegacyModePitchbendRange(), 2);
                expect (test.getLegacyModeChannelRange() == Range<int> (1, 17));

                test.setLegacyModePitchbendRange (96);
                expectEquals (test.getLegacyModePitchbendRange(), 96);

                test.setLegacyModeChannelRange (Range<int> (10, 12));
                expect (test.getLegacyModeChannelRange() == Range<int> (10, 12));
            }
            {
                // note on should trigger notes on all 16 channels

                UnitTestInstrument test;
                test.enableLegacyMode();

                test.noteOn (1,  60, MPEValue::from7BitInt (100));
                test.noteOn (2,  60, MPEValue::from7BitInt (100));
                test.noteOn (15, 60, MPEValue::from7BitInt (100));
                test.noteOn (16, 60, MPEValue::from7BitInt (100));
                expectEquals (test.getNumPlayingNotes(), 4);

                // polyphonic modulation should work across all 16 channels

                test.pitchbend (1, MPEValue::from14BitInt (9999));
                test.pressure (2, MPEValue::from7BitInt (88));
                test.timbre (15, MPEValue::from7BitInt (77));

                expectNote (test.getNote (1, 60),  100, 0, 9999, 64, MPENote::keyDown);
                expectNote (test.getNote (2, 60),  100, 88,  8192, 64, MPENote::keyDown);
                expectNote (test.getNote (15, 60), 100, 0, 8192, 77, MPENote::keyDown);
                expectNote (test.getNote (16, 60), 100, 0, 8192, 64, MPENote::keyDown);

                // note off should work in legacy mode

                test.noteOff (15, 60, MPEValue::from7BitInt (0));
                test.noteOff (1,  60, MPEValue::from7BitInt (0));
                test.noteOff (2,  60, MPEValue::from7BitInt (0));
                test.noteOff (16, 60, MPEValue::from7BitInt (0));
                expectEquals (test.getNumPlayingNotes(), 0);
            }
            {
                // legacy mode w/ custom channel range: note on should trigger notes only within range

                UnitTestInstrument test;
                test.enableLegacyMode (2, Range<int> (3, 8));  // channels 3-7

                test.noteOn (1,  60, MPEValue::from7BitInt (100));
                test.noteOn (2,  60, MPEValue::from7BitInt (100));
                test.noteOn (3, 60, MPEValue::from7BitInt (100));   // should trigger
                test.noteOn (4, 60, MPEValue::from7BitInt (100));   // should trigger
                test.noteOn (6, 60, MPEValue::from7BitInt (100));   // should trigger
                test.noteOn (7, 60, MPEValue::from7BitInt (100));   // should trigger
                test.noteOn (8, 60, MPEValue::from7BitInt (100));
                test.noteOn (16, 60, MPEValue::from7BitInt (100));

                expectEquals (test.getNumPlayingNotes(), 4);
                expectNote (test.getNote (3, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (4, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (6, 60), 100, 0, 8192, 64, MPENote::keyDown);
                expectNote (test.getNote (7, 60), 100, 0, 8192, 64, MPENote::keyDown);
            }
            {
                // tracking mode in legacy mode
                {
                    UnitTestInstrument test;
                    test.enableLegacyMode();

                    test.setPitchbendTrackingMode (MPEInstrument::lastNotePlayedOnChannel);
                    test.noteOn (1,  60, MPEValue::from7BitInt (100));
                    test.noteOn (1,  62, MPEValue::from7BitInt (100));
                    test.noteOn (1,  61, MPEValue::from7BitInt (100));
                    test.pitchbend (1, MPEValue::from14BitInt (9999));
                    expectNote (test.getNote (1, 60),  100, 0, 8192, 64, MPENote::keyDown);
                    expectNote (test.getNote (1, 61),  100, 0, 9999, 64, MPENote::keyDown);
                    expectNote (test.getNote (1, 62),  100, 0, 8192, 64, MPENote::keyDown);
                }
                {
                    UnitTestInstrument test;
                    test.enableLegacyMode();

                    test.setPitchbendTrackingMode (MPEInstrument::lowestNoteOnChannel);
                    test.noteOn (1,  60, MPEValue::from7BitInt (100));
                    test.noteOn (1,  62, MPEValue::from7BitInt (100));
                    test.noteOn (1,  61, MPEValue::from7BitInt (100));
                    test.pitchbend (1, MPEValue::from14BitInt (9999));
                    expectNote (test.getNote (1, 60),  100, 0, 9999, 64, MPENote::keyDown);
                    expectNote (test.getNote (1, 61),  100, 0,  8192, 64, MPENote::keyDown);
                    expectNote (test.getNote (1, 62),  100, 0,  8192, 64, MPENote::keyDown);
                }
                {
                    UnitTestInstrument test;
                    test.enableLegacyMode();

                    test.setPitchbendTrackingMode (MPEInstrument::highestNoteOnChannel);
                    test.noteOn (1,  60, MPEValue::from7BitInt (100));
                    test.noteOn (1,  62, MPEValue::from7BitInt (100));
                    test.noteOn (1,  61, MPEValue::from7BitInt (100));
                    test.pitchbend (1, MPEValue::from14BitInt (9999));
                    expectNote (test.getNote (1, 60),  100, 0, 8192, 64, MPENote::keyDown);
                    expectNote (test.getNote (1, 61),  100, 0,  8192, 64, MPENote::keyDown);
                    expectNote (test.getNote (1, 62),  100, 0,  9999, 64, MPENote::keyDown);
                }
                {
                    UnitTestInstrument test;
                    test.enableLegacyMode();

                    test.setPitchbendTrackingMode (MPEInstrument::allNotesOnChannel);
                    test.noteOn (1,  60, MPEValue::from7BitInt (100));
                    test.noteOn (1,  62, MPEValue::from7BitInt (100));
                    test.noteOn (1,  61, MPEValue::from7BitInt (100));
                    test.pitchbend (1, MPEValue::from14BitInt (9999));
                    expectNote (test.getNote (1, 60),  100, 0, 9999, 64, MPENote::keyDown);
                    expectNote (test.getNote (1, 61),  100, 0,  9999, 64, MPENote::keyDown);
                    expectNote (test.getNote (1, 62),  100, 0,  9999, 64, MPENote::keyDown);
                }
            }
            {
                // custom pitchbend range in legacy mode.
                UnitTestInstrument test;
                test.enableLegacyMode (11);

                test.pitchbend (1, MPEValue::from14BitInt (4096));
                test.noteOn (1, 60, MPEValue::from7BitInt (100));
                expectDoubleWithinRelativeError (test.getMostRecentNote (1).totalPitchbendInSemitones, -5.5, 0.01);
            }
            {
                // sustain pedal should be per channel in legacy mode.
                UnitTestInstrument test;
                test.enableLegacyMode();

                test.sustainPedal (1, true);
                test.noteOn (2,  61, MPEValue::from7BitInt (100));
                test.noteOff (2,  61, MPEValue::from7BitInt (100));
                test.noteOn (1, 60, MPEValue::from7BitInt (100));
                test.noteOff (1, 60, MPEValue::from7BitInt (100));

                expectEquals (test.getNumPlayingNotes(), 1);
                expectNote (test.getNote (1, 60), 100, 0, 8192, 64, MPENote::sustained);

                test.sustainPedal (1, false);
                expectEquals (test.getNumPlayingNotes(), 0);

                test.noteOn (2,  61, MPEValue::from7BitInt (100));
                test.sustainPedal (1, true);
                test.noteOff (2,  61, MPEValue::from7BitInt (100));
                expectEquals (test.getNumPlayingNotes(), 0);

            }
            {
                // sostenuto pedal should be per channel in legacy mode.
                UnitTestInstrument test;
                test.enableLegacyMode();

                test.noteOn (1, 60, MPEValue::from7BitInt (100));
                test.sostenutoPedal (1, true);
                test.noteOff (1, 60, MPEValue::from7BitInt (100));
                test.noteOn (2,  61, MPEValue::from7BitInt (100));
                test.noteOff (2,  61, MPEValue::from7BitInt (100));

                expectEquals (test.getNumPlayingNotes(), 1);
                expectNote (test.getNote (1, 60), 100, 0, 8192, 64, MPENote::sustained);

                test.sostenutoPedal (1, false);
                expectEquals (test.getNumPlayingNotes(), 0);

                test.noteOn (2,  61, MPEValue::from7BitInt (100));
                test.sostenutoPedal (1, true);
                test.noteOff (2,  61, MPEValue::from7BitInt (100));
                expectEquals (test.getNumPlayingNotes(), 0);
            }
            {
                // all notes released when switching layout
                UnitTestInstrument test;
                test.setZoneLayout (testLayout);
                test.noteOn (3,  60, MPEValue::from7BitInt (100));
                expectEquals (test.getNumPlayingNotes(), 1);

                test.enableLegacyMode();
                expectEquals (test.getNumPlayingNotes(), 0);
                test.noteOn (3,  60, MPEValue::from7BitInt (100));
                expectEquals (test.getNumPlayingNotes(), 1);

                test.setZoneLayout (testLayout);
                expectEquals (test.getNumPlayingNotes(), 0);
            }
        }
    }

private:
    //==============================================================================
    /* This mock class is used for unit testing whether the methods of
       MPEInstrument are called correctly.
    */
    class UnitTestInstrument : public MPEInstrument,
                               private MPEInstrument::Listener
    {
        using Base = MPEInstrument;

    public:
        UnitTestInstrument()
            : noteOnCallCounter (0),  noteOffCallCounter (0), pitchbendCallCounter (0),
              pressureCallCounter (0), timbreCallCounter (0), sustainPedalCallCounter (0),
              sostenutoPedalCallCounter (0), noteAddedCallCounter (0), notePressureChangedCallCounter (0),
              notePitchbendChangedCallCounter (0), noteTimbreChangedCallCounter (0),
              noteKeyStateChangedCallCounter (0), noteReleasedCallCounter (0),
              lastMidiChannelReceived (-1), lastMidiNoteNumberReceived (-1),
              lastSustainPedalValueReceived (false), lastSostenutoPedalValueReceived (false)
        {
            addListener (this);
        }

        void noteOn (int midiChannel, int midiNoteNumber, MPEValue midiNoteOnVelocity) override
        {
            Base::noteOn (midiChannel, midiNoteNumber, midiNoteOnVelocity);

            noteOnCallCounter++;
            lastMidiChannelReceived = midiChannel;
            lastMidiNoteNumberReceived = midiNoteNumber;
            lastMPEValueReceived = midiNoteOnVelocity;
        }

        void noteOff (int midiChannel, int midiNoteNumber, MPEValue midiNoteOffVelocity) override
        {
            Base::noteOff (midiChannel, midiNoteNumber, midiNoteOffVelocity);

            noteOffCallCounter++;
            lastMidiChannelReceived = midiChannel;
            lastMidiNoteNumberReceived = midiNoteNumber;
            lastMPEValueReceived = midiNoteOffVelocity;
        }

        void pitchbend (int midiChannel, MPEValue value) override
        {
            Base::pitchbend (midiChannel, value);

            pitchbendCallCounter++;
            lastMidiChannelReceived = midiChannel;
            lastMPEValueReceived = value;
        }

        void pressure (int midiChannel, MPEValue value) override
        {
            Base::pressure (midiChannel, value);

            pressureCallCounter++;
            lastMidiChannelReceived = midiChannel;
            lastMPEValueReceived = value;
        }

        void timbre (int midiChannel, MPEValue value) override
        {
            Base::timbre (midiChannel, value);

            timbreCallCounter++;
            lastMidiChannelReceived = midiChannel;
            lastMPEValueReceived = value;
        }

        void sustainPedal (int midiChannel, bool value) override
        {
            Base::sustainPedal (midiChannel, value);

            sustainPedalCallCounter++;
            lastMidiChannelReceived = midiChannel;
            lastSustainPedalValueReceived = value;
        }

        void sostenutoPedal (int midiChannel, bool value) override
        {
            Base::sostenutoPedal (midiChannel, value);

            sostenutoPedalCallCounter++;
            lastMidiChannelReceived = midiChannel;
            lastSostenutoPedalValueReceived = value;
        }

        void aftertouch (int midiChannel, int midiNoteNumber, MPEValue value)
        {
            const auto message = juce::MidiMessage::aftertouchChange (midiChannel, midiNoteNumber, value.as7BitInt());
            processNextMidiEvent (message);
        }

        int noteOnCallCounter,  noteOffCallCounter, pitchbendCallCounter,
            pressureCallCounter, timbreCallCounter, sustainPedalCallCounter,
            sostenutoPedalCallCounter, noteAddedCallCounter,
            notePressureChangedCallCounter, notePitchbendChangedCallCounter,
            noteTimbreChangedCallCounter, noteKeyStateChangedCallCounter,
            noteReleasedCallCounter, lastMidiChannelReceived, lastMidiNoteNumberReceived;

        bool lastSustainPedalValueReceived, lastSostenutoPedalValueReceived;
        MPEValue lastMPEValueReceived;
        std::unique_ptr<MPENote> lastNoteFinished;

    private:
        //==============================================================================
        void noteAdded (MPENote) override              { noteAddedCallCounter++; }

        void notePressureChanged (MPENote) override    { notePressureChangedCallCounter++; }
        void notePitchbendChanged (MPENote) override   { notePitchbendChangedCallCounter++; }
        void noteTimbreChanged (MPENote) override      { noteTimbreChangedCallCounter++; }
        void noteKeyStateChanged (MPENote) override    { noteKeyStateChangedCallCounter++; }

        void noteReleased (MPENote finishedNote) override
        {
            noteReleasedCallCounter++;
            lastNoteFinished.reset (new MPENote (finishedNote));
        }
    };

    //==============================================================================
    void expectNote (MPENote noteToTest,
                     int noteOnVelocity7Bit,
                     int pressure7Bit,
                     int pitchbend14Bit,
                     int timbre7Bit,
                     MPENote::KeyState keyState)
    {
        expect (noteToTest.isValid());
        expectEquals (noteToTest.noteOnVelocity.as7BitInt(), noteOnVelocity7Bit);
        expectEquals (noteToTest.pressure.as7BitInt(), pressure7Bit);
        expectEquals (noteToTest.pitchbend.as14BitInt(), pitchbend14Bit);
        expectEquals (noteToTest.timbre.as7BitInt(),timbre7Bit);
        expect (noteToTest.keyState == keyState);
    }

    void expectHasFinishedNote (const UnitTestInstrument& test,
                                int channel, int noteNumber, int noteOffVelocity7Bit)
    {
        expect (test.lastNoteFinished != nullptr);
        expectEquals (int (test.lastNoteFinished->midiChannel), channel);
        expectEquals (int (test.lastNoteFinished->initialNote), noteNumber);
        expectEquals (test.lastNoteFinished->noteOffVelocity.as7BitInt(), noteOffVelocity7Bit);
        expect (test.lastNoteFinished->keyState == MPENote::off);
    }

    void expectDoubleWithinRelativeError (double actual, double expected, double maxRelativeError)
    {
        const double maxAbsoluteError = jmax (1.0, std::abs (expected)) * maxRelativeError;
        expect (std::abs (expected - actual) < maxAbsoluteError);
    }

    //==============================================================================
    MPEZoneLayout testLayout;
};

static MPEInstrumentTests MPEInstrumentUnitTests;

#endif

} // namespace juce
