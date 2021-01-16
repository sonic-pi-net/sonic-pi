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

MidiMessageSequence::MidiEventHolder::MidiEventHolder (const MidiMessage& mm) : message (mm) {}
MidiMessageSequence::MidiEventHolder::MidiEventHolder (MidiMessage&& mm) : message (std::move (mm)) {}
MidiMessageSequence::MidiEventHolder::~MidiEventHolder() {}

//==============================================================================
MidiMessageSequence::MidiMessageSequence()
{
}

MidiMessageSequence::MidiMessageSequence (const MidiMessageSequence& other)
{
    list.addCopiesOf (other.list);

    for (int i = 0; i < list.size(); ++i)
    {
        auto noteOffIndex = other.getIndexOfMatchingKeyUp (i);

        if (noteOffIndex >= 0)
            list.getUnchecked(i)->noteOffObject = list.getUnchecked (noteOffIndex);
    }
}

MidiMessageSequence& MidiMessageSequence::operator= (const MidiMessageSequence& other)
{
    MidiMessageSequence otherCopy (other);
    swapWith (otherCopy);
    return *this;
}

MidiMessageSequence::MidiMessageSequence (MidiMessageSequence&& other) noexcept
    : list (std::move (other.list))
{
}

MidiMessageSequence& MidiMessageSequence::operator= (MidiMessageSequence&& other) noexcept
{
    list = std::move (other.list);
    return *this;
}

MidiMessageSequence::~MidiMessageSequence()
{
}

void MidiMessageSequence::swapWith (MidiMessageSequence& other) noexcept
{
    list.swapWith (other.list);
}

void MidiMessageSequence::clear()
{
    list.clear();
}

int MidiMessageSequence::getNumEvents() const noexcept
{
    return list.size();
}

MidiMessageSequence::MidiEventHolder* MidiMessageSequence::getEventPointer (int index) const noexcept
{
    return list[index];
}

MidiMessageSequence::MidiEventHolder** MidiMessageSequence::begin() noexcept               { return list.begin(); }
MidiMessageSequence::MidiEventHolder* const* MidiMessageSequence::begin() const noexcept   { return list.begin(); }
MidiMessageSequence::MidiEventHolder** MidiMessageSequence::end() noexcept                 { return list.end(); }
MidiMessageSequence::MidiEventHolder* const* MidiMessageSequence::end() const noexcept     { return list.end(); }

double MidiMessageSequence::getTimeOfMatchingKeyUp (int index) const noexcept
{
    if (auto* meh = list[index])
        if (auto* noteOff = meh->noteOffObject)
            return noteOff->message.getTimeStamp();

    return 0;
}

int MidiMessageSequence::getIndexOfMatchingKeyUp (int index) const noexcept
{
    if (auto* meh = list[index])
    {
        if (auto* noteOff = meh->noteOffObject)
        {
            for (int i = index; i < list.size(); ++i)
                if (list.getUnchecked(i) == noteOff)
                    return i;

            jassertfalse; // we've somehow got a pointer to a note-off object that isn't in the sequence
        }
    }

    return -1;
}

int MidiMessageSequence::getIndexOf (const MidiEventHolder* event) const noexcept
{
    return list.indexOf (event);
}

int MidiMessageSequence::getNextIndexAtTime (double timeStamp) const noexcept
{
    auto numEvents = list.size();
    int i;

    for (i = 0; i < numEvents; ++i)
        if (list.getUnchecked(i)->message.getTimeStamp() >= timeStamp)
            break;

    return i;
}

//==============================================================================
double MidiMessageSequence::getStartTime() const noexcept
{
    return getEventTime (0);
}

double MidiMessageSequence::getEndTime() const noexcept
{
    return getEventTime (list.size() - 1);
}

double MidiMessageSequence::getEventTime (const int index) const noexcept
{
    if (auto* meh = list[index])
        return meh->message.getTimeStamp();

    return 0;
}

//==============================================================================
MidiMessageSequence::MidiEventHolder* MidiMessageSequence::addEvent (MidiEventHolder* newEvent, double timeAdjustment)
{
    newEvent->message.addToTimeStamp (timeAdjustment);
    auto time = newEvent->message.getTimeStamp();
    int i;

    for (i = list.size(); --i >= 0;)
        if (list.getUnchecked(i)->message.getTimeStamp() <= time)
            break;

    list.insert (i + 1, newEvent);
    return newEvent;
}

MidiMessageSequence::MidiEventHolder* MidiMessageSequence::addEvent (const MidiMessage& newMessage, double timeAdjustment)
{
    return addEvent (new MidiEventHolder (newMessage), timeAdjustment);
}

MidiMessageSequence::MidiEventHolder* MidiMessageSequence::addEvent (MidiMessage&& newMessage, double timeAdjustment)
{
    return addEvent (new MidiEventHolder (std::move (newMessage)), timeAdjustment);
}

void MidiMessageSequence::deleteEvent (int index, bool deleteMatchingNoteUp)
{
    if (isPositiveAndBelow (index, list.size()))
    {
        if (deleteMatchingNoteUp)
            deleteEvent (getIndexOfMatchingKeyUp (index), false);

        list.remove (index);
    }
}

void MidiMessageSequence::addSequence (const MidiMessageSequence& other, double timeAdjustment)
{
    for (auto* m : other)
    {
        auto newOne = new MidiEventHolder (m->message);
        newOne->message.addToTimeStamp (timeAdjustment);
        list.add (newOne);
    }

    sort();
}

void MidiMessageSequence::addSequence (const MidiMessageSequence& other,
                                       double timeAdjustment,
                                       double firstAllowableTime,
                                       double endOfAllowableDestTimes)
{
    for (auto* m : other)
    {
        auto t = m->message.getTimeStamp() + timeAdjustment;

        if (t >= firstAllowableTime && t < endOfAllowableDestTimes)
        {
            auto newOne = new MidiEventHolder (m->message);
            newOne->message.setTimeStamp (t);
            list.add (newOne);
        }
    }

    sort();
}

void MidiMessageSequence::sort() noexcept
{
    std::stable_sort (list.begin(), list.end(),
                      [] (const MidiEventHolder* a, const MidiEventHolder* b) { return a->message.getTimeStamp() < b->message.getTimeStamp(); });
}

void MidiMessageSequence::updateMatchedPairs() noexcept
{
    for (int i = 0; i < list.size(); ++i)
    {
        auto* meh = list.getUnchecked(i);
        auto& m1 = meh->message;

        if (m1.isNoteOn())
        {
            meh->noteOffObject = nullptr;
            auto note = m1.getNoteNumber();
            auto chan = m1.getChannel();
            auto len = list.size();

            for (int j = i + 1; j < len; ++j)
            {
                auto* meh2 = list.getUnchecked(j);
                auto& m = meh2->message;

                if (m.getNoteNumber() == note && m.getChannel() == chan)
                {
                    if (m.isNoteOff())
                    {
                        meh->noteOffObject = meh2;
                        break;
                    }

                    if (m.isNoteOn())
                    {
                        auto newEvent = new MidiEventHolder (MidiMessage::noteOff (chan, note));
                        list.insert (j, newEvent);
                        newEvent->message.setTimeStamp (m.getTimeStamp());
                        meh->noteOffObject = newEvent;
                        break;
                    }
                }
            }
        }
    }
}

void MidiMessageSequence::addTimeToMessages (double delta) noexcept
{
    if (delta != 0)
        for (auto* m : list)
            m->message.addToTimeStamp (delta);
}

//==============================================================================
void MidiMessageSequence::extractMidiChannelMessages (const int channelNumberToExtract,
                                                      MidiMessageSequence& destSequence,
                                                      const bool alsoIncludeMetaEvents) const
{
    for (auto* meh : list)
        if (meh->message.isForChannel (channelNumberToExtract)
             || (alsoIncludeMetaEvents && meh->message.isMetaEvent()))
            destSequence.addEvent (meh->message);
}

void MidiMessageSequence::extractSysExMessages (MidiMessageSequence& destSequence) const
{
    for (auto* meh : list)
        if (meh->message.isSysEx())
            destSequence.addEvent (meh->message);
}

void MidiMessageSequence::deleteMidiChannelMessages (const int channelNumberToRemove)
{
    for (int i = list.size(); --i >= 0;)
        if (list.getUnchecked(i)->message.isForChannel (channelNumberToRemove))
            list.remove(i);
}

void MidiMessageSequence::deleteSysExMessages()
{
    for (int i = list.size(); --i >= 0;)
        if (list.getUnchecked(i)->message.isSysEx())
            list.remove(i);
}

//==============================================================================
void MidiMessageSequence::createControllerUpdatesForTime (int channelNumber, double time, Array<MidiMessage>& dest)
{
    bool doneProg = false;
    bool donePitchWheel = false;
    bool doneControllers[128] = {};

    for (int i = list.size(); --i >= 0;)
    {
        auto& mm = list.getUnchecked(i)->message;

        if (mm.isForChannel (channelNumber) && mm.getTimeStamp() <= time)
        {
            if (mm.isProgramChange() && ! doneProg)
            {
                doneProg = true;
                dest.add (MidiMessage (mm, 0.0));
            }
            else if (mm.isPitchWheel() && ! donePitchWheel)
            {
                donePitchWheel = true;
                dest.add (MidiMessage (mm, 0.0));
            }
            else if (mm.isController())
            {
                auto controllerNumber = mm.getControllerNumber();
                jassert (isPositiveAndBelow (controllerNumber, 128));

                if (! doneControllers[controllerNumber])
                {
                    doneControllers[controllerNumber] = true;
                    dest.add (MidiMessage (mm, 0.0));
                }
            }
        }
    }
}


//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

struct MidiMessageSequenceTest  : public UnitTest
{
    MidiMessageSequenceTest()
        : UnitTest ("MidiMessageSequence", UnitTestCategories::midi)
    {}

    void runTest() override
    {
        MidiMessageSequence s;

        s.addEvent (MidiMessage::noteOn  (1, 60, 0.5f).withTimeStamp (0.0));
        s.addEvent (MidiMessage::noteOff (1, 60, 0.5f).withTimeStamp (4.0));
        s.addEvent (MidiMessage::noteOn  (1, 30, 0.5f).withTimeStamp (2.0));
        s.addEvent (MidiMessage::noteOff (1, 30, 0.5f).withTimeStamp (8.0));

        beginTest ("Start & end time");
        expectEquals (s.getStartTime(), 0.0);
        expectEquals (s.getEndTime(), 8.0);
        expectEquals (s.getEventTime (1), 2.0);

        beginTest ("Matching note off & ons");
        s.updateMatchedPairs();
        expectEquals (s.getTimeOfMatchingKeyUp (0), 4.0);
        expectEquals (s.getTimeOfMatchingKeyUp (1), 8.0);
        expectEquals (s.getIndexOfMatchingKeyUp (0), 2);
        expectEquals (s.getIndexOfMatchingKeyUp (1), 3);

        beginTest ("Time & indices");
        expectEquals (s.getNextIndexAtTime (0.5), 1);
        expectEquals (s.getNextIndexAtTime (2.5), 2);
        expectEquals (s.getNextIndexAtTime (9.0), 4);

        beginTest ("Deleting events");
        s.deleteEvent (0, true);
        expectEquals (s.getNumEvents(), 2);

        beginTest ("Merging sequences");
        MidiMessageSequence s2;
        s2.addEvent (MidiMessage::noteOn  (2, 25, 0.5f).withTimeStamp (0.0));
        s2.addEvent (MidiMessage::noteOn  (2, 40, 0.5f).withTimeStamp (1.0));
        s2.addEvent (MidiMessage::noteOff (2, 40, 0.5f).withTimeStamp (5.0));
        s2.addEvent (MidiMessage::noteOn  (2, 80, 0.5f).withTimeStamp (3.0));
        s2.addEvent (MidiMessage::noteOff (2, 80, 0.5f).withTimeStamp (7.0));
        s2.addEvent (MidiMessage::noteOff (2, 25, 0.5f).withTimeStamp (9.0));

        s.addSequence (s2, 0.0, 0.0, 8.0); // Intentionally cut off the last note off
        s.updateMatchedPairs();

        expectEquals (s.getNumEvents(), 7);
        expectEquals (s.getIndexOfMatchingKeyUp (0), -1); // Truncated note, should be no note off
        expectEquals (s.getTimeOfMatchingKeyUp (1), 5.0);
    }
};

static MidiMessageSequenceTest midiMessageSequenceTests;

#endif

} // namespace juce
