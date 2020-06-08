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

ChangeBroadcaster::ChangeBroadcaster() noexcept
{
    broadcastCallback.owner = this;
}

ChangeBroadcaster::~ChangeBroadcaster()
{
}

void ChangeBroadcaster::addChangeListener (ChangeListener* const listener)
{
    // Listeners can only be safely added when the event thread is locked
    // You can  use a MessageManagerLock if you need to call this from another thread.
    JUCE_ASSERT_MESSAGE_MANAGER_IS_LOCKED

    changeListeners.add (listener);
    anyListeners = true;
}

void ChangeBroadcaster::removeChangeListener (ChangeListener* const listener)
{
    // Listeners can only be safely removed when the event thread is locked
    // You can  use a MessageManagerLock if you need to call this from another thread.
    JUCE_ASSERT_MESSAGE_MANAGER_IS_LOCKED

    changeListeners.remove (listener);
    anyListeners = changeListeners.size() > 0;
}

void ChangeBroadcaster::removeAllChangeListeners()
{
    // Listeners can only be safely removed when the event thread is locked
    // You can  use a MessageManagerLock if you need to call this from another thread.
    JUCE_ASSERT_MESSAGE_MANAGER_IS_LOCKED

    changeListeners.clear();
    anyListeners = false;
}

void ChangeBroadcaster::sendChangeMessage()
{
    if (anyListeners)
        broadcastCallback.triggerAsyncUpdate();
}

void ChangeBroadcaster::sendSynchronousChangeMessage()
{
    // This can only be called by the event thread.
    JUCE_ASSERT_MESSAGE_MANAGER_IS_LOCKED

    broadcastCallback.cancelPendingUpdate();
    callListeners();
}

void ChangeBroadcaster::dispatchPendingMessages()
{
    broadcastCallback.handleUpdateNowIfNeeded();
}

void ChangeBroadcaster::callListeners()
{
    changeListeners.call ([this] (ChangeListener& l) { l.changeListenerCallback (this); });
}

//==============================================================================
ChangeBroadcaster::ChangeBroadcasterCallback::ChangeBroadcasterCallback()
    : owner (nullptr)
{
}

void ChangeBroadcaster::ChangeBroadcasterCallback::handleAsyncUpdate()
{
    jassert (owner != nullptr);
    owner->callListeners();
}

} // namespace juce
