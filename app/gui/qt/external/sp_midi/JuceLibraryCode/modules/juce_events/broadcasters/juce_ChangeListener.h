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

class ChangeBroadcaster;

//==============================================================================
/**
    Receives change event callbacks that are sent out by a ChangeBroadcaster.

    A ChangeBroadcaster keeps a set of listeners to which it broadcasts a message when
    the ChangeBroadcaster::sendChangeMessage() method is called. A subclass of
    ChangeListener is used to receive these callbacks.

    Note that the major difference between an ActionListener and a ChangeListener
    is that for a ChangeListener, multiple changes will be coalesced into fewer
    callbacks, but ActionListeners perform one callback for every event posted.

    @see ChangeBroadcaster, ActionListener

    @tags{Events}
*/
class JUCE_API  ChangeListener
{
public:
    /** Destructor. */
    virtual ~ChangeListener() = default;

    /** Your subclass should implement this method to receive the callback.
        @param source the ChangeBroadcaster that triggered the callback.
    */
    virtual void changeListenerCallback (ChangeBroadcaster* source) = 0;
};

} // namespace juce
