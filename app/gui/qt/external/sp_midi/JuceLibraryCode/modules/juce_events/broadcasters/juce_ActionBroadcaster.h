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
/** Manages a list of ActionListeners, and can send them messages.

    To quickly add methods to your class that can add/remove action
    listeners and broadcast to them, you can derive from this.

    @see ActionListener, ChangeListener

    @tags{Events}
*/
class JUCE_API  ActionBroadcaster
{
public:
    //==============================================================================
    /** Creates an ActionBroadcaster. */
    ActionBroadcaster();

    /** Destructor. */
    virtual ~ActionBroadcaster();

    //==============================================================================
    /** Adds a listener to the list.
        Trying to add a listener that's already on the list will have no effect.
    */
    void addActionListener (ActionListener* listener);

    /** Removes a listener from the list.
        If the listener isn't on the list, this won't have any effect.
    */
    void removeActionListener (ActionListener* listener);

    /** Removes all listeners from the list. */
    void removeAllActionListeners();

    //==============================================================================
    /** Broadcasts a message to all the registered listeners.
        @see ActionListener::actionListenerCallback
    */
    void sendActionMessage (const String& message) const;


private:
    //==============================================================================
    class ActionMessage;
    friend class ActionMessage;

    SortedSet<ActionListener*> actionListeners;
    CriticalSection actionListenerLock;

    JUCE_DECLARE_WEAK_REFERENCEABLE (ActionBroadcaster)
    JUCE_DECLARE_NON_COPYABLE (ActionBroadcaster)
};

} // namespace juce
