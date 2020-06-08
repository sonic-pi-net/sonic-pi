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

//==============================================================================
/**
    A message that invokes a callback method when it gets delivered.

    You can use this class to fire off actions that you want to be performed later
    on the message thread.

    To use it, create a subclass of CallbackMessage which implements the messageCallback()
    method, then call post() to dispatch it. The event thread will then invoke your
    messageCallback() method later on, and will automatically delete the message object
    afterwards.

    Always create a new instance of a CallbackMessage on the heap, as it will be
    deleted automatically after the message has been delivered.

    Note that this class was essential back in the days before C++11, but in modern
    times you may prefer to use MessageManager::callAsync() with a lambda.

    @see MessageManager::callAsync, MessageListener, ActionListener, ChangeListener

    @tags{Events}
*/
class JUCE_API  CallbackMessage   : public MessageManager::MessageBase
{
public:
    //==============================================================================
    CallbackMessage() = default;

    /** Destructor. */
    ~CallbackMessage() override = default;

    //==============================================================================
    /** Called when the message is delivered.

        You should implement this method and make it do whatever action you want
        to perform.

        Note that like all other messages, this object will be deleted immediately
        after this method has been invoked.
    */
    virtual void messageCallback() override = 0;

private:
    // Avoid the leak-detector because for plugins, the host can unload our DLL with undelivered
    // messages still in the system event queue. These aren't harmful, but can cause annoying assertions.
    JUCE_DECLARE_NON_COPYABLE (CallbackMessage)
};

} // namespace juce
