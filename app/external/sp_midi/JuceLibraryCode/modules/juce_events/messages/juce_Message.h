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

class MessageListener;


//==============================================================================
/** The base class for objects that can be sent to a MessageListener.

    If you want to send a message that carries some kind of custom data, just
    create a subclass of Message with some appropriate member variables to hold
    your data.

    Always create a new instance of a Message object on the heap, as it will be
    deleted automatically after the message has been delivered.

    @see MessageListener, MessageManager, ActionListener, ChangeListener

    @tags{Events}
*/
class JUCE_API  Message  : public MessageManager::MessageBase
{
public:
    //==============================================================================
    /** Creates an uninitialised message. */
    Message() noexcept;
    ~Message() override;

    using Ptr = ReferenceCountedObjectPtr<Message>;

    //==============================================================================
private:
    friend class MessageListener;
    WeakReference<MessageListener> recipient;
    void messageCallback() override;

    // Avoid the leak-detector because for plugins, the host can unload our DLL with undelivered
    // messages still in the system event queue. These aren't harmful, but can cause annoying assertions.
    JUCE_DECLARE_NON_COPYABLE (Message)
};

} // namespace juce
