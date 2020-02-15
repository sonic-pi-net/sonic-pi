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
    An OSC message sender.

    An OSCSender object can connect to a network port. It then can send OSC
    messages and bundles to a specified host over an UDP socket.
 */
class JUCE_API  OSCSender
{
public:
    //==============================================================================
    /** Constructs a new OSCSender. */
    OSCSender();

    /** Destructor. */
    ~OSCSender();

    //==============================================================================
    /** Connects to a datagram socket and prepares the socket for sending OSC
        packets to the specified target.

        @param  targetHostName   The remote host to which messages will be send.
        @param  targetPortNumber The remote UDP port number on which the host will
                                 receive the messages.

        @returns true if the connection was successful; false otherwise.

        Note: the operating system will choose which specific network adapter(s)
        to bind your socket to, and which local port to use for the sender.

        @see send, disconnect.
    */
    bool connect (const String& targetHostName, int targetPortNumber);

    //==============================================================================
    /** Disconnects from the currently used UDP port.
        @returns true if the disconnection was successful; false otherwise.

        @see connect.
    */
    bool disconnect();

    //==============================================================================
    /** Sends an OSC message to the target.
        @param  message   The OSC message to send.
        @returns true if the operation was successful.
    */
    bool send (const OSCMessage& message);

    /** Send an OSC bundle to the target.
        @param  bundle    The OSC bundle to send.
        @returns true if the operation was successful.
    */
    bool send (const OSCBundle& bundle);

    /** Sends an OSC message to a specific IP address and port.
        This overrides the address and port that was originally set for this sender.
        @param  targetIPAddress   The IP address to send to
        @param  targetPortNumber  The target port number
        @param  message           The OSC message to send.
        @returns true if the operation was successful.
    */
    bool sendToIPAddress (const String& targetIPAddress, int targetPortNumber,
                          const OSCMessage& message);

    /** Sends an OSC bundle to a specific IP address and port.
        This overrides the address and port that was originally set for this sender.
        @param  targetIPAddress   The IP address to send to
        @param  targetPortNumber  The target port number
        @param  bundle            The OSC bundle to send.
        @returns true if the operation was successful.
    */
    bool sendToIPAddress (const String& targetIPAddress, int targetPortNumber,
                          const OSCBundle& bundle);

   #if JUCE_COMPILER_SUPPORTS_VARIADIC_TEMPLATES
    /** Creates a new OSC message with the specified address pattern and list
        of arguments, and sends it to the target.

        @param  address  The OSC address pattern of the message
                         (you can use a string literal here).
        @param  args     The list of arguments for the message.
    */
    template <typename... Args>
    bool send (const OSCAddressPattern& address, Args&&... args);

    /** Creates a new OSC message with the specified address pattern and list
        of arguments, and sends it to the target.

        @param  targetIPAddress   The IP address to send to
        @param  targetPortNumber  The target port number
        @param  address  The OSC address pattern of the message
                         (you can use a string literal here).
        @param  args     The list of arguments for the message.
    */
    template <typename... Args>
    bool sendToIPAddress (const String& targetIPAddress, int targetPortNumber,
                          const OSCAddressPattern& address, Args&&... args);
   #endif

private:
    //==============================================================================
    struct Pimpl;
    friend struct Pimpl;
    friend struct ContainerDeletePolicy<Pimpl>;
    ScopedPointer<Pimpl> pimpl;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCSender)
};


//==============================================================================
#if JUCE_COMPILER_SUPPORTS_VARIADIC_TEMPLATES
 template <typename... Args>
 bool OSCSender::send (const OSCAddressPattern& address, Args&&... args)
 {
     return send (OSCMessage (address, std::forward<Args> (args)...));
 }

 template <typename... Args>
 bool OSCSender::sendToIPAddress (const String& targetIPAddress, int targetPortNumber,
                                  const OSCAddressPattern& address, Args&&... args)
 {
     return sendToIPAddress (targetIPAddress, targetPortNumber, OSCMessage (address, std::forward<Args> (args)...));
 }
#endif // JUCE_COMPILER_SUPPORTS_VARIADIC_TEMPLATES

} // namespace juce
