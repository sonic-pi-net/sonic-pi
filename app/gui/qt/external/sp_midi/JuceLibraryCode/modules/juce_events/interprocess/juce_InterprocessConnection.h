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

class InterprocessConnectionServer;
class MemoryBlock;


//==============================================================================
/**
    Manages a simple two-way messaging connection to another process, using either
    a socket or a named pipe as the transport medium.

    To connect to a waiting socket or an open pipe, use the connectToSocket() or
    connectToPipe() methods. If this succeeds, messages can be sent to the other end,
    and incoming messages will result in a callback via the messageReceived()
    method.

    To open a pipe and wait for another client to connect to it, use the createPipe()
    method.

    To act as a socket server and create connections for one or more client, see the
    InterprocessConnectionServer class.

    @see InterprocessConnectionServer, Socket, NamedPipe

    @tags{Events}
*/
class JUCE_API  InterprocessConnection
{
public:
    //==============================================================================
    /** Creates a connection.

        Connections are created manually, connecting them with the connectToSocket()
        or connectToPipe() methods, or they are created automatically by a InterprocessConnectionServer
        when a client wants to connect.

        @param callbacksOnMessageThread     if true, callbacks to the connectionMade(),
                                            connectionLost() and messageReceived() methods will
                                            always be made using the message thread; if false,
                                            these will be called immediately on the connection's
                                            own thread.
        @param magicMessageHeaderNumber     a magic number to use in the header to check the
                                            validity of the data blocks being sent and received. This
                                            can be any number, but the sender and receiver must obviously
                                            use matching values or they won't recognise each other.
    */
    InterprocessConnection (bool callbacksOnMessageThread = true,
                            uint32 magicMessageHeaderNumber = 0xf2b49e2c);

    /** Destructor. */
    virtual ~InterprocessConnection();

    //==============================================================================
    /** Tries to connect this object to a socket.

        For this to work, the machine on the other end needs to have a InterprocessConnectionServer
        object waiting to receive client connections on this port number.

        @param hostName             the host computer, either a network address or name
        @param portNumber           the socket port number to try to connect to
        @param timeOutMillisecs     how long to keep trying before giving up
        @returns true if the connection is established successfully
        @see Socket
    */
    bool connectToSocket (const String& hostName,
                          int portNumber,
                          int timeOutMillisecs);

    /** Tries to connect the object to an existing named pipe.

        For this to work, another process on the same computer must already have opened
        an InterprocessConnection object and used createPipe() to create a pipe for this
        to connect to.

        @param pipeName     the name to use for the pipe - this should be unique to your app
        @param pipeReceiveMessageTimeoutMs  a timeout length to be used when reading or writing
                                            to the pipe, or -1 for an infinite timeout.
        @returns true if it connects successfully.
        @see createPipe, NamedPipe
    */
    bool connectToPipe (const String& pipeName, int pipeReceiveMessageTimeoutMs);

    /** Tries to create a new pipe for other processes to connect to.

        This creates a pipe with the given name, so that other processes can use
        connectToPipe() to connect to the other end.

        @param pipeName       the name to use for the pipe - this should be unique to your app
        @param pipeReceiveMessageTimeoutMs  a timeout length to be used when reading or writing
                                            to the pipe, or -1 for an infinite timeout
        @param mustNotExist   if set to true, the method will fail if the pipe already exists
        @returns true if the pipe was created, or false if it fails (e.g. if another process is
                 already using the pipe)
    */
    bool createPipe (const String& pipeName, int pipeReceiveMessageTimeoutMs, bool mustNotExist = false);

    /** Disconnects and closes any currently-open sockets or pipes. */
    void disconnect();

    /** True if a socket or pipe is currently active. */
    bool isConnected() const;

    /** Returns the socket that this connection is using (or nullptr if it uses a pipe). */
    StreamingSocket* getSocket() const noexcept                 { return socket.get(); }

    /** Returns the pipe that this connection is using (or nullptr if it uses a socket). */
    NamedPipe* getPipe() const noexcept                         { return pipe.get(); }

    /** Returns the name of the machine at the other end of this connection.
        This may return an empty string if the name is unknown.
    */
    String getConnectedHostName() const;

    //==============================================================================
    /** Tries to send a message to the other end of this connection.

        This will fail if it's not connected, or if there's some kind of write error. If
        it succeeds, the connection object at the other end will receive the message by
        a callback to its messageReceived() method.

        @see messageReceived
    */
    bool sendMessage (const MemoryBlock& message);

    //==============================================================================
    /** Called when the connection is first connected.

        If the connection was created with the callbacksOnMessageThread flag set, then
        this will be called on the message thread; otherwise it will be called on a server
        thread.
    */
    virtual void connectionMade() = 0;

    /** Called when the connection is broken.

        If the connection was created with the callbacksOnMessageThread flag set, then
        this will be called on the message thread; otherwise it will be called on a server
        thread.
    */
    virtual void connectionLost() = 0;

    /** Called when a message arrives.

        When the object at the other end of this connection sends us a message with sendMessage(),
        this callback is used to deliver it to us.

        If the connection was created with the callbacksOnMessageThread flag set, then
        this will be called on the message thread; otherwise it will be called on a server
        thread.

        @see sendMessage
    */
    virtual void messageReceived (const MemoryBlock& message) = 0;


private:
    //==============================================================================
    CriticalSection pipeAndSocketLock;
    std::unique_ptr<StreamingSocket> socket;
    std::unique_ptr<NamedPipe> pipe;
    bool callbackConnectionState = false;
    const bool useMessageThread;
    const uint32 magicMessageHeader;
    int pipeReceiveMessageTimeout = -1;

    friend class InterprocessConnectionServer;
    void initialiseWithSocket (StreamingSocket*);
    void initialiseWithPipe (NamedPipe*);
    void deletePipeAndSocket();
    void connectionMadeInt();
    void connectionLostInt();
    void deliverDataInt (const MemoryBlock&);
    bool readNextMessage();
    int readData (void*, int);

    struct ConnectionThread;
    std::unique_ptr<ConnectionThread> thread;
    std::atomic<bool> threadIsRunning { false };

    void runThread();
    int writeData (void*, int);

    JUCE_DECLARE_WEAK_REFERENCEABLE (InterprocessConnection)
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (InterprocessConnection)
};

} // namespace juce
