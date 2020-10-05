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
/**
    This struct contains information about a MIDI input or output device.

    You can get one of these structs by calling the static getAvailableDevices() or
    getDefaultDevice() methods of MidiInput and MidiOutput or by calling getDeviceInfo()
    on an instance of these classes. Devices can be opened by passing the identifier to
    the openDevice() method.

    @tags{Audio}
*/
struct MidiDeviceInfo
{
    MidiDeviceInfo() = default;

    MidiDeviceInfo (const String& deviceName, const String& deviceIdentifier)
        : name (deviceName), identifier (deviceIdentifier)
    {
    }

    /** The name of this device.

        This will be provided by the OS unless the device has been created with the
        createNewDevice() method.

        Note that the name is not guaranteed to be unique and two devices with the
        same name will be indistinguishable. If you want to address a specific device
        it is better to use the identifier.
    */
    String name;

    /** The identifier for this device.

        This will be provided by the OS and it's format will differ on different systems
        e.g. on macOS it will be a number whereas on Windows it will be a long alphanumeric string.
    */
    String identifier;

    //==============================================================================
    bool operator== (const MidiDeviceInfo& other) const noexcept   { return name == other.name && identifier == other.identifier; }
    bool operator!= (const MidiDeviceInfo& other) const noexcept   { return ! operator== (other); }
};

class MidiInputCallback;

//==============================================================================
/**
    Represents a midi input device.

    To create one of these, use the static getAvailableDevices() method to find out what
    inputs are available, and then use the openDevice() method to try to open one.

    @see MidiOutput

    @tags{Audio}
*/
class JUCE_API  MidiInput  final
{
public:
    //==============================================================================
    /** Returns a list of the available midi input devices.

        You can open one of the devices by passing its identifier into the openDevice() method.

        @see MidiDeviceInfo, getDevices, getDefaultDeviceIndex, openDevice
    */
    static Array<MidiDeviceInfo> getAvailableDevices();

    /** Returns the MidiDeviceInfo of the default midi input device to use. */
    static MidiDeviceInfo getDefaultDevice();

    /** Tries to open one of the midi input devices.

        This will return a MidiInput object if it manages to open it, you can then
        call start() and stop() on this device.

        If the device can't be opened, this will return an empty object.

        @param deviceIdentifier  the ID of the device to open - use the getAvailableDevices() method to
                                 find the available devices that can be opened
        @param callback          the object that will receive the midi messages from this device

        @see MidiInputCallback, getDevices
    */
    static std::unique_ptr<MidiInput> openDevice (const String& deviceIdentifier, MidiInputCallback* callback);

   #if JUCE_LINUX || JUCE_MAC || JUCE_IOS || DOXYGEN
    /** This will try to create a new midi input device (only available on Linux, macOS and iOS).

        This will attempt to create a new midi input device with the specified name for other
        apps to connect to.

        NB - if you are calling this method on iOS you must have enabled the "Audio Background Capability"
        setting in the iOS exporter otherwise this method will fail.

        Returns an empty object if a device can't be created.

        @param deviceName  the name of the device to create
        @param callback    the object that will receive the midi messages from this device
    */
    static std::unique_ptr<MidiInput> createNewDevice (const String& deviceName, MidiInputCallback* callback);
   #endif

    //==============================================================================
    /** Destructor. */
    ~MidiInput();

    /** Starts the device running.

        After calling this, the device will start sending midi messages to the MidiInputCallback
        object that was specified when the openDevice() method was called.

        @see stop
    */
    void start();

    /** Stops the device running.

        @see start
    */
    void stop();

    /** Returns the MidiDeviceInfo struct containing some information about this device. */
    MidiDeviceInfo getDeviceInfo() const noexcept    { return deviceInfo; }

    /** Returns the identifier of this device. */
    String getIdentifier() const noexcept            { return deviceInfo.identifier; }

    /** Returns the name of this device. */
    String getName() const noexcept                  { return deviceInfo.name; }

    /** Sets a custom name for the device. */
    void setName (const String& newName) noexcept    { deviceInfo.name = newName; }

    //==============================================================================
    /** Deprecated. */
    static StringArray getDevices();
    /** Deprecated. */
    static int getDefaultDeviceIndex();
    /** Deprecated. */
    static std::unique_ptr<MidiInput> openDevice (int, MidiInputCallback*);

private:
    //==============================================================================
    explicit MidiInput (const String&, const String&);

    MidiDeviceInfo deviceInfo;
    void* internal = nullptr;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MidiInput)
};

//==============================================================================
/**
    Receives incoming messages from a physical MIDI input device.

    This class is overridden to handle incoming midi messages. See the MidiInput
    class for more details.

    @see MidiInput

    @tags{Audio}
*/
class JUCE_API  MidiInputCallback
{
public:
    /** Destructor. */
    virtual ~MidiInputCallback()  = default;

    /** Receives an incoming message.

        A MidiInput object will call this method when a midi event arrives. It'll be
        called on a high-priority system thread, so avoid doing anything time-consuming
        in here, and avoid making any UI calls. You might find the MidiBuffer class helpful
        for queueing incoming messages for use later.

        @param source   the MidiInput object that generated the message
        @param message  the incoming message. The message's timestamp is set to a value
                        equivalent to (Time::getMillisecondCounter() / 1000.0) to specify the
                        time when the message arrived
    */
    virtual void handleIncomingMidiMessage (MidiInput* source,
                                            const MidiMessage& message) = 0;

    /** Notification sent each time a packet of a multi-packet sysex message arrives.

        If a long sysex message is broken up into multiple packets, this callback is made
        for each packet that arrives until the message is finished, at which point
        the normal handleIncomingMidiMessage() callback will be made with the entire
        message.

        The message passed in will contain the start of a sysex, but won't be finished
        with the terminating 0xf7 byte.
    */
    virtual void handlePartialSysexMessage (MidiInput* source,
                                            const uint8* messageData,
                                            int numBytesSoFar,
                                            double timestamp)
    {
        ignoreUnused (source, messageData, numBytesSoFar, timestamp);
    }
};

//==============================================================================
/**
    Represents a midi output device.

    To create one of these, use the static getAvailableDevices() method to find out what
    outputs are available, and then use the openDevice() method to try to open one.

    @see MidiInput

    @tags{Audio}
*/
class JUCE_API  MidiOutput  final  : private Thread
{
public:
    //==============================================================================
    /** Returns a list of the available midi output devices.

        You can open one of the devices by passing its identifier into the openDevice() method.

        @see MidiDeviceInfo, getDevices, getDefaultDeviceIndex, openDevice
    */
    static Array<MidiDeviceInfo> getAvailableDevices();

    /** Returns the MidiDeviceInfo of the default midi output device to use. */
    static MidiDeviceInfo getDefaultDevice();

    /** Tries to open one of the midi output devices.

        This will return a MidiOutput object if it manages to open it, you can then
        send messages to this device.

        If the device can't be opened, this will return an empty object.

        @param deviceIdentifier  the ID of the device to open - use the getAvailableDevices() method to
                                 find the available devices that can be opened
        @see getDevices
    */
    static std::unique_ptr<MidiOutput> openDevice (const String& deviceIdentifier);

   #if JUCE_LINUX || JUCE_MAC || JUCE_IOS || DOXYGEN
    /** This will try to create a new midi output device (only available on Linux, macOS and iOS).

        This will attempt to create a new midi output device with the specified name that other
        apps can connect to and use as their midi input.

        NB - if you are calling this method on iOS you must have enabled the "Audio Background Capability"
        setting in the iOS exporter otherwise this method will fail.

        Returns an empty object if a device can't be created.

        @param deviceName  the name of the device to create
    */
    static std::unique_ptr<MidiOutput> createNewDevice (const String& deviceName);
   #endif

    //==============================================================================
    /** Destructor. */
    ~MidiOutput() override;

    /** Returns the MidiDeviceInfo struct containing some information about this device. */
    MidiDeviceInfo getDeviceInfo() const noexcept    { return deviceInfo; }

    /** Returns the identifier of this device. */
    String getIdentifier() const noexcept            { return deviceInfo.identifier; }

    /** Returns the name of this device. */
    String getName() const noexcept                  { return deviceInfo.name; }

    /** Sets a custom name for the device. */
    void setName (const String& newName) noexcept    { deviceInfo.name = newName; }

    //==============================================================================
    /** Sends out a MIDI message immediately. */
    void sendMessageNow (const MidiMessage& message);

    /** Sends out a sequence of MIDI messages immediately. */
    void sendBlockOfMessagesNow (const MidiBuffer& buffer);

    /** This lets you supply a block of messages that will be sent out at some point
        in the future.

        The MidiOutput class has an internal thread that can send out timestamped
        messages - this appends a set of messages to its internal buffer, ready for
        sending.

        This will only work if you've already started the thread with startBackgroundThread().

        A time is specified, at which the block of messages should be sent. This time uses
        the same time base as Time::getMillisecondCounter(), and must be in the future.

        The samplesPerSecondForBuffer parameter indicates the number of samples per second
        used by the MidiBuffer. Each event in a MidiBuffer has a sample position, and the
        samplesPerSecondForBuffer value is needed to convert this sample position to a
        real time.
    */
    void sendBlockOfMessages (const MidiBuffer& buffer,
                              double millisecondCounterToStartAt,
                              double samplesPerSecondForBuffer);

    /** Gets rid of any midi messages that had been added by sendBlockOfMessages(). */
    void clearAllPendingMessages();

    /** Starts up a background thread so that the device can send blocks of data.
        Call this to get the device ready, before using sendBlockOfMessages().
    */
    void startBackgroundThread();

    /** Stops the background thread, and clears any pending midi events.
        @see startBackgroundThread
    */
    void stopBackgroundThread();

    /** Returns true if the background thread used to send blocks of data is running.
        @see startBackgroundThread, stopBackgroundThread
    */
    bool isBackgroundThreadRunning() const noexcept  { return isThreadRunning(); }

    //==============================================================================
    /** Deprecated. */
    static StringArray getDevices();
    /** Deprecated. */
    static int getDefaultDeviceIndex();
    /** Deprecated. */
    static std::unique_ptr<MidiOutput> openDevice (int);

private:
    //==============================================================================
    struct PendingMessage
    {
        PendingMessage (const void* data, int len, double timeStamp)
            : message (data, len, timeStamp)
        {
        }

        MidiMessage message;
        PendingMessage* next;
    };

    //==============================================================================
    explicit MidiOutput (const String&, const String&);
    void run() override;

    MidiDeviceInfo deviceInfo;
    void* internal = nullptr;
    CriticalSection lock;
    PendingMessage* firstMessage = nullptr;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MidiOutput)
};

} // namespace juce
