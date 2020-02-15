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

class MidiInput;


//==============================================================================
/**
    Receives incoming messages from a physical MIDI input device.

    This class is overridden to handle incoming midi messages. See the MidiInput
    class for more details.

    @see MidiInput
*/
class JUCE_API  MidiInputCallback
{
public:
    /** Destructor. */
    virtual ~MidiInputCallback()  {}


    /** Receives an incoming message.

        A MidiInput object will call this method when a midi event arrives. It'll be
        called on a high-priority system thread, so avoid doing anything time-consuming
        in here, and avoid making any UI calls. You might find the MidiBuffer class helpful
        for queueing incoming messages for use later.

        @param source   the MidiInput object that generated the message
        @param message  the incoming message. The message's timestamp is set to a value
                        equivalent to (Time::getMillisecondCounter() / 1000.0) to specify the
                        time when the message arrived.
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
    Represents a midi input device.

    To create one of these, use the static getDevices() method to find out what inputs are
    available, and then use the openDevice() method to try to open one.

    @see MidiOutput
*/
class JUCE_API  MidiInput
{
public:
    //==============================================================================
    /** Returns a list of the available midi input devices.

        You can open one of the devices by passing its index into the
        openDevice() method.

        @see getDefaultDeviceIndex, openDevice
    */
    static StringArray getDevices();

    /** Returns the index of the default midi input device to use.

        This refers to the index in the list returned by getDevices().
    */
    static int getDefaultDeviceIndex();

    /** Tries to open one of the midi input devices.

        This will return a MidiInput object if it manages to open it. You can then
        call start() and stop() on this device, and delete it when no longer needed.

        If the device can't be opened, this will return a null pointer.

        @param deviceIndex  the index of a device from the list returned by getDevices()
        @param callback     the object that will receive the midi messages from this device.

        @see MidiInputCallback, getDevices
    */
    static MidiInput* openDevice (int deviceIndex,
                                  MidiInputCallback* callback);

   #if JUCE_LINUX || JUCE_MAC || JUCE_IOS || DOXYGEN
    /** This will try to create a new midi input device (Not available on Windows).

        This will attempt to create a new midi input device with the specified name,
        for other apps to connect to.

        Returns nullptr if a device can't be created.

        @param deviceName   the name to use for the new device
        @param callback     the object that will receive the midi messages from this device.
    */
    static MidiInput* createNewDevice (const String& deviceName,
                                       MidiInputCallback* callback);
   #endif

    //==============================================================================
    /** Destructor. */
    ~MidiInput();

    /** Returns the name of this device. */
    const String& getName() const noexcept                      { return name; }

    /** Allows you to set a custom name for the device, in case you don't like the name
        it was given when created.
    */
    void setName (const String& newName) noexcept               { name = newName; }

    //==============================================================================
    /** Starts the device running.

        After calling this, the device will start sending midi messages to the
        MidiInputCallback object that was specified when the openDevice() method
        was called.

        @see stop
    */
    void start();

    /** Stops the device running.
        @see start
    */
    void stop();

private:
    //==============================================================================
    String name;
    void* internal = nullptr;

    // The input objects are created with the openDevice() method.
    explicit MidiInput (const String&);

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MidiInput)
};

} // namespace juce
