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

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD) \
 METHOD (getJuceAndroidMidiInputDevices, "getJuceAndroidMidiInputDevices", "()[Ljava/lang/String;") \
 METHOD (getJuceAndroidMidiOutputDevices, "getJuceAndroidMidiOutputDevices", "()[Ljava/lang/String;") \
 METHOD (openMidiInputPortWithJuceIndex, "openMidiInputPortWithJuceIndex", "(IJ)L" JUCE_ANDROID_ACTIVITY_CLASSPATH "$JuceMidiPort;") \
 METHOD (openMidiOutputPortWithJuceIndex, "openMidiOutputPortWithJuceIndex", "(I)L" JUCE_ANDROID_ACTIVITY_CLASSPATH "$JuceMidiPort;") \
 METHOD (getInputPortNameForJuceIndex, "getInputPortNameForJuceIndex", "(I)Ljava/lang/String;") \
 METHOD (getOutputPortNameForJuceIndex, "getOutputPortNameForJuceIndex", "(I)Ljava/lang/String;")
 DECLARE_JNI_CLASS (MidiDeviceManager, JUCE_ANDROID_ACTIVITY_CLASSPATH "$MidiDeviceManager")
#undef JNI_CLASS_MEMBERS

#define JNI_CLASS_MEMBERS(METHOD, STATICMETHOD, FIELD, STATICFIELD) \
 METHOD (start, "start", "()V" )\
 METHOD (stop, "stop", "()V") \
 METHOD (close, "close", "()V") \
 METHOD (sendMidi, "sendMidi", "([BII)V")
 DECLARE_JNI_CLASS (JuceMidiPort, JUCE_ANDROID_ACTIVITY_CLASSPATH "$JuceMidiPort")
#undef JNI_CLASS_MEMBERS


//==============================================================================
class AndroidMidiInput
{
public:
    AndroidMidiInput (MidiInput* midiInput, int portIdx,
                      juce::MidiInputCallback* midiInputCallback, jobject deviceManager)
        : juceMidiInput (midiInput),
          callback (midiInputCallback),
          midiConcatenator (2048),
          javaMidiDevice (getEnv()->CallObjectMethod (deviceManager,
                                                      MidiDeviceManager.openMidiInputPortWithJuceIndex,
                                                      (jint) portIdx,
                                                      (jlong) this))
    {
    }

    ~AndroidMidiInput()
    {
        if (jobject d = javaMidiDevice.get())
        {
            getEnv()->CallVoidMethod (d, JuceMidiPort.close);
            javaMidiDevice.clear();
        }
    }

    bool isOpen() const noexcept
    {
        return javaMidiDevice != nullptr;
    }

    void start()
    {
        if (jobject d = javaMidiDevice.get())
            getEnv()->CallVoidMethod (d, JuceMidiPort.start);
    }

    void stop()
    {
        if (jobject d = javaMidiDevice.get())
            getEnv()->CallVoidMethod (d, JuceMidiPort.stop);

        callback = nullptr;
    }

    void receive (jbyteArray byteArray, jlong offset, jint len, jlong timestamp)
    {
        jassert (byteArray != nullptr);
        jbyte* data = getEnv()->GetByteArrayElements (byteArray, nullptr);

        HeapBlock<uint8> buffer (static_cast<size_t> (len));
        std::memcpy (buffer.get(), data + offset, static_cast<size_t> (len));

        midiConcatenator.pushMidiData (buffer.get(),
                                       len, static_cast<double> (timestamp) * 1.0e-9,
                                       juceMidiInput, *callback);

        getEnv()->ReleaseByteArrayElements (byteArray, data, 0);
    }

private:
    MidiInput* juceMidiInput;
    MidiInputCallback* callback;
    MidiDataConcatenator midiConcatenator;
    GlobalRef javaMidiDevice;
};

//==============================================================================
class AndroidMidiOutput
{
public:
    AndroidMidiOutput (jobject midiDevice)
        : javaMidiDevice (midiDevice)
    {
    }

    ~AndroidMidiOutput()
    {
        if (jobject d = javaMidiDevice.get())
        {
            getEnv()->CallVoidMethod (d, JuceMidiPort.close);
            javaMidiDevice.clear();
        }
    }

    void send (jbyteArray byteArray, jint offset, jint len)
    {
        if (jobject d = javaMidiDevice.get())
            getEnv()->CallVoidMethod (d,
                                      JuceMidiPort.sendMidi,
                                      byteArray, offset, len);
    }

private:
    GlobalRef javaMidiDevice;
};

JUCE_JNI_CALLBACK (JUCE_JOIN_MACRO (JUCE_ANDROID_ACTIVITY_CLASSNAME, _00024JuceMidiInputPort), handleReceive,
                   void, (JNIEnv* env, jobject, jlong host, jbyteArray byteArray,
                          jint offset, jint count, jlong timestamp))
{
    // Java may create a Midi thread which JUCE doesn't know about and this callback may be
    // received on this thread. Java will have already created a JNI Env for this new thread,
    // which we need to tell Juce about
    setEnv (env);

    reinterpret_cast<AndroidMidiInput*> (host)->receive (byteArray, offset, count, timestamp);
}

//==============================================================================
class AndroidMidiDeviceManager
{
public:
    AndroidMidiDeviceManager()
        : deviceManager (android.activity.callObjectMethod (JuceAppActivity.getAndroidMidiDeviceManager))
    {
    }

    String getInputPortNameForJuceIndex (int idx)
    {
        if (jobject dm = deviceManager.get())
        {
            LocalRef<jstring> string ((jstring) getEnv()->CallObjectMethod (dm, MidiDeviceManager.getInputPortNameForJuceIndex, idx));
            return juceString (string);
        }

        return {};
    }

    String getOutputPortNameForJuceIndex (int idx)
    {
        if (jobject dm = deviceManager.get())
        {
            LocalRef<jstring> string ((jstring) getEnv()->CallObjectMethod (dm, MidiDeviceManager.getOutputPortNameForJuceIndex, idx));
            return juceString (string);
        }

        return {};
    }

    StringArray getDevices (bool input)
    {
        if (jobject dm = deviceManager.get())
        {
            jobjectArray jDevices
                = (jobjectArray) getEnv()->CallObjectMethod (dm, input ? MidiDeviceManager.getJuceAndroidMidiInputDevices
                                                                  : MidiDeviceManager.getJuceAndroidMidiOutputDevices);

            // Create a local reference as converting this
            // to a JUCE string will call into JNI
            LocalRef<jobjectArray> devices (jDevices);
            return javaStringArrayToJuce (devices);
        }

        return {};
    }

    AndroidMidiInput* openMidiInputPortWithIndex (int idx, MidiInput* juceMidiInput, juce::MidiInputCallback* callback)
    {
        if (jobject dm = deviceManager.get())
        {
            ScopedPointer<AndroidMidiInput> androidMidiInput (new AndroidMidiInput (juceMidiInput, idx, callback, dm));

            if (androidMidiInput->isOpen())
                return androidMidiInput.release();
        }

        return nullptr;
    }

    AndroidMidiOutput* openMidiOutputPortWithIndex (int idx)
    {
        if (jobject dm = deviceManager.get())
            if (jobject javaMidiPort = getEnv()->CallObjectMethod (dm, MidiDeviceManager.openMidiOutputPortWithJuceIndex, (jint) idx))
                return new AndroidMidiOutput (javaMidiPort);

        return nullptr;
    }

private:
    static StringArray javaStringArrayToJuce (jobjectArray jStrings)
    {
        StringArray retval;

        JNIEnv* env = getEnv();
        const int count = env->GetArrayLength (jStrings);

        for (int i = 0; i < count; ++i)
        {
            LocalRef<jstring> string ((jstring) env->GetObjectArrayElement (jStrings, i));
            retval.add (juceString (string));
        }

        return retval;
    }

    GlobalRef deviceManager;
};

//==============================================================================
StringArray MidiOutput::getDevices()
{
    AndroidMidiDeviceManager manager;
    return manager.getDevices (false);
}

int MidiOutput::getDefaultDeviceIndex()
{
    return 0;
}

MidiOutput* MidiOutput::openDevice (int index)
{
    if (index < 0)
        return nullptr;

    AndroidMidiDeviceManager manager;

    String midiOutputName = manager.getOutputPortNameForJuceIndex (index);

    if (midiOutputName.isEmpty())
    {
        // you supplied an invalid device index!
        jassertfalse;
        return nullptr;
    }

    if (AndroidMidiOutput* midiOutput = manager.openMidiOutputPortWithIndex (index))
    {
        MidiOutput* retval = new MidiOutput (midiOutputName);
        retval->internal = midiOutput;

        return retval;
    }

    return nullptr;
}

MidiOutput::~MidiOutput()
{
    stopBackgroundThread();

    delete reinterpret_cast<AndroidMidiOutput*> (internal);
}

void MidiOutput::sendMessageNow (const MidiMessage& message)
{
    if (AndroidMidiOutput* androidMidi = reinterpret_cast<AndroidMidiOutput*>(internal))
    {
        JNIEnv* env = getEnv();
        const int messageSize = message.getRawDataSize();

        LocalRef<jbyteArray> messageContent = LocalRef<jbyteArray> (env->NewByteArray (messageSize));
        jbyteArray content = messageContent.get();

        jbyte* rawBytes = env->GetByteArrayElements (content, nullptr);
        std::memcpy (rawBytes, message.getRawData(), static_cast<size_t> (messageSize));
        env->ReleaseByteArrayElements (content, rawBytes, 0);

        androidMidi->send (content, (jint) 0, (jint) messageSize);
    }
}

//==============================================================================
MidiInput::MidiInput (const String& nm)  : name (nm)
{
}

StringArray MidiInput::getDevices()
{
    AndroidMidiDeviceManager manager;
    return manager.getDevices (true);
}

int MidiInput::getDefaultDeviceIndex()
{
    return 0;
}

MidiInput* MidiInput::openDevice (int index, juce::MidiInputCallback* callback)
{
    if (index < 0)
        return nullptr;

    AndroidMidiDeviceManager manager;

    String midiInputName = manager.getInputPortNameForJuceIndex (index);

    if (midiInputName.isEmpty())
    {
        // you supplied an invalid device index!
        jassertfalse;
        return nullptr;
    }

    ScopedPointer<MidiInput> midiInput (new MidiInput (midiInputName));

    midiInput->internal = manager.openMidiInputPortWithIndex (index, midiInput, callback);

    return midiInput->internal != nullptr ? midiInput.release()
                                          : nullptr;
}

void MidiInput::start()
{
    if (AndroidMidiInput* mi = reinterpret_cast<AndroidMidiInput*> (internal))
        mi->start();
}

void MidiInput::stop()
{
    if (AndroidMidiInput* mi = reinterpret_cast<AndroidMidiInput*> (internal))
        mi->stop();
}

MidiInput::~MidiInput()
{
    delete reinterpret_cast<AndroidMidiInput*> (internal);
}

} // namespace juce
