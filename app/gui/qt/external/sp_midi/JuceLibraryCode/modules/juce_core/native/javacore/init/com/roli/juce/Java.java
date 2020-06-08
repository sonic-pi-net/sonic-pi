package com.roli.juce;

import android.content.Context;

public class Java
{
    static
    {
        System.loadLibrary ("juce_jni");
    }

    public native static void initialiseJUCE (Context appContext);
}
