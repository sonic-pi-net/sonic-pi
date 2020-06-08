package com.roli.juce;

import com.roli.juce.Java;

import android.app.Application;

public class JuceApp extends Application
{
    @Override
    public void onCreate ()
    {
        super.onCreate ();
        Java.initialiseJUCE (this);
    }
}
