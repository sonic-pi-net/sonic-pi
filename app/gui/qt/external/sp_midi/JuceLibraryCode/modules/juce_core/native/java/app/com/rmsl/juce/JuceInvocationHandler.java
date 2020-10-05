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

package com.rmsl.juce;

import java.lang.reflect.*;

public class JuceInvocationHandler implements InvocationHandler
{
        public JuceInvocationHandler (long nativeContextRef)
        {
                nativeContext = nativeContextRef;
        }

        public void clear()
        {
                nativeContext = 0;
        }

        @Override
        public void finalize()
        {
                if (nativeContext != 0)
                        dispatchFinalize (nativeContext);
        }

        @Override
        public Object invoke (Object proxy, Method method, Object[] args) throws Throwable
        {
                if (nativeContext != 0)
                        return dispatchInvoke (nativeContext, proxy, method, args);

                return null;
        }

        //==============================================================================
        private long nativeContext = 0;

        private native void dispatchFinalize (long nativeContextRef);
        private native Object dispatchInvoke (long nativeContextRef, Object proxy, Method method, Object[] args);
}
