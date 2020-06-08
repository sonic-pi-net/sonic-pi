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
/** Initialises JUCE's GUI classes.

    If you're embedding JUCE into an application that uses its own event-loop rather
    than using the START_JUCE_APPLICATION macro, call this function before making any
    JUCE calls, to make sure things are initialised correctly.

    Note that if you're creating a JUCE DLL for Windows, you may also need to call the
    Process::setCurrentModuleInstanceHandle() method.

    @see shutdownJuce_GUI()
*/
JUCE_API void JUCE_CALLTYPE  initialiseJuce_GUI();

/** Clears up any static data being used by JUCE's GUI classes.

    If you're embedding JUCE into an application that uses its own event-loop rather
    than using the START_JUCE_APPLICATION macro, call this function in your shutdown
    code to clean up any JUCE objects that might be lying around.

    @see initialiseJuce_GUI()
*/
JUCE_API void JUCE_CALLTYPE  shutdownJuce_GUI();


//==============================================================================
/** A utility object that helps you initialise and shutdown JUCE correctly
    using an RAII pattern.

    When the first instance of this class is created, it calls initialiseJuce_GUI(),
    and when the last instance is deleted, it calls shutdownJuce_GUI(), so that you
    can easily be sure that as long as at least one instance of the class exists, the
    library will be initialised.

    This class is particularly handy to use at the beginning of a console app's
    main() function, because it'll take care of shutting down whenever you return
    from the main() call.

    Be careful with your threading though - to be safe, you should always make sure
    that these objects are created and deleted on the message thread.

    @tags{Events}
*/
class JUCE_API  ScopedJuceInitialiser_GUI  final
{
public:
    /** The constructor simply calls initialiseJuce_GUI(). */
    ScopedJuceInitialiser_GUI();

    /** The destructor simply calls shutdownJuce_GUI(). */
    ~ScopedJuceInitialiser_GUI();
};


//==============================================================================
/**
    To start a JUCE app, use this macro: START_JUCE_APPLICATION (AppSubClass) where
    AppSubClass is the name of a class derived from JUCEApplication or JUCEApplicationBase.

    See the JUCEApplication and JUCEApplicationBase class documentation for more details.
*/
#ifdef DOXYGEN
 #define START_JUCE_APPLICATION(AppClass)
#else
 #if JUCE_WINDOWS && ! defined (_CONSOLE)
  #define JUCE_MAIN_FUNCTION       int __stdcall WinMain (struct HINSTANCE__*, struct HINSTANCE__*, char*, int)
  #define JUCE_MAIN_FUNCTION_ARGS
 #else
  #define JUCE_MAIN_FUNCTION       int main (int argc, char* argv[])
  #define JUCE_MAIN_FUNCTION_ARGS  argc, (const char**) argv
 #endif

 #if JUCE_IOS

  #define JUCE_CREATE_APPLICATION_DEFINE(AppClass) \
    juce::JUCEApplicationBase* juce_CreateApplication() { return new AppClass(); } \
    void* juce_GetIOSCustomDelegateClass()              { return nullptr; }

  #define JUCE_CREATE_APPLICATION_DEFINE_CUSTOM_DELEGATE(AppClass, DelegateClass) \
    juce::JUCEApplicationBase* juce_CreateApplication() { return new AppClass(); } \
    void* juce_GetIOSCustomDelegateClass()              { return [DelegateClass class]; }

  #define JUCE_MAIN_FUNCTION_DEFINITION \
    extern "C" JUCE_MAIN_FUNCTION \
    { \
       juce::JUCEApplicationBase::createInstance = &juce_CreateApplication; \
       juce::JUCEApplicationBase::iOSCustomDelegate = juce_GetIOSCustomDelegateClass(); \
       return juce::JUCEApplicationBase::main (JUCE_MAIN_FUNCTION_ARGS); \
    }

 #elif JUCE_ANDROID

  #define JUCE_CREATE_APPLICATION_DEFINE(AppClass) \
    extern "C" juce::JUCEApplicationBase* juce_CreateApplication() { return new AppClass(); }

  #define JUCE_MAIN_FUNCTION_DEFINITION

 #else

  #define JUCE_CREATE_APPLICATION_DEFINE(AppClass) \
    juce::JUCEApplicationBase* juce_CreateApplication(); \
    juce::JUCEApplicationBase* juce_CreateApplication() { return new AppClass(); }

  #define JUCE_MAIN_FUNCTION_DEFINITION \
    extern "C" JUCE_MAIN_FUNCTION \
    { \
       juce::JUCEApplicationBase::createInstance = &juce_CreateApplication; \
       return juce::JUCEApplicationBase::main (JUCE_MAIN_FUNCTION_ARGS); \
    }

 #endif

 #if JucePlugin_Build_Standalone
  #if JUCE_USE_CUSTOM_PLUGIN_STANDALONE_APP
    #define START_JUCE_APPLICATION(AppClass) JUCE_CREATE_APPLICATION_DEFINE(AppClass)
    #if JUCE_IOS
     #define START_JUCE_APPLICATION_WITH_CUSTOM_DELEGATE(AppClass, DelegateClass) JUCE_CREATE_APPLICATION_DEFINE_CUSTOM_DELEGATE(AppClass, DelegateClass)
    #endif
  #else
   #define START_JUCE_APPLICATION(AppClass) static_assert(false, "You are trying to use START_JUCE_APPLICATION in an audio plug-in. Define JUCE_USE_CUSTOM_PLUGIN_STANDALONE_APP=1 if you want to use a custom standalone target app.");
   #if JUCE_IOS
    #define START_JUCE_APPLICATION_WITH_CUSTOM_DELEGATE(AppClass, DelegateClass) static_assert(false, "You are trying to use START_JUCE_APPLICATION in an audio plug-in. Define JUCE_USE_CUSTOM_PLUGIN_STANDALONE_APP=1 if you want to use a custom standalone target app.");
   #endif
  #endif
 #else

  #define START_JUCE_APPLICATION(AppClass) \
     JUCE_CREATE_APPLICATION_DEFINE(AppClass) \
     JUCE_MAIN_FUNCTION_DEFINITION

  #if JUCE_IOS
    /**
       You can instruct JUCE to use a custom iOS app delegate class instead of JUCE's default
       app delegate. For JUCE to work you must pass all messages to JUCE's internal app delegate.
       Below is an example of minimal forwarding custom delegate. Note that you are at your own
       risk if you decide to use your own delegate and subtle, hard to debug bugs may occur.

       @interface MyCustomDelegate : NSObject <UIApplicationDelegate> { NSObject<UIApplicationDelegate>* juceDelegate; } @end

       @implementation MyCustomDelegate

       -(id) init
       {
           self = [super init];
           juceDelegate = reinterpret_cast<NSObject<UIApplicationDelegate>*> ([[NSClassFromString (@"JuceAppStartupDelegate") alloc] init]);
           return self;
       }

       -(void) dealloc
       {
           [juceDelegate release];
           [super dealloc];
       }

       - (void) forwardInvocation: (NSInvocation*) anInvocation
       {
           if (juceDelegate != nullptr && [juceDelegate respondsToSelector: [anInvocation selector]])
               [anInvocation invokeWithTarget: juceDelegate];
           else
               [super forwardInvocation: anInvocation];
       }

       -(BOOL) respondsToSelector: (SEL) aSelector
       {
           if (juceDelegate != nullptr && [juceDelegate respondsToSelector: aSelector])
               return YES;

           return [super respondsToSelector: aSelector];
       }
       @end
   */
   #define START_JUCE_APPLICATION_WITH_CUSTOM_DELEGATE(AppClass, DelegateClass) \
      JUCE_CREATE_APPLICATION_DEFINE_CUSTOM_DELEGATE(AppClass, DelegateClass) \
      JUCE_MAIN_FUNCTION_DEFINITION
  #endif
 #endif
#endif

} // namespace juce
