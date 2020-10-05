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
    Used by the JUCE_DECLARE_SINGLETON macros to manage a static pointer
    to a singleton instance.

    You generally won't use this directly, but see the macros JUCE_DECLARE_SINGLETON,
    JUCE_DECLARE_SINGLETON_SINGLETHREADED, JUCE_DECLARE_SINGLETON_SINGLETHREADED_MINIMAL,
    and JUCE_IMPLEMENT_SINGLETON for how it is intended to be used.

    @tags{Core}
*/
template <typename Type, typename MutexType, bool onlyCreateOncePerRun>
struct SingletonHolder  : private MutexType // (inherited so we can use the empty-base-class optimisation)
{
    SingletonHolder() = default;

    ~SingletonHolder()
    {
        /* The static singleton holder is being deleted before the object that it holds
           has been deleted. This could mean that you've forgotten to call clearSingletonInstance()
           in the class's destructor, or have failed to delete it before your app shuts down.
           If you're having trouble cleaning up your singletons, perhaps consider using the
           SharedResourcePointer class instead.
        */
        jassert (instance == nullptr);
    }

    /** Returns the current instance, or creates a new instance if there isn't one. */
    Type* get()
    {
        if (instance == nullptr)
        {
            typename MutexType::ScopedLockType sl (*this);

            if (instance == nullptr)
            {
                auto once = onlyCreateOncePerRun; // (local copy avoids VS compiler warning about this being constant)

                if (once)
                {
                    static bool createdOnceAlready = false;

                    if (createdOnceAlready)
                    {
                        // This means that the doNotRecreateAfterDeletion flag was set
                        // and you tried to create the singleton more than once.
                        jassertfalse;
                        return nullptr;
                    }

                    createdOnceAlready = true;
                }

                static bool alreadyInside = false;

                if (alreadyInside)
                {
                    // This means that your object's constructor has done something which has
                    // ended up causing a recursive loop of singleton creation..
                    jassertfalse;
                }
                else
                {
                    alreadyInside = true;
                    getWithoutChecking();
                    alreadyInside = false;
                }
            }
        }

        return instance;
    }

    /** Returns the current instance, or creates a new instance if there isn't one, but doesn't do
        any locking, or checking for recursion or error conditions.
    */
    Type* getWithoutChecking()
    {
        if (instance == nullptr)
        {
            auto newObject = new Type(); // (create into a local so that instance is still null during construction)
            instance = newObject;
        }

        return instance;
    }

    /** Deletes and resets the current instance, if there is one. */
    void deleteInstance()
    {
        typename MutexType::ScopedLockType sl (*this);
        auto old = instance;
        instance = nullptr;
        delete old;
    }

    /** Called by the class's destructor to clear the pointer if it is currently set to the given object. */
    void clear (Type* expectedObject) noexcept
    {
        if (instance == expectedObject)
            instance = nullptr;
    }

    Type* instance = nullptr;
};


//==============================================================================
/**
    Macro to generate the appropriate methods and boilerplate for a singleton class.

    To use this, add the line JUCE_DECLARE_SINGLETON(MyClass, doNotRecreateAfterDeletion)
    to the class's definition.

    Then put a macro JUCE_IMPLEMENT_SINGLETON(MyClass) along with the class's
    implementation code.

    It's also a very good idea to also add the call clearSingletonInstance() in your class's
    destructor, in case it is deleted by other means than deleteInstance()

    Clients can then call the static method MyClass::getInstance() to get a pointer
    to the singleton, or MyClass::getInstanceWithoutCreating() which will return nullptr if
    no instance currently exists.

    e.g. @code

        struct MySingleton
        {
            MySingleton() {}

            ~MySingleton()
            {
                // this ensures that no dangling pointers are left when the
                // singleton is deleted.
                clearSingletonInstance();
            }

            JUCE_DECLARE_SINGLETON (MySingleton, false)
        };

        // ..and this goes in a suitable .cpp file:
        JUCE_IMPLEMENT_SINGLETON (MySingleton)


        // example of usage:
        auto* m = MySingleton::getInstance(); // creates the singleton if there isn't already one.

        ...

        MySingleton::deleteInstance(); // safely deletes the singleton (if it's been created).

    @endcode

    If doNotRecreateAfterDeletion = true, it won't allow the object to be created more
    than once during the process's lifetime - i.e. after you've created and deleted the
    object, getInstance() will refuse to create another one. This can be useful to stop
    objects being accidentally re-created during your app's shutdown code.

    If you know that your object will only be created and deleted by a single thread, you
    can use the slightly more efficient JUCE_DECLARE_SINGLETON_SINGLETHREADED macro instead
    of this one.

    @see JUCE_IMPLEMENT_SINGLETON, JUCE_DECLARE_SINGLETON_SINGLETHREADED
*/
#define JUCE_DECLARE_SINGLETON(Classname, doNotRecreateAfterDeletion) \
\
    static juce::SingletonHolder<Classname, juce::CriticalSection, doNotRecreateAfterDeletion> singletonHolder; \
    friend decltype (singletonHolder); \
\
    static Classname* JUCE_CALLTYPE getInstance()                           { return singletonHolder.get(); } \
    static Classname* JUCE_CALLTYPE getInstanceWithoutCreating() noexcept   { return singletonHolder.instance; } \
    static void JUCE_CALLTYPE deleteInstance() noexcept                     { singletonHolder.deleteInstance(); } \
    void clearSingletonInstance() noexcept                                  { singletonHolder.clear (this); }


//==============================================================================
/** This is a counterpart to the JUCE_DECLARE_SINGLETON macros.

    After adding the JUCE_DECLARE_SINGLETON to the class definition, this macro has
    to be used in the cpp file.
*/
#define JUCE_IMPLEMENT_SINGLETON(Classname) \
\
    decltype (Classname::singletonHolder) Classname::singletonHolder;


//==============================================================================
/**
    Macro to declare member variables and methods for a singleton class.

    This is exactly the same as JUCE_DECLARE_SINGLETON, but doesn't use a critical
    section to make access to it thread-safe. If you know that your object will
    only ever be created or deleted by a single thread, then this is a
    more efficient version to use.

    If doNotRecreateAfterDeletion = true, it won't allow the object to be created more
    than once during the process's lifetime - i.e. after you've created and deleted the
    object, getInstance() will refuse to create another one. This can be useful to stop
    objects being accidentally re-created during your app's shutdown code.

    See the documentation for JUCE_DECLARE_SINGLETON for more information about
    how to use it. Just like JUCE_DECLARE_SINGLETON you need to also have a
    corresponding JUCE_IMPLEMENT_SINGLETON statement somewhere in your code.

    @see JUCE_IMPLEMENT_SINGLETON, JUCE_DECLARE_SINGLETON, JUCE_DECLARE_SINGLETON_SINGLETHREADED_MINIMAL
*/
#define JUCE_DECLARE_SINGLETON_SINGLETHREADED(Classname, doNotRecreateAfterDeletion) \
\
    static juce::SingletonHolder<Classname, juce::DummyCriticalSection, doNotRecreateAfterDeletion> singletonHolder; \
    friend decltype (singletonHolder); \
\
    static Classname* JUCE_CALLTYPE getInstance()                           { return singletonHolder.get(); } \
    static Classname* JUCE_CALLTYPE getInstanceWithoutCreating() noexcept   { return singletonHolder.instance; } \
    static void JUCE_CALLTYPE deleteInstance() noexcept                     { singletonHolder.deleteInstance(); } \
    void clearSingletonInstance() noexcept                                  { singletonHolder.clear (this); }


//==============================================================================
/**
    Macro to declare member variables and methods for a singleton class.

    This is like JUCE_DECLARE_SINGLETON_SINGLETHREADED, but doesn't do any checking
    for recursion or repeated instantiation. It's intended for use as a lightweight
    version of a singleton, where you're using it in very straightforward
    circumstances and don't need the extra checking.

    See the documentation for JUCE_DECLARE_SINGLETON for more information about
    how to use it. Just like JUCE_DECLARE_SINGLETON you need to also have a
    corresponding JUCE_IMPLEMENT_SINGLETON statement somewhere in your code.

    @see JUCE_IMPLEMENT_SINGLETON, JUCE_DECLARE_SINGLETON
*/
#define JUCE_DECLARE_SINGLETON_SINGLETHREADED_MINIMAL(Classname) \
\
    static juce::SingletonHolder<Classname, juce::DummyCriticalSection, false> singletonHolder; \
    friend decltype (singletonHolder); \
\
    static Classname* JUCE_CALLTYPE getInstance()                           { return singletonHolder.getWithoutChecking(); } \
    static Classname* JUCE_CALLTYPE getInstanceWithoutCreating() noexcept   { return singletonHolder.instance; } \
    static void JUCE_CALLTYPE deleteInstance() noexcept                     { singletonHolder.deleteInstance(); } \
    void clearSingletonInstance() noexcept                                  { singletonHolder.clear (this); }


//==============================================================================
#ifndef DOXYGEN
 // These are ancient macros, and have now been updated with new names to match the JUCE style guide,
 // so please update your code to use the newer versions!
 #define juce_DeclareSingleton(Classname, doNotRecreate)                JUCE_DECLARE_SINGLETON(Classname, doNotRecreate)
 #define juce_DeclareSingleton_SingleThreaded(Classname, doNotRecreate) JUCE_DECLARE_SINGLETON_SINGLETHREADED(Classname, doNotRecreate)
 #define juce_DeclareSingleton_SingleThreaded_Minimal(Classname)        JUCE_DECLARE_SINGLETON_SINGLETHREADED_MINIMAL(Classname)
 #define juce_ImplementSingleton(Classname)                             JUCE_IMPLEMENT_SINGLETON(Classname)
 #define juce_ImplementSingleton_SingleThreaded(Classname)              JUCE_IMPLEMENT_SINGLETON(Classname)
#endif

} // namespace juce
