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
    Manages a collection of properties.

    This is a slightly higher-level wrapper for managing PropertiesFile objects.

    It holds two different PropertiesFile objects internally, one for user-specific
    settings (stored in your user directory), and one for settings that are common to
    all users (stored in a folder accessible to all users).

    The class manages the creation of these files on-demand, allowing access via the
    getUserSettings() and getCommonSettings() methods.

    After creating an instance of an ApplicationProperties object, you should first
    of all call setStorageParameters() to tell it the parameters to use to create
    its files.

    @see PropertiesFile
*/
class JUCE_API  ApplicationProperties
{
public:
    //==============================================================================
    /**
        Creates an ApplicationProperties object.

        Before using it, you must call setStorageParameters() to give it the info
        it needs to create the property files.
    */
    ApplicationProperties();

    /** Destructor. */
    ~ApplicationProperties();

    //==============================================================================
    /** Gives the object the information it needs to create the appropriate properties files.
        See the PropertiesFile::Options class for details about what options you need to set.
    */
    void setStorageParameters (const PropertiesFile::Options& options);

    /** Returns the current storage parameters.
        @see setStorageParameters
    */
    const PropertiesFile::Options& getStorageParameters() const noexcept        { return options; }

    //==============================================================================
    /** Returns the user settings file.

        The first time this is called, it will create and load the properties file.

        Note that when you search the user PropertiesFile for a value that it doesn't contain,
        the common settings are used as a second-chance place to look. This is done via the
        PropertySet::setFallbackPropertySet() method - by default the common settings are set
        to the fallback for the user settings.

        @see getCommonSettings
    */
    PropertiesFile* getUserSettings();

    /** Returns the common settings file.

        The first time this is called, it will create and load the properties file.

        @param returnUserPropsIfReadOnly  if this is true, and the common properties file is
                            read-only (e.g. because the user doesn't have permission to write
                            to shared files), then this will return the user settings instead,
                            (like getUserSettings() would do). This is handy if you'd like to
                            write a value to the common settings, but if that's no possible,
                            then you'd rather write to the user settings than none at all.
                            If returnUserPropsIfReadOnly is false, this method will always return
                            the common settings, even if any changes to them can't be saved.
        @see getUserSettings
    */
    PropertiesFile* getCommonSettings (bool returnUserPropsIfReadOnly);

    //==============================================================================
    /** Saves both files if they need to be saved.

        @see PropertiesFile::saveIfNeeded
    */
    bool saveIfNeeded();

    /** Flushes and closes both files if they are open.

        This flushes any pending changes to disk with PropertiesFile::saveIfNeeded()
        and closes both files. They will then be re-opened the next time getUserSettings()
        or getCommonSettings() is called.
    */
    void closeFiles();


private:
    //==============================================================================
    PropertiesFile::Options options;
    ScopedPointer<PropertiesFile> userProps, commonProps;
    int commonSettingsAreReadOnly;

    void openFiles();

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ApplicationProperties)
};

} // namespace juce
