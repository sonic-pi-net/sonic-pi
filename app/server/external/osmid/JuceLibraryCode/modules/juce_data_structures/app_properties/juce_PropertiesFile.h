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
/** Wrapper on a file that stores a list of key/value data pairs.

    Useful for storing application settings, etc. See the PropertySet class for
    the interfaces that read and write values.

    Not designed for very large amounts of data, as it keeps all the values in
    memory and writes them out to disk lazily when they are changed.

    Because this class derives from ChangeBroadcaster, ChangeListeners can be registered
    with it, and these will be signalled when a value changes.

    @see PropertySet
*/
class JUCE_API  PropertiesFile  : public PropertySet,
                                  public ChangeBroadcaster,
                                  private Timer
{
public:
    //==============================================================================
    enum StorageFormat
    {
        storeAsBinary,
        storeAsCompressedBinary,
        storeAsXML
    };

    //==============================================================================
    struct JUCE_API  Options
    {
        /** Creates an empty Options structure.
            You'll need to fill-in the data members appropriately before using this structure.
        */
        Options();

        /** The name of your application - this is used to help generate the path and filename
            at which the properties file will be stored. */
        String applicationName;

        /** The suffix to use for your properties file.
            It doesn't really matter what this is - you may want to use ".settings" or
            ".properties" or something. If the suffix includes the prefixing dot (for example
            ".settings") then the suffix of applicationName will be replaced with your suffix
            ("MyApp.exe" -> "MyApp.settings"). If your filenameSuffix does NOT include the dot,
            then the suffix will be appended to the applicationName ("MyApp.exe" ->
            "MyApp.exe.settings").
        */
        String filenameSuffix;

        /** The name of a subfolder in which you'd like your properties file to live.
            See the getDefaultFile() method for more details about how this is used.
        */
        String folderName;

        /** If you're using properties files on a Mac, you must set this value - failure to
            do so will cause a runtime assertion.

            The PropertiesFile class always used to put its settings files in "Library/Preferences", but Apple
            have changed their advice, and now stipulate that settings should go in "Library/Application Support".

            Because older apps would be broken by a silent change in this class's behaviour, you must now
            explicitly set the osxLibrarySubFolder value to indicate which path you want to use.

            In newer apps, you should always set this to "Application Support" or
            "Application Support/YourSubFolderName".

            If your app needs to load settings files that were created by older versions of juce and
            you want to maintain backwards-compatibility, then you can set this to "Preferences".
            But.. for better Apple-compliance, the recommended approach would be to write some code that
            finds your old settings files in ~/Library/Preferences, moves them to ~/Library/Application Support,
            and then uses the new path.
        */
        String osxLibrarySubFolder;

        /** If true, the file will be created in a location that's shared between users.
            The default constructor initialises this value to false.
        */
        bool commonToAllUsers;

        /** If true, this means that property names are matched in a case-insensitive manner.
            See the PropertySet constructor for more info.
            The default constructor initialises this value to false.
        */
        bool ignoreCaseOfKeyNames;

        /** If set to true, this prevents the file from being written to disk. */
        bool doNotSave;

        /** If this is zero or greater, then after a value is changed, the object will wait
            for this amount of time and then save the file. If this zero, the file will be
            written to disk immediately on being changed (which might be slow, as it'll re-write
            synchronously each time a value-change method is called). If it is less than zero,
            the file won't be saved until save() or saveIfNeeded() are explicitly called.
            The default constructor sets this to a reasonable value of a few seconds, so you
            only need to change it if you need a special case.
        */
        int millisecondsBeforeSaving;

        /** Specifies whether the file should be written as XML, binary, etc.
            The default constructor sets this to storeAsXML, so you only need to set it explicitly
            if you want to use a different format.
        */
        StorageFormat storageFormat;

        /** An optional InterprocessLock object that will be used to prevent multiple threads or
            processes from writing to the file at the same time. The PropertiesFile will keep a
            pointer to this object but will not take ownership of it - the caller is responsible for
            making sure that the lock doesn't get deleted before the PropertiesFile has been deleted.
            The default constructor initialises this value to nullptr, so you don't need to touch it
            unless you want to use a lock.
        */
        InterProcessLock* processLock;

        /** This can be called to suggest a file that should be used, based on the values
            in this structure.

            So on a Mac, this will return a file called:
            ~/Library/[osxLibrarySubFolder]/[folderName]/[applicationName].[filenameSuffix]

            On Windows it'll return something like:
            C:\\Documents and Settings\\username\\Application Data\\[folderName]\\[applicationName].[filenameSuffix]

            On Linux it'll return
            ~/[folderName]/[applicationName].[filenameSuffix]

            If the folderName variable is empty, it'll use the app name for this (or omit the
            folder name on the Mac).

            The paths will also vary depending on whether commonToAllUsers is true.
        */
        File getDefaultFile() const;
    };

    //==============================================================================
    /** Creates a PropertiesFile object.
        The file used will be chosen by calling PropertiesFile::Options::getDefaultFile()
        for the options provided. To set the file explicitly, use the other constructor.
    */
    explicit PropertiesFile (const Options& options);

    /** Creates a PropertiesFile object.
        Unlike the other constructor, this one allows you to explicitly set the file that you
        want to be used, rather than using the default one.
    */
    PropertiesFile (const File& file,
                    const Options& options);

    /** Destructor.
        When deleted, the file will first call saveIfNeeded() to flush any changes to disk.
    */
    ~PropertiesFile();

    //==============================================================================
    /** Returns true if this file was created from a valid (or non-existent) file.
        If the file failed to load correctly because it was corrupt or had insufficient
        access, this will be false.
    */
    bool isValidFile() const noexcept               { return loadedOk; }

    //==============================================================================
    /** This will flush all the values to disk if they've changed since the last
        time they were saved.

        Returns false if it fails to write to the file for some reason (maybe because
        it's read-only or the directory doesn't exist or something).

        @see save
    */
    bool saveIfNeeded();

    /** This will force a write-to-disk of the current values, regardless of whether
        anything has changed since the last save.

        Returns false if it fails to write to the file for some reason (maybe because
        it's read-only or the directory doesn't exist or something).

        @see saveIfNeeded
    */
    bool save();

    /** Returns true if the properties have been altered since the last time they were saved.
        The file is flagged as needing to be saved when you change a value, but you can
        explicitly set this flag with setNeedsToBeSaved().
    */
    bool needsToBeSaved() const;

    /** Explicitly sets the flag to indicate whether the file needs saving or not.
        @see needsToBeSaved
    */
    void setNeedsToBeSaved (bool needsToBeSaved);

    /** Attempts to reload the settings from the file. */
    bool reload();

    //==============================================================================
    /** Returns the file that's being used. */
    const File& getFile() const noexcept            { return file; }


protected:
    /** @internal */
    void propertyChanged() override;

private:
    //==============================================================================
    File file;
    Options options;
    bool loadedOk, needsWriting;

    typedef const ScopedPointer<InterProcessLock::ScopedLockType> ProcessScopedLock;
    InterProcessLock::ScopedLockType* createProcessLock() const;

    void timerCallback() override;
    bool saveAsXml();
    bool saveAsBinary();
    bool loadAsXml();
    bool loadAsBinary();
    bool loadAsBinary (InputStream&);

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PropertiesFile)
};

} // namespace juce
