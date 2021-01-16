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
    A simple implementation of a Logger that writes to a file.

    @see Logger

    @tags{Core}
*/
class JUCE_API  FileLogger  : public Logger
{
public:
    //==============================================================================
    /** Creates a FileLogger for a given file.

        @param fileToWriteTo    the file that to use - new messages will be appended
                                to the file. If the file doesn't exist, it will be created,
                                along with any parent directories that are needed.
        @param welcomeMessage   when opened, the logger will write a header to the log, along
                                with the current date and time, and this welcome message
        @param maxInitialFileSizeBytes  if this is zero or greater, then if the file already exists
                                but is larger than this number of bytes, then the start of the
                                file will be truncated to keep the size down. This prevents a log
                                file getting ridiculously large over time. The file will be truncated
                                at a new-line boundary. If this value is less than zero, no size limit
                                will be imposed; if it's zero, the file will always be deleted. Note that
                                the size is only checked once when this object is created - any logging
                                that is done later will be appended without any checking
    */
    FileLogger (const File& fileToWriteTo,
                const String& welcomeMessage,
                const int64 maxInitialFileSizeBytes = 128 * 1024);

    /** Destructor. */
    ~FileLogger() override;

    //==============================================================================
    /** Returns the file that this logger is writing to. */
    const File& getLogFile() const noexcept               { return logFile; }

    //==============================================================================
    /** Helper function to create a log file in the correct place for this platform.

        The method might return nullptr if the file can't be created for some reason.

        @param logFileSubDirectoryName      the name of the subdirectory to create inside the logs folder (as
                                            returned by getSystemLogFileFolder). It's best to use something
                                            like the name of your application here.
        @param logFileName                  the name of the file to create, e.g. "MyAppLog.txt".
        @param welcomeMessage               a message that will be written to the log when it's opened.
        @param maxInitialFileSizeBytes      (see the FileLogger constructor for more info on this)
    */
    static FileLogger* createDefaultAppLogger (const String& logFileSubDirectoryName,
                                               const String& logFileName,
                                               const String& welcomeMessage,
                                               const int64 maxInitialFileSizeBytes = 128 * 1024);

    /** Helper function to create a log file in the correct place for this platform.

        The filename used is based on the root and suffix strings provided, along with a
        time and date string, meaning that a new, empty log file will be always be created
        rather than appending to an existing one.

        The method might return nullptr if the file can't be created for some reason.

        @param logFileSubDirectoryName      the name of the subdirectory to create inside the logs folder (as
                                            returned by getSystemLogFileFolder). It's best to use something
                                            like the name of your application here.
        @param logFileNameRoot              the start of the filename to use, e.g. "MyAppLog_". This will have
                                            a timestamp and the logFileNameSuffix appended to it
        @param logFileNameSuffix            the file suffix to use, e.g. ".txt"
        @param welcomeMessage               a message that will be written to the log when it's opened.
    */
    static FileLogger* createDateStampedLogger (const String& logFileSubDirectoryName,
                                                const String& logFileNameRoot,
                                                const String& logFileNameSuffix,
                                                const String& welcomeMessage);

    //==============================================================================
    /** Returns an OS-specific folder where log-files should be stored.

        On Windows this will return a logger with a path such as:
        c:\\Documents and Settings\\username\\Application Data\\[logFileSubDirectoryName]\\[logFileName]

        On the Mac it'll create something like:
        ~/Library/Logs/[logFileSubDirectoryName]/[logFileName]

        @see createDefaultAppLogger
    */
    static File getSystemLogFileFolder();

    // (implementation of the Logger virtual method)
    void logMessage (const String&) override;

    //==============================================================================
    /** This is a utility function which removes lines from the start of a text
        file to make sure that its total size is below the given size.
    */
    static void trimFileSize (const File& file, int64 maxFileSize);

private:
    //==============================================================================
    File logFile;
    CriticalSection logLock;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (FileLogger)
};

} // namespace juce
