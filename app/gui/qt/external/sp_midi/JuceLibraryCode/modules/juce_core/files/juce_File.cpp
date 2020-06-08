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

File::File (const String& fullPathName)
    : fullPath (parseAbsolutePath (fullPathName))
{
}

File File::createFileWithoutCheckingPath (const String& path) noexcept
{
    File f;
    f.fullPath = path;
    return f;
}

File::File (const File& other)
    : fullPath (other.fullPath)
{
}

File& File::operator= (const String& newPath)
{
    fullPath = parseAbsolutePath (newPath);
    return *this;
}

File& File::operator= (const File& other)
{
    fullPath = other.fullPath;
    return *this;
}

File::File (File&& other) noexcept
    : fullPath (std::move (other.fullPath))
{
}

File& File::operator= (File&& other) noexcept
{
    fullPath = std::move (other.fullPath);
    return *this;
}

JUCE_DECLARE_DEPRECATED_STATIC (const File File::nonexistent{};)

//==============================================================================
static String removeEllipsis (const String& path)
{
    // This will quickly find both /../ and /./ at the expense of a minor
    // false-positive performance hit when path elements end in a dot.
   #if JUCE_WINDOWS
    if (path.contains (".\\"))
   #else
    if (path.contains ("./"))
   #endif
    {
        StringArray toks;
        toks.addTokens (path, File::getSeparatorString(), {});
        bool anythingChanged = false;

        for (int i = 1; i < toks.size(); ++i)
        {
            auto& t = toks[i];

            if (t == ".." && toks[i - 1] != "..")
            {
                anythingChanged = true;
                toks.removeRange (i - 1, 2);
                i = jmax (0, i - 2);
            }
            else if (t == ".")
            {
                anythingChanged = true;
                toks.remove (i--);
            }
        }

        if (anythingChanged)
            return toks.joinIntoString (File::getSeparatorString());
    }

    return path;
}

static String normaliseSeparators (const String& path)
{
    auto normalisedPath = path;

    String separator (File::getSeparatorString());
    String doubleSeparator (separator + separator);

    auto uncPath = normalisedPath.startsWith (doubleSeparator)
                  && ! normalisedPath.fromFirstOccurrenceOf (doubleSeparator, false, false).startsWith (separator);

    if (uncPath)
        normalisedPath = normalisedPath.fromFirstOccurrenceOf (doubleSeparator, false, false);

    while (normalisedPath.contains (doubleSeparator))
         normalisedPath = normalisedPath.replace (doubleSeparator, separator);

    return uncPath ? doubleSeparator + normalisedPath
                   : normalisedPath;
}

bool File::isRoot() const
{
    return fullPath.isNotEmpty() && *this == getParentDirectory();
}

String File::parseAbsolutePath (const String& p)
{
    if (p.isEmpty())
        return {};

   #if JUCE_WINDOWS
    // Windows..
    auto path = normaliseSeparators (removeEllipsis (p.replaceCharacter ('/', '\\')));

    if (path.startsWithChar (getSeparatorChar()))
    {
        if (path[1] != getSeparatorChar())
        {
            /*  When you supply a raw string to the File object constructor, it must be an absolute path.
                If you're trying to parse a string that may be either a relative path or an absolute path,
                you MUST provide a context against which the partial path can be evaluated - you can do
                this by simply using File::getChildFile() instead of the File constructor. E.g. saying
                "File::getCurrentWorkingDirectory().getChildFile (myUnknownPath)" would return an absolute
                path if that's what was supplied, or would evaluate a partial path relative to the CWD.
            */
            jassertfalse;

            path = File::getCurrentWorkingDirectory().getFullPathName().substring (0, 2) + path;
        }
    }
    else if (! path.containsChar (':'))
    {
        /*  When you supply a raw string to the File object constructor, it must be an absolute path.
            If you're trying to parse a string that may be either a relative path or an absolute path,
            you MUST provide a context against which the partial path can be evaluated - you can do
            this by simply using File::getChildFile() instead of the File constructor. E.g. saying
            "File::getCurrentWorkingDirectory().getChildFile (myUnknownPath)" would return an absolute
            path if that's what was supplied, or would evaluate a partial path relative to the CWD.
        */
        jassertfalse;

        return File::getCurrentWorkingDirectory().getChildFile (path).getFullPathName();
    }
   #else
    // Mac or Linux..

    // Yes, I know it's legal for a unix pathname to contain a backslash, but this assertion is here
    // to catch anyone who's trying to run code that was written on Windows with hard-coded path names.
    // If that's why you've ended up here, use File::getChildFile() to build your paths instead.
    jassert ((! p.containsChar ('\\')) || (p.indexOfChar ('/') >= 0 && p.indexOfChar ('/') < p.indexOfChar ('\\')));

    auto path = normaliseSeparators (removeEllipsis (p));

    if (path.startsWithChar ('~'))
    {
        if (path[1] == getSeparatorChar() || path[1] == 0)
        {
            // expand a name of the form "~/abc"
            path = File::getSpecialLocation (File::userHomeDirectory).getFullPathName()
                    + path.substring (1);
        }
        else
        {
            // expand a name of type "~dave/abc"
            auto userName = path.substring (1).upToFirstOccurrenceOf ("/", false, false);

            if (auto* pw = getpwnam (userName.toUTF8()))
                path = addTrailingSeparator (pw->pw_dir) + path.fromFirstOccurrenceOf ("/", false, false);
        }
    }
    else if (! path.startsWithChar (getSeparatorChar()))
    {
       #if JUCE_DEBUG || JUCE_LOG_ASSERTIONS
        if (! (path.startsWith ("./") || path.startsWith ("../")))
        {
            /*  When you supply a raw string to the File object constructor, it must be an absolute path.
                If you're trying to parse a string that may be either a relative path or an absolute path,
                you MUST provide a context against which the partial path can be evaluated - you can do
                this by simply using File::getChildFile() instead of the File constructor. E.g. saying
                "File::getCurrentWorkingDirectory().getChildFile (myUnknownPath)" would return an absolute
                path if that's what was supplied, or would evaluate a partial path relative to the CWD.
            */
            jassertfalse;

           #if JUCE_LOG_ASSERTIONS
            Logger::writeToLog ("Illegal absolute path: " + path);
           #endif
        }
       #endif

        return File::getCurrentWorkingDirectory().getChildFile (path).getFullPathName();
    }
   #endif

    while (path.endsWithChar (getSeparatorChar()) && path != getSeparatorString()) // careful not to turn a single "/" into an empty string.
        path = path.dropLastCharacters (1);

    return path;
}

String File::addTrailingSeparator (const String& path)
{
    return path.endsWithChar (getSeparatorChar()) ? path
                                                  : path + getSeparatorChar();
}

//==============================================================================
#if JUCE_LINUX
 #define NAMES_ARE_CASE_SENSITIVE 1
#endif

bool File::areFileNamesCaseSensitive()
{
   #if NAMES_ARE_CASE_SENSITIVE
    return true;
   #else
    return false;
   #endif
}

static int compareFilenames (const String& name1, const String& name2) noexcept
{
   #if NAMES_ARE_CASE_SENSITIVE
    return name1.compare (name2);
   #else
    return name1.compareIgnoreCase (name2);
   #endif
}

bool File::operator== (const File& other) const     { return compareFilenames (fullPath, other.fullPath) == 0; }
bool File::operator!= (const File& other) const     { return compareFilenames (fullPath, other.fullPath) != 0; }
bool File::operator<  (const File& other) const     { return compareFilenames (fullPath, other.fullPath) <  0; }
bool File::operator>  (const File& other) const     { return compareFilenames (fullPath, other.fullPath) >  0; }

//==============================================================================
bool File::setReadOnly (const bool shouldBeReadOnly,
                        const bool applyRecursively) const
{
    bool worked = true;

    if (applyRecursively && isDirectory())
        for (auto& f : findChildFiles (File::findFilesAndDirectories, false))
            worked = f.setReadOnly (shouldBeReadOnly, true) && worked;

    return setFileReadOnlyInternal (shouldBeReadOnly) && worked;
}

bool File::setExecutePermission (bool shouldBeExecutable) const
{
    return setFileExecutableInternal (shouldBeExecutable);
}

bool File::deleteRecursively (bool followSymlinks) const
{
    bool worked = true;

    if (isDirectory() && (followSymlinks || ! isSymbolicLink()))
        for (auto& f : findChildFiles (File::findFilesAndDirectories, false))
            worked = f.deleteRecursively (followSymlinks) && worked;

    return deleteFile() && worked;
}

bool File::moveFileTo (const File& newFile) const
{
    if (newFile.fullPath == fullPath)
        return true;

    if (! exists())
        return false;

   #if ! NAMES_ARE_CASE_SENSITIVE
    if (*this != newFile)
   #endif
        if (! newFile.deleteFile())
            return false;

    return moveInternal (newFile);
}

bool File::copyFileTo (const File& newFile) const
{
    return (*this == newFile)
            || (exists() && newFile.deleteFile() && copyInternal (newFile));
}

bool File::replaceFileIn (const File& newFile) const
{
    if (newFile.fullPath == fullPath)
        return true;

    if (! newFile.exists())
        return moveFileTo (newFile);

    if (! replaceInternal (newFile))
        return false;

    deleteFile();
    return true;
}

bool File::copyDirectoryTo (const File& newDirectory) const
{
    if (isDirectory() && newDirectory.createDirectory())
    {
        for (auto& f : findChildFiles (File::findFiles, false))
            if (! f.copyFileTo (newDirectory.getChildFile (f.getFileName())))
                return false;

        for (auto& f : findChildFiles (File::findDirectories, false))
            if (! f.copyDirectoryTo (newDirectory.getChildFile (f.getFileName())))
                return false;

        return true;
    }

    return false;
}

//==============================================================================
String File::getPathUpToLastSlash() const
{
    auto lastSlash = fullPath.lastIndexOfChar (getSeparatorChar());

    if (lastSlash > 0)
        return fullPath.substring (0, lastSlash);

    if (lastSlash == 0)
        return getSeparatorString();

    return fullPath;
}

File File::getParentDirectory() const
{
    return createFileWithoutCheckingPath (getPathUpToLastSlash());
}

//==============================================================================
String File::getFileName() const
{
    return fullPath.substring (fullPath.lastIndexOfChar (getSeparatorChar()) + 1);
}

String File::getFileNameWithoutExtension() const
{
    auto lastSlash = fullPath.lastIndexOfChar (getSeparatorChar()) + 1;
    auto lastDot   = fullPath.lastIndexOfChar ('.');

    if (lastDot > lastSlash)
        return fullPath.substring (lastSlash, lastDot);

    return fullPath.substring (lastSlash);
}

bool File::isAChildOf (const File& potentialParent) const
{
    if (potentialParent.fullPath.isEmpty())
        return false;

    auto ourPath = getPathUpToLastSlash();

    if (compareFilenames (potentialParent.fullPath, ourPath) == 0)
        return true;

    if (potentialParent.fullPath.length() >= ourPath.length())
        return false;

    return getParentDirectory().isAChildOf (potentialParent);
}

int   File::hashCode() const    { return fullPath.hashCode(); }
int64 File::hashCode64() const  { return fullPath.hashCode64(); }

//==============================================================================
bool File::isAbsolutePath (StringRef path)
{
    auto firstChar = *(path.text);

    return firstChar == getSeparatorChar()
           #if JUCE_WINDOWS
            || (firstChar != 0 && path.text[1] == ':');
           #else
            || firstChar == '~';
           #endif
}

File File::getChildFile (StringRef relativePath) const
{
    auto r = relativePath.text;

    if (isAbsolutePath (r))
        return File (String (r));

   #if JUCE_WINDOWS
    if (r.indexOf ((juce_wchar) '/') >= 0)
        return getChildFile (String (r).replaceCharacter ('/', '\\'));
   #endif

    auto path = fullPath;
    auto separatorChar = getSeparatorChar();

    while (*r == '.')
    {
        auto lastPos = r;
        auto secondChar = *++r;

        if (secondChar == '.') // remove "../"
        {
            auto thirdChar = *++r;

            if (thirdChar == separatorChar || thirdChar == 0)
            {
                auto lastSlash = path.lastIndexOfChar (separatorChar);

                if (lastSlash >= 0)
                    path = path.substring (0, lastSlash);

                while (*r == separatorChar) // ignore duplicate slashes
                    ++r;
            }
            else
            {
                r = lastPos;
                break;
            }
        }
        else if (secondChar == separatorChar || secondChar == 0)  // remove "./"
        {
            while (*r == separatorChar) // ignore duplicate slashes
                ++r;
        }
        else
        {
            r = lastPos;
            break;
        }
    }

    path = addTrailingSeparator (path);
    path.appendCharPointer (r);
    return File (path);
}

File File::getSiblingFile (StringRef fileName) const
{
    return getParentDirectory().getChildFile (fileName);
}

//==============================================================================
String File::descriptionOfSizeInBytes (const int64 bytes)
{
    const char* suffix;
    double divisor = 0;

    if (bytes == 1)                       { suffix = " byte"; }
    else if (bytes < 1024)                { suffix = " bytes"; }
    else if (bytes < 1024 * 1024)         { suffix = " KB"; divisor = 1024.0; }
    else if (bytes < 1024 * 1024 * 1024)  { suffix = " MB"; divisor = 1024.0 * 1024.0; }
    else                                  { suffix = " GB"; divisor = 1024.0 * 1024.0 * 1024.0; }

    return (divisor > 0 ? String (bytes / divisor, 1) : String (bytes)) + suffix;
}

//==============================================================================
Result File::create() const
{
    if (exists())
        return Result::ok();

    auto parentDir = getParentDirectory();

    if (parentDir == *this)
        return Result::fail ("Cannot create parent directory");

    auto r = parentDir.createDirectory();

    if (r.wasOk())
    {
        FileOutputStream fo (*this, 8);
        r = fo.getStatus();
    }

    return r;
}

Result File::createDirectory() const
{
    if (isDirectory())
        return Result::ok();

    auto parentDir = getParentDirectory();

    if (parentDir == *this)
        return Result::fail ("Cannot create parent directory");

    auto r = parentDir.createDirectory();

    if (r.wasOk())
        r = createDirectoryInternal (fullPath.trimCharactersAtEnd (getSeparatorString()));

    return r;
}

//==============================================================================
Time File::getLastModificationTime() const           { int64 m, a, c; getFileTimesInternal (m, a, c); return Time (m); }
Time File::getLastAccessTime() const                 { int64 m, a, c; getFileTimesInternal (m, a, c); return Time (a); }
Time File::getCreationTime() const                   { int64 m, a, c; getFileTimesInternal (m, a, c); return Time (c); }

bool File::setLastModificationTime (Time t) const    { return setFileTimesInternal (t.toMilliseconds(), 0, 0); }
bool File::setLastAccessTime (Time t) const          { return setFileTimesInternal (0, t.toMilliseconds(), 0); }
bool File::setCreationTime (Time t) const            { return setFileTimesInternal (0, 0, t.toMilliseconds()); }

//==============================================================================
bool File::loadFileAsData (MemoryBlock& destBlock) const
{
    if (! existsAsFile())
        return false;

    FileInputStream in (*this);
    return in.openedOk() && getSize() == (int64) in.readIntoMemoryBlock (destBlock);
}

String File::loadFileAsString() const
{
    if (! existsAsFile())
        return {};

    FileInputStream in (*this);
    return in.openedOk() ? in.readEntireStreamAsString()
                         : String();
}

void File::readLines (StringArray& destLines) const
{
    destLines.addLines (loadFileAsString());
}

//==============================================================================
Array<File> File::findChildFiles (int whatToLookFor, bool searchRecursively, const String& wildcard) const
{
    Array<File> results;
    findChildFiles (results, whatToLookFor, searchRecursively, wildcard);
    return results;
}

int File::findChildFiles (Array<File>& results, int whatToLookFor, bool searchRecursively, const String& wildcard) const
{
    int total = 0;

    for (DirectoryIterator di (*this, searchRecursively, wildcard, whatToLookFor); di.next();)
    {
        results.add (di.getFile());
        ++total;
    }

    return total;
}

int File::getNumberOfChildFiles (const int whatToLookFor, const String& wildCardPattern) const
{
    int total = 0;

    for (DirectoryIterator di (*this, false, wildCardPattern, whatToLookFor); di.next();)
        ++total;

    return total;
}

bool File::containsSubDirectories() const
{
    if (! isDirectory())
        return false;

    DirectoryIterator di (*this, false, "*", findDirectories);
    return di.next();
}

//==============================================================================
File File::getNonexistentChildFile (const String& suggestedPrefix,
                                    const String& suffix,
                                    bool putNumbersInBrackets) const
{
    auto f = getChildFile (suggestedPrefix + suffix);

    if (f.exists())
    {
        int number = 1;
        auto prefix = suggestedPrefix;

        // remove any bracketed numbers that may already be on the end..
        if (prefix.trim().endsWithChar (')'))
        {
            putNumbersInBrackets = true;

            auto openBracks  = prefix.lastIndexOfChar ('(');
            auto closeBracks = prefix.lastIndexOfChar (')');

            if (openBracks > 0
                 && closeBracks > openBracks
                 && prefix.substring (openBracks + 1, closeBracks).containsOnly ("0123456789"))
            {
                number = prefix.substring (openBracks + 1, closeBracks).getIntValue();
                prefix = prefix.substring (0, openBracks);
            }
        }

        do
        {
            auto newName = prefix;

            if (putNumbersInBrackets)
            {
                newName << '(' << ++number << ')';
            }
            else
            {
                if (CharacterFunctions::isDigit (prefix.getLastCharacter()))
                    newName << '_'; // pad with an underscore if the name already ends in a digit

                newName << ++number;
            }

            f = getChildFile (newName + suffix);

        } while (f.exists());
    }

    return f;
}

File File::getNonexistentSibling (const bool putNumbersInBrackets) const
{
    if (! exists())
        return *this;

    return getParentDirectory().getNonexistentChildFile (getFileNameWithoutExtension(),
                                                         getFileExtension(),
                                                         putNumbersInBrackets);
}

//==============================================================================
String File::getFileExtension() const
{
    auto indexOfDot = fullPath.lastIndexOfChar ('.');

    if (indexOfDot > fullPath.lastIndexOfChar (getSeparatorChar()))
        return fullPath.substring (indexOfDot);

    return {};
}

bool File::hasFileExtension (StringRef possibleSuffix) const
{
    if (possibleSuffix.isEmpty())
        return fullPath.lastIndexOfChar ('.') <= fullPath.lastIndexOfChar (getSeparatorChar());

    auto semicolon = possibleSuffix.text.indexOf ((juce_wchar) ';');

    if (semicolon >= 0)
        return hasFileExtension (String (possibleSuffix.text).substring (0, semicolon).trimEnd())
                || hasFileExtension ((possibleSuffix.text + (semicolon + 1)).findEndOfWhitespace());

    if (fullPath.endsWithIgnoreCase (possibleSuffix))
    {
        if (possibleSuffix.text[0] == '.')
            return true;

        auto dotPos = fullPath.length() - possibleSuffix.length() - 1;

        if (dotPos >= 0)
            return fullPath[dotPos] == '.';
    }

    return false;
}

File File::withFileExtension (StringRef newExtension) const
{
    if (fullPath.isEmpty())
        return {};

    auto filePart = getFileName();

    auto lastDot = filePart.lastIndexOfChar ('.');

    if (lastDot >= 0)
        filePart = filePart.substring (0, lastDot);

    if (newExtension.isNotEmpty() && newExtension.text[0] != '.')
        filePart << '.';

    return getSiblingFile (filePart + newExtension);
}

//==============================================================================
bool File::startAsProcess (const String& parameters) const
{
    return exists() && Process::openDocument (fullPath, parameters);
}

//==============================================================================
FileInputStream* File::createInputStream() const
{
    std::unique_ptr<FileInputStream> fin (new FileInputStream (*this));

    if (fin->openedOk())
        return fin.release();

    return nullptr;
}

FileOutputStream* File::createOutputStream (size_t bufferSize) const
{
    std::unique_ptr<FileOutputStream> out (new FileOutputStream (*this, bufferSize));

    return out->failedToOpen() ? nullptr
                               : out.release();
}

//==============================================================================
bool File::appendData (const void* const dataToAppend,
                       const size_t numberOfBytes) const
{
    jassert (((ssize_t) numberOfBytes) >= 0);

    if (numberOfBytes == 0)
        return true;

    FileOutputStream out (*this, 8192);
    return out.openedOk() && out.write (dataToAppend, numberOfBytes);
}

bool File::replaceWithData (const void* const dataToWrite,
                            const size_t numberOfBytes) const
{
    if (numberOfBytes == 0)
        return deleteFile();

    TemporaryFile tempFile (*this, TemporaryFile::useHiddenFile);
    tempFile.getFile().appendData (dataToWrite, numberOfBytes);
    return tempFile.overwriteTargetFileWithTemporary();
}

bool File::appendText (const String& text, bool asUnicode, bool writeHeaderBytes, const char* lineFeed) const
{
    FileOutputStream out (*this);

    if (out.failedToOpen())
        return false;

    return out.writeText (text, asUnicode, writeHeaderBytes, lineFeed);
}

bool File::replaceWithText (const String& textToWrite, bool asUnicode, bool writeHeaderBytes, const char* lineFeed) const
{
    TemporaryFile tempFile (*this, TemporaryFile::useHiddenFile);
    tempFile.getFile().appendText (textToWrite, asUnicode, writeHeaderBytes, lineFeed);
    return tempFile.overwriteTargetFileWithTemporary();
}

bool File::hasIdenticalContentTo (const File& other) const
{
    if (other == *this)
        return true;

    if (getSize() == other.getSize() && existsAsFile() && other.existsAsFile())
    {
        FileInputStream in1 (*this), in2 (other);

        if (in1.openedOk() && in2.openedOk())
        {
            const int bufferSize = 4096;
            HeapBlock<char> buffer1 (bufferSize), buffer2 (bufferSize);

            for (;;)
            {
                auto num1 = in1.read (buffer1, bufferSize);
                auto num2 = in2.read (buffer2, bufferSize);

                if (num1 != num2)
                    break;

                if (num1 <= 0)
                    return true;

                if (memcmp (buffer1, buffer2, (size_t) num1) != 0)
                    break;
            }
        }
    }

    return false;
}

//==============================================================================
String File::createLegalPathName (const String& original)
{
    auto s = original;
    String start;

    if (s.isNotEmpty() && s[1] == ':')
    {
        start = s.substring (0, 2);
        s = s.substring (2);
    }

    return start + s.removeCharacters ("\"#@,;:<>*^|?")
                    .substring (0, 1024);
}

String File::createLegalFileName (const String& original)
{
    auto s = original.removeCharacters ("\"#@,;:<>*^|?\\/");

    const int maxLength = 128; // only the length of the filename, not the whole path
    auto len = s.length();

    if (len > maxLength)
    {
        auto lastDot = s.lastIndexOfChar ('.');

        if (lastDot > jmax (0, len - 12))
        {
            s = s.substring (0, maxLength - (len - lastDot))
                 + s.substring (lastDot);
        }
        else
        {
            s = s.substring (0, maxLength);
        }
    }

    return s;
}

//==============================================================================
static int countNumberOfSeparators (String::CharPointerType s)
{
    int num = 0;

    for (;;)
    {
        auto c = s.getAndAdvance();

        if (c == 0)
            break;

        if (c == File::getSeparatorChar())
            ++num;
    }

    return num;
}

String File::getRelativePathFrom (const File& dir) const
{
    if (dir == *this)
        return ".";

    auto thisPath = fullPath;

    while (thisPath.endsWithChar (getSeparatorChar()))
        thisPath = thisPath.dropLastCharacters (1);

    auto dirPath = addTrailingSeparator (dir.existsAsFile() ? dir.getParentDirectory().getFullPathName()
                                                            : dir.fullPath);

    int commonBitLength = 0;
    auto thisPathAfterCommon = thisPath.getCharPointer();
    auto dirPathAfterCommon  = dirPath.getCharPointer();

    {
        auto thisPathIter = thisPath.getCharPointer();
        auto dirPathIter = dirPath.getCharPointer();

        for (int i = 0;;)
        {
            auto c1 = thisPathIter.getAndAdvance();
            auto c2 = dirPathIter.getAndAdvance();

           #if NAMES_ARE_CASE_SENSITIVE
            if (c1 != c2
           #else
            if ((c1 != c2 && CharacterFunctions::toLowerCase (c1) != CharacterFunctions::toLowerCase (c2))
           #endif
                 || c1 == 0)
                break;

            ++i;

            if (c1 == getSeparatorChar())
            {
                thisPathAfterCommon = thisPathIter;
                dirPathAfterCommon  = dirPathIter;
                commonBitLength = i;
            }
        }
    }

    // if the only common bit is the root, then just return the full path..
    if (commonBitLength == 0 || (commonBitLength == 1 && thisPath[1] == getSeparatorChar()))
        return fullPath;

    auto numUpDirectoriesNeeded = countNumberOfSeparators (dirPathAfterCommon);

    if (numUpDirectoriesNeeded == 0)
        return thisPathAfterCommon;

   #if JUCE_WINDOWS
    auto s = String::repeatedString ("..\\", numUpDirectoriesNeeded);
   #else
    auto s = String::repeatedString ("../",  numUpDirectoriesNeeded);
   #endif
    s.appendCharPointer (thisPathAfterCommon);
    return s;
}

//==============================================================================
File File::createTempFile (StringRef fileNameEnding)
{
    auto tempFile = getSpecialLocation (tempDirectory)
                      .getChildFile ("temp_" + String::toHexString (Random::getSystemRandom().nextInt()))
                      .withFileExtension (fileNameEnding);

    if (tempFile.exists())
        return createTempFile (fileNameEnding);

    return tempFile;
}

bool File::createSymbolicLink (const File& linkFileToCreate,
                               const String& nativePathOfTarget,
                               bool overwriteExisting)
{
    if (linkFileToCreate.exists())
    {
        if (! linkFileToCreate.isSymbolicLink())
        {
            // user has specified an existing file / directory as the link
            // this is bad! the user could end up unintentionally destroying data
            jassertfalse;
            return false;
        }

        if (overwriteExisting)
            linkFileToCreate.deleteFile();
    }

   #if JUCE_MAC || JUCE_LINUX
    // one common reason for getting an error here is that the file already exists
    if (symlink (nativePathOfTarget.toRawUTF8(), linkFileToCreate.getFullPathName().toRawUTF8()) == -1)
    {
        jassertfalse;
        return false;
    }

    return true;
   #elif JUCE_MSVC
    File targetFile (linkFileToCreate.getSiblingFile (nativePathOfTarget));

    return CreateSymbolicLink (linkFileToCreate.getFullPathName().toWideCharPointer(),
                               nativePathOfTarget.toWideCharPointer(),
                               targetFile.isDirectory() ? SYMBOLIC_LINK_FLAG_DIRECTORY : 0) != FALSE;
   #else
    ignoreUnused (nativePathOfTarget);
    jassertfalse; // symbolic links not supported on this platform!
    return false;
   #endif
}

bool File::createSymbolicLink (const File& linkFileToCreate, bool overwriteExisting) const
{
    return createSymbolicLink (linkFileToCreate, getFullPathName(), overwriteExisting);
}

#if ! JUCE_WINDOWS
File File::getLinkedTarget() const
{
    if (isSymbolicLink())
        return getSiblingFile (getNativeLinkedTarget());

    return *this;
}
#endif

//==============================================================================
MemoryMappedFile::MemoryMappedFile (const File& file, MemoryMappedFile::AccessMode mode, bool exclusive)
    : range (0, file.getSize())
{
    openInternal (file, mode, exclusive);
}

MemoryMappedFile::MemoryMappedFile (const File& file, const Range<int64>& fileRange, AccessMode mode, bool exclusive)
    : range (fileRange.getIntersectionWith (Range<int64> (0, file.getSize())))
{
    openInternal (file, mode, exclusive);
}


//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

class FileTests  : public UnitTest
{
public:
    FileTests()
        : UnitTest ("Files", UnitTestCategories::files)
    {}

    void runTest() override
    {
        beginTest ("Reading");

        const File home (File::getSpecialLocation (File::userHomeDirectory));
        const File temp (File::getSpecialLocation (File::tempDirectory));

        expect (! File().exists());
        expect (! File().existsAsFile());
        expect (! File().isDirectory());
       #if ! JUCE_WINDOWS
        expect (File("/").isDirectory());
       #endif
        expect (home.isDirectory());
        expect (home.exists());
        expect (! home.existsAsFile());
        expect (File::getSpecialLocation (File::userApplicationDataDirectory).isDirectory());
        expect (File::getSpecialLocation (File::currentExecutableFile).exists());
        expect (File::getSpecialLocation (File::currentApplicationFile).exists());
        expect (File::getSpecialLocation (File::invokedExecutableFile).exists());
        expect (home.getVolumeTotalSize() > 1024 * 1024);
        expect (home.getBytesFreeOnVolume() > 0);
        expect (! home.isHidden());
        expect (home.isOnHardDisk());
        expect (! home.isOnCDRomDrive());
        expect (File::getCurrentWorkingDirectory().exists());
        expect (home.setAsCurrentWorkingDirectory());
        expect (File::getCurrentWorkingDirectory() == home);

        {
            Array<File> roots;
            File::findFileSystemRoots (roots);
            expect (roots.size() > 0);

            int numRootsExisting = 0;
            for (int i = 0; i < roots.size(); ++i)
                if (roots[i].exists())
                    ++numRootsExisting;

            // (on windows, some of the drives may not contain media, so as long as at least one is ok..)
            expect (numRootsExisting > 0);
        }

        beginTest ("Writing");

        File demoFolder (temp.getChildFile ("JUCE UnitTests Temp Folder.folder"));
        expect (demoFolder.deleteRecursively());
        expect (demoFolder.createDirectory());
        expect (demoFolder.isDirectory());
        expect (demoFolder.getParentDirectory() == temp);
        expect (temp.isDirectory());
        expect (temp.findChildFiles (File::findFilesAndDirectories, false, "*").contains (demoFolder));
        expect (temp.findChildFiles (File::findDirectories, true, "*.folder").contains (demoFolder));

        File tempFile (demoFolder.getNonexistentChildFile ("test", ".txt", false));

        expect (tempFile.getFileExtension() == ".txt");
        expect (tempFile.hasFileExtension (".txt"));
        expect (tempFile.hasFileExtension ("txt"));
        expect (tempFile.withFileExtension ("xyz").hasFileExtension (".xyz"));
        expect (tempFile.withFileExtension ("xyz").hasFileExtension ("abc;xyz;foo"));
        expect (tempFile.withFileExtension ("xyz").hasFileExtension ("xyz;foo"));
        expect (! tempFile.withFileExtension ("h").hasFileExtension ("bar;foo;xx"));
        expect (tempFile.getSiblingFile ("foo").isAChildOf (temp));
        expect (tempFile.hasWriteAccess());

        expect (home.getChildFile (".") == home);
        expect (home.getChildFile ("..") == home.getParentDirectory());
        expect (home.getChildFile (".xyz").getFileName() == ".xyz");
        expect (home.getChildFile ("..xyz").getFileName() == "..xyz");
        expect (home.getChildFile ("...xyz").getFileName() == "...xyz");
        expect (home.getChildFile ("./xyz") == home.getChildFile ("xyz"));
        expect (home.getChildFile ("././xyz") == home.getChildFile ("xyz"));
        expect (home.getChildFile ("../xyz") == home.getParentDirectory().getChildFile ("xyz"));
        expect (home.getChildFile (".././xyz") == home.getParentDirectory().getChildFile ("xyz"));
        expect (home.getChildFile (".././xyz/./abc") == home.getParentDirectory().getChildFile ("xyz/abc"));
        expect (home.getChildFile ("./../xyz") == home.getParentDirectory().getChildFile ("xyz"));
        expect (home.getChildFile ("a1/a2/a3/./../../a4") == home.getChildFile ("a1/a4"));

        {
            FileOutputStream fo (tempFile);
            fo.write ("0123456789", 10);
        }

        expect (tempFile.exists());
        expect (tempFile.getSize() == 10);
        expect (std::abs ((int) (tempFile.getLastModificationTime().toMilliseconds() - Time::getCurrentTime().toMilliseconds())) < 3000);
        expectEquals (tempFile.loadFileAsString(), String ("0123456789"));
        expect (! demoFolder.containsSubDirectories());

        expectEquals (tempFile.getRelativePathFrom (demoFolder.getParentDirectory()), demoFolder.getFileName() + File::getSeparatorString() + tempFile.getFileName());
        expectEquals (demoFolder.getParentDirectory().getRelativePathFrom (tempFile), ".." + File::getSeparatorString() + ".." + File::getSeparatorString() + demoFolder.getParentDirectory().getFileName());

        expect (demoFolder.getNumberOfChildFiles (File::findFiles) == 1);
        expect (demoFolder.getNumberOfChildFiles (File::findFilesAndDirectories) == 1);
        expect (demoFolder.getNumberOfChildFiles (File::findDirectories) == 0);
        demoFolder.getNonexistentChildFile ("tempFolder", "", false).createDirectory();
        expect (demoFolder.getNumberOfChildFiles (File::findDirectories) == 1);
        expect (demoFolder.getNumberOfChildFiles (File::findFilesAndDirectories) == 2);
        expect (demoFolder.containsSubDirectories());

        expect (tempFile.hasWriteAccess());
        tempFile.setReadOnly (true);
        expect (! tempFile.hasWriteAccess());
        tempFile.setReadOnly (false);
        expect (tempFile.hasWriteAccess());

        Time t (Time::getCurrentTime());
        tempFile.setLastModificationTime (t);
        Time t2 = tempFile.getLastModificationTime();
        expect (std::abs ((int) (t2.toMilliseconds() - t.toMilliseconds())) <= 1000);

        {
            MemoryBlock mb;
            tempFile.loadFileAsData (mb);
            expect (mb.getSize() == 10);
            expect (mb[0] == '0');
        }

        {
            expect (tempFile.getSize() == 10);
            FileOutputStream fo (tempFile);
            expect (fo.openedOk());

            expect (fo.setPosition  (7));
            expect (fo.truncate().wasOk());
            expect (tempFile.getSize() == 7);
            fo.write ("789", 3);
            fo.flush();
            expect (tempFile.getSize() == 10);
        }

        beginTest ("Memory-mapped files");

        {
            MemoryMappedFile mmf (tempFile, MemoryMappedFile::readOnly);
            expect (mmf.getSize() == 10);
            expect (mmf.getData() != nullptr);
            expect (memcmp (mmf.getData(), "0123456789", 10) == 0);
        }

        {
            const File tempFile2 (tempFile.getNonexistentSibling (false));
            expect (tempFile2.create());
            expect (tempFile2.appendData ("xxxxxxxxxx", 10));

            {
                MemoryMappedFile mmf (tempFile2, MemoryMappedFile::readWrite);
                expect (mmf.getSize() == 10);
                expect (mmf.getData() != nullptr);
                memcpy (mmf.getData(), "abcdefghij", 10);
            }

            {
                MemoryMappedFile mmf (tempFile2, MemoryMappedFile::readWrite);
                expect (mmf.getSize() == 10);
                expect (mmf.getData() != nullptr);
                expect (memcmp (mmf.getData(), "abcdefghij", 10) == 0);
            }

            expect (tempFile2.deleteFile());
        }

        beginTest ("More writing");

        expect (tempFile.appendData ("abcdefghij", 10));
        expect (tempFile.getSize() == 20);
        expect (tempFile.replaceWithData ("abcdefghij", 10));
        expect (tempFile.getSize() == 10);

        File tempFile2 (tempFile.getNonexistentSibling (false));
        expect (tempFile.copyFileTo (tempFile2));
        expect (tempFile2.exists());
        expect (tempFile2.hasIdenticalContentTo (tempFile));
        expect (tempFile.deleteFile());
        expect (! tempFile.exists());
        expect (tempFile2.moveFileTo (tempFile));
        expect (tempFile.exists());
        expect (! tempFile2.exists());

        expect (demoFolder.deleteRecursively());
        expect (! demoFolder.exists());

        {
            URL url ("https://audio.dev/foo/bar/");
            expectEquals (url.toString (false), String ("https://audio.dev/foo/bar/"));
            expectEquals (url.getChildURL ("x").toString (false), String ("https://audio.dev/foo/bar/x"));
            expectEquals (url.getParentURL().toString (false), String ("https://audio.dev/foo"));
            expectEquals (url.getParentURL().getParentURL().toString (false), String ("https://audio.dev/"));
            expectEquals (url.getParentURL().getParentURL().getParentURL().toString (false), String ("https://audio.dev/"));
            expectEquals (url.getParentURL().getChildURL ("x").toString (false), String ("https://audio.dev/foo/x"));
            expectEquals (url.getParentURL().getParentURL().getParentURL().getChildURL ("x").toString (false), String ("https://audio.dev/x"));
        }

        {
            URL url ("https://audio.dev/foo/bar");
            expectEquals (url.toString (false), String ("https://audio.dev/foo/bar"));
            expectEquals (url.getChildURL ("x").toString (false), String ("https://audio.dev/foo/bar/x"));
            expectEquals (url.getParentURL().toString (false), String ("https://audio.dev/foo"));
            expectEquals (url.getParentURL().getParentURL().toString (false), String ("https://audio.dev/"));
            expectEquals (url.getParentURL().getParentURL().getParentURL().toString (false), String ("https://audio.dev/"));
            expectEquals (url.getParentURL().getChildURL ("x").toString (false), String ("https://audio.dev/foo/x"));
            expectEquals (url.getParentURL().getParentURL().getParentURL().getChildURL ("x").toString (false), String ("https://audio.dev/x"));
        }
    }
};

static FileTests fileUnitTests;

#endif

} // namespace juce
