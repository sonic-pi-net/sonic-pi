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

struct ZipFile::ZipEntryHolder
{
    ZipEntryHolder (const char* buffer, int fileNameLen)
    {
        isCompressed            = ByteOrder::littleEndianShort (buffer + 10) != 0;
        entry.fileTime          = parseFileTime (ByteOrder::littleEndianShort (buffer + 12),
                                                 ByteOrder::littleEndianShort (buffer + 14));
        compressedSize          = (int64) ByteOrder::littleEndianInt (buffer + 20);
        entry.uncompressedSize  = (int64) ByteOrder::littleEndianInt (buffer + 24);
        streamOffset            = (int64) ByteOrder::littleEndianInt (buffer + 42);
        entry.filename          = String::fromUTF8 (buffer + 46, fileNameLen);
    }

    struct FileNameComparator
    {
        static int compareElements (const ZipEntryHolder* e1, const ZipEntryHolder* e2) noexcept
        {
            return e1->entry.filename.compare (e2->entry.filename);
        }
    };

    static Time parseFileTime (uint32 time, uint32 date) noexcept
    {
        int year      = 1980 + (date >> 9);
        int month     = ((date >> 5) & 15) - 1;
        int day       = date & 31;
        int hours     = time >> 11;
        int minutes   = (time >> 5) & 63;
        int seconds   = (int) ((time & 31) << 1);

        return { year, month, day, hours, minutes, seconds };
    }

    ZipEntry entry;
    int64 streamOffset, compressedSize;
    bool isCompressed;
};

//==============================================================================
static int64 findCentralDirectoryFileHeader (InputStream& input, int& numEntries)
{
    BufferedInputStream in (input, 8192);

    in.setPosition (in.getTotalLength());
    auto pos = in.getPosition();
    auto lowestPos = jmax ((int64) 0, pos - 1024);
    char buffer[32] = {};

    while (pos > lowestPos)
    {
        in.setPosition (pos - 22);
        pos = in.getPosition();
        memcpy (buffer + 22, buffer, 4);

        if (in.read (buffer, 22) != 22)
            return 0;

        for (int i = 0; i < 22; ++i)
        {
            if (ByteOrder::littleEndianInt (buffer + i) == 0x06054b50)
            {
                in.setPosition (pos + i);
                in.read (buffer, 22);
                numEntries = ByteOrder::littleEndianShort (buffer + 10);
                auto offset = (int64) ByteOrder::littleEndianInt (buffer + 16);

                if (offset >= 4)
                {
                    in.setPosition (offset);

                    // This is a workaround for some zip files which seem to contain the
                    // wrong offset for the central directory - instead of including the
                    // header, they point to the byte immediately after it.
                    if (in.readInt() != 0x02014b50)
                    {
                        in.setPosition (offset - 4);

                        if (in.readInt() == 0x02014b50)
                            offset -= 4;
                    }
                }

                return offset;
            }
        }
    }

    return 0;
}

//==============================================================================
struct ZipFile::ZipInputStream  : public InputStream
{
    ZipInputStream (ZipFile& zf, const ZipFile::ZipEntryHolder& zei)
        : file (zf),
          zipEntryHolder (zei),
          inputStream (zf.inputStream)
    {
        if (zf.inputSource != nullptr)
        {
            inputStream = streamToDelete = file.inputSource->createInputStream();
        }
        else
        {
           #if JUCE_DEBUG
            zf.streamCounter.numOpenStreams++;
           #endif
        }

        char buffer[30];

        if (inputStream != nullptr
             && inputStream->setPosition (zei.streamOffset)
             && inputStream->read (buffer, 30) == 30
             && ByteOrder::littleEndianInt (buffer) == 0x04034b50)
        {
            headerSize = 30 + ByteOrder::littleEndianShort (buffer + 26)
                            + ByteOrder::littleEndianShort (buffer + 28);
        }
    }

    ~ZipInputStream()
    {
       #if JUCE_DEBUG
        if (inputStream != nullptr && inputStream == file.inputStream)
            file.streamCounter.numOpenStreams--;
       #endif
    }

    int64 getTotalLength() override
    {
        return zipEntryHolder.compressedSize;
    }

    int read (void* buffer, int howMany) override
    {
        if (headerSize <= 0)
            return 0;

        howMany = (int) jmin ((int64) howMany, zipEntryHolder.compressedSize - pos);

        if (inputStream == nullptr)
            return 0;

        int num;

        if (inputStream == file.inputStream)
        {
            const ScopedLock sl (file.lock);
            inputStream->setPosition (pos + zipEntryHolder.streamOffset + headerSize);
            num = inputStream->read (buffer, howMany);
        }
        else
        {
            inputStream->setPosition (pos + zipEntryHolder.streamOffset + headerSize);
            num = inputStream->read (buffer, howMany);
        }

        pos += num;
        return num;
    }

    bool isExhausted() override
    {
        return headerSize <= 0 || pos >= zipEntryHolder.compressedSize;
    }

    int64 getPosition() override
    {
        return pos;
    }

    bool setPosition (int64 newPos) override
    {
        pos = jlimit ((int64) 0, zipEntryHolder.compressedSize, newPos);
        return true;
    }

private:
    ZipFile& file;
    ZipEntryHolder zipEntryHolder;
    int64 pos = 0;
    int headerSize = 0;
    InputStream* inputStream;
    ScopedPointer<InputStream> streamToDelete;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ZipInputStream)
};


//==============================================================================
ZipFile::ZipFile (InputStream* stream, bool deleteStreamWhenDestroyed)
   : inputStream (stream)
{
    if (deleteStreamWhenDestroyed)
        streamToDelete = inputStream;

    init();
}

ZipFile::ZipFile (InputStream& stream)  : inputStream (&stream)
{
    init();
}

ZipFile::ZipFile (const File& file)  : inputSource (new FileInputSource (file))
{
    init();
}

ZipFile::ZipFile (InputSource* source)  : inputSource (source)
{
    init();
}

ZipFile::~ZipFile()
{
    entries.clear();
}

#if JUCE_DEBUG
ZipFile::OpenStreamCounter::~OpenStreamCounter()
{
    /* If you hit this assertion, it means you've created a stream to read one of the items in the
       zipfile, but you've forgotten to delete that stream object before deleting the file..
       Streams can't be kept open after the file is deleted because they need to share the input
       stream that is managed by the ZipFile object.
    */
    jassert (numOpenStreams == 0);
}
#endif

//==============================================================================
int ZipFile::getNumEntries() const noexcept
{
    return entries.size();
}

const ZipFile::ZipEntry* ZipFile::getEntry (const int index) const noexcept
{
    if (auto* zei = entries[index])
        return &(zei->entry);

    return nullptr;
}

int ZipFile::getIndexOfFileName (const String& fileName, bool ignoreCase) const noexcept
{
    for (int i = 0; i < entries.size(); ++i)
    {
        auto& entryFilename = entries.getUnchecked (i)->entry.filename;

        if (ignoreCase ? entryFilename.equalsIgnoreCase (fileName)
                       : entryFilename == fileName)
            return i;
    }

    return -1;
}

const ZipFile::ZipEntry* ZipFile::getEntry (const String& fileName, bool ignoreCase) const noexcept
{
    return getEntry (getIndexOfFileName (fileName, ignoreCase));
}

InputStream* ZipFile::createStreamForEntry (const int index)
{
    InputStream* stream = nullptr;

    if (auto* zei = entries[index])
    {
        stream = new ZipInputStream (*this, *zei);

        if (zei->isCompressed)
        {
            stream = new GZIPDecompressorInputStream (stream, true,
                                                      GZIPDecompressorInputStream::deflateFormat,
                                                      zei->entry.uncompressedSize);

            // (much faster to unzip in big blocks using a buffer..)
            stream = new BufferedInputStream (stream, 32768, true);
        }
    }

    return stream;
}

InputStream* ZipFile::createStreamForEntry (const ZipEntry& entry)
{
    for (int i = 0; i < entries.size(); ++i)
        if (&entries.getUnchecked (i)->entry == &entry)
            return createStreamForEntry (i);

    return nullptr;
}

void ZipFile::sortEntriesByFilename()
{
    ZipEntryHolder::FileNameComparator sorter;
    entries.sort (sorter);
}

//==============================================================================
void ZipFile::init()
{
    ScopedPointer<InputStream> toDelete;
    InputStream* in = inputStream;

    if (inputSource != nullptr)
    {
        in = inputSource->createInputStream();
        toDelete = in;
    }

    if (in != nullptr)
    {
        int numEntries = 0;
        auto centralDirectoryPos = findCentralDirectoryFileHeader (*in, numEntries);

        if (centralDirectoryPos >= 0 && centralDirectoryPos < in->getTotalLength())
        {
            auto size = (size_t) (in->getTotalLength() - centralDirectoryPos);

            in->setPosition (centralDirectoryPos);
            MemoryBlock headerData;

            if (in->readIntoMemoryBlock (headerData, (ssize_t) size) == size)
            {
                size_t pos = 0;

                for (int i = 0; i < numEntries; ++i)
                {
                    if (pos + 46 > size)
                        break;

                    auto* buffer = static_cast<const char*> (headerData.getData()) + pos;
                    auto fileNameLen = ByteOrder::littleEndianShort (buffer + 28);

                    if (pos + 46 + fileNameLen > size)
                        break;

                    entries.add (new ZipEntryHolder (buffer, fileNameLen));

                    pos += 46 + fileNameLen
                            + ByteOrder::littleEndianShort (buffer + 30)
                            + ByteOrder::littleEndianShort (buffer + 32);
                }
            }
        }
    }
}

Result ZipFile::uncompressTo (const File& targetDirectory,
                              const bool shouldOverwriteFiles)
{
    for (int i = 0; i < entries.size(); ++i)
    {
        auto result = uncompressEntry (i, targetDirectory, shouldOverwriteFiles);

        if (result.failed())
            return result;
    }

    return Result::ok();
}

Result ZipFile::uncompressEntry (int index, const File& targetDirectory, bool shouldOverwriteFiles)
{
    auto* zei = entries.getUnchecked (index);

   #if JUCE_WINDOWS
    auto entryPath = zei->entry.filename;
   #else
    auto entryPath = zei->entry.filename.replaceCharacter ('\\', '/');
   #endif

    if (entryPath.isEmpty())
        return Result::ok();

    auto targetFile = targetDirectory.getChildFile (entryPath);

    if (entryPath.endsWithChar ('/') || entryPath.endsWithChar ('\\'))
        return targetFile.createDirectory(); // (entry is a directory, not a file)

    ScopedPointer<InputStream> in (createStreamForEntry (index));

    if (in == nullptr)
        return Result::fail ("Failed to open the zip file for reading");

    if (targetFile.exists())
    {
        if (! shouldOverwriteFiles)
            return Result::ok();

        if (! targetFile.deleteFile())
            return Result::fail ("Failed to write to target file: " + targetFile.getFullPathName());
    }

    if (! targetFile.getParentDirectory().createDirectory())
        return Result::fail ("Failed to create target folder: " + targetFile.getParentDirectory().getFullPathName());

    {
        FileOutputStream out (targetFile);

        if (out.failedToOpen())
            return Result::fail ("Failed to write to target file: " + targetFile.getFullPathName());

        out << *in;
    }

    targetFile.setCreationTime (zei->entry.fileTime);
    targetFile.setLastModificationTime (zei->entry.fileTime);
    targetFile.setLastAccessTime (zei->entry.fileTime);

    return Result::ok();
}


//==============================================================================
struct ZipFile::Builder::Item
{
    Item (const File& f, InputStream* s, int compression, const String& storedPath, Time time)
        : file (f), stream (s), storedPathname (storedPath), fileTime (time), compressionLevel (compression)
    {
    }

    bool writeData (OutputStream& target, const int64 overallStartPosition)
    {
        MemoryOutputStream compressedData ((size_t) file.getSize());

        if (compressionLevel > 0)
        {
            GZIPCompressorOutputStream compressor (&compressedData, compressionLevel, false,
                                                   GZIPCompressorOutputStream::windowBitsRaw);
            if (! writeSource (compressor))
                return false;
        }
        else
        {
            if (! writeSource (compressedData))
                return false;
        }

        compressedSize = (int64) compressedData.getDataSize();
        headerStart = target.getPosition() - overallStartPosition;

        target.writeInt (0x04034b50);
        writeFlagsAndSizes (target);
        target << storedPathname
               << compressedData;

        return true;
    }

    bool writeDirectoryEntry (OutputStream& target)
    {
        target.writeInt (0x02014b50);
        target.writeShort (20); // version written
        writeFlagsAndSizes (target);
        target.writeShort (0); // comment length
        target.writeShort (0); // start disk num
        target.writeShort (0); // internal attributes
        target.writeInt (0); // external attributes
        target.writeInt ((int) (uint32) headerStart);
        target << storedPathname;

        return true;
    }

private:
    const File file;
    ScopedPointer<InputStream> stream;
    String storedPathname;
    Time fileTime;
    int64 compressedSize = 0, uncompressedSize = 0, headerStart = 0;
    int compressionLevel = 0;
    unsigned long checksum = 0;

    static void writeTimeAndDate (OutputStream& target, Time t)
    {
        target.writeShort ((short) (t.getSeconds() + (t.getMinutes() << 5) + (t.getHours() << 11)));
        target.writeShort ((short) (t.getDayOfMonth() + ((t.getMonth() + 1) << 5) + ((t.getYear() - 1980) << 9)));
    }

    bool writeSource (OutputStream& target)
    {
        if (stream == nullptr)
        {
            stream = file.createInputStream();

            if (stream == nullptr)
                return false;
        }

        checksum = 0;
        uncompressedSize = 0;
        const int bufferSize = 4096;
        HeapBlock<unsigned char> buffer (bufferSize);

        while (! stream->isExhausted())
        {
            auto bytesRead = stream->read (buffer, bufferSize);

            if (bytesRead < 0)
                return false;

            checksum = zlibNamespace::crc32 (checksum, buffer, (unsigned int) bytesRead);
            target.write (buffer, (size_t) bytesRead);
            uncompressedSize += bytesRead;
        }

        stream = nullptr;
        return true;
    }

    void writeFlagsAndSizes (OutputStream& target) const
    {
        target.writeShort (10); // version needed
        target.writeShort ((short) (1 << 11)); // this flag indicates UTF-8 filename encoding
        target.writeShort (compressionLevel > 0 ? (short) 8 : (short) 0);
        writeTimeAndDate (target, fileTime);
        target.writeInt ((int) checksum);
        target.writeInt ((int) (uint32) compressedSize);
        target.writeInt ((int) (uint32) uncompressedSize);
        target.writeShort ((short) storedPathname.toUTF8().sizeInBytes() - 1);
        target.writeShort (0); // extra field length
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Item)
};

//==============================================================================
ZipFile::Builder::Builder() {}
ZipFile::Builder::~Builder() {}

void ZipFile::Builder::addFile (const File& file, int compression, const String& path)
{
    items.add (new Item (file, nullptr, compression,
                         path.isEmpty() ? file.getFileName() : path,
                         file.getLastModificationTime()));
}

void ZipFile::Builder::addEntry (InputStream* stream, int compression, const String& path, Time time)
{
    jassert (stream != nullptr); // must not be null!
    jassert (path.isNotEmpty());
    items.add (new Item ({}, stream, compression, path, time));
}

bool ZipFile::Builder::writeToStream (OutputStream& target, double* const progress) const
{
    auto fileStart = target.getPosition();

    for (int i = 0; i < items.size(); ++i)
    {
        if (progress != nullptr)
            *progress = (i + 0.5) / items.size();

        if (! items.getUnchecked (i)->writeData (target, fileStart))
            return false;
    }

    auto directoryStart = target.getPosition();

    for (auto* item : items)
        if (! item->writeDirectoryEntry (target))
            return false;

    auto directoryEnd = target.getPosition();

    target.writeInt (0x06054b50);
    target.writeShort (0);
    target.writeShort (0);
    target.writeShort ((short) items.size());
    target.writeShort ((short) items.size());
    target.writeInt ((int) (directoryEnd - directoryStart));
    target.writeInt ((int) (directoryStart - fileStart));
    target.writeShort (0);

    if (progress != nullptr)
        *progress = 1.0;

    return true;
}

} // namespace juce
