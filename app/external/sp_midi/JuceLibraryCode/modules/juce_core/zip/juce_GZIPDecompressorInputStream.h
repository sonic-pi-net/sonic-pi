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
    This stream will decompress a source-stream using zlib.

    Tip: if you're reading lots of small items from one of these streams, you
         can increase the performance enormously by passing it through a
         BufferedInputStream, so that it has to read larger blocks less often.

    @see GZIPCompressorOutputStream

    @tags{Core}
*/
class JUCE_API  GZIPDecompressorInputStream  : public InputStream
{
public:
    enum Format
    {
        zlibFormat = 0,
        deflateFormat,
        gzipFormat
    };

    //==============================================================================
    /** Creates a decompressor stream.

        @param sourceStream                 the stream to read from
        @param deleteSourceWhenDestroyed    whether or not to delete the source stream
                                            when this object is destroyed
        @param sourceFormat                 can be used to select which of the supported
                                            formats the data is expected to be in
        @param uncompressedStreamLength     if the creator knows the length that the
                                            uncompressed stream will be, then it can supply this
                                            value, which will be returned by getTotalLength()
    */
    GZIPDecompressorInputStream (InputStream* sourceStream,
                                 bool deleteSourceWhenDestroyed,
                                 Format sourceFormat = zlibFormat,
                                 int64 uncompressedStreamLength = -1);

    /** Creates a decompressor stream.

        @param sourceStream     the stream to read from - the source stream must not be
                                deleted until this object has been destroyed
    */
    GZIPDecompressorInputStream (InputStream& sourceStream);

    /** Destructor. */
    ~GZIPDecompressorInputStream() override;

    //==============================================================================
    int64 getPosition() override;
    bool setPosition (int64 pos) override;
    int64 getTotalLength() override;
    bool isExhausted() override;
    int read (void* destBuffer, int maxBytesToRead) override;

private:
    //==============================================================================
    OptionalScopedPointer<InputStream> sourceStream;
    const int64 uncompressedStreamLength;
    const Format format;
    bool isEof = false;
    int activeBufferSize = 0;
    int64 originalSourcePos, currentPos = 0;
    HeapBlock<uint8> buffer;

    class GZIPDecompressHelper;
    std::unique_ptr<GZIPDecompressHelper> helper;

   #if JUCE_CATCH_DEPRECATED_CODE_MISUSE
    // The arguments to this method have changed! Please pass a Format enum instead of the old dontWrap bool.
    GZIPDecompressorInputStream (InputStream*, bool, bool, int64 x = -1);
   #endif

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (GZIPDecompressorInputStream)
};

} // namespace juce
