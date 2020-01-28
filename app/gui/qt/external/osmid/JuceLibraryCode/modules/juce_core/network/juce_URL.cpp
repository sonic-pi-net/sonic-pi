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

struct FallbackDownloadTask  : public URL::DownloadTask,
                               public Thread
{
    FallbackDownloadTask (FileOutputStream* outputStreamToUse,
                          size_t bufferSizeToUse,
                          WebInputStream* streamToUse,
                          URL::DownloadTask::Listener* listenerToUse)
        : Thread ("DownloadTask thread"),
          fileStream (outputStreamToUse),
          stream (streamToUse),
          bufferSize (bufferSizeToUse),
          buffer (bufferSize),
          listener (listenerToUse)
    {
        jassert (fileStream != nullptr);
        jassert (stream != nullptr);

        contentLength = stream->getTotalLength();
        httpCode      = stream->getStatusCode();

        startThread();
    }

    ~FallbackDownloadTask()
    {
        signalThreadShouldExit();
        stream->cancel();
        waitForThreadToExit (-1);
    }

    //==============================================================================
    void run() override
    {
        while (! (stream->isExhausted() || stream->isError() || threadShouldExit()))
        {
            if (listener != nullptr)
                listener->progress (this, downloaded, contentLength);

            const int max = jmin ((int) bufferSize, contentLength < 0 ? std::numeric_limits<int>::max()
                                                                      : static_cast<int> (contentLength - downloaded));

            const int actual = stream->read (buffer.get(), max);

            if (actual < 0 || threadShouldExit() || stream->isError())
                break;

            if (! fileStream->write (buffer.get(), static_cast<size_t> (actual)))
            {
                error = true;
                break;
            }

            downloaded += actual;

            if (downloaded == contentLength)
                break;
        }

        fileStream->flush();

        if (threadShouldExit() || stream->isError())
            error = true;

        if (contentLength > 0 && downloaded < contentLength)
            error = true;

        finished = true;

        if (listener != nullptr && ! threadShouldExit())
            listener->finished (this, ! error);
    }

    //==============================================================================
    const ScopedPointer<FileOutputStream> fileStream;
    const ScopedPointer<WebInputStream> stream;
    const size_t bufferSize;
    HeapBlock<char> buffer;
    URL::DownloadTask::Listener* const listener;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (FallbackDownloadTask)
};

void URL::DownloadTask::Listener::progress (DownloadTask*, int64, int64) {}
URL::DownloadTask::Listener::~Listener() {}

//==============================================================================
URL::DownloadTask* URL::DownloadTask::createFallbackDownloader (const URL& urlToUse,
                                                                const File& targetFileToUse,
                                                                const String& extraHeadersToUse,
                                                                Listener* listenerToUse,
                                                                bool usePostRequest)
{
    const size_t bufferSize = 0x8000;
    targetFileToUse.deleteFile();

    if (ScopedPointer<FileOutputStream> outputStream = targetFileToUse.createOutputStream (bufferSize))
    {
        ScopedPointer<WebInputStream> stream = new WebInputStream (urlToUse, usePostRequest);
        stream->withExtraHeaders (extraHeadersToUse);

        if (stream->connect (nullptr))
            return new FallbackDownloadTask (outputStream.release(), bufferSize, stream.release(), listenerToUse);
    }

    return nullptr;
}

URL::DownloadTask::DownloadTask() {}
URL::DownloadTask::~DownloadTask() {}

//==============================================================================
URL::URL() noexcept {}

URL::URL (const String& u)  : url (u)
{
    int i = url.indexOfChar ('?');

    if (i >= 0)
    {
        do
        {
            const int nextAmp   = url.indexOfChar (i + 1, '&');
            const int equalsPos = url.indexOfChar (i + 1, '=');

            if (nextAmp < 0)
            {
                addParameter (removeEscapeChars (equalsPos < 0 ? url.substring (i + 1) : url.substring (i + 1, equalsPos)),
                              equalsPos < 0 ? String() : removeEscapeChars (url.substring (equalsPos + 1)));
            }
            else if (nextAmp > 0 && equalsPos < nextAmp)
            {
                addParameter (removeEscapeChars (equalsPos < 0 ? url.substring (i + 1, nextAmp) : url.substring (i + 1, equalsPos)),
                              equalsPos < 0 ? String() : removeEscapeChars (url.substring (equalsPos + 1, nextAmp)));
            }

            i = nextAmp;
        }
        while (i >= 0);

        url = url.upToFirstOccurrenceOf ("?", false, false);
    }
}

URL::URL (const String& u, int)  : url (u) {}

URL::URL (URL&& other)
    : url             (static_cast<String&&> (other.url)),
      postData        (static_cast<MemoryBlock&&> (other.postData)),
      parameterNames  (static_cast<StringArray&&> (other.parameterNames)),
      parameterValues (static_cast<StringArray&&> (other.parameterValues)),
      filesToUpload   (static_cast<ReferenceCountedArray<Upload>&&> (other.filesToUpload))
{
}

URL& URL::operator= (URL&& other)
{
    url             = static_cast<String&&> (other.url);
    postData        = static_cast<MemoryBlock&&> (other.postData);
    parameterNames  = static_cast<StringArray&&> (other.parameterNames);
    parameterValues = static_cast<StringArray&&> (other.parameterValues);
    filesToUpload   = static_cast<ReferenceCountedArray<Upload>&&> (other.filesToUpload);

    return *this;
}

URL::~URL() {}

URL URL::createWithoutParsing (const String& u)
{
    return URL (u, 0);
}

bool URL::operator== (const URL& other) const
{
    return url == other.url
        && postData == other.postData
        && parameterNames == other.parameterNames
        && parameterValues == other.parameterValues
        && filesToUpload == other.filesToUpload;
}

bool URL::operator!= (const URL& other) const
{
    return ! operator== (other);
}

namespace URLHelpers
{
    static String getMangledParameters (const URL& url)
    {
        jassert (url.getParameterNames().size() == url.getParameterValues().size());
        String p;

        for (int i = 0; i < url.getParameterNames().size(); ++i)
        {
            if (i > 0)
                p << '&';

            auto val = url.getParameterValues()[i];

            p << URL::addEscapeChars (url.getParameterNames()[i], true);

            if (val.isNotEmpty())
                p << '=' << URL::addEscapeChars (val, true);
        }

        return p;
    }

    static int findEndOfScheme (const String& url)
    {
        int i = 0;

        while (CharacterFunctions::isLetterOrDigit (url[i])
               || url[i] == '+' || url[i] == '-' || url[i] == '.')
            ++i;

        return url.substring (i).startsWith ("://") ? i + 1 : 0;
    }

    static int findStartOfNetLocation (const String& url)
    {
        int start = findEndOfScheme (url);

        while (url[start] == '/')
            ++start;

        return start;
    }

    static int findStartOfPath (const String& url)
    {
        return url.indexOfChar (findStartOfNetLocation (url), '/') + 1;
    }

    static void concatenatePaths (String& path, const String& suffix)
    {
        if (! path.endsWithChar ('/'))
            path << '/';

        if (suffix.startsWithChar ('/'))
            path += suffix.substring (1);
        else
            path += suffix;
    }
}

void URL::addParameter (const String& name, const String& value)
{
    parameterNames.add (name);
    parameterValues.add (value);
}

String URL::toString (const bool includeGetParameters) const
{
    if (includeGetParameters && parameterNames.size() > 0)
        return url + "?" + URLHelpers::getMangledParameters (*this);

    return url;
}

bool URL::isEmpty() const noexcept
{
    return url.isEmpty();
}

bool URL::isWellFormed() const
{
    //xxx TODO
    return url.isNotEmpty();
}

String URL::getDomain() const
{
    auto start = URLHelpers::findStartOfNetLocation (url);
    auto end1 = url.indexOfChar (start, '/');
    auto end2 = url.indexOfChar (start, ':');

    auto end = (end1 < 0 && end2 < 0) ? std::numeric_limits<int>::max()
                                      : ((end1 < 0 || end2 < 0) ? jmax (end1, end2)
                                                                : jmin (end1, end2));
    return url.substring (start, end);
}

String URL::getSubPath() const
{
    auto startOfPath = URLHelpers::findStartOfPath (url);

    return startOfPath <= 0 ? String()
        : url.substring (startOfPath);
}

String URL::getScheme() const
{
    return url.substring (0, URLHelpers::findEndOfScheme (url) - 1);
}

int URL::getPort() const
{
    auto colonPos = url.indexOfChar (URLHelpers::findStartOfNetLocation (url), ':');

    return colonPos > 0 ? url.substring (colonPos + 1).getIntValue() : 0;
}

URL URL::withNewDomainAndPath (const String& newURL) const
{
    URL u (*this);
    u.url = newURL;
    return u;
}

URL URL::withNewSubPath (const String& newPath) const
{
    const int startOfPath = URLHelpers::findStartOfPath (url);

    URL u (*this);

    if (startOfPath > 0)
        u.url = url.substring (0, startOfPath);

    URLHelpers::concatenatePaths (u.url, newPath);
    return u;
}

URL URL::getChildURL (const String& subPath) const
{
    URL u (*this);
    URLHelpers::concatenatePaths (u.url, subPath);
    return u;
}

void URL::createHeadersAndPostData (String& headers, MemoryBlock& postDataToWrite) const
{
    MemoryOutputStream data (postDataToWrite, false);

    if (filesToUpload.size() > 0)
    {
        // (this doesn't currently support mixing custom post-data with uploads..)
        jassert (postData.getSize() == 0);

        auto boundary = String::toHexString (Random::getSystemRandom().nextInt64());

        headers << "Content-Type: multipart/form-data; boundary=" << boundary << "\r\n";

        data << "--" << boundary;

        for (int i = 0; i < parameterNames.size(); ++i)
        {
            data << "\r\nContent-Disposition: form-data; name=\"" << parameterNames[i]
                 << "\"\r\n\r\n" << parameterValues[i]
                 << "\r\n--" << boundary;
        }

        for (auto* f : filesToUpload)
        {
            data << "\r\nContent-Disposition: form-data; name=\"" << f->parameterName
                 << "\"; filename=\"" << f->filename << "\"\r\n";

            if (f->mimeType.isNotEmpty())
                data << "Content-Type: " << f->mimeType << "\r\n";

            data << "Content-Transfer-Encoding: binary\r\n\r\n";

            if (f->data != nullptr)
                data << *f->data;
            else
                data << f->file;

            data << "\r\n--" << boundary;
        }

        data << "--\r\n";
    }
    else
    {
        data << URLHelpers::getMangledParameters (*this)
             << postData;

        // if the user-supplied headers didn't contain a content-type, add one now..
        if (! headers.containsIgnoreCase ("Content-Type"))
            headers << "Content-Type: application/x-www-form-urlencoded\r\n";

        headers << "Content-length: " << (int) data.getDataSize() << "\r\n";
    }
}

//==============================================================================
bool URL::isProbablyAWebsiteURL (const String& possibleURL)
{
    static const char* validProtocols[] = { "http:", "ftp:", "https:" };

    for (auto* protocol : validProtocols)
        if (possibleURL.startsWithIgnoreCase (protocol))
            return true;

    if (possibleURL.containsChar ('@')
        || possibleURL.containsChar (' '))
        return false;

    const String topLevelDomain (possibleURL.upToFirstOccurrenceOf ("/", false, false)
                                 .fromLastOccurrenceOf (".", false, false));

    return topLevelDomain.isNotEmpty() && topLevelDomain.length() <= 3;
}

bool URL::isProbablyAnEmailAddress (const String& possibleEmailAddress)
{
    auto atSign = possibleEmailAddress.indexOfChar ('@');

    return atSign > 0
        && possibleEmailAddress.lastIndexOfChar ('.') > (atSign + 1)
        && ! possibleEmailAddress.endsWithChar ('.');
}

//==============================================================================
WebInputStream* URL::createInputStream (const bool usePostCommand,
                                        OpenStreamProgressCallback* const progressCallback,
                                        void* const progressCallbackContext,
                                        String headers,
                                        const int timeOutMs,
                                        StringPairArray* const responseHeaders,
                                        int* statusCode,
                                        const int numRedirectsToFollow,
                                        String httpRequestCmd) const
{
    ScopedPointer<WebInputStream> wi (new WebInputStream (*this, usePostCommand));

    struct ProgressCallbackCaller : WebInputStream::Listener
    {
        ProgressCallbackCaller (OpenStreamProgressCallback* const progressCallbackToUse,
                                void* const progressCallbackContextToUse)
            : callback (progressCallbackToUse), data (progressCallbackContextToUse)
        {}

        bool postDataSendProgress (WebInputStream&, int bytesSent, int totalBytes) override
        {
            return callback(data, bytesSent, totalBytes);
        }

        OpenStreamProgressCallback* const callback;
        void* const data;

        // workaround a MSVC 2013 compiler warning
        ProgressCallbackCaller (const ProgressCallbackCaller& o) : callback (o.callback), data (o.data) { jassertfalse; }
        ProgressCallbackCaller& operator= (const ProgressCallbackCaller&) { jassertfalse; return *this; }
    };

    ScopedPointer<ProgressCallbackCaller> callbackCaller =
        (progressCallback != nullptr ? new ProgressCallbackCaller (progressCallback, progressCallbackContext) : nullptr);

    if (headers.isNotEmpty())
        wi->withExtraHeaders (headers);

    if (timeOutMs != 0)
        wi->withConnectionTimeout (timeOutMs);

    if (httpRequestCmd.isNotEmpty())
        wi->withCustomRequestCommand (httpRequestCmd);

    wi->withNumRedirectsToFollow (numRedirectsToFollow);

    bool success = wi->connect (callbackCaller);

    if (statusCode != nullptr)
        *statusCode = wi->getStatusCode();

    if (responseHeaders != nullptr)
        *responseHeaders = wi->getResponseHeaders();

    if (! success || wi->isError())
        return nullptr;

    return wi.release();
}

//==============================================================================
bool URL::readEntireBinaryStream (MemoryBlock& destData, bool usePostCommand) const
{
    const ScopedPointer<InputStream> in (createInputStream (usePostCommand));

    if (in != nullptr)
    {
        in->readIntoMemoryBlock (destData);
        return true;
    }

    return false;
}

String URL::readEntireTextStream (bool usePostCommand) const
{
    const ScopedPointer<InputStream> in (createInputStream (usePostCommand));

    if (in != nullptr)
        return in->readEntireStreamAsString();

    return {};
}

XmlElement* URL::readEntireXmlStream (bool usePostCommand) const
{
    return XmlDocument::parse (readEntireTextStream (usePostCommand));
}

//==============================================================================
URL URL::withParameter (const String& parameterName,
                        const String& parameterValue) const
{
    URL u (*this);
    u.addParameter (parameterName, parameterValue);
    return u;
}

URL URL::withParameters (const StringPairArray& parametersToAdd) const
{
    URL u (*this);

    for (int i = 0; i < parametersToAdd.size(); ++i)
        u.addParameter (parametersToAdd.getAllKeys()[i],
                        parametersToAdd.getAllValues()[i]);

    return u;
}

URL URL::withPOSTData (const String& newPostData) const
{
    return withPOSTData (MemoryBlock (newPostData.toRawUTF8(), newPostData.getNumBytesAsUTF8()));
}

URL URL::withPOSTData (const MemoryBlock& newPostData) const
{
    URL u (*this);
    u.postData = newPostData;
    return u;
}

URL::Upload::Upload (const String& param, const String& name,
                     const String& mime, const File& f, MemoryBlock* mb)
    : parameterName (param), filename (name), mimeType (mime), file (f), data (mb)
{
    jassert (mimeType.isNotEmpty()); // You need to supply a mime type!
}

URL URL::withUpload (Upload* const f) const
{
    URL u (*this);

    for (int i = u.filesToUpload.size(); --i >= 0;)
        if (u.filesToUpload.getObjectPointerUnchecked(i)->parameterName == f->parameterName)
            u.filesToUpload.remove (i);

    u.filesToUpload.add (f);
    return u;
}

URL URL::withFileToUpload (const String& parameterName, const File& fileToUpload,
                           const String& mimeType) const
{
    return withUpload (new Upload (parameterName, fileToUpload.getFileName(),
                                   mimeType, fileToUpload, nullptr));
}

URL URL::withDataToUpload (const String& parameterName, const String& filename,
                           const MemoryBlock& fileContentToUpload, const String& mimeType) const
{
    return withUpload (new Upload (parameterName, filename, mimeType, File(),
                                   new MemoryBlock (fileContentToUpload)));
}

//==============================================================================
String URL::removeEscapeChars (const String& s)
{
    auto result = s.replaceCharacter ('+', ' ');

    if (! result.containsChar ('%'))
        return result;

    // We need to operate on the string as raw UTF8 chars, and then recombine them into unicode
    // after all the replacements have been made, so that multi-byte chars are handled.
    Array<char> utf8 (result.toRawUTF8(), (int) result.getNumBytesAsUTF8());

    for (int i = 0; i < utf8.size(); ++i)
    {
        if (utf8.getUnchecked(i) == '%')
        {
            const int hexDigit1 = CharacterFunctions::getHexDigitValue ((juce_wchar) (uint8) utf8 [i + 1]);
            const int hexDigit2 = CharacterFunctions::getHexDigitValue ((juce_wchar) (uint8) utf8 [i + 2]);

            if (hexDigit1 >= 0 && hexDigit2 >= 0)
            {
                utf8.set (i, (char) ((hexDigit1 << 4) + hexDigit2));
                utf8.removeRange (i + 1, 2);
            }
        }
    }

    return String::fromUTF8 (utf8.getRawDataPointer(), utf8.size());
}

String URL::addEscapeChars (const String& s, bool isParameter, bool roundBracketsAreLegal)
{
    String legalChars (isParameter ? "_-.~"
                                   : ",$_-.*!'");

    if (roundBracketsAreLegal)
        legalChars += "()";

    Array<char> utf8 (s.toRawUTF8(), (int) s.getNumBytesAsUTF8());

    for (int i = 0; i < utf8.size(); ++i)
    {
        auto c = utf8.getUnchecked(i);

        if (! (CharacterFunctions::isLetterOrDigit (c)
                 || legalChars.containsChar ((juce_wchar) c)))
        {
            utf8.set (i, '%');
            utf8.insert (++i, "0123456789ABCDEF" [((uint8) c) >> 4]);
            utf8.insert (++i, "0123456789ABCDEF" [c & 15]);
        }
    }

    return String::fromUTF8 (utf8.getRawDataPointer(), utf8.size());
}

//==============================================================================
bool URL::launchInDefaultBrowser() const
{
    auto u = toString (true);

    if (u.containsChar ('@') && ! u.containsChar (':'))
        u = "mailto:" + u;

    return Process::openDocument (u, {});
}

} // namespace juce
