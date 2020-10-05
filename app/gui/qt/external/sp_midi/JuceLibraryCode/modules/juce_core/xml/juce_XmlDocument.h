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
    Parses a text-based XML document and creates an XmlElement object from it.

    The parser will parse DTDs to load external entities but won't
    check the document for validity against the DTD.

    e.g.
    @code
    XmlDocument myDocument (File ("myfile.xml"));

    if (auto mainElement = myDocument.getDocumentElement())
    {
        ..use the element
    }
    else
    {
        String error = myDocument.getLastParseError();
    }
    @endcode

    Or you can use the helper functions for much less verbose parsing..

    @code
    if (auto xml = parseXML (myXmlFile))
    {
        if (xml->hasTagName ("foobar"))
        {
            ...etc
        }
    }
    @endcode

    @see XmlElement

    @tags{Core}
*/
class JUCE_API  XmlDocument
{
public:
    //==============================================================================
    /** Creates an XmlDocument from the xml text.
        The text doesn't actually get parsed until the getDocumentElement() method is called.
    */
    XmlDocument (const String& documentText);

    /** Creates an XmlDocument from a file.
        The text doesn't actually get parsed until the getDocumentElement() method is called.
    */
    XmlDocument (const File& file);

    /** Destructor. */
    ~XmlDocument();

    //==============================================================================
    /** Creates an XmlElement object to represent the main document node.

        This method will do the actual parsing of the text, and if there's a
        parse error, it may returns nullptr (and you can find out the error using
        the getLastParseError() method).

        See also the parse() methods, which provide a shorthand way to quickly
        parse a file or string.

        @param onlyReadOuterDocumentElement     if true, the parser will only read the
                                                first section of the file, and will only
                                                return the outer document element - this
                                                allows quick checking of large files to
                                                see if they contain the correct type of
                                                tag, without having to parse the entire file
        @returns    a new XmlElement, or nullptr if there was an error.
        @see getLastParseError, getDocumentElementIfTagMatches
    */
    std::unique_ptr<XmlElement> getDocumentElement (bool onlyReadOuterDocumentElement = false);

    /** Does an inexpensive check to see whether the outer element has the given tag name, and
        then does a full parse if it matches.
        If the tag is different, or the XML parse fails, this will return nullptr.
    */
    std::unique_ptr<XmlElement> getDocumentElementIfTagMatches (StringRef requiredTag);

    /** Returns the parsing error that occurred the last time getDocumentElement was called.
        @returns the error, or an empty string if there was no error.
    */
    const String& getLastParseError() const noexcept;

    /** Sets an input source object to use for parsing documents that reference external entities.

        If the document has been created from a file, this probably won't be needed, but
        if you're parsing some text and there might be a DTD that references external
        files, you may need to create a custom input source that can retrieve the
        other files it needs.

        The object that is passed-in will be deleted automatically when no longer needed.

        @see InputSource
    */
    void setInputSource (InputSource* newSource) noexcept;

    /** Sets a flag to change the treatment of empty text elements.

        If this is true (the default state), then any text elements that contain only
        whitespace characters will be ingored during parsing. If you need to catch
        whitespace-only text, then you should set this to false before calling the
        getDocumentElement() method.
    */
    void setEmptyTextElementsIgnored (bool shouldBeIgnored) noexcept;

    //==============================================================================
    /** A handy static method that parses a file.
        This is a shortcut for creating an XmlDocument object and calling getDocumentElement() on it.
        An even better shortcut is the juce::parseXML() function, which returns a std::unique_ptr<XmlElement>!
        @returns    a new XmlElement, or nullptr if there was an error.
    */
    static std::unique_ptr<XmlElement> parse (const File& file);

    /** A handy static method that parses some XML data.
        This is a shortcut for creating an XmlDocument object and calling getDocumentElement() on it.
        An even better shortcut is the juce::parseXML() function, which returns a std::unique_ptr<XmlElement>!
        @returns    a new XmlElement, or nullptr if there was an error.
    */
    static std::unique_ptr<XmlElement> parse (const String& xmlData);


    //==============================================================================
private:
    String originalText;
    String::CharPointerType input { nullptr };
    bool outOfData = false, errorOccurred = false;
    String lastError, dtdText;
    StringArray tokenisedDTD;
    bool needToLoadDTD = false, ignoreEmptyTextElements = true;
    std::unique_ptr<InputSource> inputSource;

    std::unique_ptr<XmlElement> parseDocumentElement (String::CharPointerType, bool outer);
    void setLastError (const String&, bool carryOn);
    bool parseHeader();
    bool parseDTD();
    void skipNextWhiteSpace();
    juce_wchar readNextChar() noexcept;
    XmlElement* readNextElement (bool alsoParseSubElements);
    void readChildElements (XmlElement&);
    void readQuotedString (String&);
    void readEntity (String&);

    String getFileContents (const String&) const;
    String expandEntity (const String&);
    String expandExternalEntity (const String&);
    String getParameterEntity (const String&);

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (XmlDocument)
};

//==============================================================================
/** Attempts to parse some XML text, returning a new XmlElement if it was valid.
    If the parse fails, this will return a nullptr - if you need more information about
    errors or more parsing options, see the XmlDocument class instead.
    @see XmlDocument, parseXMLIfTagMatches
*/
std::unique_ptr<XmlElement> parseXML (const String& textToParse);

/** Attempts to parse some XML text, returning a new XmlElement if it was valid.
    If the parse fails, this will return a nullptr - if you need more information about
    errors or more parsing options, see the XmlDocument class instead.
    @see XmlDocument, parseXMLIfTagMatches
*/
std::unique_ptr<XmlElement> parseXML (const File& fileToParse);

/** Does an inexpensive check to see whether the top-level element has the given tag
    name, and if that's true, does a full parse and returns the result.
    If the outer tag doesn't match, or the XML has errors, this will return nullptr;
    @see parseXML
*/
std::unique_ptr<XmlElement> parseXMLIfTagMatches (const String& textToParse, StringRef requiredTag);

/** Does an inexpensive check to see whether the top-level element has the given tag
    name, and if that's true, does a full parse and returns the result.
    If the outer tag doesn't match, or the XML has errors, this will return nullptr;
    @see parseXML
*/
std::unique_ptr<XmlElement> parseXMLIfTagMatches (const File& fileToParse, StringRef requiredTag);


} // namespace juce
