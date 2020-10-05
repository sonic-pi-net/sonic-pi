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
/** A handy macro to make it easy to iterate all the child elements in an XmlElement.

    The parentXmlElement should be a reference to the parent XML, and the childElementVariableName
    will be the name of a pointer to each child element.

    E.g. @code
    XmlElement* myParentXml = createSomeKindOfXmlDocument();

    forEachXmlChildElement (*myParentXml, child)
    {
        if (child->hasTagName ("FOO"))
            doSomethingWithXmlElement (child);
    }

    @endcode

    @see forEachXmlChildElementWithTagName
*/
#define forEachXmlChildElement(parentXmlElement, childElementVariableName) \
\
    for (auto* childElementVariableName = (parentXmlElement).getFirstChildElement(); \
         childElementVariableName != nullptr; \
         childElementVariableName = childElementVariableName->getNextElement())

/** A macro that makes it easy to iterate all the child elements of an XmlElement
    which have a specified tag.

    This does the same job as the forEachXmlChildElement macro, but only for those
    elements that have a particular tag name.

    The parentXmlElement should be a reference to the parent XML, and the childElementVariableName
    will be the name of a pointer to each child element. The requiredTagName is the
    tag name to match.

    E.g. @code
    XmlElement* myParentXml = createSomeKindOfXmlDocument();

    forEachXmlChildElementWithTagName (*myParentXml, child, "MYTAG")
    {
        // the child object is now guaranteed to be a <MYTAG> element..
        doSomethingWithMYTAGElement (child);
    }

    @endcode

    @see forEachXmlChildElement
*/
#define forEachXmlChildElementWithTagName(parentXmlElement, childElementVariableName, requiredTagName) \
\
    for (auto* childElementVariableName = (parentXmlElement).getChildByName (requiredTagName); \
         childElementVariableName != nullptr; \
         childElementVariableName = childElementVariableName->getNextElementWithTagName (requiredTagName))


//==============================================================================
/** Used to build a tree of elements representing an XML document.

    An XML document can be parsed into a tree of XmlElements, each of which
    represents an XML tag structure, and which may itself contain other
    nested elements.

    An XmlElement can also be converted back into a text document, and has
    lots of useful methods for manipulating its attributes and sub-elements,
    so XmlElements can actually be used as a handy general-purpose data
    structure.

    Here's an example of parsing some elements: @code
    // check we're looking at the right kind of document..
    if (myElement->hasTagName ("ANIMALS"))
    {
        // now we'll iterate its sub-elements looking for 'giraffe' elements..
        forEachXmlChildElement (*myElement, e)
        {
            if (e->hasTagName ("GIRAFFE"))
            {
                // found a giraffe, so use some of its attributes..

                String giraffeName  = e->getStringAttribute ("name");
                int giraffeAge      = e->getIntAttribute ("age");
                bool isFriendly     = e->getBoolAttribute ("friendly");
            }
        }
    }
    @endcode

    And here's an example of how to create an XML document from scratch: @code
    // create an outer node called "ANIMALS"
    XmlElement animalsList ("ANIMALS");

    for (int i = 0; i < numAnimals; ++i)
    {
        // create an inner element..
        XmlElement* giraffe = new XmlElement ("GIRAFFE");

        giraffe->setAttribute ("name", "nigel");
        giraffe->setAttribute ("age", 10);
        giraffe->setAttribute ("friendly", true);

        // ..and add our new element to the parent node
        animalsList.addChildElement (giraffe);
    }

    // now we can turn the whole thing into textual XML
    auto xmlString = animalsList.toString();
    @endcode

    @see parseXML, parseXMLIfTagMatches, XmlDocument

    @tags{Core}
*/
class JUCE_API  XmlElement
{
public:
    //==============================================================================
    /** Creates an XmlElement with this tag name. */
    explicit XmlElement (const String& tagName);

    /** Creates an XmlElement with this tag name. */
    explicit XmlElement (const char* tagName);

    /** Creates an XmlElement with this tag name. */
    explicit XmlElement (const Identifier& tagName);

    /** Creates an XmlElement with this tag name. */
    explicit XmlElement (StringRef tagName);

    /** Creates an XmlElement with this tag name. */
    XmlElement (String::CharPointerType tagNameBegin, String::CharPointerType tagNameEnd);

    /** Creates a (deep) copy of another element. */
    XmlElement (const XmlElement&);

    /** Creates a (deep) copy of another element. */
    XmlElement& operator= (const XmlElement&);

    /** Move assignment operator */
    XmlElement& operator= (XmlElement&&) noexcept;

    /** Move constructor */
    XmlElement (XmlElement&&) noexcept;

    /** Deleting an XmlElement will also delete all of its child elements. */
    ~XmlElement() noexcept;

    //==============================================================================
    /** Compares two XmlElements to see if they contain the same text and attributes.

        The elements are only considered equivalent if they contain the same attributes
        with the same values, and have the same sub-nodes.

        @param other                    the other element to compare to
        @param ignoreOrderOfAttributes  if true, this means that two elements with the
                                        same attributes in a different order will be
                                        considered the same; if false, the attributes must
                                        be in the same order as well
    */
    bool isEquivalentTo (const XmlElement* other,
                         bool ignoreOrderOfAttributes) const noexcept;

    //==============================================================================
    /** A struct containing options for formatting the text when representing an
        XML element as a string.
    */
    struct TextFormat
    {
        /** Default constructor. */
        TextFormat();

        String dtd;                        /**< If supplied, this DTD will be added to the document. */
        String customHeader;               /**< If supplied, this header will be used (and customEncoding & addDefaultHeader will be ignored). */
        String customEncoding;             /**< If not empty and addDefaultHeader is true, this will be set as the encoding. Otherwise, a default of "UTF-8" will be used */
        bool addDefaultHeader = true;      /**< If true, a default header will be generated; otherwise just bare XML will be emitted. */
        int lineWrapLength = 60;           /**< A maximum line length before wrapping is done. (If newLineChars is nullptr, this is ignored) */
        const char* newLineChars = "\r\n"; /**< Allows the newline characters to be set. If you set this to nullptr, then the whole XML document will be placed on a single line. */

        TextFormat singleLine() const;     /**< returns a copy of this format with newLineChars set to nullptr. */
        TextFormat withoutHeader() const;  /**< returns a copy of this format with the addDefaultHeader flag set to false. */
    };

    /** Returns a text version of this XML element.
        If your intention is to write the XML to a file or stream, it's probably more efficient to
        use writeTo() instead of creating an intermediate string.
        @see writeTo
    */
    String toString (const TextFormat& format = {}) const;

    /** Writes the document to a stream as UTF-8.
        @see writeTo, toString
    */
    void writeTo (OutputStream& output, const TextFormat& format = {}) const;

    /** Writes the document to a file as UTF-8.
        @see writeTo, toString
    */
    bool writeTo (const File& destinationFile, const TextFormat& format = {}) const;

    //==============================================================================
    /** Returns this element's tag type name.
        E.g. for an element such as \<MOOSE legs="4" antlers="2">, this would return "MOOSE".
        @see hasTagName
    */
    const String& getTagName() const noexcept            { return tagName; }

    /** Returns the namespace portion of the tag-name, or an empty string if none is specified. */
    String getNamespace() const;

    /** Returns the part of the tag-name that follows any namespace declaration. */
    String getTagNameWithoutNamespace() const;

    /** Tests whether this element has a particular tag name.
        @param possibleTagName  the tag name you're comparing it with
        @see getTagName
    */
    bool hasTagName (StringRef possibleTagName) const noexcept;

    /** Tests whether this element has a particular tag name, ignoring any XML namespace prefix.
        So a test for e.g. "xyz" will return true for "xyz" and also "foo:xyz", "bar::xyz", etc.
        @see getTagName
    */
    bool hasTagNameIgnoringNamespace (StringRef possibleTagName) const;

    /** Changes this elements tag name.
        @see getTagName
     */
    void setTagName (StringRef newTagName);

    //==============================================================================
    /** Returns the number of XML attributes this element contains.

        E.g. for an element such as \<MOOSE legs="4" antlers="2">, this would
        return 2.
    */
    int getNumAttributes() const noexcept;

    /** Returns the name of one of the elements attributes.

        E.g. for an element such as \<MOOSE legs="4" antlers="2">, then
        getAttributeName(1) would return "antlers".

        @see getAttributeValue, getStringAttribute
    */
    const String& getAttributeName (int attributeIndex) const noexcept;

    /** Returns the value of one of the elements attributes.

        E.g. for an element such as \<MOOSE legs="4" antlers="2">, then
        getAttributeName(1) would return "2".

        @see getAttributeName, getStringAttribute
    */
    const String& getAttributeValue (int attributeIndex) const noexcept;

    //==============================================================================
    // Attribute-handling methods..

    /** Checks whether the element contains an attribute with a certain name. */
    bool hasAttribute (StringRef attributeName) const noexcept;

    /** Returns the value of a named attribute.
        @param attributeName        the name of the attribute to look up
    */
    const String& getStringAttribute (StringRef attributeName) const noexcept;

    /** Returns the value of a named attribute.
        @param attributeName        the name of the attribute to look up
        @param defaultReturnValue   a value to return if the element doesn't have an attribute
                                    with this name
    */
    String getStringAttribute (StringRef attributeName, const String& defaultReturnValue) const;

    /** Compares the value of a named attribute with a value passed-in.

        @param attributeName            the name of the attribute to look up
        @param stringToCompareAgainst   the value to compare it with
        @param ignoreCase               whether the comparison should be case-insensitive
        @returns    true if the value of the attribute is the same as the string passed-in;
                    false if it's different (or if no such attribute exists)
    */
    bool compareAttribute (StringRef attributeName,
                           StringRef stringToCompareAgainst,
                           bool ignoreCase = false) const noexcept;

    /** Returns the value of a named attribute as an integer.

        This will try to find the attribute and convert it to an integer (using
        the String::getIntValue() method).

        @param attributeName        the name of the attribute to look up
        @param defaultReturnValue   a value to return if the element doesn't have an attribute
                                    with this name
        @see setAttribute
    */
    int getIntAttribute (StringRef attributeName, int defaultReturnValue = 0) const;

    /** Returns the value of a named attribute as floating-point.

        This will try to find the attribute and convert it to a double (using
        the String::getDoubleValue() method).

        @param attributeName        the name of the attribute to look up
        @param defaultReturnValue   a value to return if the element doesn't have an attribute
                                    with this name
        @see setAttribute
    */
    double getDoubleAttribute (StringRef attributeName, double defaultReturnValue = 0.0) const;

    /** Returns the value of a named attribute as a boolean.

        This will try to find the attribute and interpret it as a boolean. To do this,
        it'll return true if the value is "1", "true", "y", etc, or false for other
        values.

        @param attributeName        the name of the attribute to look up
        @param defaultReturnValue   a value to return if the element doesn't have an attribute
                                    with this name
    */
    bool getBoolAttribute (StringRef attributeName, bool defaultReturnValue = false) const;

    /** Adds a named attribute to the element.

        If the element already contains an attribute with this name, it's value will
        be updated to the new value. If there's no such attribute yet, a new one will
        be added.

        Note that there are other setAttribute() methods that take integers,
        doubles, etc. to make it easy to store numbers.

        @param attributeName        the name of the attribute to set
        @param newValue             the value to set it to
        @see removeAttribute
    */
    void setAttribute (const Identifier& attributeName, const String& newValue);

    /** Adds a named attribute to the element, setting it to an integer value.

        If the element already contains an attribute with this name, it's value will
        be updated to the new value. If there's no such attribute yet, a new one will
        be added.

        Note that there are other setAttribute() methods that take integers,
        doubles, etc. to make it easy to store numbers.

        @param attributeName        the name of the attribute to set
        @param newValue             the value to set it to
    */
    void setAttribute (const Identifier& attributeName, int newValue);

    /** Adds a named attribute to the element, setting it to a floating-point value.

        If the element already contains an attribute with this name, it's value will
        be updated to the new value. If there's no such attribute yet, a new one will
        be added.

        Note that there are other setAttribute() methods that take integers,
        doubles, etc. to make it easy to store numbers.

        @param attributeName        the name of the attribute to set
        @param newValue             the value to set it to
    */
    void setAttribute (const Identifier& attributeName, double newValue);

    /** Removes a named attribute from the element.

        @param attributeName    the name of the attribute to remove
        @see removeAllAttributes
    */
    void removeAttribute (const Identifier& attributeName) noexcept;

    /** Removes all attributes from this element. */
    void removeAllAttributes() noexcept;

    //==============================================================================
    // Child element methods..

    /** Returns the first of this element's sub-elements.
        see getNextElement() for an example of how to iterate the sub-elements.
        @see forEachXmlChildElement
    */
    XmlElement* getFirstChildElement() const noexcept       { return firstChildElement; }

    /** Returns the next of this element's siblings.

        This can be used for iterating an element's sub-elements, e.g.
        @code
        XmlElement* child = myXmlDocument->getFirstChildElement();

        while (child != nullptr)
        {
            ...do stuff with this child..

            child = child->getNextElement();
        }
        @endcode

        Note that when iterating the child elements, some of them might be
        text elements as well as XML tags - use isTextElement() to work this
        out.

        Also, it's much easier and neater to use this method indirectly via the
        forEachXmlChildElement macro.

        @returns    the sibling element that follows this one, or a nullptr if
                    this is the last element in its parent

        @see getNextElement, isTextElement, forEachXmlChildElement
    */
    inline XmlElement* getNextElement() const noexcept          { return nextListItem; }

    /** Returns the next of this element's siblings which has the specified tag
        name.

        This is like getNextElement(), but will scan through the list until it
        finds an element with the given tag name.

        @see getNextElement, forEachXmlChildElementWithTagName
    */
    XmlElement* getNextElementWithTagName (StringRef requiredTagName) const;

    /** Returns the number of sub-elements in this element.
        @see getChildElement
    */
    int getNumChildElements() const noexcept;

    /** Returns the sub-element at a certain index.

        It's not very efficient to iterate the sub-elements by index - see
        getNextElement() for an example of how best to iterate.

        @returns the n'th child of this element, or nullptr if the index is out-of-range
        @see getNextElement, isTextElement, getChildByName
    */
    XmlElement* getChildElement (int index) const noexcept;

    /** Returns the first sub-element with a given tag-name.

        @param tagNameToLookFor     the tag name of the element you want to find
        @returns the first element with this tag name, or nullptr if none is found
        @see getNextElement, isTextElement, getChildElement, getChildByAttribute
    */
    XmlElement* getChildByName (StringRef tagNameToLookFor) const noexcept;

    /** Returns the first sub-element which has an attribute that matches the given value.

        @param attributeName     the name of the attribute to check
        @param attributeValue    the target value of the attribute
        @returns the first element with this attribute value, or nullptr if none is found
        @see getChildByName
    */
    XmlElement* getChildByAttribute (StringRef attributeName,
                                     StringRef attributeValue) const noexcept;

    //==============================================================================
    /** Appends an element to this element's list of children.

        Child elements are deleted automatically when their parent is deleted, so
        make sure the object that you pass in will not be deleted by anything else,
        and make sure it's not already the child of another element.

        Note that due to the XmlElement using a singly-linked-list, prependChildElement()
        is an O(1) operation, but addChildElement() is an O(N) operation - so if
        you're adding large number of elements, you may prefer to do so in reverse order!

        @see getFirstChildElement, getNextElement, getNumChildElements,
             getChildElement, removeChildElement
    */
    void addChildElement (XmlElement* newChildElement) noexcept;

    /** Inserts an element into this element's list of children.

        Child elements are deleted automatically when their parent is deleted, so
        make sure the object that you pass in will not be deleted by anything else,
        and make sure it's not already the child of another element.

        @param newChildElement  the element to add
        @param indexToInsertAt  the index at which to insert the new element - if this is
                                below zero, it will be added to the end of the list
        @see addChildElement, insertChildElement
    */
    void insertChildElement (XmlElement* newChildElement,
                             int indexToInsertAt) noexcept;

    /** Inserts an element at the beginning of this element's list of children.

        Child elements are deleted automatically when their parent is deleted, so
        make sure the object that you pass in will not be deleted by anything else,
        and make sure it's not already the child of another element.

        Note that due to the XmlElement using a singly-linked-list, prependChildElement()
        is an O(1) operation, but addChildElement() is an O(N) operation - so if
        you're adding large number of elements, you may prefer to do so in reverse order!

        @see addChildElement, insertChildElement
    */
    void prependChildElement (XmlElement* newChildElement) noexcept;

    /** Creates a new element with the given name and returns it, after adding it
        as a child element.

        This is a handy method that means that instead of writing this:
        @code
        XmlElement* newElement = new XmlElement ("foobar");
        myParentElement->addChildElement (newElement);
        @endcode

        ..you could just write this:
        @code
        XmlElement* newElement = myParentElement->createNewChildElement ("foobar");
        @endcode
    */
    XmlElement* createNewChildElement (StringRef tagName);

    /** Replaces one of this element's children with another node.

        If the current element passed-in isn't actually a child of this element,
        this will return false and the new one won't be added. Otherwise, the
        existing element will be deleted, replaced with the new one, and it
        will return true.
    */
    bool replaceChildElement (XmlElement* currentChildElement,
                              XmlElement* newChildNode) noexcept;

    /** Removes a child element.

        @param childToRemove            the child to look for and remove
        @param shouldDeleteTheChild     if true, the child will be deleted, if false it'll
                                        just remove it
    */
    void removeChildElement (XmlElement* childToRemove,
                             bool shouldDeleteTheChild) noexcept;

    /** Deletes all the child elements in the element.
        @see removeChildElement, deleteAllChildElementsWithTagName
    */
    void deleteAllChildElements() noexcept;

    /** Deletes all the child elements with a given tag name.
        @see removeChildElement
    */
    void deleteAllChildElementsWithTagName (StringRef tagName) noexcept;

    /** Returns true if the given element is a child of this one. */
    bool containsChildElement (const XmlElement* possibleChild) const noexcept;

    /** Recursively searches all sub-elements of this one, looking for an element
        which is the direct parent of the specified element.

        Because elements don't store a pointer to their parent, if you have one
        and need to find its parent, the only way to do so is to exhaustively
        search the whole tree for it.

        If the given child is found somewhere in this element's hierarchy, then
        this method will return its parent. If not, it will return nullptr.
    */
    XmlElement* findParentElementOf (const XmlElement* childToSearchFor) noexcept;

    //==============================================================================
    /** Sorts the child elements using a comparator.

        This will use a comparator object to sort the elements into order. The object
        passed must have a method of the form:
        @code
        int compareElements (const XmlElement* first, const XmlElement* second);
        @endcode

        ..and this method must return:
          - a value of < 0 if the first comes before the second
          - a value of 0 if the two objects are equivalent
          - a value of > 0 if the second comes before the first

        To improve performance, the compareElements() method can be declared as static or const.

        @param comparator   the comparator to use for comparing elements.
        @param retainOrderOfEquivalentItems     if this is true, then items which the comparator
                            says are equivalent will be kept in the order in which they
                            currently appear in the array. This is slower to perform, but
                            may be important in some cases. If it's false, a faster algorithm
                            is used, but equivalent elements may be rearranged.
    */
    template <class ElementComparator>
    void sortChildElements (ElementComparator& comparator,
                            bool retainOrderOfEquivalentItems = false)
    {
        auto num = getNumChildElements();

        if (num > 1)
        {
            HeapBlock<XmlElement*> elems (num);
            getChildElementsAsArray (elems);
            sortArray (comparator, (XmlElement**) elems, 0, num - 1, retainOrderOfEquivalentItems);
            reorderChildElements (elems, num);
        }
    }

    //==============================================================================
    /** Returns true if this element is a section of text.

        Elements can either be an XML tag element or a section of text, so this
        is used to find out what kind of element this one is.

        @see getAllText, addTextElement, deleteAllTextElements
    */
    bool isTextElement() const noexcept;

    /** Returns the text for a text element.

        Note that if you have an element like this:

        @code<xyz>hello</xyz>@endcode

        then calling getText on the "xyz" element won't return "hello", because that is
        actually stored in a special text sub-element inside the xyz element. To get the
        "hello" string, you could either call getText on the (unnamed) sub-element, or
        use getAllSubText() to do this automatically.

        Note that leading and trailing whitespace will be included in the string - to remove
        if, just call String::trim() on the result.

        @see isTextElement, getAllSubText, getChildElementAllSubText
    */
    const String& getText() const noexcept;

    /** Sets the text in a text element.

        Note that this is only a valid call if this element is a text element. If it's
        not, then no action will be performed. If you're trying to add text inside a normal
        element, you probably want to use addTextElement() instead.
    */
    void setText (const String& newText);

    /** Returns all the text from this element's child nodes.

        This iterates all the child elements and when it finds text elements,
        it concatenates their text into a big string which it returns.

        E.g. @code<xyz>hello <x>there</x> world</xyz>@endcode
        if you called getAllSubText on the "xyz" element, it'd return "hello there world".

        Note that leading and trailing whitespace will be included in the string - to remove
        if, just call String::trim() on the result.

        @see isTextElement, getChildElementAllSubText, getText, addTextElement
    */
    String getAllSubText() const;

    /** Returns all the sub-text of a named child element.

        If there is a child element with the given tag name, this will return
        all of its sub-text (by calling getAllSubText() on it). If there is
        no such child element, this will return the default string passed-in.

        @see getAllSubText
    */
    String getChildElementAllSubText (StringRef childTagName,
                                      const String& defaultReturnValue) const;

    /** Appends a section of text to this element.
        @see isTextElement, getText, getAllSubText
    */
    void addTextElement (const String& text);

    /** Removes all the text elements from this element.
        @see isTextElement, getText, getAllSubText, addTextElement
    */
    void deleteAllTextElements() noexcept;

    /** Creates a text element that can be added to a parent element. */
    static XmlElement* createTextElement (const String& text);

    /** Checks if a given string is a valid XML name */
    static bool isValidXmlName (StringRef possibleName) noexcept;

    //==============================================================================
    /** This has been deprecated in favour of the toString() method. */
    JUCE_DEPRECATED (String createDocument (StringRef dtdToUse,
                                            bool allOnOneLine = false,
                                            bool includeXmlHeader = true,
                                            StringRef encodingType = "UTF-8",
                                            int lineWrapLength = 60) const);

    /** This has been deprecated in favour of the writeTo() method. */
    JUCE_DEPRECATED (void writeToStream (OutputStream& output,
                                         StringRef dtdToUse,
                                         bool allOnOneLine = false,
                                         bool includeXmlHeader = true,
                                         StringRef encodingType = "UTF-8",
                                         int lineWrapLength = 60) const);

    /** This has been deprecated in favour of the writeTo() method. */
    JUCE_DEPRECATED (bool writeToFile (const File& destinationFile,
                                       StringRef dtdToUse,
                                       StringRef encodingType = "UTF-8",
                                       int lineWrapLength = 60) const);

    //==============================================================================
private:
    struct XmlAttributeNode
    {
        XmlAttributeNode (const XmlAttributeNode&) noexcept;
        XmlAttributeNode (const Identifier&, const String&) noexcept;
        XmlAttributeNode (String::CharPointerType, String::CharPointerType);

        LinkedListPointer<XmlAttributeNode> nextListItem;
        Identifier name;
        String value;

    private:
        XmlAttributeNode& operator= (const XmlAttributeNode&) = delete;
    };

    friend class XmlDocument;
    friend class LinkedListPointer<XmlAttributeNode>;
    friend class LinkedListPointer<XmlElement>;
    friend class LinkedListPointer<XmlElement>::Appender;
    friend class NamedValueSet;

    LinkedListPointer<XmlElement> nextListItem, firstChildElement;
    LinkedListPointer<XmlAttributeNode> attributes;
    String tagName;

    XmlElement (int) noexcept;
    void copyChildrenAndAttributesFrom (const XmlElement&);
    void writeElementAsText (OutputStream&, int, int, const char*) const;
    void getChildElementsAsArray (XmlElement**) const noexcept;
    void reorderChildElements (XmlElement**, int) noexcept;
    XmlAttributeNode* getAttribute (StringRef) const noexcept;

    // Sigh.. L"" or _T("") string literals are problematic in general, and really inappropriate
    // for XML tags. Use a UTF-8 encoded literal instead, or if you're really determined to use
    // UTF-16, cast it to a String and use the other constructor.
    XmlElement (const wchar_t*) = delete;

    JUCE_LEAK_DETECTOR (XmlElement)
};

} // namespace juce
