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
    A set of named property values, which can be strings, integers, floating point, etc.

    Effectively, this just wraps a StringPairArray in an interface that makes it easier
    to load and save types other than strings.

    See the PropertiesFile class for a subclass of this, which automatically broadcasts change
    messages and saves/loads the list from a file.

    @tags{Core}
*/
class JUCE_API  PropertySet
{
public:
    //==============================================================================
    /** Creates an empty PropertySet.
        @param ignoreCaseOfKeyNames   if true, the names of properties are compared in a
                                      case-insensitive way
    */
    PropertySet (bool ignoreCaseOfKeyNames = false);

    /** Creates a copy of another PropertySet. */
    PropertySet (const PropertySet& other);

    /** Copies another PropertySet over this one. */
    PropertySet& operator= (const PropertySet& other);

    /** Destructor. */
    virtual ~PropertySet();

    //==============================================================================
    /** Returns one of the properties as a string.

        If the value isn't found in this set, then this will look for it in a fallback
        property set (if you've specified one with the setFallbackPropertySet() method),
        and if it can't find one there, it'll return the default value passed-in.

        @param keyName              the name of the property to retrieve
        @param defaultReturnValue   a value to return if the named property doesn't actually exist
    */
    String getValue (StringRef keyName, const String& defaultReturnValue = String()) const noexcept;

    /** Returns one of the properties as an integer.

        If the value isn't found in this set, then this will look for it in a fallback
        property set (if you've specified one with the setFallbackPropertySet() method),
        and if it can't find one there, it'll return the default value passed-in.

        @param keyName              the name of the property to retrieve
        @param defaultReturnValue   a value to return if the named property doesn't actually exist
    */
    int getIntValue (StringRef keyName, int defaultReturnValue = 0) const noexcept;

    /** Returns one of the properties as an double.

        If the value isn't found in this set, then this will look for it in a fallback
        property set (if you've specified one with the setFallbackPropertySet() method),
        and if it can't find one there, it'll return the default value passed-in.

        @param keyName              the name of the property to retrieve
        @param defaultReturnValue   a value to return if the named property doesn't actually exist
    */
    double getDoubleValue (StringRef keyName, double defaultReturnValue = 0.0) const noexcept;

    /** Returns one of the properties as an boolean.

        The result will be true if the string found for this key name can be parsed as a non-zero
        integer.

        If the value isn't found in this set, then this will look for it in a fallback
        property set (if you've specified one with the setFallbackPropertySet() method),
        and if it can't find one there, it'll return the default value passed-in.

        @param keyName              the name of the property to retrieve
        @param defaultReturnValue   a value to return if the named property doesn't actually exist
    */
    bool getBoolValue (StringRef keyName, bool defaultReturnValue = false) const noexcept;

    /** Returns one of the properties as an XML element.

        The result will a new XMLElement object. It may return nullptr if the key isn't found,
        or if the entry contains an string that isn't valid XML.

        If the value isn't found in this set, then this will look for it in a fallback
        property set (if you've specified one with the setFallbackPropertySet() method),
        and if it can't find one there, it'll return the default value passed-in.

        @param keyName              the name of the property to retrieve
    */
    std::unique_ptr<XmlElement> getXmlValue (StringRef keyName) const;

    //==============================================================================
    /** Sets a named property.

        @param keyName      the name of the property to set. (This mustn't be an empty string)
        @param value        the new value to set it to
    */
    void setValue (StringRef keyName, const var& value);

    /** Sets a named property to an XML element.

        @param keyName      the name of the property to set. (This mustn't be an empty string)
        @param xml          the new element to set it to. If this is a nullptr, the value will
                            be set to an empty string
        @see getXmlValue
    */
    void setValue (StringRef keyName, const XmlElement* xml);

    /** This copies all the values from a source PropertySet to this one.
        This won't remove any existing settings, it just adds any that it finds in the source set.
    */
    void addAllPropertiesFrom (const PropertySet& source);

    //==============================================================================
    /** Deletes a property.
        @param keyName      the name of the property to delete. (This mustn't be an empty string)
    */
    void removeValue (StringRef keyName);

    /** Returns true if the properties include the given key. */
    bool containsKey (StringRef keyName) const noexcept;

    /** Removes all values. */
    void clear();

    //==============================================================================
    /** Returns the keys/value pair array containing all the properties. */
    StringPairArray& getAllProperties() noexcept                        { return properties; }

    /** Returns the lock used when reading or writing to this set */
    const CriticalSection& getLock() const noexcept                     { return lock; }

    //==============================================================================
    /** Returns an XML element which encapsulates all the items in this property set.
        The string parameter is the tag name that should be used for the node.
        @see restoreFromXml
    */
    std::unique_ptr<XmlElement> createXml (const String& nodeName) const;

    /** Reloads a set of properties that were previously stored as XML.
        The node passed in must have been created by the createXml() method.
        @see createXml
    */
    void restoreFromXml (const XmlElement& xml);

    //==============================================================================
    /** Sets up a second PopertySet that will be used to look up any values that aren't
        set in this one.

        If you set this up to be a pointer to a second property set, then whenever one
        of the getValue() methods fails to find an entry in this set, it will look up that
        value in the fallback set, and if it finds it, it will return that.

        Make sure that you don't delete the fallback set while it's still being used by
        another set! To remove the fallback set, just call this method with a null pointer.

        @see getFallbackPropertySet
    */
    void setFallbackPropertySet (PropertySet* fallbackProperties) noexcept;

    /** Returns the fallback property set.
        @see setFallbackPropertySet
    */
    PropertySet* getFallbackPropertySet() const noexcept                { return fallbackProperties; }

protected:
    /** Subclasses can override this to be told when one of the properties has been changed. */
    virtual void propertyChanged();

private:
    StringPairArray properties;
    PropertySet* fallbackProperties;
    CriticalSection lock;
    bool ignoreCaseOfKeys;

    JUCE_LEAK_DETECTOR (PropertySet)
};

} // namespace juce
