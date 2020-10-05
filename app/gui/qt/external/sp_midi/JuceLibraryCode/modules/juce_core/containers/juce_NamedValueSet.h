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
/** Holds a set of named var objects.

    This can be used as a basic structure to hold a set of var object, which can
    be retrieved by using their identifier.

    @tags{Core}
*/
class JUCE_API  NamedValueSet
{
public:
    //==============================================================================
    /** Structure for a named var object */
    struct JUCE_API  NamedValue
    {
        NamedValue() noexcept;
        ~NamedValue() noexcept;

        NamedValue (const Identifier& name, const var& value);
        NamedValue (const Identifier& name, var&& value) noexcept;
        NamedValue (Identifier&& name, var&& value) noexcept;

        NamedValue (const NamedValue&);
        NamedValue (NamedValue&&) noexcept;
        NamedValue& operator= (NamedValue&&) noexcept;

        bool operator== (const NamedValue&) const noexcept;
        bool operator!= (const NamedValue&) const noexcept;

        Identifier name;
        var value;
    };

    //==============================================================================
    /** Creates an empty set. */
    NamedValueSet() noexcept;

    NamedValueSet (const NamedValueSet&);
    NamedValueSet (NamedValueSet&&) noexcept;
    NamedValueSet& operator= (const NamedValueSet&);
    NamedValueSet& operator= (NamedValueSet&&) noexcept;

    /** Creates a NamedValueSet from a list of names and properties. */
    NamedValueSet (std::initializer_list<NamedValue>);

    /** Destructor. */
    ~NamedValueSet() noexcept;

    /** Two NamedValueSets are considered equal if they contain all the same key/value
        pairs, regardless of the order.
    */
    bool operator== (const NamedValueSet&) const noexcept;
    bool operator!= (const NamedValueSet&) const noexcept;

    const NamedValueSet::NamedValue* begin() const noexcept     { return values.begin(); }
    const NamedValueSet::NamedValue* end() const noexcept       { return values.end();   }

    //==============================================================================
    /** Returns the total number of values that the set contains. */
    int size() const noexcept;

    /** Returns true if the set is empty. */
    bool isEmpty() const noexcept;

    /** Returns the value of a named item.
        If the name isn't found, this will return a void variant.
    */
    const var& operator[] (const Identifier& name) const noexcept;

    /** Tries to return the named value, but if no such value is found, this will
        instead return the supplied default value.
    */
    var getWithDefault (const Identifier& name, const var& defaultReturnValue) const;

    /** Changes or adds a named value.
        @returns    true if a value was changed or added; false if the
                    value was already set the value passed-in.
    */
    bool set (const Identifier& name, const var& newValue);

    /** Changes or adds a named value.
        @returns    true if a value was changed or added; false if the
                    value was already set the value passed-in.
    */
    bool set (const Identifier& name, var&& newValue);

    /** Returns true if the set contains an item with the specified name. */
    bool contains (const Identifier& name) const noexcept;

    /** Removes a value from the set.
        @returns    true if a value was removed; false if there was no value
                    with the name that was given.
    */
    bool remove (const Identifier& name);

    /** Returns the name of the value at a given index.
        The index must be between 0 and size() - 1.
    */
    Identifier getName (int index) const noexcept;

    /** Returns a pointer to the var that holds a named value, or null if there is
        no value with this name.

        Do not use this method unless you really need access to the internal var object
        for some reason - for normal reading and writing always prefer operator[]() and set().
        Also note that the pointer returned may become invalid as soon as any subsequent
        methods are called on the NamedValueSet.
    */
    var* getVarPointer (const Identifier& name) noexcept;

    /** Returns a pointer to the var that holds a named value, or null if there is
        no value with this name.

        Do not use this method unless you really need access to the internal var object
        for some reason - for normal reading and writing always prefer operator[]() and set().
        Also note that the pointer returned may become invalid as soon as any subsequent
        methods are called on the NamedValueSet.
    */
    const var* getVarPointer (const Identifier& name) const noexcept;

    /** Returns the value of the item at a given index.
        The index must be between 0 and size() - 1.
    */
    const var& getValueAt (int index) const noexcept;

    /** Returns the value of the item at a given index.
        The index must be between 0 and size() - 1, or this will return a nullptr
        Also note that the pointer returned may become invalid as soon as any subsequent
        methods are called on the NamedValueSet.
    */
    var* getVarPointerAt (int index) noexcept;

    /** Returns the value of the item at a given index.
        The index must be between 0 and size() - 1, or this will return a nullptr
        Also note that the pointer returned may become invalid as soon as any subsequent
        methods are called on the NamedValueSet.
    */
    const var* getVarPointerAt (int index) const noexcept;

    /** Returns the index of the given name, or -1 if it's not found. */
    int indexOf (const Identifier& name) const noexcept;

    /** Removes all values. */
    void clear();

    //==============================================================================
    /** Sets properties to the values of all of an XML element's attributes. */
    void setFromXmlAttributes (const XmlElement& xml);

    /** Sets attributes in an XML element corresponding to each of this object's
        properties.
    */
    void copyToXmlAttributes (XmlElement& xml) const;

private:
    //==============================================================================
    Array<NamedValue> values;
};

} // namespace juce
