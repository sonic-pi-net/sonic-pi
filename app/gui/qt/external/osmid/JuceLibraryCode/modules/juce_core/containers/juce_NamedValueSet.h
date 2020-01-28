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

//==============================================================================
/** Holds a set of named var objects.

    This can be used as a basic structure to hold a set of var object, which can
    be retrieved by using their identifier.
*/
class JUCE_API  NamedValueSet
{
public:
    /** Creates an empty set. */
    NamedValueSet() noexcept;

    /** Creates a copy of another set. */
    NamedValueSet (const NamedValueSet&);

    /** Replaces this set with a copy of another set. */
    NamedValueSet& operator= (const NamedValueSet&);

    /** Move constructor */
    NamedValueSet (NamedValueSet&&) noexcept;

    /** Move assignment operator */
    NamedValueSet& operator= (NamedValueSet&&) noexcept;

    /** Destructor. */
    ~NamedValueSet() noexcept;

    bool operator== (const NamedValueSet&) const;
    bool operator!= (const NamedValueSet&) const;

    //==============================================================================
    struct NamedValue
    {
        NamedValue() noexcept {}
        NamedValue (const Identifier& n, const var& v)  : name (n), value (v) {}
        NamedValue (const NamedValue& other) : name (other.name), value (other.value) {}

        NamedValue (NamedValue&& other) noexcept
        : name (static_cast<Identifier&&> (other.name)),
          value (static_cast<var&&> (other.value))
        {
        }

        NamedValue (Identifier&& n, var&& v) noexcept
        : name (static_cast<Identifier&&> (n)),
          value (static_cast<var&&> (v))
        {
        }

        NamedValue& operator= (NamedValue&& other) noexcept
        {
            name = static_cast<Identifier&&> (other.name);
            value = static_cast<var&&> (other.value);
            return *this;
        }

        bool operator== (const NamedValue& other) const noexcept   { return name == other.name && value == other.value; }
        bool operator!= (const NamedValue& other) const noexcept   { return ! operator== (other); }

        Identifier name;
        var value;
    };

    NamedValueSet::NamedValue* begin() { return values.begin(); }
    NamedValueSet::NamedValue* end()   { return values.end();   }

    //==============================================================================

    /** Returns the total number of values that the set contains. */
    int size() const noexcept;

    /** Returns true if the set is empty. */
    bool isEmpty() const noexcept;

    /** Returns the value of a named item.
        If the name isn't found, this will return a void variant.
        @see getProperty
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
    */
    var* getVarPointer (const Identifier& name) const noexcept;

    /** Returns the value of the item at a given index.
        The index must be between 0 and size() - 1.
    */
    const var& getValueAt (int index) const noexcept;

    /** Returns the value of the item at a given index.
        The index must be between 0 and size() - 1, or this will return a nullptr
    */
    var* getVarPointerAt (int index) const noexcept;

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
