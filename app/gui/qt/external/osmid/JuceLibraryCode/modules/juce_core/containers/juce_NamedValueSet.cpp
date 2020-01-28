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

NamedValueSet::NamedValueSet() noexcept
{
}

NamedValueSet::NamedValueSet (const NamedValueSet& other)
   : values (other.values)
{
}

NamedValueSet& NamedValueSet::operator= (const NamedValueSet& other)
{
    clear();
    values = other.values;
    return *this;
}

NamedValueSet::NamedValueSet (NamedValueSet&& other) noexcept
    : values (static_cast<Array<NamedValue>&&> (other.values))
{
}

NamedValueSet& NamedValueSet::operator= (NamedValueSet&& other) noexcept
{
    other.values.swapWith (values);
    return *this;
}

NamedValueSet::~NamedValueSet() noexcept
{
}

void NamedValueSet::clear()
{
    values.clear();
}

bool NamedValueSet::operator== (const NamedValueSet& other) const
{
    return values == other.values;
}

bool NamedValueSet::operator!= (const NamedValueSet& other) const
{
    return ! operator== (other);
}

int NamedValueSet::size() const noexcept
{
    return values.size();
}

bool NamedValueSet::isEmpty() const noexcept
{
    return values.isEmpty();
}

static const var& getNullVarRef() noexcept
{
   #if JUCE_ALLOW_STATIC_NULL_VARIABLES
    return var::null;
   #else
    static var nullVar;
    return nullVar;
   #endif
}

const var& NamedValueSet::operator[] (const Identifier& name) const noexcept
{
    if (const var* v = getVarPointer (name))
        return *v;

    return getNullVarRef();
}

var NamedValueSet::getWithDefault (const Identifier& name, const var& defaultReturnValue) const
{
    if (const var* const v = getVarPointer (name))
        return *v;

    return defaultReturnValue;
}

var* NamedValueSet::getVarPointer (const Identifier& name) const noexcept
{
    for (NamedValue* e = values.end(), *i = values.begin(); i != e; ++i)
        if (i->name == name)
            return &(i->value);

    return nullptr;
}

bool NamedValueSet::set (const Identifier& name, var&& newValue)
{
    if (var* const v = getVarPointer (name))
    {
        if (v->equalsWithSameType (newValue))
            return false;

        *v = static_cast<var&&> (newValue);
        return true;
    }

    values.add (NamedValue (name, static_cast<var&&> (newValue)));
    return true;
}

bool NamedValueSet::set (const Identifier& name, const var& newValue)
{
    if (var* const v = getVarPointer (name))
    {
        if (v->equalsWithSameType (newValue))
            return false;

        *v = newValue;
        return true;
    }

    values.add (NamedValue (name, newValue));
    return true;
}

bool NamedValueSet::contains (const Identifier& name) const noexcept
{
    return getVarPointer (name) != nullptr;
}

int NamedValueSet::indexOf (const Identifier& name) const noexcept
{
    const int numValues = values.size();

    for (int i = 0; i < numValues; ++i)
        if (values.getReference(i).name == name)
            return i;

    return -1;
}

bool NamedValueSet::remove (const Identifier& name)
{
    const int numValues = values.size();

    for (int i = 0; i < numValues; ++i)
    {
        if (values.getReference(i).name == name)
        {
            values.remove (i);
            return true;
        }
    }

    return false;
}

Identifier NamedValueSet::getName (const int index) const noexcept
{
    if (isPositiveAndBelow (index, values.size()))
        return values.getReference (index).name;

    jassertfalse;
    return Identifier();
}

const var& NamedValueSet::getValueAt (const int index) const noexcept
{
    if (isPositiveAndBelow (index, values.size()))
        return values.getReference (index).value;

    jassertfalse;
    return getNullVarRef();
}

var* NamedValueSet::getVarPointerAt (int index) const noexcept
{
    if (isPositiveAndBelow (index, values.size()))
        return &(values.getReference (index).value);

    return nullptr;
}

void NamedValueSet::setFromXmlAttributes (const XmlElement& xml)
{
    values.clearQuick();

    for (const XmlElement::XmlAttributeNode* att = xml.attributes; att != nullptr; att = att->nextListItem)
    {
        if (att->name.toString().startsWith ("base64:"))
        {
            MemoryBlock mb;

            if (mb.fromBase64Encoding (att->value))
            {
                values.add (NamedValue (att->name.toString().substring (7), var (mb)));
                continue;
            }
        }

        values.add (NamedValue (att->name, var (att->value)));
    }
}

void NamedValueSet::copyToXmlAttributes (XmlElement& xml) const
{
    for (NamedValue* e = values.end(), *i = values.begin(); i != e; ++i)
    {
        if (const MemoryBlock* mb = i->value.getBinaryData())
        {
            xml.setAttribute ("base64:" + i->name.toString(), mb->toBase64Encoding());
        }
        else
        {
            // These types can't be stored as XML!
            jassert (! i->value.isObject());
            jassert (! i->value.isMethod());
            jassert (! i->value.isArray());

            xml.setAttribute (i->name.toString(),
                              i->value.toString());
        }
    }
}

} // namespace juce
