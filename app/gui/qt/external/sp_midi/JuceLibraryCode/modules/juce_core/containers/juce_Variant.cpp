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

enum VariantStreamMarkers
{
    varMarker_Int       = 1,
    varMarker_BoolTrue  = 2,
    varMarker_BoolFalse = 3,
    varMarker_Double    = 4,
    varMarker_String    = 5,
    varMarker_Int64     = 6,
    varMarker_Array     = 7,
    varMarker_Binary    = 8,
    varMarker_Undefined = 9
};

//==============================================================================
class var::VariantType
{
public:
    VariantType() noexcept {}
    virtual ~VariantType() noexcept {}

    virtual int toInt (const ValueUnion&) const noexcept                        { return 0; }
    virtual int64 toInt64 (const ValueUnion&) const noexcept                    { return 0; }
    virtual double toDouble (const ValueUnion&) const noexcept                  { return 0; }
    virtual String toString (const ValueUnion&) const                           { return {}; }
    virtual bool toBool (const ValueUnion&) const noexcept                      { return false; }
    virtual ReferenceCountedObject* toObject (const ValueUnion&) const noexcept { return nullptr; }
    virtual Array<var>* toArray (const ValueUnion&) const noexcept              { return nullptr; }
    virtual MemoryBlock* toBinary (const ValueUnion&) const noexcept            { return nullptr; }
    virtual var clone (const var& original) const                               { return original; }

    virtual bool isVoid() const noexcept       { return false; }
    virtual bool isUndefined() const noexcept  { return false; }
    virtual bool isInt() const noexcept        { return false; }
    virtual bool isInt64() const noexcept      { return false; }
    virtual bool isBool() const noexcept       { return false; }
    virtual bool isDouble() const noexcept     { return false; }
    virtual bool isString() const noexcept     { return false; }
    virtual bool isObject() const noexcept     { return false; }
    virtual bool isArray() const noexcept      { return false; }
    virtual bool isBinary() const noexcept     { return false; }
    virtual bool isMethod() const noexcept     { return false; }
    virtual bool isComparable() const noexcept { return false; }

    virtual void cleanUp (ValueUnion&) const noexcept {}
    virtual void createCopy (ValueUnion& dest, const ValueUnion& source) const      { dest = source; }
    virtual bool equals (const ValueUnion& data, const ValueUnion& otherData, const VariantType& otherType) const noexcept = 0;
    virtual void writeToStream (const ValueUnion& data, OutputStream& output) const = 0;
};

//==============================================================================
class var::VariantType_Void  : public var::VariantType
{
public:
    VariantType_Void() noexcept {}
    static const VariantType_Void instance;

    bool isVoid() const noexcept override           { return true; }
    bool isComparable() const noexcept override     { return true; }
    bool equals (const ValueUnion&, const ValueUnion&, const VariantType& otherType) const noexcept override { return otherType.isVoid() || otherType.isUndefined(); }
    void writeToStream (const ValueUnion&, OutputStream& output) const override   { output.writeCompressedInt (0); }
};

//==============================================================================
class var::VariantType_Undefined  : public var::VariantType
{
public:
    VariantType_Undefined() noexcept {}
    static const VariantType_Undefined instance;

    bool isUndefined() const noexcept override           { return true; }
    String toString (const ValueUnion&) const override   { return "undefined"; }
    bool equals (const ValueUnion&, const ValueUnion&, const VariantType& otherType) const noexcept override { return otherType.isVoid() || otherType.isUndefined(); }

    void writeToStream (const ValueUnion&, OutputStream& output) const override
    {
        output.writeCompressedInt (1);
        output.writeByte (varMarker_Undefined);
    }
};

//==============================================================================
class var::VariantType_Int  : public var::VariantType
{
public:
    VariantType_Int() noexcept {}
    static const VariantType_Int instance;

    int toInt (const ValueUnion& data) const noexcept override       { return data.intValue; }
    int64 toInt64 (const ValueUnion& data) const noexcept override   { return (int64) data.intValue; }
    double toDouble (const ValueUnion& data) const noexcept override { return (double) data.intValue; }
    String toString (const ValueUnion& data) const override          { return String (data.intValue); }
    bool toBool (const ValueUnion& data) const noexcept override     { return data.intValue != 0; }
    bool isInt() const noexcept override                             { return true; }
    bool isComparable() const noexcept override                      { return true; }

    bool equals (const ValueUnion& data, const ValueUnion& otherData, const VariantType& otherType) const noexcept override
    {
        if (otherType.isDouble() || otherType.isInt64() || otherType.isString())
            return otherType.equals (otherData, data, *this);

        return otherType.toInt (otherData) == data.intValue;
    }

    void writeToStream (const ValueUnion& data, OutputStream& output) const override
    {
        output.writeCompressedInt (5);
        output.writeByte (varMarker_Int);
        output.writeInt (data.intValue);
    }
};

//==============================================================================
class var::VariantType_Int64  : public var::VariantType
{
public:
    VariantType_Int64() noexcept {}
    static const VariantType_Int64 instance;

    int toInt (const ValueUnion& data) const noexcept override       { return (int) data.int64Value; }
    int64 toInt64 (const ValueUnion& data) const noexcept override   { return data.int64Value; }
    double toDouble (const ValueUnion& data) const noexcept override { return (double) data.int64Value; }
    String toString (const ValueUnion& data) const override          { return String (data.int64Value); }
    bool toBool (const ValueUnion& data) const noexcept override     { return data.int64Value != 0; }
    bool isInt64() const noexcept override                           { return true; }
    bool isComparable() const noexcept override                      { return true; }

    bool equals (const ValueUnion& data, const ValueUnion& otherData, const VariantType& otherType) const noexcept override
    {
        if (otherType.isDouble() || otherType.isString())
            return otherType.equals (otherData, data, *this);

        return otherType.toInt64 (otherData) == data.int64Value;
    }

    void writeToStream (const ValueUnion& data, OutputStream& output) const override
    {
        output.writeCompressedInt (9);
        output.writeByte (varMarker_Int64);
        output.writeInt64 (data.int64Value);
    }
};

//==============================================================================
class var::VariantType_Double   : public var::VariantType
{
public:
    VariantType_Double() noexcept {}
    static const VariantType_Double instance;

    int toInt (const ValueUnion& data) const noexcept override       { return (int) data.doubleValue; }
    int64 toInt64 (const ValueUnion& data) const noexcept override   { return (int64) data.doubleValue; }
    double toDouble (const ValueUnion& data) const noexcept override { return data.doubleValue; }
    String toString (const ValueUnion& data) const override          { return serialiseDouble (data.doubleValue); }
    bool toBool (const ValueUnion& data) const noexcept override     { return data.doubleValue != 0.0; }
    bool isDouble() const noexcept override                          { return true; }
    bool isComparable() const noexcept override                      { return true; }

    bool equals (const ValueUnion& data, const ValueUnion& otherData, const VariantType& otherType) const noexcept override
    {
        return std::abs (otherType.toDouble (otherData) - data.doubleValue) < std::numeric_limits<double>::epsilon();
    }

    void writeToStream (const ValueUnion& data, OutputStream& output) const override
    {
        output.writeCompressedInt (9);
        output.writeByte (varMarker_Double);
        output.writeDouble (data.doubleValue);
    }
};

//==============================================================================
class var::VariantType_Bool   : public var::VariantType
{
public:
    VariantType_Bool() noexcept {}
    static const VariantType_Bool instance;

    int toInt (const ValueUnion& data) const noexcept override       { return data.boolValue ? 1 : 0; }
    int64 toInt64 (const ValueUnion& data) const noexcept override   { return data.boolValue ? 1 : 0; }
    double toDouble (const ValueUnion& data) const noexcept override { return data.boolValue ? 1.0 : 0.0; }
    String toString (const ValueUnion& data) const override          { return String::charToString (data.boolValue ? (juce_wchar) '1' : (juce_wchar) '0'); }
    bool toBool (const ValueUnion& data) const noexcept override     { return data.boolValue; }
    bool isBool() const noexcept override                            { return true; }
    bool isComparable() const noexcept override                      { return true; }

    bool equals (const ValueUnion& data, const ValueUnion& otherData, const VariantType& otherType) const noexcept override
    {
        return otherType.toBool (otherData) == data.boolValue;
    }

    void writeToStream (const ValueUnion& data, OutputStream& output) const override
    {
        output.writeCompressedInt (1);
        output.writeByte (data.boolValue ? (char) varMarker_BoolTrue : (char) varMarker_BoolFalse);
    }
};

//==============================================================================
class var::VariantType_String   : public var::VariantType
{
public:
    VariantType_String() noexcept {}
    static const VariantType_String instance;

    void cleanUp (ValueUnion& data) const noexcept override                       { getString (data)-> ~String(); }
    void createCopy (ValueUnion& dest, const ValueUnion& source) const override   { new (dest.stringValue) String (*getString (source)); }

    bool isString() const noexcept override                          { return true; }
    int toInt (const ValueUnion& data) const noexcept override       { return getString (data)->getIntValue(); }
    int64 toInt64 (const ValueUnion& data) const noexcept override   { return getString (data)->getLargeIntValue(); }
    double toDouble (const ValueUnion& data) const noexcept override { return getString (data)->getDoubleValue(); }
    String toString (const ValueUnion& data) const override          { return *getString (data); }
    bool toBool (const ValueUnion& data) const noexcept override     { return getString (data)->getIntValue() != 0
                                                                           || getString (data)->trim().equalsIgnoreCase ("true")
                                                                           || getString (data)->trim().equalsIgnoreCase ("yes"); }
    bool isComparable() const noexcept override                      { return true; }

    bool equals (const ValueUnion& data, const ValueUnion& otherData, const VariantType& otherType) const noexcept override
    {
        return otherType.toString (otherData) == *getString (data);
    }

    void writeToStream (const ValueUnion& data, OutputStream& output) const override
    {
        auto* s = getString (data);
        const size_t len = s->getNumBytesAsUTF8() + 1;
        HeapBlock<char> temp (len);
        s->copyToUTF8 (temp, len);
        output.writeCompressedInt ((int) (len + 1));
        output.writeByte (varMarker_String);
        output.write (temp, len);
    }

private:
    static inline const String* getString (const ValueUnion& data) noexcept { return reinterpret_cast<const String*> (data.stringValue); }
    static inline String* getString (ValueUnion& data) noexcept             { return reinterpret_cast<String*> (data.stringValue); }
};

//==============================================================================
class var::VariantType_Object   : public var::VariantType
{
public:
    VariantType_Object() noexcept {}
    static const VariantType_Object instance;

    void cleanUp (ValueUnion& data) const noexcept override   { if (data.objectValue != nullptr) data.objectValue->decReferenceCount(); }

    void createCopy (ValueUnion& dest, const ValueUnion& source) const override
    {
        dest.objectValue = source.objectValue;
        if (dest.objectValue != nullptr)
            dest.objectValue->incReferenceCount();
    }

    String toString (const ValueUnion& data) const override                            { return "Object 0x" + String::toHexString ((int) (pointer_sized_int) data.objectValue); }
    bool toBool (const ValueUnion& data) const noexcept override                       { return data.objectValue != nullptr; }
    ReferenceCountedObject* toObject (const ValueUnion& data) const noexcept override  { return data.objectValue; }
    bool isObject() const noexcept override                                            { return true; }

    bool equals (const ValueUnion& data, const ValueUnion& otherData, const VariantType& otherType) const noexcept override
    {
        return otherType.toObject (otherData) == data.objectValue;
    }

    var clone (const var& original) const override
    {
        if (auto* d = original.getDynamicObject())
            return d->clone().get();

        jassertfalse; // can only clone DynamicObjects!
        return {};
    }

    void writeToStream (const ValueUnion&, OutputStream& output) const override
    {
        jassertfalse; // Can't write an object to a stream!
        output.writeCompressedInt (0);
    }
};

//==============================================================================
class var::VariantType_Array   : public var::VariantType_Object
{
public:
    VariantType_Array() noexcept {}
    static const VariantType_Array instance;

    String toString (const ValueUnion&) const override                           { return "[Array]"; }
    ReferenceCountedObject* toObject (const ValueUnion&) const noexcept override { return nullptr; }
    bool isArray() const noexcept override                                       { return true; }

    Array<var>* toArray (const ValueUnion& data) const noexcept override
    {
        if (auto* a = dynamic_cast<RefCountedArray*> (data.objectValue))
            return &(a->array);

        return nullptr;
    }

    bool equals (const ValueUnion& data, const ValueUnion& otherData, const VariantType& otherType) const noexcept override
    {
        auto* thisArray = toArray (data);
        auto* otherArray = otherType.toArray (otherData);
        return thisArray == otherArray || (thisArray != nullptr && otherArray != nullptr && *otherArray == *thisArray);
    }

    var clone (const var& original) const override
    {
        Array<var> arrayCopy;

        if (auto* array = toArray (original.value))
        {
            arrayCopy.ensureStorageAllocated (array->size());

            for (auto& i : *array)
                arrayCopy.add (i.clone());
        }

        return var (arrayCopy);
    }

    void writeToStream (const ValueUnion& data, OutputStream& output) const override
    {
        if (auto* array = toArray (data))
        {
            MemoryOutputStream buffer (512);
            buffer.writeCompressedInt (array->size());

            for (auto& i : *array)
                i.writeToStream (buffer);

            output.writeCompressedInt (1 + (int) buffer.getDataSize());
            output.writeByte (varMarker_Array);
            output << buffer;
        }
    }

    struct RefCountedArray  : public ReferenceCountedObject
    {
        RefCountedArray (const Array<var>& a)  : array (a)  { incReferenceCount(); }
        RefCountedArray (Array<var>&& a)  : array (std::move (a)) { incReferenceCount(); }
        Array<var> array;
    };
};

//==============================================================================
class var::VariantType_Binary   : public var::VariantType
{
public:
    VariantType_Binary() noexcept {}

    static const VariantType_Binary instance;

    void cleanUp (ValueUnion& data) const noexcept override                      { delete data.binaryValue; }
    void createCopy (ValueUnion& dest, const ValueUnion& source) const override  { dest.binaryValue = new MemoryBlock (*source.binaryValue); }

    String toString (const ValueUnion& data) const override                      { return data.binaryValue->toBase64Encoding(); }
    bool isBinary() const noexcept override                                      { return true; }
    MemoryBlock* toBinary (const ValueUnion& data) const noexcept override       { return data.binaryValue; }

    bool equals (const ValueUnion& data, const ValueUnion& otherData, const VariantType& otherType) const noexcept override
    {
        const MemoryBlock* const otherBlock = otherType.toBinary (otherData);
        return otherBlock != nullptr && *otherBlock == *data.binaryValue;
    }

    void writeToStream (const ValueUnion& data, OutputStream& output) const override
    {
        output.writeCompressedInt (1 + (int) data.binaryValue->getSize());
        output.writeByte (varMarker_Binary);
        output << *data.binaryValue;
    }
};

//==============================================================================
class var::VariantType_Method   : public var::VariantType
{
public:
    VariantType_Method() noexcept {}
    static const VariantType_Method instance;

    void cleanUp (ValueUnion& data) const noexcept override                      { if (data.methodValue != nullptr ) delete data.methodValue; }
    void createCopy (ValueUnion& dest, const ValueUnion& source) const override  { dest.methodValue = new NativeFunction (*source.methodValue); }

    String toString (const ValueUnion&) const override               { return "Method"; }
    bool toBool (const ValueUnion& data) const noexcept override     { return data.methodValue != nullptr; }
    bool isMethod() const noexcept override                          { return true; }

    bool equals (const ValueUnion& data, const ValueUnion& otherData, const VariantType& otherType) const noexcept override
    {
        return otherType.isMethod() && otherData.methodValue == data.methodValue;
    }

    void writeToStream (const ValueUnion&, OutputStream& output) const override
    {
        jassertfalse; // Can't write a method to a stream!
        output.writeCompressedInt (0);
    }
};

//==============================================================================
const var::VariantType_Void         var::VariantType_Void::instance;
const var::VariantType_Undefined    var::VariantType_Undefined::instance;
const var::VariantType_Int          var::VariantType_Int::instance;
const var::VariantType_Int64        var::VariantType_Int64::instance;
const var::VariantType_Bool         var::VariantType_Bool::instance;
const var::VariantType_Double       var::VariantType_Double::instance;
const var::VariantType_String       var::VariantType_String::instance;
const var::VariantType_Object       var::VariantType_Object::instance;
const var::VariantType_Array        var::VariantType_Array::instance;
const var::VariantType_Binary       var::VariantType_Binary::instance;
const var::VariantType_Method       var::VariantType_Method::instance;


//==============================================================================
var::var() noexcept : type (&VariantType_Void::instance) {}
var::var (const VariantType& t) noexcept  : type (&t) {}
var::~var() noexcept  { type->cleanUp (value); }

JUCE_DECLARE_DEPRECATED_STATIC (const var var::null;)

//==============================================================================
var::var (const var& valueToCopy)  : type (valueToCopy.type)
{
    type->createCopy (value, valueToCopy.value);
}

var::var (const int v) noexcept       : type (&VariantType_Int::instance)    { value.intValue = v; }
var::var (const int64 v) noexcept     : type (&VariantType_Int64::instance)  { value.int64Value = v; }
var::var (const bool v) noexcept      : type (&VariantType_Bool::instance)   { value.boolValue = v; }
var::var (const double v) noexcept    : type (&VariantType_Double::instance) { value.doubleValue = v; }
var::var (NativeFunction m) noexcept  : type (&VariantType_Method::instance) { value.methodValue = new NativeFunction (m); }
var::var (const Array<var>& v)        : type (&VariantType_Array::instance)  { value.objectValue = new VariantType_Array::RefCountedArray(v); }
var::var (const String& v)            : type (&VariantType_String::instance) { new (value.stringValue) String (v); }
var::var (const char* const v)        : type (&VariantType_String::instance) { new (value.stringValue) String (v); }
var::var (const wchar_t* const v)     : type (&VariantType_String::instance) { new (value.stringValue) String (v); }
var::var (const void* v, size_t sz)   : type (&VariantType_Binary::instance) { value.binaryValue = new MemoryBlock (v, sz); }
var::var (const MemoryBlock& v)       : type (&VariantType_Binary::instance) { value.binaryValue = new MemoryBlock (v); }

var::var (const StringArray& v)       : type (&VariantType_Array::instance)
{
    Array<var> strings;
    strings.ensureStorageAllocated (v.size());

    for (auto& i : v)
        strings.add (var (i));

    value.objectValue = new VariantType_Array::RefCountedArray (strings);
}

var::var (ReferenceCountedObject* const object)  : type (&VariantType_Object::instance)
{
    value.objectValue = object;

    if (object != nullptr)
        object->incReferenceCount();
}

var var::undefined() noexcept           { return var (VariantType_Undefined::instance); }

//==============================================================================
bool var::isVoid() const noexcept       { return type->isVoid(); }
bool var::isUndefined() const noexcept  { return type->isUndefined(); }
bool var::isInt() const noexcept        { return type->isInt(); }
bool var::isInt64() const noexcept      { return type->isInt64(); }
bool var::isBool() const noexcept       { return type->isBool(); }
bool var::isDouble() const noexcept     { return type->isDouble(); }
bool var::isString() const noexcept     { return type->isString(); }
bool var::isObject() const noexcept     { return type->isObject(); }
bool var::isArray() const noexcept      { return type->isArray(); }
bool var::isBinaryData() const noexcept { return type->isBinary(); }
bool var::isMethod() const noexcept     { return type->isMethod(); }

var::operator int() const noexcept                      { return type->toInt (value); }
var::operator int64() const noexcept                    { return type->toInt64 (value); }
var::operator bool() const noexcept                     { return type->toBool (value); }
var::operator float() const noexcept                    { return (float) type->toDouble (value); }
var::operator double() const noexcept                   { return type->toDouble (value); }
String var::toString() const                            { return type->toString (value); }
var::operator String() const                            { return type->toString (value); }
ReferenceCountedObject* var::getObject() const noexcept { return type->toObject (value); }
Array<var>* var::getArray() const noexcept              { return type->toArray (value); }
MemoryBlock* var::getBinaryData() const noexcept        { return type->toBinary (value); }
DynamicObject* var::getDynamicObject() const noexcept   { return dynamic_cast<DynamicObject*> (getObject()); }

//==============================================================================
void var::swapWith (var& other) noexcept
{
    std::swap (type, other.type);
    std::swap (value, other.value);
}

var& var::operator= (const var& v)               { type->cleanUp (value); type = v.type; type->createCopy (value, v.value); return *this; }
var& var::operator= (const int v)                { type->cleanUp (value); type = &VariantType_Int::instance; value.intValue = v; return *this; }
var& var::operator= (const int64 v)              { type->cleanUp (value); type = &VariantType_Int64::instance; value.int64Value = v; return *this; }
var& var::operator= (const bool v)               { type->cleanUp (value); type = &VariantType_Bool::instance; value.boolValue = v; return *this; }
var& var::operator= (const double v)             { type->cleanUp (value); type = &VariantType_Double::instance; value.doubleValue = v; return *this; }
var& var::operator= (const char* const v)        { type->cleanUp (value); type = &VariantType_String::instance; new (value.stringValue) String (v); return *this; }
var& var::operator= (const wchar_t* const v)     { type->cleanUp (value); type = &VariantType_String::instance; new (value.stringValue) String (v); return *this; }
var& var::operator= (const String& v)            { type->cleanUp (value); type = &VariantType_String::instance; new (value.stringValue) String (v); return *this; }
var& var::operator= (const MemoryBlock& v)       { type->cleanUp (value); type = &VariantType_Binary::instance; value.binaryValue = new MemoryBlock (v); return *this; }
var& var::operator= (const Array<var>& v)        { var v2 (v); swapWith (v2); return *this; }
var& var::operator= (ReferenceCountedObject* v)  { var v2 (v); swapWith (v2); return *this; }
var& var::operator= (NativeFunction v)           { var v2 (v); swapWith (v2); return *this; }

var::var (var&& other) noexcept
    : type (other.type),
      value (other.value)
{
    other.type = &VariantType_Void::instance;
}

var& var::operator= (var&& other) noexcept
{
    swapWith (other);
    return *this;
}

var::var (String&& v)  : type (&VariantType_String::instance)
{
    new (value.stringValue) String (std::move (v));
}

var::var (MemoryBlock&& v)  : type (&VariantType_Binary::instance)
{
    value.binaryValue = new MemoryBlock (std::move (v));
}

var::var (Array<var>&& v)  : type (&VariantType_Array::instance)
{
    value.objectValue = new VariantType_Array::RefCountedArray (std::move (v));
}

var& var::operator= (String&& v)
{
    type->cleanUp (value);
    type = &VariantType_String::instance;
    new (value.stringValue) String (std::move (v));
    return *this;
}

//==============================================================================
bool var::equals (const var& other) const noexcept
{
    return type->equals (value, other.value, *other.type);
}

bool var::equalsWithSameType (const var& other) const noexcept
{
    return hasSameTypeAs (other) && equals (other);
}

bool var::hasSameTypeAs (const var& other) const noexcept
{
    return type == other.type;
}

bool canCompare (const var& v1, const var& v2)
{
    return v1.type->isComparable() && v2.type->isComparable();
}

static int compare (const var& v1, const var& v2)
{
    if (v1.isString() && v2.isString())
        return v1.toString().compare (v2.toString());

    auto diff = static_cast<double> (v1) - static_cast<double> (v2);
    return diff == 0 ? 0 : (diff < 0 ? -1 : 1);
}

bool operator== (const var& v1, const var& v2)     { return v1.equals (v2); }
bool operator!= (const var& v1, const var& v2)     { return ! v1.equals (v2); }
bool operator<  (const var& v1, const var& v2)     { return canCompare (v1, v2) && compare (v1, v2) <  0; }
bool operator>  (const var& v1, const var& v2)     { return canCompare (v1, v2) && compare (v1, v2) >  0; }
bool operator<= (const var& v1, const var& v2)     { return canCompare (v1, v2) && compare (v1, v2) <= 0; }
bool operator>= (const var& v1, const var& v2)     { return canCompare (v1, v2) && compare (v1, v2) >= 0; }

bool operator== (const var& v1, const String& v2)  { return v1.toString() == v2; }
bool operator!= (const var& v1, const String& v2)  { return v1.toString() != v2; }
bool operator== (const var& v1, const char* v2)    { return v1.toString() == v2; }
bool operator!= (const var& v1, const char* v2)    { return v1.toString() != v2; }

//==============================================================================
var var::clone() const noexcept
{
    return type->clone (*this);
}

//==============================================================================
const var& var::operator[] (const Identifier& propertyName) const
{
    if (auto* o = getDynamicObject())
        return o->getProperty (propertyName);

    return getNullVarRef();
}

const var& var::operator[] (const char* const propertyName) const
{
    return operator[] (Identifier (propertyName));
}

var var::getProperty (const Identifier& propertyName, const var& defaultReturnValue) const
{
    if (auto* o = getDynamicObject())
        return o->getProperties().getWithDefault (propertyName, defaultReturnValue);

    return defaultReturnValue;
}

bool var::hasProperty (const Identifier& propertyName) const noexcept
{
    if (auto* o = getDynamicObject())
        return o->hasProperty (propertyName);

    return false;
}

var::NativeFunction var::getNativeFunction() const
{
    return isMethod() && (value.methodValue != nullptr) ? *value.methodValue : nullptr;
}

var var::invoke (const Identifier& method, const var* arguments, int numArguments) const
{
    if (auto* o = getDynamicObject())
        return o->invokeMethod (method, var::NativeFunctionArgs (*this, arguments, numArguments));

    return {};
}

var var::call (const Identifier& method) const
{
    return invoke (method, nullptr, 0);
}

var var::call (const Identifier& method, const var& arg1) const
{
    return invoke (method, &arg1, 1);
}

var var::call (const Identifier& method, const var& arg1, const var& arg2) const
{
    var args[] = { arg1, arg2 };
    return invoke (method, args, 2);
}

var var::call (const Identifier& method, const var& arg1, const var& arg2, const var& arg3)
{
    var args[] = { arg1, arg2, arg3 };
    return invoke (method, args, 3);
}

var var::call (const Identifier& method, const var& arg1, const var& arg2, const var& arg3, const var& arg4) const
{
    var args[] = { arg1, arg2, arg3, arg4 };
    return invoke (method, args, 4);
}

var var::call (const Identifier& method, const var& arg1, const var& arg2, const var& arg3, const var& arg4, const var& arg5) const
{
    var args[] = { arg1, arg2, arg3, arg4, arg5 };
    return invoke (method, args, 5);
}

//==============================================================================
int var::size() const
{
    if (auto array = getArray())
        return array->size();

    return 0;
}

const var& var::operator[] (int arrayIndex) const
{
    auto array = getArray();

    // When using this method, the var must actually be an array, and the index
    // must be in-range!
    jassert (array != nullptr && isPositiveAndBelow (arrayIndex, array->size()));

    return array->getReference (arrayIndex);
}

var& var::operator[] (int arrayIndex)
{
    auto array = getArray();

    // When using this method, the var must actually be an array, and the index
    // must be in-range!
    jassert (array != nullptr && isPositiveAndBelow (arrayIndex, array->size()));

    return array->getReference (arrayIndex);
}

Array<var>* var::convertToArray()
{
    if (auto array = getArray())
        return array;

    Array<var> tempVar;

    if (! isVoid())
        tempVar.add (*this);

    *this = tempVar;
    return getArray();
}

void var::append (const var& n)
{
    convertToArray()->add (n);
}

void var::remove (const int index)
{
    if (auto array = getArray())
        array->remove (index);
}

void var::insert (const int index, const var& n)
{
    convertToArray()->insert (index, n);
}

void var::resize (const int numArrayElementsWanted)
{
    convertToArray()->resize (numArrayElementsWanted);
}

int var::indexOf (const var& n) const
{
    if (auto array = getArray())
        return array->indexOf (n);

    return -1;
}

//==============================================================================
void var::writeToStream (OutputStream& output) const
{
    type->writeToStream (value, output);
}

var var::readFromStream (InputStream& input)
{
    const int numBytes = input.readCompressedInt();

    if (numBytes > 0)
    {
        switch (input.readByte())
        {
            case varMarker_Int:         return var (input.readInt());
            case varMarker_Int64:       return var (input.readInt64());
            case varMarker_BoolTrue:    return var (true);
            case varMarker_BoolFalse:   return var (false);
            case varMarker_Double:      return var (input.readDouble());

            case varMarker_String:
            {
                MemoryOutputStream mo;
                mo.writeFromInputStream (input, numBytes - 1);
                return var (mo.toUTF8());
            }

            case varMarker_Binary:
            {
                MemoryBlock mb ((size_t) numBytes - 1);

                if (numBytes > 1)
                {
                    const int numRead = input.read (mb.getData(), numBytes - 1);
                    mb.setSize ((size_t) numRead);
                }

                return var (mb);
            }

            case varMarker_Array:
            {
                var v;
                auto* destArray = v.convertToArray();

                for (int i = input.readCompressedInt(); --i >= 0;)
                    destArray->add (readFromStream (input));

                return v;
            }

            default:
                input.skipNextBytes (numBytes - 1); break;
        }
    }

    return {};
}

var::NativeFunctionArgs::NativeFunctionArgs (const var& t, const var* args, int numArgs) noexcept
    : thisObject (t), arguments (args), numArguments (numArgs)
{
}

} // namespace juce
