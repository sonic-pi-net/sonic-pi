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

/* This file contains a few helper functions that are used internally but which
   need to be kept away from the public headers because they use obj-C symbols.
*/
namespace juce
{

//==============================================================================
static inline String nsStringToJuce (NSString* s)
{
    return CharPointer_UTF8 ([s UTF8String]);
}

static inline NSString* juceStringToNS (const String& s)
{
    return [NSString stringWithUTF8String: s.toUTF8()];
}

static inline NSString* nsStringLiteral (const char* const s) noexcept
{
    return [NSString stringWithUTF8String: s];
}

static inline NSString* nsEmptyString() noexcept
{
    return [NSString string];
}

static inline NSURL* createNSURLFromFile (const String& f)
{
    return [NSURL fileURLWithPath: juceStringToNS (f)];
}

static inline NSURL* createNSURLFromFile (const File& f)
{
    return createNSURLFromFile (f.getFullPathName());
}

static inline NSArray* createNSArrayFromStringArray (const StringArray& strings)
{
    auto array = [[NSMutableArray alloc] init];

    for (auto string: strings)
        [array addObject:juceStringToNS (string)];

    return [array autorelease];
}

static NSArray* varArrayToNSArray (const var& varToParse);

static NSDictionary* varObjectToNSDictionary (const var& varToParse)
{
    auto dictionary = [NSMutableDictionary dictionary];

    if (varToParse.isObject())
    {
        auto* dynamicObject = varToParse.getDynamicObject();

        auto& properties = dynamicObject->getProperties();

        for (int i = 0; i < properties.size(); ++i)
        {
            auto* keyString = juceStringToNS (properties.getName (i).toString());

            const var& valueVar = properties.getValueAt (i);

            if (valueVar.isObject())
            {
                auto* valueDictionary = varObjectToNSDictionary (valueVar);

                [dictionary setObject: valueDictionary forKey: keyString];
            }
            else if (valueVar.isArray())
            {
                auto* valueArray = varArrayToNSArray (valueVar);

                [dictionary setObject: valueArray forKey: keyString];
            }
            else
            {
                auto* valueString = juceStringToNS (valueVar.toString());

                [dictionary setObject: valueString forKey: keyString];
            }
        }
    }

    return dictionary;
}

static NSArray* varArrayToNSArray (const var& varToParse)
{
    jassert (varToParse.isArray());

    if (! varToParse.isArray())
        return nil;

    const auto* varArray = varToParse.getArray();

    auto array = [NSMutableArray arrayWithCapacity: (NSUInteger) varArray->size()];

    for (const auto& aVar : *varArray)
    {
        if (aVar.isObject())
        {
            auto* valueDictionary = varObjectToNSDictionary (aVar);

            [array addObject: valueDictionary];
        }
        else if (aVar.isArray())
        {
            auto* valueArray = varArrayToNSArray (aVar);

            [array addObject: valueArray];
        }
        else
        {
            auto* valueString = juceStringToNS (aVar.toString());

            [array addObject: valueString];
        }
    }

    return array;
}

static var nsObjectToVar (NSObject* array);

static var nsDictionaryToVar (NSDictionary* dictionary)
{
    DynamicObject::Ptr dynamicObject (new DynamicObject());

    for (NSString* key in dictionary)
        dynamicObject->setProperty (nsStringToJuce (key), nsObjectToVar (dictionary[key]));

    return var (dynamicObject.get());
}

static var nsArrayToVar (NSArray* array)
{
    Array<var> resultArray;

    for (id value in array)
        resultArray.add (nsObjectToVar (value));

    return var (resultArray);
}

static var nsObjectToVar (NSObject* obj)
{
    if ([obj isKindOfClass: [NSString class]])          return nsStringToJuce ((NSString*) obj);
    else if ([obj isKindOfClass: [NSNumber class]])     return nsStringToJuce ([(NSNumber*) obj stringValue]);
    else if ([obj isKindOfClass: [NSDictionary class]]) return nsDictionaryToVar ((NSDictionary*) obj);
    else if ([obj isKindOfClass: [NSArray class]])      return nsArrayToVar ((NSArray*) obj);
    else
    {
        // Unsupported yet, add here!
        jassertfalse;
    }

    return {};
}

#if JUCE_MAC
template <typename RectangleType>
static NSRect makeNSRect (const RectangleType& r) noexcept
{
    return NSMakeRect (static_cast<CGFloat> (r.getX()),
                       static_cast<CGFloat> (r.getY()),
                       static_cast<CGFloat> (r.getWidth()),
                       static_cast<CGFloat> (r.getHeight()));
}
#endif
#if JUCE_MAC || JUCE_IOS

// This is necessary as on iOS builds, some arguments may be passed on registers
// depending on the argument type. The re-cast objc_msgSendSuper to a function
// take the same arguments as the target method.
template <typename ReturnValue, typename... Params>
static inline ReturnValue ObjCMsgSendSuper (struct objc_super* s, SEL sel, Params... params)
{
    using SuperFn = ReturnValue (*)(struct objc_super*, SEL, Params...);
    SuperFn fn = reinterpret_cast<SuperFn> (objc_msgSendSuper);
    return fn (s, sel, params...);
}

// These hacks are a workaround for newer Xcode builds which by default prevent calls to these objc functions..
typedef id (*MsgSendSuperFn) (struct objc_super*, SEL, ...);
static inline MsgSendSuperFn getMsgSendSuperFn() noexcept   { return (MsgSendSuperFn) (void*) objc_msgSendSuper; }

#if ! JUCE_IOS
typedef double (*MsgSendFPRetFn) (id, SEL op, ...);
static inline MsgSendFPRetFn getMsgSendFPRetFn() noexcept   { return (MsgSendFPRetFn) (void*) objc_msgSend_fpret; }
#endif
#endif

//==============================================================================
struct NSObjectDeleter
{
    void operator()(NSObject* object) const
    {
        [object release];
    }
};

//==============================================================================
template <typename SuperclassType>
struct ObjCClass
{
    ObjCClass (const char* nameRoot)
        : cls (objc_allocateClassPair ([SuperclassType class], getRandomisedName (nameRoot).toUTF8(), 0))
    {
    }

    ~ObjCClass()
    {
        objc_disposeClassPair (cls);
    }

    void registerClass()
    {
        objc_registerClassPair (cls);
    }

    SuperclassType* createInstance() const
    {
        return class_createInstance (cls, 0);
    }

    template <typename Type>
    void addIvar (const char* name)
    {
        BOOL b = class_addIvar (cls, name, sizeof (Type), (uint8_t) rint (log2 (sizeof (Type))), @encode (Type));
        jassert (b); ignoreUnused (b);
    }

    template <typename FunctionType>
    void addMethod (SEL selector, FunctionType callbackFn, const char* signature)
    {
        BOOL b = class_addMethod (cls, selector, (IMP) callbackFn, signature);
        jassert (b); ignoreUnused (b);
    }

    template <typename FunctionType>
    void addMethod (SEL selector, FunctionType callbackFn, const char* sig1, const char* sig2)
    {
        addMethod (selector, callbackFn, (String (sig1) + sig2).toUTF8());
    }

    template <typename FunctionType>
    void addMethod (SEL selector, FunctionType callbackFn, const char* sig1, const char* sig2, const char* sig3)
    {
        addMethod (selector, callbackFn, (String (sig1) + sig2 + sig3).toUTF8());
    }

    template <typename FunctionType>
    void addMethod (SEL selector, FunctionType callbackFn, const char* sig1, const char* sig2, const char* sig3, const char* sig4)
    {
        addMethod (selector, callbackFn, (String (sig1) + sig2 + sig3 + sig4).toUTF8());
    }

    void addProtocol (Protocol* protocol)
    {
        BOOL b = class_addProtocol (cls, protocol);
        jassert (b); ignoreUnused (b);
    }

   #if JUCE_MAC || JUCE_IOS
    static id sendSuperclassMessage (id self, SEL selector)
    {
        objc_super s = { self, [SuperclassType class] };
        return getMsgSendSuperFn() (&s, selector);
    }
   #endif

    template <typename Type>
    static Type getIvar (id self, const char* name)
    {
        void* v = nullptr;
        object_getInstanceVariable (self, name, &v);
        return static_cast<Type> (v);
    }

    Class cls;

private:
    static String getRandomisedName (const char* root)
    {
        return root + String::toHexString (juce::Random::getSystemRandom().nextInt64());
    }

    JUCE_DECLARE_NON_COPYABLE (ObjCClass)
};

//==============================================================================
#ifndef DOXYGEN
template <class JuceClass>
struct ObjCLifetimeManagedClass : public ObjCClass<NSObject>
{
    ObjCLifetimeManagedClass()
        : ObjCClass<NSObject> ("ObjCLifetimeManagedClass_")
    {
        addIvar<JuceClass*> ("cppObject");

       #pragma clang diagnostic push
       #pragma clang diagnostic ignored "-Wundeclared-selector"
        addMethod (@selector (initWithJuceObject:), initWithJuceObject, "@@:@");
       #pragma clang diagnostic pop

        addMethod (@selector (dealloc),             dealloc,            "v@:");


        registerClass();
    }

    static id initWithJuceObject (id _self, SEL, JuceClass* obj)
    {
        NSObject* self = _self;

        objc_super s = { self, [NSObject class] };
        self = ObjCMsgSendSuper<NSObject*> (&s, @selector(init));

        object_setInstanceVariable (self, "cppObject", obj);
        return self;
    }

    static void dealloc (id _self, SEL)
    {
        if (auto* obj = getIvar<JuceClass*> (_self, "cppObject"))
        {
            delete obj;
            object_setInstanceVariable (_self, "cppObject", nullptr);
        }

        objc_super s = { _self, [NSObject class] };
        ObjCMsgSendSuper<void> (&s, @selector(dealloc));
    }


    static ObjCLifetimeManagedClass objCLifetimeManagedClass;
};

template <typename Class>
ObjCLifetimeManagedClass<Class> ObjCLifetimeManagedClass<Class>::objCLifetimeManagedClass;
#endif

// this will return an NSObject which takes ownership of the JUCE instance passed-in
// This is useful to tie the life-time of a juce instance to the life-time of an NSObject
template <typename Class>
NSObject* createNSObjectFromJuceClass (Class* obj)
{
   #pragma clang diagnostic push
   #pragma clang diagnostic ignored "-Wobjc-method-access"
    return [ObjCLifetimeManagedClass<Class>::objCLifetimeManagedClass.createInstance() initWithJuceObject:obj];
   #pragma clang diagnostic pop
}

// Get the JUCE class instance that was tied to the life-time of an NSObject with the
// function above
template <typename Class>
Class* getJuceClassFromNSObject (NSObject* obj)
{
    return obj != nullptr ? ObjCLifetimeManagedClass<Class>:: template getIvar<Class*> (obj, "cppObject") : nullptr;
}

template <typename ReturnT, class Class, typename... Params>
ReturnT (^CreateObjCBlock(Class* object, ReturnT (Class::*fn)(Params...))) (Params...)
{
    __block Class* _this = object;
    __block ReturnT (Class::*_fn)(Params...) = fn;

    return [[^ReturnT (Params... params) { return (_this->*_fn) (params...); } copy] autorelease];
}

template <typename BlockType>
class ObjCBlock
{
public:
    ObjCBlock()  { block = nullptr; }
    template <typename R, class C, typename... P>
    ObjCBlock (C* _this, R (C::*fn)(P...))  : block (CreateObjCBlock (_this, fn)) {}
    ObjCBlock (BlockType b) : block ([b copy]) {}
    ObjCBlock& operator= (const BlockType& other) { if (block != nullptr) { [block release]; } block = [other copy]; return *this; }
    bool operator== (const void* ptr) const  { return ((const void*) block == ptr); }
    bool operator!= (const void* ptr) const  { return ((const void*) block != ptr); }
    ~ObjCBlock() { if (block != nullptr) [block release]; }

    operator BlockType() { return block; }

private:
    BlockType block;
};

struct ScopedCFString
{
    ScopedCFString() = default;
    ScopedCFString (String s) : cfString (s.toCFString())  {}

    ~ScopedCFString() noexcept
    {
        if (cfString != nullptr)
            CFRelease (cfString);
    }

    CFStringRef cfString = {};
};


} // namespace juce
