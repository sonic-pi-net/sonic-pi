/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   JUCE is an open source library subject to commercial or open-source
   licensing.

   By using JUCE, you agree to the terms of both the JUCE 5 End-User License
   Agreement and JUCE 5 Privacy Policy (both updated and effective as of the
   27th April 2017).

   End User License Agreement: www.juce.com/juce-5-licence
   Privacy Policy: www.juce.com/juce-5-privacy-policy

   Or: You may also use this code under the terms of the GPL v3 (see
   www.gnu.org/licenses).

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

namespace juce
{

//==============================================================================
/**
    This class acts as a typed wrapper around a property inside a ValueTree.

    A CachedValue provides an easy way to read and write a ValueTree property with
    a chosen type. So for example a CachedValue<int> allows you to read or write the
    property as an int, and a CachedValue<String> lets you work with it as a String.

    It also allows efficient access to the value, by caching a copy of it in the
    type that is being used.

    You can give the CachedValue an optional UndoManager which it will use when writing
    to the underlying ValueTree.

    If the property inside the ValueTree is missing, the CachedValue will automatically
    return an optional default value, which can be specified when initialising the CachedValue.

    To create one, you can either use the constructor to attach the CachedValue to a
    ValueTree, or can create an uninitialised CachedValue with its default constructor and
    then attach it later with the referTo() methods.

    Common types like String, int, double which can be easily converted to a var should work
    out-of-the-box, but if you want to use more complex custom types, you may need to implement
    some template specialisations of VariantConverter which this class uses to convert between
    the type and the ValueTree's internal var.
*/
template <typename Type>
class CachedValue   : private ValueTree::Listener
{
public:
    //==============================================================================
    /** Default constructor.
        Creates a default CachedValue not referring to any property. To initialise the
        object, call one of the referTo() methods.
    */
    CachedValue();

    /** Constructor.

        Creates a CachedValue referring to a Value property inside a ValueTree.
        If you use this constructor, the fallback value will be a default-constructed
        instance of Type.

        @param tree          The ValueTree containing the property
        @param propertyID    The identifier of the property
        @param undoManager   The UndoManager to use when writing to the property
    */
    CachedValue (ValueTree& tree, const Identifier& propertyID,
                 UndoManager* undoManager);

    /** Constructor.

        Creates a default Cached Value referring to a Value property inside a ValueTree,
        and specifies a fallback value to use if the property does not exist.

        @param tree          The ValueTree containing the property
        @param propertyID    The identifier of the property
        @param undoManager   The UndoManager to use when writing to the property
        @param defaultToUse  The fallback default value to use.
    */
    CachedValue (ValueTree& tree, const Identifier& propertyID,
                 UndoManager* undoManager, const Type& defaultToUse);

    //==============================================================================
    /** Returns the current value of the property. If the property does not exist,
        returns the fallback default value.

        This is the same as calling get().
    */
    operator Type() const noexcept                   { return cachedValue; }

    /** Returns the current value of the property. If the property does not exist,
        returns the fallback default value.
    */
    Type get() const noexcept                        { return cachedValue; }

    /** Dereference operator. Provides direct access to the property.  */
    Type& operator*() noexcept                       { return cachedValue; }

    /** Dereference operator. Provides direct access to members of the property
        if it is of object type.
    */
    Type* operator->() noexcept                      { return &cachedValue; }

    /** Returns true if the current value of the property (or the fallback value)
        is equal to other.
    */
    template <typename OtherType>
    bool operator== (const OtherType& other) const   { return cachedValue == other; }

    /** Returns true if the current value of the property (or the fallback value)
        is not equal to other.
     */
    template <typename OtherType>
    bool operator!= (const OtherType& other) const   { return cachedValue != other; }

    //==============================================================================
    /** Returns the current property as a Value object. */
    Value getPropertyAsValue();

    /** Returns true if the current property does not exist and the CachedValue is using
        the fallback default value instead.
    */
    bool isUsingDefault() const;

    /** Returns the current fallback default value. */
    Type getDefault() const                          { return defaultValue; }

    //==============================================================================
    /** Sets the property. This will actually modify the property in the referenced ValueTree. */
    CachedValue& operator= (const Type& newValue);

    /** Sets the property. This will actually modify the property in the referenced ValueTree. */
    void setValue (const Type& newValue, UndoManager* undoManagerToUse);

    /** Removes the property from the referenced ValueTree and makes the CachedValue
        return the fallback default value instead.
     */
    void resetToDefault();

    /** Removes the property from the referenced ValueTree and makes the CachedValue
        return the fallback default value instead.
     */
    void resetToDefault (UndoManager* undoManagerToUse);

    /** Resets the fallback default value. */
    void setDefault (const Type& value)                { defaultValue = value; }

    //==============================================================================
    /** Makes the CachedValue refer to the specified property inside the given ValueTree. */
    void referTo (ValueTree& tree, const Identifier& property, UndoManager* um);

    /** Makes the CachedValue refer to the specified property inside the given ValueTree,
        and specifies a fallback value to use if the property does not exist.
    */
    void referTo (ValueTree& tree, const Identifier& property, UndoManager* um, const Type& defaultVal);

    /** Force an update in case the referenced property has been changed from elsewhere.

        Note: The CachedValue is a ValueTree::Listener and therefore will be informed of
        changes of the referenced property anyway (and update itself). But this may happen
        asynchronously. forceUpdateOfCachedValue() forces an update immediately.
    */
    void forceUpdateOfCachedValue();

    //==============================================================================
    /** Returns a reference to the ValueTree containing the referenced property. */
    ValueTree& getValueTree() noexcept                      { return targetTree; }

    /** Returns the property ID of the referenced property. */
    const Identifier& getPropertyID() const noexcept        { return targetProperty; }

private:
    //==============================================================================
    ValueTree targetTree;
    Identifier targetProperty;
    UndoManager* undoManager;
    Type defaultValue;
    Type cachedValue;

    //==============================================================================
    void referToWithDefault (ValueTree&, const Identifier&, UndoManager*, const Type&);
    Type getTypedValue() const;

    void valueTreePropertyChanged (ValueTree& changedTree, const Identifier& changedProperty) override;
    void valueTreeChildAdded (ValueTree&, ValueTree&) override {}
    void valueTreeChildRemoved (ValueTree&, ValueTree&, int) override {}
    void valueTreeChildOrderChanged (ValueTree&, int, int) override {}
    void valueTreeParentChanged (ValueTree&) override {}

    JUCE_DECLARE_NON_COPYABLE (CachedValue)
};


//==============================================================================
template <typename Type>
inline CachedValue<Type>::CachedValue()  : undoManager (nullptr) {}

template <typename Type>
inline CachedValue<Type>::CachedValue (ValueTree& v, const Identifier& i, UndoManager* um)
    : targetTree (v), targetProperty (i), undoManager (um),
      defaultValue(), cachedValue (getTypedValue())
{
    targetTree.addListener (this);
}

template <typename Type>
inline CachedValue<Type>::CachedValue (ValueTree& v, const Identifier& i, UndoManager* um, const Type& defaultToUse)
    : targetTree (v), targetProperty (i), undoManager (um),
      defaultValue (defaultToUse), cachedValue (getTypedValue())
{
    targetTree.addListener (this);
}

template <typename Type>
inline Value CachedValue<Type>::getPropertyAsValue()
{
    return targetTree.getPropertyAsValue (targetProperty, undoManager);
}

template <typename Type>
inline bool CachedValue<Type>::isUsingDefault() const
{
    return ! targetTree.hasProperty (targetProperty);
}

template <typename Type>
inline CachedValue<Type>& CachedValue<Type>::operator= (const Type& newValue)
{
    setValue (newValue, undoManager);
    return *this;
}

template <typename Type>
inline void CachedValue<Type>::setValue (const Type& newValue, UndoManager* undoManagerToUse)
{
    if (cachedValue != newValue || isUsingDefault())
    {
        cachedValue = newValue;
        targetTree.setProperty (targetProperty, VariantConverter<Type>::toVar (newValue), undoManagerToUse);
    }
}

template <typename Type>
inline void CachedValue<Type>::resetToDefault()
{
    resetToDefault (undoManager);
}

template <typename Type>
inline void CachedValue<Type>::resetToDefault (UndoManager* undoManagerToUse)
{
    targetTree.removeProperty (targetProperty, undoManagerToUse);
    forceUpdateOfCachedValue();
}

template <typename Type>
inline void CachedValue<Type>::referTo (ValueTree& v, const Identifier& i, UndoManager* um)
{
    referToWithDefault (v, i, um, Type());
}

template <typename Type>
inline void CachedValue<Type>::referTo (ValueTree& v, const Identifier& i, UndoManager* um, const Type& defaultVal)
{
    referToWithDefault (v, i, um, defaultVal);
}

template <typename Type>
inline void CachedValue<Type>::forceUpdateOfCachedValue()
{
    cachedValue = getTypedValue();
}

template <typename Type>
inline void CachedValue<Type>::referToWithDefault (ValueTree& v, const Identifier& i, UndoManager* um, const Type& defaultVal)
{
    targetTree.removeListener (this);
    targetTree = v;
    targetProperty = i;
    undoManager = um;
    defaultValue = defaultVal;
    cachedValue = getTypedValue();
    targetTree.addListener (this);
}

template <typename Type>
inline Type CachedValue<Type>::getTypedValue() const
{
    if (const var* property = targetTree.getPropertyPointer (targetProperty))
        return VariantConverter<Type>::fromVar (*property);

    return defaultValue;
}

template <typename Type>
inline void CachedValue<Type>::valueTreePropertyChanged (ValueTree& changedTree, const Identifier& changedProperty)
{
    if (changedProperty == targetProperty && targetTree == changedTree)
        forceUpdateOfCachedValue();
}

} // namespace juce
