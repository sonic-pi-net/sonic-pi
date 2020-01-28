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
    This class acts as a wrapper around a property inside a ValueTree.

    If the property inside the ValueTree is missing or empty the ValueWithDefault will automatically
    return a default value, which can be specified when initialising the ValueWithDefault.

    @tags{DataStructures}
*/
class ValueWithDefault
{
public:
    //==============================================================================
    /** Creates an unitialised ValueWithDefault. Initialise it using one of the referTo() methods. */
    ValueWithDefault()    : undoManager (nullptr) {}

    /** Creates an ValueWithDefault object. The default value will be an empty var. */
    ValueWithDefault (ValueTree& tree, const Identifier& propertyID, UndoManager* um)
        : targetTree (tree),
          targetProperty (propertyID),
          undoManager (um),
          defaultValue()
    {
    }

    /** Creates an ValueWithDefault object. The default value will be defaultToUse. */
    ValueWithDefault (ValueTree& tree, const Identifier& propertyID, UndoManager* um,
                      const var& defaultToUse)
        : targetTree (tree),
          targetProperty (propertyID),
          undoManager (um),
          defaultValue (defaultToUse)
    {
    }

    /** Creates an ValueWithDefault object. The default value will be defaultToUse.

        Use this constructor if the underlying var object being controlled is an array and
        it will handle the conversion to/from a delimited String that can be written to
        XML format.
    */
    ValueWithDefault (ValueTree& tree, const Identifier& propertyID, UndoManager* um,
                      const var& defaultToUse, StringRef arrayDelimiter)
        : targetTree (tree),
          targetProperty (propertyID),
          undoManager (um),
          defaultValue (defaultToUse),
          delimiter (arrayDelimiter)
    {
    }

    /** Creates a ValueWithDefault object from another ValueWithDefault object. */
    ValueWithDefault (const ValueWithDefault& other)
        : targetTree (other.targetTree),
          targetProperty (other.targetProperty),
          undoManager (other.undoManager),
          defaultValue (other.defaultValue),
          delimiter (other.delimiter)
    {
    }

    //==============================================================================
    /** Returns the current value of the property. If the property does not exist or is empty,
        returns the default value.
    */
    var get() const noexcept
    {
        if (isUsingDefault())
            return defaultValue;

        if (delimiter.isNotEmpty())
            return delimitedStringToVarArray (targetTree[targetProperty].toString());

        return targetTree[targetProperty];
    }

    /** Returns the current property as a Value object. */
    Value getPropertyAsValue()     { return targetTree.getPropertyAsValue (targetProperty, undoManager); }

    /** Returns the current default value. */
    var getDefault() const         { return defaultValue; }

    /** Sets the default value to a new var. */
    void setDefault (const var& newDefault)
    {
        if (defaultValue != newDefault)
        {
            defaultValue = newDefault;

            if (onDefaultChange != nullptr)
                onDefaultChange();
        }
    }

    /** Returns true if the property does not exist or is empty. */
    bool isUsingDefault() const
    {
        return ! targetTree.hasProperty (targetProperty);
    }

    /** Resets the property to an empty var. */
    void resetToDefault() noexcept
    {
        targetTree.removeProperty (targetProperty, nullptr);
    }

    /** You can assign a lambda to this callback object to have it called when the default value is changed. */
    std::function<void()> onDefaultChange;

    //==============================================================================
    /** Sets the property and returns the new ValueWithDefault. This will modify the property in the referenced ValueTree. */
    ValueWithDefault& operator= (const var& newValue)
    {
        setValue (newValue, undoManager);
        return *this;
    }

    /** Sets the property. This will actually modify the property in the referenced ValueTree. */
    void setValue (const var& newValue, UndoManager* undoManagerToUse)
    {
        if (auto* array = newValue.getArray())
            targetTree.setProperty (targetProperty, varArrayToDelimitedString (*array), undoManagerToUse);
        else
            targetTree.setProperty (targetProperty, newValue, undoManagerToUse);
    }

    //==============================================================================
    /** Makes the ValueWithDefault refer to the specified property inside the given ValueTree. */
    void referTo (ValueTree& tree, const Identifier& property, UndoManager* um)
    {
        referToWithDefault (tree, property, um, var(), {});
    }

    /** Makes the ValueWithDefault refer to the specified property inside the given ValueTree,
        and specifies a default value to use.
     */
    void referTo (ValueTree& tree, const Identifier& property, UndoManager* um, const var& defaultVal)
    {
        referToWithDefault (tree, property, um, defaultVal, {});
    }

    void referTo (ValueTree& tree, const Identifier& property, UndoManager* um,
                  const var& defaultVal, StringRef arrayDelimiter)
    {
        referToWithDefault (tree, property, um, defaultVal, arrayDelimiter);
    }

    //==============================================================================
    /** Returns a reference to the ValueTree containing the referenced property. */
    ValueTree& getValueTree() noexcept                      { return targetTree; }

    /** Returns the property ID of the referenced property. */
    Identifier& getPropertyID() noexcept                    { return targetProperty; }

private:
    //==============================================================================
    ValueTree targetTree;
    Identifier targetProperty;
    UndoManager* undoManager;
    var defaultValue;

    String delimiter;

    //==============================================================================
    void referToWithDefault (ValueTree& v, const Identifier& i, UndoManager* um,
                             const var& defaultVal, StringRef del)
    {
        targetTree = v;
        targetProperty = i;
        undoManager = um;
        defaultValue = defaultVal;
        delimiter = del;
    }

    //==============================================================================
    String varArrayToDelimitedString (const Array<var>& input) const noexcept
    {
        // if you are trying to control a var that is an array then you need to
        // set a delimiter string that will be used when writing to XML!
        jassert (delimiter.isNotEmpty());

        StringArray elements;
        for (auto& v : input)
            elements.add (v.toString());

        return elements.joinIntoString (delimiter);
    }

    Array<var> delimitedStringToVarArray (StringRef input) const noexcept
    {
        Array<var> arr;

        for (auto t : StringArray::fromTokens (input, delimiter, {}))
            arr.add (t);

        return arr;
    }
};

} // namespace juce
