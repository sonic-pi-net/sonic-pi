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

namespace ValueTreeSynchroniserHelpers
{
    enum ChangeType
    {
        propertyChanged  = 1,
        fullSync         = 2,
        childAdded       = 3,
        childRemoved     = 4,
        childMoved       = 5,
        propertyRemoved  = 6
    };

    static void getValueTreePath (ValueTree v, const ValueTree& topLevelTree, Array<int>& path)
    {
        while (v != topLevelTree)
        {
            ValueTree parent (v.getParent());

            if (! parent.isValid())
                break;

            path.add (parent.indexOf (v));
            v = parent;
        }
    }

    static void writeHeader (MemoryOutputStream& stream, ChangeType type)
    {
        stream.writeByte ((char) type);
    }

    static void writeHeader (ValueTreeSynchroniser& target, MemoryOutputStream& stream,
                             ChangeType type, ValueTree v)
    {
        writeHeader (stream, type);

        Array<int> path;
        getValueTreePath (v, target.getRoot(), path);

        stream.writeCompressedInt (path.size());

        for (int i = path.size(); --i >= 0;)
            stream.writeCompressedInt (path.getUnchecked(i));
    }

    static ValueTree readSubTreeLocation (MemoryInputStream& input, ValueTree v)
    {
        const int numLevels = input.readCompressedInt();

        if (! isPositiveAndBelow (numLevels, 65536)) // sanity-check
            return {};

        for (int i = numLevels; --i >= 0;)
        {
            const int index = input.readCompressedInt();

            if (! isPositiveAndBelow (index, v.getNumChildren()))
                return {};

            v = v.getChild (index);
        }

        return v;
    }
}

ValueTreeSynchroniser::ValueTreeSynchroniser (const ValueTree& tree)  : valueTree (tree)
{
    valueTree.addListener (this);
}

ValueTreeSynchroniser::~ValueTreeSynchroniser()
{
    valueTree.removeListener (this);
}

void ValueTreeSynchroniser::sendFullSyncCallback()
{
    MemoryOutputStream m;
    writeHeader (m, ValueTreeSynchroniserHelpers::fullSync);
    valueTree.writeToStream (m);
    stateChanged (m.getData(), m.getDataSize());
}

void ValueTreeSynchroniser::valueTreePropertyChanged (ValueTree& vt, const Identifier& property)
{
    MemoryOutputStream m;

    if (auto* value = vt.getPropertyPointer (property))
    {
        ValueTreeSynchroniserHelpers::writeHeader (*this, m, ValueTreeSynchroniserHelpers::propertyChanged, vt);
        m.writeString (property.toString());
        value->writeToStream (m);
    }
    else
    {
        ValueTreeSynchroniserHelpers::writeHeader (*this, m, ValueTreeSynchroniserHelpers::propertyRemoved, vt);
        m.writeString (property.toString());
    }

    stateChanged (m.getData(), m.getDataSize());
}

void ValueTreeSynchroniser::valueTreeChildAdded (ValueTree& parentTree, ValueTree& childTree)
{
    const int index = parentTree.indexOf (childTree);
    jassert (index >= 0);

    MemoryOutputStream m;
    ValueTreeSynchroniserHelpers::writeHeader (*this, m, ValueTreeSynchroniserHelpers::childAdded, parentTree);
    m.writeCompressedInt (index);
    childTree.writeToStream (m);
    stateChanged (m.getData(), m.getDataSize());
}

void ValueTreeSynchroniser::valueTreeChildRemoved (ValueTree& parentTree, ValueTree&, int oldIndex)
{
    MemoryOutputStream m;
    ValueTreeSynchroniserHelpers::writeHeader (*this, m, ValueTreeSynchroniserHelpers::childRemoved, parentTree);
    m.writeCompressedInt (oldIndex);
    stateChanged (m.getData(), m.getDataSize());
}

void ValueTreeSynchroniser::valueTreeChildOrderChanged (ValueTree& parent, int oldIndex, int newIndex)
{
    MemoryOutputStream m;
    ValueTreeSynchroniserHelpers::writeHeader (*this, m, ValueTreeSynchroniserHelpers::childMoved, parent);
    m.writeCompressedInt (oldIndex);
    m.writeCompressedInt (newIndex);
    stateChanged (m.getData(), m.getDataSize());
}

void ValueTreeSynchroniser::valueTreeParentChanged (ValueTree&)  {} // (No action needed here)

bool ValueTreeSynchroniser::applyChange (ValueTree& root, const void* data, size_t dataSize, UndoManager* undoManager)
{
    MemoryInputStream input (data, dataSize, false);

    const ValueTreeSynchroniserHelpers::ChangeType type = (ValueTreeSynchroniserHelpers::ChangeType) input.readByte();

    if (type == ValueTreeSynchroniserHelpers::fullSync)
    {
        root = ValueTree::readFromStream (input);
        return true;
    }

    ValueTree v (ValueTreeSynchroniserHelpers::readSubTreeLocation (input, root));

    if (! v.isValid())
        return false;

    switch (type)
    {
        case ValueTreeSynchroniserHelpers::propertyChanged:
        {
            Identifier property (input.readString());
            v.setProperty (property, var::readFromStream (input), undoManager);
            return true;
        }

        case ValueTreeSynchroniserHelpers::propertyRemoved:
        {
            Identifier property (input.readString());
            v.removeProperty (property, undoManager);
            return true;
        }

        case ValueTreeSynchroniserHelpers::childAdded:
        {
            const int index = input.readCompressedInt();
            v.addChild (ValueTree::readFromStream (input), index, undoManager);
            return true;
        }

        case ValueTreeSynchroniserHelpers::childRemoved:
        {
            const int index = input.readCompressedInt();

            if (isPositiveAndBelow (index, v.getNumChildren()))
            {
                v.removeChild (index, undoManager);
                return true;
            }

            jassertfalse; // Either received some corrupt data, or the trees have drifted out of sync
            break;
        }

        case ValueTreeSynchroniserHelpers::childMoved:
        {
            const int oldIndex = input.readCompressedInt();
            const int newIndex = input.readCompressedInt();

            if (isPositiveAndBelow (oldIndex, v.getNumChildren())
                 && isPositiveAndBelow (newIndex, v.getNumChildren()))
            {
                v.moveChild (oldIndex, newIndex, undoManager);
                return true;
            }

            jassertfalse; // Either received some corrupt data, or the trees have drifted out of sync
            break;
        }

        default:
            jassertfalse; // Seem to have received some corrupt data?
            break;
    }

    return false;
}

} // namespace juce
