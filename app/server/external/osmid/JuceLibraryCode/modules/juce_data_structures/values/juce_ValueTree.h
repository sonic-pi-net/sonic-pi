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
    A powerful tree structure that can be used to hold free-form data, and which can
    handle its own undo and redo behaviour.

    A ValueTree contains a list of named properties as var objects, and also holds
    any number of sub-trees.

    Create ValueTree objects on the stack, and don't be afraid to copy them around, as
    they're simply a lightweight reference to a shared data container. Creating a copy
    of another ValueTree simply creates a new reference to the same underlying object - to
    make a separate, deep copy of a tree you should explicitly call createCopy().

    Each ValueTree has a type name, in much the same way as an XmlElement has a tag name,
    and much of the structure of a ValueTree is similar to an XmlElement tree.
    You can convert a ValueTree to and from an XmlElement, and as long as the XML doesn't
    contain text elements, the conversion works well and makes a good serialisation
    format. They can also be serialised to a binary format, which is very fast and compact.

    All the methods that change data take an optional UndoManager, which will be used
    to track any changes to the object. For this to work, you have to be careful to
    consistently always use the same UndoManager for all operations to any node inside
    the tree.

    A ValueTree can only be a child of one parent at a time, so if you're moving one from
    one tree to another, be careful to always remove it first, before adding it. This
    could also mess up your undo/redo chain, so be wary! In a debug build you should hit
    assertions if you try to do anything dangerous, but there are still plenty of ways it
    could go wrong.

    Note that although the children in a tree have a fixed order, the properties are not
    guaranteed to be stored in any particular order, so don't expect that a property's index
    will correspond to the order in which the property was added, or that it will remain
    constant when other properties are added or removed.

    Listeners can be added to a ValueTree to be told when properies change and when
    nodes are added or removed.

    @see var, XmlElement
*/
class JUCE_API  ValueTree  final
{
public:
    //==============================================================================
    /** Creates an empty, invalid ValueTree.

        A ValueTree that is created with this constructor can't actually be used for anything,
        it's just a default 'null' ValueTree that can be returned to indicate some sort of failure.
        To create a real one, use the constructor that takes a string.
    */
    ValueTree() noexcept;

    /** Creates an empty ValueTree with the given type name.
        Like an XmlElement, each ValueTree node has a type, which you can access with
        getType() and hasType().
    */
    explicit ValueTree (const Identifier& type);

    /** Creates a reference to another ValueTree. */
    ValueTree (const ValueTree&) noexcept;

    /** Makes this object reference another node. */
    ValueTree& operator= (const ValueTree&);

    /** Move constructor */
    ValueTree (ValueTree&&) noexcept;

    /** Destructor. */
    ~ValueTree();

    /** Returns true if both this and the other tree node refer to the same underlying structure.
        Note that this isn't a value comparison - two independently-created trees which
        contain identical data are not considered equal.
    */
    bool operator== (const ValueTree&) const noexcept;

    /** Returns true if this and the other node refer to different underlying structures.
        Note that this isn't a value comparison - two independently-created trees which
        contain identical data are not considered equal.
    */
    bool operator!= (const ValueTree&) const noexcept;

    /** Performs a deep comparison between the properties and children of two trees.
        If all the properties and children of the two trees are the same (recursively), this
        returns true.
        The normal operator==() only checks whether two trees refer to the same shared data
        structure, so use this method if you need to do a proper value comparison.
    */
    bool isEquivalentTo (const ValueTree&) const;

    //==============================================================================
    /** Returns true if this node refers to some valid data.
        It's hard to create an invalid node, but you might get one returned, e.g. by an out-of-range
        call to getChild().
    */
    bool isValid() const noexcept                           { return object != nullptr; }

    /** Returns a deep copy of this tree and all its sub-nodes. */
    ValueTree createCopy() const;

    //==============================================================================
    /** Returns the type of this node.
        The type is specified when the ValueTree is created.
        @see hasType
    */
    Identifier getType() const noexcept;

    /** Returns true if the node has this type.
        The comparison is case-sensitive.
    */
    bool hasType (const Identifier& typeName) const noexcept;

    //==============================================================================
    /** Returns the value of a named property.
        If no such property has been set, this will return a void variant.
        You can also use operator[] to get a property.
        @see var, setProperty, getPropertyPointer, hasProperty
    */
    const var& getProperty (const Identifier& name) const noexcept;

    /** Returns the value of a named property, or the value of defaultReturnValue
        if the property doesn't exist.
        You can also use operator[] and getProperty to get a property.
        @see var, getProperty, getPropertyPointer, setProperty, hasProperty
    */
    var getProperty (const Identifier& name, const var& defaultReturnValue) const;

    /** Returns a pointer to the value of a named property, or nullptr if the property
        doesn't exist.
        @see var, getProperty, setProperty, hasProperty
    */
    const var* getPropertyPointer (const Identifier& name) const noexcept;

    /** Returns the value of a named property.
        If no such property has been set, this will return a void variant. This is the same as
        calling getProperty().
        @see getProperty
    */
    const var& operator[] (const Identifier& name) const noexcept;

    /** Changes a named property of the node.
        The name identifier must not be an empty string.
        If the undoManager parameter is non-null, its UndoManager::perform() method will be used,
        so that this change can be undone.
        @see var, getProperty, removeProperty
        @returns a reference to the value tree, so that you can daisy-chain calls to this method.
    */
    ValueTree& setProperty (const Identifier& name, const var& newValue, UndoManager* undoManager);

    /** Returns true if the node contains a named property. */
    bool hasProperty (const Identifier& name) const noexcept;

    /** Removes a property from the node.
        If the undoManager parameter is non-null, its UndoManager::perform() method will be used,
        so that this change can be undone.
    */
    void removeProperty (const Identifier& name, UndoManager* undoManager);

    /** Removes all properties from the node.
        If the undoManager parameter is non-null, its UndoManager::perform() method will be used,
        so that this change can be undone.
    */
    void removeAllProperties (UndoManager* undoManager);

    /** Returns the total number of properties that the node contains.
        @see getProperty.
    */
    int getNumProperties() const noexcept;

    /** Returns the identifier of the property with a given index.
        Note that properties are not guaranteed to be stored in any particular order, so don't
        expect that the index will correspond to the order in which the property was added, or
        that it will remain constant when other properties are added or removed.
        @see getNumProperties
    */
    Identifier getPropertyName (int index) const noexcept;

    /** Returns a Value object that can be used to control and respond to one of the tree's properties.

        The Value object will maintain a reference to this tree, and will use the undo manager when
        it needs to change the value. Attaching a Value::Listener to the value object will provide
        callbacks whenever the property changes.
    */
    Value getPropertyAsValue (const Identifier& name, UndoManager* undoManager);

    /** Overwrites all the properties in this tree with the properties of the source tree.
        Any properties that already exist will be updated; and new ones will be added, and
        any that are not present in the source tree will be removed.
    */
    void copyPropertiesFrom (const ValueTree& source, UndoManager* undoManager);

    //==============================================================================
    /** Returns the number of child nodes belonging to this one.
        @see getChild
    */
    int getNumChildren() const noexcept;

    /** Returns one of this node's child nodes.
        If the index is out of range, it'll return an invalid node. (See isValid() to find out
        whether a node is valid).
    */
    ValueTree getChild (int index) const;

    /** Returns the first child node with the specified type name.
        If no such node is found, it'll return an invalid node. (See isValid() to find out
        whether a node is valid).
        @see getOrCreateChildWithName
    */
    ValueTree getChildWithName (const Identifier& type) const;

    /** Returns the first child node with the specified type name, creating and adding
        a child with this name if there wasn't already one there.

        The only time this will return an invalid object is when the object that you're calling
        the method on is itself invalid.
        @see getChildWithName
    */
    ValueTree getOrCreateChildWithName (const Identifier& type, UndoManager* undoManager);

    /** Looks for the first child node that has the specified property value.

        This will scan the child nodes in order, until it finds one that has property that matches
        the specified value.

        If no such node is found, it'll return an invalid node. (See isValid() to find out
        whether a node is valid).
    */
    ValueTree getChildWithProperty (const Identifier& propertyName, const var& propertyValue) const;

    /** Adds a child to this node.

        Make sure that the child is removed from any former parent node before calling this, or
        you'll hit an assertion.

        If the index is < 0 or greater than the current number of child nodes, the new node will
        be added at the end of the list.

        If the undoManager parameter is non-null, its UndoManager::perform() method will be used,
        so that this change can be undone.
    */
    void addChild (const ValueTree& child, int index, UndoManager* undoManager);

    /** Removes the specified child from this node's child-list.
        If the undoManager parameter is non-null, its UndoManager::perform() method will be used,
        so that this change can be undone.
    */
    void removeChild (const ValueTree& child, UndoManager* undoManager);

    /** Removes a child from this node's child-list.
        If the undoManager parameter is non-null, its UndoManager::perform() method will be used,
        so that this change can be undone.
    */
    void removeChild (int childIndex, UndoManager* undoManager);

    /** Removes all child-nodes from this node.
        If the undoManager parameter is non-null, its UndoManager::perform() method will be used,
        so that this change can be undone.
    */
    void removeAllChildren (UndoManager* undoManager);

    /** Moves one of the children to a different index.

        This will move the child to a specified index, shuffling along any intervening
        items as required. So for example, if you have a list of { 0, 1, 2, 3, 4, 5 }, then
        calling move (2, 4) would result in { 0, 1, 3, 4, 2, 5 }.

        @param currentIndex     the index of the item to be moved. If this isn't a
                                valid index, then nothing will be done
        @param newIndex         the index at which you'd like this item to end up. If this
                                is less than zero, the value will be moved to the end
                                of the list
        @param undoManager      the optional UndoManager to use to store this transaction
    */
    void moveChild (int currentIndex, int newIndex, UndoManager* undoManager);

    /** Returns true if this node is anywhere below the specified parent node.
        This returns true if the node is a child-of-a-child, as well as a direct child.
    */
    bool isAChildOf (const ValueTree& possibleParent) const noexcept;

    /** Returns the index of a child item in this parent.
        If the child isn't found, this returns -1.
    */
    int indexOf (const ValueTree& child) const noexcept;

    /** Returns the parent node that contains this one.
        If the node has no parent, this will return an invalid node. (See isValid() to find out
        whether a node is valid).
    */
    ValueTree getParent() const noexcept;

    /** Recusrively finds the highest-level parent node that contains this one.
        If the node has no parent, this will return itself.
    */
    ValueTree getRoot() const noexcept;

    /** Returns one of this node's siblings in its parent's child list.

        The delta specifies how far to move through the list, so a value of 1 would return the node
        that follows this one, -1 would return the node before it, 0 will return this node itself, etc.
        If the requested position is beyond the range of available nodes, this will return an empty ValueTree().
    */
    ValueTree getSibling (int delta) const noexcept;

    //==============================================================================
    struct Iterator
    {
        Iterator (const ValueTree&, bool isEnd) noexcept;
        Iterator& operator++() noexcept;

        bool operator!= (const Iterator&) const noexcept;
        ValueTree operator*() const;

    private:
        void* internal;
    };

    /** Returns a start iterator for the children in this tree. */
    Iterator begin() const noexcept;

    /** Returns an end iterator for the children in this tree. */
    Iterator end() const noexcept;

    //==============================================================================
    /** Creates an XmlElement that holds a complete image of this node and all its children.

        If this node is invalid, this may return nullptr. Otherwise, the XML that is produced can
        be used to recreate a similar node by calling fromXml().

        The caller must delete the object that is returned.

        @see fromXml
    */
    XmlElement* createXml() const;

    /** Tries to recreate a node from its XML representation.

        This isn't designed to cope with random XML data - for a sensible result, it should only
        be fed XML that was created by the createXml() method.
    */
    static ValueTree fromXml (const XmlElement& xml);

    /** This returns a string containing an XML representation of the tree.
        This is quite handy for debugging purposes, as it provides a quick way to view a tree.
    */
    String toXmlString() const;

    //==============================================================================
    /** Stores this tree (and all its children) in a binary format.

        Once written, the data can be read back with readFromStream().

        It's much faster to load/save your tree in binary form than as XML, but
        obviously isn't human-readable.
    */
    void writeToStream (OutputStream& output) const;

    /** Reloads a tree from a stream that was written with writeToStream(). */
    static ValueTree readFromStream (InputStream& input);

    /** Reloads a tree from a data block that was written with writeToStream(). */
    static ValueTree readFromData (const void* data, size_t numBytes);

    /** Reloads a tree from a data block that was written with writeToStream() and
        then zipped using GZIPCompressorOutputStream.
    */
    static ValueTree readFromGZIPData (const void* data, size_t numBytes);

    //==============================================================================
    /** Listener class for events that happen to a ValueTree.

        To get events from a ValueTree, make your class implement this interface, and use
        ValueTree::addListener() and ValueTree::removeListener() to register it.
    */
    class JUCE_API  Listener
    {
    public:
        /** Destructor. */
        virtual ~Listener() {}

        /** This method is called when a property of this node (or of one of its sub-nodes) has
            changed.

            The tree parameter indicates which tree has had its property changed, and the property
            parameter indicates the property.

            Note that when you register a listener to a tree, it will receive this callback for
            property changes in that tree, and also for any of its children, (recursively, at any depth).
            If your tree has sub-trees but you only want to know about changes to the top level tree,
            simply check the tree parameter in this callback to make sure it's the tree you're interested in.
        */
        virtual void valueTreePropertyChanged (ValueTree& treeWhosePropertyHasChanged,
                                               const Identifier& property) = 0;

        /** This method is called when a child sub-tree is added.

            Note that when you register a listener to a tree, it will receive this callback for
            child changes in both that tree and any of its children, (recursively, at any depth).
            If your tree has sub-trees but you only want to know about changes to the top level tree,
            just check the parentTree parameter to make sure it's the one that you're interested in.
        */
        virtual void valueTreeChildAdded (ValueTree& parentTree,
                                          ValueTree& childWhichHasBeenAdded) = 0;

        /** This method is called when a child sub-tree is removed.

            Note that when you register a listener to a tree, it will receive this callback for
            child changes in both that tree and any of its children, (recursively, at any depth).
            If your tree has sub-trees but you only want to know about changes to the top level tree,
            just check the parentTree parameter to make sure it's the one that you're interested in.
        */
        virtual void valueTreeChildRemoved (ValueTree& parentTree,
                                            ValueTree& childWhichHasBeenRemoved,
                                            int indexFromWhichChildWasRemoved) = 0;

        /** This method is called when a tree's children have been re-shuffled.

            Note that when you register a listener to a tree, it will receive this callback for
            child changes in both that tree and any of its children, (recursively, at any depth).
            If your tree has sub-trees but you only want to know about changes to the top level tree,
            just check the parameter to make sure it's the tree that you're interested in.
        */
        virtual void valueTreeChildOrderChanged (ValueTree& parentTreeWhoseChildrenHaveMoved,
                                                 int oldIndex, int newIndex) = 0;

        /** This method is called when a tree has been added or removed from a parent node.

            This callback happens when the tree to which the listener was registered is added or
            removed from a parent. Unlike the other callbacks, it applies only to the tree to which
            the listener is registered, and not to any of its children.
        */
        virtual void valueTreeParentChanged (ValueTree& treeWhoseParentHasChanged) = 0;

        /** This method is called when a tree is made to point to a different internal shared object.
            When operator= is used to make a ValueTree refer to a different object, this callback
            will be made.
        */
        virtual void valueTreeRedirected (ValueTree& treeWhichHasBeenChanged);
    };

    /** Adds a listener to receive callbacks when this node is changed.

        The listener is added to this specific ValueTree object, and not to the shared
        object that it refers to. When this object is deleted, all the listeners will
        be lost, even if other references to the same ValueTree still exist. And if you
        use the operator= to make this refer to a different ValueTree, any listeners will
        begin listening to changes to the new tree instead of the old one.

        When you're adding a listener, make sure that you add it to a ValueTree instance that
        will last for as long as you need the listener. In general, you'd never want to add a
        listener to a local stack-based ValueTree, and would usually add one to a member variable.

        @see removeListener
    */
    void addListener (Listener* listener);

    /** Removes a listener that was previously added with addListener(). */
    void removeListener (Listener* listener);

    /** Changes a named property of the node, but will not notify a specified listener of the change.
        @see setProperty
    */
    ValueTree& setPropertyExcludingListener (Listener* listenerToExclude,
                                             const Identifier& name, const var& newValue,
                                             UndoManager* undoManager);

    /** Causes a property-change callback to be triggered for the specified property,
        calling any listeners that are registered.
    */
    void sendPropertyChangeMessage (const Identifier& property);

    //==============================================================================
    /** This method uses a comparator object to sort the tree's children into order.

        The object provided must have a method of the form:
        @code
        int compareElements (const ValueTree& first, const ValueTree& second);
        @endcode

        ..and this method must return:
          - a value of < 0 if the first comes before the second
          - a value of 0 if the two objects are equivalent
          - a value of > 0 if the second comes before the first

        To improve performance, the compareElements() method can be declared as static or const.

        @param comparator   the comparator to use for comparing elements.
        @param undoManager  optional UndoManager for storing the changes
        @param retainOrderOfEquivalentItems     if this is true, then items which the comparator says are
                            equivalent will be kept in the order in which they currently appear in the array.
                            This is slower to perform, but may be important in some cases. If it's false, a
                            faster algorithm is used, but equivalent elements may be rearranged.
    */
    template <typename ElementComparator>
    void sort (ElementComparator& comparator, UndoManager* undoManager, bool retainOrderOfEquivalentItems)
    {
        if (object != nullptr)
        {
            OwnedArray<ValueTree> sortedList;
            createListOfChildren (sortedList);
            ComparatorAdapter<ElementComparator> adapter (comparator);
            sortedList.sort (adapter, retainOrderOfEquivalentItems);
            reorderChildren (sortedList, undoManager);
        }
    }

   #if JUCE_ALLOW_STATIC_NULL_VARIABLES
    /** An invalid ValueTree that can be used if you need to return one as an error condition, etc.
        This invalid object is equivalent to ValueTree created with its default constructor, but
        you should always prefer to avoid it and use ValueTree() or {} instead.
    */
    static const ValueTree invalid;
   #endif

    /** Returns the total number of references to the shared underlying data structure that this
        ValueTree is using.
    */
    int getReferenceCount() const noexcept;

private:
    //==============================================================================
    JUCE_PUBLIC_IN_DLL_BUILD (class SharedObject)
    friend class SharedObject;

    ReferenceCountedObjectPtr<SharedObject> object;
    ListenerList<Listener> listeners;

    template <typename ElementComparator>
    struct ComparatorAdapter
    {
        ComparatorAdapter (ElementComparator& comp) noexcept : comparator (comp) {}

        int compareElements (const ValueTree* const first, const ValueTree* const second)
        {
            return comparator.compareElements (*first, *second);
        }

    private:
        ElementComparator& comparator;
        JUCE_DECLARE_NON_COPYABLE (ComparatorAdapter)
    };

    void createListOfChildren (OwnedArray<ValueTree>&) const;
    void reorderChildren (const OwnedArray<ValueTree>&, UndoManager*);

    explicit ValueTree (SharedObject*) noexcept;
};

} // namespace juce
