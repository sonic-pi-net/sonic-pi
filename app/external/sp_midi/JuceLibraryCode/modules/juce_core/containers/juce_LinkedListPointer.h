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
    Helps to manipulate singly-linked lists of objects.

    For objects that are designed to contain a pointer to the subsequent item in the
    list, this class contains methods to deal with the list. To use it, the ObjectType
    class that it points to must contain a LinkedListPointer called nextListItem, e.g.

    @code
    struct MyObject
    {
        int x, y, z;

        // A linkable object must contain a member with this name and type, which must be
        // accessible by the LinkedListPointer class. (This doesn't mean it has to be public -
        // you could make your class a friend of a LinkedListPointer<MyObject> instead).
        LinkedListPointer<MyObject> nextListItem;
    };

    LinkedListPointer<MyObject> myList;
    myList.append (new MyObject());
    myList.append (new MyObject());

    int numItems = myList.size(); // returns 2
    MyObject* lastInList = myList.getLast();
    @endcode

    @tags{Core}
*/
template <class ObjectType>
class LinkedListPointer
{
public:
    //==============================================================================
    /** Creates a null pointer to an empty list. */
    LinkedListPointer() noexcept
        : item (nullptr)
    {
    }

    /** Creates a pointer to a list whose head is the item provided. */
    explicit LinkedListPointer (ObjectType* const headItem) noexcept
        : item (headItem)
    {
    }

    /** Sets this pointer to point to a new list. */
    LinkedListPointer& operator= (ObjectType* const newItem) noexcept
    {
        item = newItem;
        return *this;
    }

    LinkedListPointer (LinkedListPointer&& other) noexcept
        : item (other.item)
    {
        other.item = nullptr;
    }

    LinkedListPointer& operator= (LinkedListPointer&& other) noexcept
    {
        jassert (this != &other); // hopefully the compiler should make this situation impossible!

        item = other.item;
        other.item = nullptr;
        return *this;
    }

    //==============================================================================
    /** Returns the item which this pointer points to. */
    inline operator ObjectType*() const noexcept
    {
        return item;
    }

    /** Returns the item which this pointer points to. */
    inline ObjectType* get() const noexcept
    {
        return item;
    }

    /** Returns the last item in the list which this pointer points to.
        This will iterate the list and return the last item found. Obviously the speed
        of this operation will be proportional to the size of the list. If the list is
        empty the return value will be this object.
        If you're planning on appending a number of items to your list, it's much more
        efficient to use the Appender class than to repeatedly call getLast() to find the end.
    */
    LinkedListPointer& getLast() noexcept
    {
        auto* l = this;

        while (l->item != nullptr)
            l = &(l->item->nextListItem);

        return *l;
    }

    /** Returns the number of items in the list.
        Obviously with a simple linked list, getting the size involves iterating the list, so
        this can be a lengthy operation - be careful when using this method in your code.
    */
    int size() const noexcept
    {
        int total = 0;

        for (auto* i = item; i != nullptr; i = i->nextListItem)
            ++total;

        return total;
    }

    /** Returns the item at a given index in the list.
        Since the only way to find an item is to iterate the list, this operation can obviously
        be slow, depending on its size, so you should be careful when using this in algorithms.
    */
    LinkedListPointer& operator[] (int index) noexcept
    {
        auto* l = this;

        while (--index >= 0 && l->item != nullptr)
            l = &(l->item->nextListItem);

        return *l;
    }

    /** Returns the item at a given index in the list.
        Since the only way to find an item is to iterate the list, this operation can obviously
        be slow, depending on its size, so you should be careful when using this in algorithms.
    */
    const LinkedListPointer& operator[] (int index) const noexcept
    {
        auto* l = this;

        while (--index >= 0 && l->item != nullptr)
            l = &(l->item->nextListItem);

        return *l;
    }

    /** Returns true if the list contains the given item. */
    bool contains (const ObjectType* const itemToLookFor) const noexcept
    {
        for (auto* i = item; i != nullptr; i = i->nextListItem)
            if (itemToLookFor == i)
                return true;

        return false;
    }

    //==============================================================================
    /** Inserts an item into the list, placing it before the item that this pointer
        currently points to.
    */
    void insertNext (ObjectType* const newItem)
    {
        jassert (newItem != nullptr);
        jassert (newItem->nextListItem == nullptr);
        newItem->nextListItem = item;
        item = newItem;
    }

    /** Inserts an item at a numeric index in the list.
        Obviously this will involve iterating the list to find the item at the given index,
        so be careful about the impact this may have on execution time.
    */
    void insertAtIndex (int index, ObjectType* newItem)
    {
        jassert (newItem != nullptr);
        auto* l = this;

        while (index != 0 && l->item != nullptr)
        {
            l = &(l->item->nextListItem);
            --index;
        }

        l->insertNext (newItem);
    }

    /** Replaces the object that this pointer points to, appending the rest of the list to
        the new object, and returning the old one.
    */
    ObjectType* replaceNext (ObjectType* const newItem) noexcept
    {
        jassert (newItem != nullptr);
        jassert (newItem->nextListItem == nullptr);

        auto oldItem = item;
        item = newItem;
        item->nextListItem = oldItem->nextListItem.item;
        oldItem->nextListItem.item = nullptr;
        return oldItem;
    }

    /** Adds an item to the end of the list.

        This operation involves iterating the whole list, so can be slow - if you need to
        append a number of items to your list, it's much more efficient to use the Appender
        class than to repeatedly call append().
    */
    void append (ObjectType* const newItem)
    {
        getLast().item = newItem;
    }

    /** Creates copies of all the items in another list and adds them to this one.
        This will use the ObjectType's copy constructor to try to create copies of each
        item in the other list, and appends them to this list.
    */
    void addCopyOfList (const LinkedListPointer& other)
    {
        auto* insertPoint = this;

        for (auto* i = other.item; i != nullptr; i = i->nextListItem)
        {
            insertPoint->insertNext (new ObjectType (*i));
            insertPoint = &(insertPoint->item->nextListItem);
        }
    }

    /** Removes the head item from the list.
        This won't delete the object that is removed, but returns it, so the caller can
        delete it if necessary.
    */
    ObjectType* removeNext() noexcept
    {
        auto oldItem = item;

        if (oldItem != nullptr)
        {
            item = oldItem->nextListItem;
            oldItem->nextListItem.item = nullptr;
        }

        return oldItem;
    }

    /** Removes a specific item from the list.
        Note that this will not delete the item, it simply unlinks it from the list.
    */
    void remove (ObjectType* const itemToRemove)
    {
        if (auto* l = findPointerTo (itemToRemove))
            l->removeNext();
    }

    /** Iterates the list, calling the delete operator on all of its elements and
        leaving this pointer empty.
    */
    void deleteAll()
    {
        while (item != nullptr)
        {
            auto oldItem = item;
            item = oldItem->nextListItem;
            delete oldItem;
        }
    }

    /** Finds a pointer to a given item.
        If the item is found in the list, this returns the pointer that points to it. If
        the item isn't found, this returns null.
    */
    LinkedListPointer* findPointerTo (ObjectType* const itemToLookFor) noexcept
    {
        auto* l = this;

        while (l->item != nullptr)
        {
            if (l->item == itemToLookFor)
                return l;

            l = &(l->item->nextListItem);
        }

        return nullptr;
    }

    /** Copies the items in the list to an array.
        The destArray must contain enough elements to hold the entire list - no checks are
        made for this!
    */
    void copyToArray (ObjectType** destArray) const noexcept
    {
        jassert (destArray != nullptr);

        for (auto* i = item; i != nullptr; i = i->nextListItem)
            *destArray++ = i;
    }

    /** Swaps this pointer with another one */
    void swapWith (LinkedListPointer& other) noexcept
    {
        std::swap (item, other.item);
    }

    //==============================================================================
    /**
        Allows efficient repeated insertions into a list.

        You can create an Appender object which points to the last element in your
        list, and then repeatedly call Appender::append() to add items to the end
        of the list in O(1) time.
    */
    class Appender
    {
    public:
        /** Creates an appender which will add items to the given list.
        */
        Appender (LinkedListPointer& endOfListPointer) noexcept
            : endOfList (&endOfListPointer)
        {
            // This can only be used to add to the end of a list.
            jassert (endOfListPointer.item == nullptr);
        }

        /** Appends an item to the list. */
        void append (ObjectType* const newItem) noexcept
        {
            *endOfList = newItem;
            endOfList = &(newItem->nextListItem);
        }

    private:
        LinkedListPointer* endOfList;

        JUCE_DECLARE_NON_COPYABLE (Appender)
    };

private:
    //==============================================================================
    ObjectType* item;

    JUCE_DECLARE_NON_COPYABLE (LinkedListPointer)
};

} // namespace juce
