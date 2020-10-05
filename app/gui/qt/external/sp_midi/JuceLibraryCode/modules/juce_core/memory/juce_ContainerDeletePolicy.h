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
    Used by container classes as an indirect way to delete an object of a
    particular type.

    The generic implementation of this class simply calls 'delete', but you can
    create a specialised version of it for a particular class if you need to
    delete that type of object in a more appropriate way.

    @see OwnedArray

    @tags{Core}
*/
template <typename ObjectType>
struct ContainerDeletePolicy
{
    static void destroy (ObjectType* object)
    {
        // If the line below triggers a compiler error, it means that you are using
        // an incomplete type for ObjectType (for example, a type that is declared
        // but not defined). This is a problem because then the following delete is
        // undefined behaviour. The purpose of the sizeof is to capture this situation.
        // If this was caused by a OwnedArray of a forward-declared type, move the
        // implementation of all methods trying to use the OwnedArray (e.g. the destructor
        // of the class owning it) into cpp files where they can see to the definition
        // of ObjectType. This should fix the error.
        ignoreUnused (sizeof (ObjectType));

        delete object;
    }
};

} // namespace juce
