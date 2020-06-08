/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2018 - ROLI Ltd.

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

#if JUCE_UNIT_TESTS

namespace ArrayBaseTestsHelpers
{
    class TriviallyCopyableType
    {
    public:
        TriviallyCopyableType() = default;

        TriviallyCopyableType (int v)
            : value (v)
        {}

        TriviallyCopyableType (float v)
            : value ((int) v)
        {}

        bool operator== (const TriviallyCopyableType& other) const
        {
            return getValue() == other.getValue();
        }

        int getValue() const   { return value; }

    private:
        int value { -1111 };
    };

    class NonTriviallyCopyableType
    {
    public:
        NonTriviallyCopyableType() = default;

        NonTriviallyCopyableType (int v)
            : value (v)
        {}

        NonTriviallyCopyableType (float v)
            : value ((int) v)
        {}

        NonTriviallyCopyableType (const NonTriviallyCopyableType& other)
            : value (other.value)
        {}

        NonTriviallyCopyableType& operator= (const NonTriviallyCopyableType& other)
        {
            value = other.value;
            return *this;
        }

        bool operator== (const NonTriviallyCopyableType& other) const
        {
            return getValue() == other.getValue();
        }

        int getValue() const   { return *ptr; }

    private:
        int value { -1111 };
        int* ptr = &value;
    };
}

bool operator== (const ArrayBaseTestsHelpers::TriviallyCopyableType& tct,
                 const ArrayBaseTestsHelpers::NonTriviallyCopyableType& ntct)
{
    return tct.getValue() == ntct.getValue();
}

bool operator== (const ArrayBaseTestsHelpers::NonTriviallyCopyableType& ntct,
                 const ArrayBaseTestsHelpers::TriviallyCopyableType& tct)
{
    return tct == ntct;
}

class ArrayBaseTests  : public UnitTest
{
    using CopyableType    = ArrayBaseTestsHelpers::TriviallyCopyableType;
    using NoncopyableType = ArrayBaseTestsHelpers::NonTriviallyCopyableType;

   #if ! (defined(__GNUC__) && __GNUC__ < 5 && ! defined(__clang__))
    static_assert (std::is_trivially_copyable<CopyableType>::value,
                   "Test TriviallyCopyableType is not trivially copyable");
    static_assert (! std::is_trivially_copyable<NoncopyableType>::value,
                   "Test NonTriviallyCopyableType is trivially copyable");
   #endif

public:
    ArrayBaseTests()
        : UnitTest ("ArrayBase", UnitTestCategories::containers)
    {}

    void runTest() override
    {
        beginTest ("grow capacity");
        {
            std::vector<CopyableType> referenceContainer;
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainer;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainer;

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            int originalCapacity = 4;
            referenceContainer.reserve ((size_t) originalCapacity);
            expectEquals ((int) referenceContainer.capacity(), originalCapacity);
            copyableContainer.setAllocatedSize (originalCapacity);
            expectEquals (copyableContainer.capacity(), originalCapacity);
            noncopyableContainer.setAllocatedSize (originalCapacity);
            expectEquals (noncopyableContainer.capacity(), originalCapacity);

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            addData (referenceContainer, copyableContainer, noncopyableContainer, 33);

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            expect ((int) referenceContainer.capacity() != originalCapacity);
            expect (copyableContainer.capacity()        != originalCapacity);
            expect (noncopyableContainer.capacity()     != originalCapacity);
        }

        beginTest ("shrink capacity");
        {
            std::vector<CopyableType> referenceContainer;
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainer;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainer;

            int numElements = 45;
            addData (referenceContainer, copyableContainer, noncopyableContainer, numElements);

            copyableContainer.shrinkToNoMoreThan (numElements);
            noncopyableContainer.setAllocatedSize (numElements + 1);

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            referenceContainer.clear();
            copyableContainer.removeElements    (0, numElements);
            noncopyableContainer.removeElements (0, numElements);

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            copyableContainer.setAllocatedSize    (0);
            noncopyableContainer.setAllocatedSize (0);

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            addData (referenceContainer, copyableContainer, noncopyableContainer, numElements);

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);
        }

        beginTest ("equality");
        {
            std::vector<int> referenceContainer = { 1, 2, 3 };
            ArrayBase<int, DummyCriticalSection> testContainer1, testContainer2;

            for (auto i : referenceContainer)
            {
                testContainer1.add (i);
                testContainer2.add (i);
            }

            expect (testContainer1 == referenceContainer);
            expect (testContainer2 == testContainer1);

            testContainer1.ensureAllocatedSize (257);
            referenceContainer.shrink_to_fit();

            expect (testContainer1 == referenceContainer);
            expect (testContainer2 == testContainer1);

            testContainer1.removeElements (0, 1);

            expect (testContainer1 != referenceContainer);
            expect (testContainer2 != testContainer1);
        }

        beginTest ("accessors");
        {
            std::vector<CopyableType> referenceContainer;
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainer;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainer;

            addData (referenceContainer, copyableContainer, noncopyableContainer, 3);

            int testValue = -123;
            referenceContainer[0]   = testValue;
            copyableContainer[0]    = testValue;
            noncopyableContainer[0] = testValue;

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            expect (copyableContainer   .getFirst().getValue() == testValue);
            expect (noncopyableContainer.getFirst().getValue() == testValue);

            auto last = referenceContainer.back().getValue();

            expectEquals (copyableContainer   .getLast().getValue(), last);
            expectEquals (noncopyableContainer.getLast().getValue(), last);

            ArrayBase<CopyableType,    DummyCriticalSection> copyableEmpty;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableEmpty;

            auto defualtValue = CopyableType().getValue();
            expectEquals (defualtValue, NoncopyableType().getValue());

            expectEquals (copyableEmpty   .getFirst().getValue(), defualtValue);
            expectEquals (noncopyableEmpty.getFirst().getValue(), defualtValue);
            expectEquals (copyableEmpty   .getLast() .getValue(), defualtValue);
            expectEquals (noncopyableEmpty.getLast() .getValue(), defualtValue);

            ArrayBase<float*, DummyCriticalSection> floatPointers;
            expect (floatPointers.getValueWithDefault (-3) == nullptr);
        }

        beginTest ("add moved");
        {
            std::vector<CopyableType> referenceContainer;
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainer;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainer;

            for (int i = 0; i < 5; ++i)
            {
                CopyableType ref    (-i);
                CopyableType ct     (-i);
                NoncopyableType nct (-i);
                referenceContainer.push_back (std::move (ref));
                copyableContainer.add (std::move (ct));
                noncopyableContainer.add (std::move (nct));
            }

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);
        }

        beginTest ("add multiple");
        {
            std::vector<CopyableType> referenceContainer;
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainer;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainer;

            for (int i = 4; i < 7; ++i)
                referenceContainer.push_back ({ -i });

            copyableContainer.add    (CopyableType    (-4), CopyableType    (-5), CopyableType    (-6));
            noncopyableContainer.add (NoncopyableType (-4), NoncopyableType (-5), NoncopyableType (-6));

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);
        }

        beginTest ("add array from a pointer");
        {
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainer;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainer;

            std::vector<CopyableType>    copyableData    = { 3, 4, 5 };
            std::vector<NoncopyableType> noncopyableData = { 3, 4, 5 };

            copyableContainer.addArray    (copyableData.data(),    (int) copyableData.size());
            noncopyableContainer.addArray (noncopyableData.data(), (int) noncopyableData.size());

            checkEqual (copyableContainer, noncopyableContainer, copyableData);
        }

        beginTest ("add array from a pointer of a different type");
        {
            std::vector<CopyableType> referenceContainer;
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainer;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainer;

            std::vector<float> floatData = { 1.4f, 2.5f, 3.6f };

            for (auto f : floatData)
                referenceContainer.push_back ({ f });

            copyableContainer.addArray    (floatData.data(), (int) floatData.size());
            noncopyableContainer.addArray (floatData.data(), (int) floatData.size());

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);
        }

        beginTest ("add array from initializer_list");
        {
            std::vector<CopyableType> referenceContainer;
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainer;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainer;

            std::initializer_list<CopyableType>    ilct  { { 3 }, { 4 }, { 5 } };
            std::initializer_list<NoncopyableType> ilnct { { 3 }, { 4 }, { 5 } };

            for (auto v : ilct)
                referenceContainer.push_back ({ v });

            copyableContainer.addArray    (ilct);
            noncopyableContainer.addArray (ilnct);

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);
        }

        beginTest ("add array from containers");
        {
            std::vector<CopyableType> referenceContainer;
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainer;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainer;

            addData (referenceContainer, copyableContainer, noncopyableContainer, 5);

            std::vector<CopyableType> referenceContainerCopy (referenceContainer);
            std::vector<NoncopyableType> noncopyableReferenceContainerCopy;
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainerCopy;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainerCopy;

            for (auto& v : referenceContainerCopy)
                noncopyableReferenceContainerCopy.push_back ({ v.getValue() });

            for (size_t i = 0; i < referenceContainerCopy.size(); ++i)
            {
                auto value = referenceContainerCopy[i].getValue();
                copyableContainerCopy.add    ({ value });
                noncopyableContainerCopy.add ({ value });
            }

            // From self-types
            copyableContainer.addArray    (copyableContainerCopy);
            noncopyableContainer.addArray (noncopyableContainerCopy);

            for (auto v : referenceContainerCopy)
                referenceContainer.push_back (v);

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            // From std containers
            copyableContainer.addArray    (referenceContainerCopy);
            noncopyableContainer.addArray (noncopyableReferenceContainerCopy);

            for (auto v : referenceContainerCopy)
                referenceContainer.push_back (v);

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            // From std containers with offset
            int offset = 5;
            copyableContainer.addArray    (referenceContainerCopy,            offset);
            noncopyableContainer.addArray (noncopyableReferenceContainerCopy, offset);

            for (size_t i = 5; i < referenceContainerCopy.size(); ++i)
                referenceContainer.push_back (referenceContainerCopy[i]);

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);
        }

        beginTest ("insert");
        {
            std::vector<CopyableType> referenceContainer;
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainer;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainer;

            addData (referenceContainer, copyableContainer, noncopyableContainer, 8);

            referenceContainer.insert (referenceContainer.begin(), -4);
            copyableContainer.insert    (0, -4, 1);
            noncopyableContainer.insert (0, -4, 1);

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            for (int i = 0; i < 3; ++i)
                referenceContainer.insert (referenceContainer.begin() + 1, -3);

            copyableContainer.insert    (1, -3, 3);
            noncopyableContainer.insert (1, -3, 3);

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            for (int i = 0; i < 50; ++i)
                referenceContainer.insert (referenceContainer.end() - 1, -9);

            copyableContainer.insert    (copyableContainer.size()    - 2, -9, 50);
            noncopyableContainer.insert (noncopyableContainer.size() - 2, -9, 50);
        }

        beginTest ("insert array");
        {
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainer;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainer;

            std::vector<CopyableType>    copyableData    = { 3, 4, 5, 6, 7, 8 };
            std::vector<NoncopyableType> noncopyableData = { 3, 4, 5, 6, 7, 8 };

            std::vector<CopyableType> referenceContainer { copyableData };

            copyableContainer.insertArray    (0, copyableData.data(),    (int) copyableData.size());
            noncopyableContainer.insertArray (0, noncopyableData.data(), (int) noncopyableData.size());

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            int insertPos = copyableContainer.size() - 1;

            for (auto it = copyableData.end(); it != copyableData.begin(); --it)
                referenceContainer.insert (referenceContainer.begin() + insertPos, CopyableType (*(it - 1)));

            copyableContainer.insertArray    (insertPos, copyableData.data(),    (int) copyableData.size());
            noncopyableContainer.insertArray (insertPos, noncopyableData.data(), (int) noncopyableData.size());

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);
        }

        beginTest ("remove");
        {
            std::vector<CopyableType> referenceContainer;
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainer;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainer;

            addData (referenceContainer, copyableContainer, noncopyableContainer, 17);

            for (int i = 0; i < 4; ++i)
            {
                referenceContainer.erase (referenceContainer.begin() + i);
                copyableContainer.removeElements (i, 1);
                noncopyableContainer.removeElements (i, 1);
            }

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            addData (referenceContainer, copyableContainer, noncopyableContainer, 17);
            int blockSize = 3;

            for (int i = 0; i < 4; ++i)
            {
                for (int j = 0; j < blockSize; ++j)
                    referenceContainer.erase (referenceContainer.begin() + i);

                copyableContainer.removeElements (i, blockSize);
                noncopyableContainer.removeElements (i, blockSize);
            }

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);

            auto numToRemove = copyableContainer.size() - 2;

            for (int i = 0; i < numToRemove; ++i)
                referenceContainer.erase (referenceContainer.begin() + 1);

            copyableContainer.removeElements    (1, numToRemove);
            noncopyableContainer.removeElements (1, numToRemove);

            checkEqual (copyableContainer, noncopyableContainer, referenceContainer);
        }

        beginTest ("move");
        {
            std::vector<CopyableType> referenceContainer;
            ArrayBase<CopyableType,    DummyCriticalSection> copyableContainer;
            ArrayBase<NoncopyableType, DummyCriticalSection> noncopyableContainer;

            addData (referenceContainer, copyableContainer, noncopyableContainer, 6);

            std::vector<std::pair<int, int>> testValues;
            testValues.emplace_back (2, 4);
            testValues.emplace_back (0, 5);
            testValues.emplace_back (4, 1);
            testValues.emplace_back (5, 0);

            for (auto p : testValues)
            {
                if (p.second > p.first)
                    std::rotate (referenceContainer.begin() + p.first,
                                 referenceContainer.begin() + p.first + 1,
                                 referenceContainer.begin() + p.second + 1);
                else
                    std::rotate (referenceContainer.begin() + p.second,
                                 referenceContainer.begin() + p.first,
                                 referenceContainer.begin() + p.first + 1);

                copyableContainer.move    (p.first, p.second);
                noncopyableContainer.move (p.first, p.second);

                checkEqual (copyableContainer, noncopyableContainer, referenceContainer);
            }
        }

        beginTest ("After converting move construction, ownership is transferred");
        {
            Derived obj;
            ArrayBase<Derived*, DummyCriticalSection> derived;
            derived.setAllocatedSize (5);
            derived.add (&obj);

            ArrayBase<Base*, DummyCriticalSection> base { std::move (derived) };

            expectEquals (base.capacity(), 5);
            expectEquals (base.size(), 1);
            expect (base.getFirst() == &obj);
            expectEquals (derived.capacity(), 0);
            expectEquals (derived.size(), 0);
            expect (derived.data() == nullptr);
        }

        beginTest ("After converting move assignment, ownership is transferred");
        {
            Derived obj;
            ArrayBase<Derived*, DummyCriticalSection> derived;
            derived.setAllocatedSize (5);
            derived.add (&obj);
            ArrayBase<Base*, DummyCriticalSection> base;

            base = std::move (derived);

            expectEquals (base.capacity(), 5);
            expectEquals (base.size(), 1);
            expect (base.getFirst() == &obj);
            expectEquals (derived.capacity(), 0);
            expectEquals (derived.size(), 0);
            expect (derived.data() == nullptr);
        }
    }

private:
    struct Base
    {
        virtual ~Base() = default;
    };

    struct Derived : Base
    {
    };

    static void addData (std::vector<CopyableType>& referenceContainer,
                         ArrayBase<CopyableType,    DummyCriticalSection>& copyableContainer,
                         ArrayBase<NoncopyableType, DummyCriticalSection>& NoncopyableContainer,
                         int numValues)
    {
        for (int i = 0; i < numValues; ++i)
        {
            referenceContainer.push_back ({ i });
            copyableContainer.add ({ i });
            NoncopyableContainer.add ({ i });
        }
    }

    template<typename A, typename B>
    void checkEqual (const ArrayBase<A, DummyCriticalSection>& a,
                     const ArrayBase<B, DummyCriticalSection>& b)
    {
        expectEquals ((int) a.size(), (int) b.size());

        for (int i = 0; i < (int) a.size(); ++i)
            expect (a[i] == b[i]);
    }

    template<typename A, typename B>
    void checkEqual (ArrayBase<A, DummyCriticalSection>& a,
                     std::vector<B>& b)
    {
        expectEquals ((int) a.size(), (int) b.size());

        for (int i = 0; i < (int) a.size(); ++i)
            expect (a[i] == b[(size_t) i]);
    }

    template<typename A, typename B, typename C>
    void checkEqual (ArrayBase<A, DummyCriticalSection>& a,
                     ArrayBase<B, DummyCriticalSection>& b,
                     std::vector<C>& c)
    {
        checkEqual (a, b);
        checkEqual (a, c);
        checkEqual (b, c);
    }
};

static ArrayBaseTests arrayBaseTests;

#endif

} // namespace juce
