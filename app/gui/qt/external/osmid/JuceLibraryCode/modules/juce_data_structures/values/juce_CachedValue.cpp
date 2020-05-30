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

#if JUCE_UNIT_TESTS

class CachedValueTests  : public UnitTest
{
public:
    CachedValueTests() : UnitTest ("CachedValues", "Values") {}

    void runTest() override
    {
        beginTest ("default constructor");
        {
            CachedValue<String> cv;
            expect (cv.isUsingDefault());
            expect (cv.get() == String());
        }

        beginTest ("without default value");
        {
            ValueTree t ("root");
            t.setProperty ("testkey", "testvalue", nullptr);

            CachedValue<String> cv (t, "testkey", nullptr);

            expect (! cv.isUsingDefault());
            expect (cv.get() == "testvalue");

            cv.resetToDefault();

            expect (cv.isUsingDefault());
            expect (cv.get() == String());
        }

        beginTest ("with default value");
        {
            ValueTree t ("root");
            t.setProperty ("testkey", "testvalue", nullptr);

            CachedValue<String> cv (t, "testkey", nullptr, "defaultvalue");

            expect (! cv.isUsingDefault());
            expect (cv.get() == "testvalue");

            cv.resetToDefault();

            expect (cv.isUsingDefault());
            expect (cv.get() == "defaultvalue");
        }

        beginTest ("with default value (int)");
        {
            ValueTree t ("root");
            t.setProperty ("testkey", 23, nullptr);

            CachedValue<int> cv (t, "testkey", nullptr, 34);

            expect (! cv.isUsingDefault());
            expect (cv == 23);
            expectEquals (cv.get(), 23);

            cv.resetToDefault();

            expect (cv.isUsingDefault());
            expect (cv == 34);
        }

        beginTest ("with void value");
        {
            ValueTree t ("root");
            t.setProperty ("testkey", var(), nullptr);

            CachedValue<String> cv (t, "testkey", nullptr, "defaultvalue");

            expect (! cv.isUsingDefault());
            expect (cv == "");
            expectEquals (cv.get(), String());
        }

        beginTest ("with non-existent value");
        {
            ValueTree t ("root");

            CachedValue<String> cv (t, "testkey", nullptr, "defaultvalue");

            expect (cv.isUsingDefault());
            expect (cv == "defaultvalue");
            expect (cv.get() == "defaultvalue");
        }

        beginTest ("with value changing");
        {
            ValueTree t ("root");
            t.setProperty ("testkey", "oldvalue", nullptr);

            CachedValue<String> cv (t, "testkey", nullptr, "defaultvalue");
            expect (cv == "oldvalue");

            t.setProperty ("testkey", "newvalue", nullptr);
            expect (cv != "oldvalue");
            expect (cv == "newvalue");
        }

        beginTest ("set value");
        {
            ValueTree t ("root");
            t.setProperty ("testkey", 23, nullptr);

            CachedValue<int> cv (t, "testkey", nullptr, 45);
            cv = 34;

            expectEquals ((int) t["testkey"], 34);

            cv.resetToDefault();
            expect (cv == 45);
            expectEquals (cv.get(), 45);

            expect (t["testkey"] == var());
        }

        beginTest ("reset value");
        {

        }
    }
};

static CachedValueTests cachedValueTests;

#endif

} // namespace juce
