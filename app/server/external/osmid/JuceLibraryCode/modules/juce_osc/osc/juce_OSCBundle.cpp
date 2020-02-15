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

OSCBundle::OSCBundle()
{
}

OSCBundle::OSCBundle (OSCTimeTag t)  : timeTag (t)
{
}

// Note: The class invariant of OSCBundle::Element is that
// at least one of the pointers bundle and message is nullptr
// and the other one always points to a valid object.

OSCBundle::Element::Element (OSCMessage m)
    : message (new OSCMessage (m)), bundle (nullptr)
{
}

OSCBundle::Element::Element (OSCBundle b)
    : message (nullptr), bundle (new OSCBundle (b))
{
}

//==============================================================================
OSCBundle::Element::Element (const Element& other)
{
    if (this != &other)
    {
        message = nullptr;
        bundle = nullptr;

        if (other.isMessage())
            message = new OSCMessage (other.getMessage());
        else
            bundle = new OSCBundle (other.getBundle());
    }
}

//==============================================================================
OSCBundle::Element::~Element()
{
    bundle = nullptr;
    message = nullptr;
}

//==============================================================================
bool OSCBundle::Element::isMessage() const noexcept
{
    return message != nullptr;
}

bool OSCBundle::Element::isBundle() const noexcept
{
    return bundle != nullptr;
}

//==============================================================================
const OSCMessage& OSCBundle::Element::getMessage() const
{
    if (message == nullptr)
    {
        // This element is not a bundle! You must check this first before accessing.
        jassertfalse;
        throw OSCInternalError ("Access error in OSC bundle element.");
    }

    return *message;
}

//==============================================================================
const OSCBundle& OSCBundle::Element::getBundle() const
{
    if (bundle == nullptr)
    {
        // This element is not a bundle! You must check this first before accessing.
        jassertfalse;
        throw OSCInternalError ("Access error in OSC bundle element.");
    }

    return *bundle;
}

//==============================================================================
#if JUCE_UNIT_TESTS

class OSCBundleTests  : public UnitTest
{
public:
    OSCBundleTests() : UnitTest ("OSCBundle class", "OSC") {}

    void runTest()
    {
        beginTest ("Construction");
        {
            OSCBundle bundle;
            expect (bundle.getTimeTag().isImmediately());
        }

        beginTest ("Construction with time tag");
        {
            Time in100Seconds = (Time (Time::currentTimeMillis()) + RelativeTime (100.0));
            OSCBundle bundle (in100Seconds);
            expect (! bundle.getTimeTag().isImmediately());
            expect (bundle.getTimeTag().toTime() == in100Seconds);
        }

        beginTest ("Usage when containing messages");
        {
            OSCBundle testBundle = generateTestBundle();
            expectBundleEqualsTestBundle (testBundle);

        }

        beginTest ("Usage when containing other bundles (recursively)");
        {
            OSCBundle complexTestBundle;
            complexTestBundle.addElement (generateTestBundle());
            complexTestBundle.addElement (OSCMessage ("/test/"));
            complexTestBundle.addElement (generateTestBundle());

            expect (complexTestBundle.size() == 3);

            OSCBundle::Element* elements = complexTestBundle.begin();

            expect (! elements[0].isMessage());
            expect (elements[0].isBundle());
            expect (elements[1].isMessage());
            expect (! elements[1].isBundle());
            expect (! elements[2].isMessage());
            expect (elements[2].isBundle());

            expectBundleEqualsTestBundle (elements[0].getBundle());
            expect (elements[1].getMessage().size() == 0);
            expect (elements[1].getMessage().getAddressPattern().toString() == "/test");
            expectBundleEqualsTestBundle (elements[2].getBundle());
        }
    }

private:

    int testInt = 127;
    float testFloat = 1.5;

    OSCBundle generateTestBundle()
    {
        OSCBundle bundle;

        OSCMessage msg1 ("/test/fader");
        msg1.addInt32 (testInt);

        OSCMessage msg2 ("/test/foo");
        msg2.addString ("bar");
        msg2.addFloat32 (testFloat);

        bundle.addElement (msg1);
        bundle.addElement (msg2);

        return bundle;
    }

    void expectBundleEqualsTestBundle (const OSCBundle& bundle)
    {
        expect (bundle.size() == 2);
        expect (bundle[0].isMessage());
        expect (! bundle[0].isBundle());
        expect (bundle[1].isMessage());
        expect (! bundle[1].isBundle());

        int numElementsCounted = 0;
        for (OSCBundle::Element* element = bundle.begin(); element != bundle.end(); ++element)
        {
            expect (element->isMessage());
            expect (! element->isBundle());
            ++numElementsCounted;
        }
        expectEquals (numElementsCounted, 2);

        OSCBundle::Element* e = bundle.begin();
        expect (e[0].getMessage().size() == 1);
        expect (e[0].getMessage().begin()->getInt32() == testInt);
        expect (e[1].getMessage().size() == 2);
        expect (e[1].getMessage()[1].getFloat32() == testFloat);
    }
};

static OSCBundleTests OSCBundleUnitTests;

//==============================================================================
class OSCBundleElementTests  : public UnitTest
{
public:
    OSCBundleElementTests() : UnitTest ("OSCBundle::Element class", "OSC") {}

    void runTest()
    {
        beginTest ("Construction from OSCMessage");
        {
            float testFloat = -0.125;
            OSCMessage msg ("/test");
            msg.addFloat32 (testFloat);

            OSCBundle::Element element (msg);

            expect (element.isMessage());
            expect (element.getMessage().size() == 1);
            expect (element.getMessage()[0].getType() == OSCTypes::float32);
            expect (element.getMessage()[0].getFloat32() == testFloat);
        }
    }
};

static OSCBundleElementTests OSCBundleElementUnitTests;

#endif // JUCE_UNIT_TESTS

} // namespace juce
