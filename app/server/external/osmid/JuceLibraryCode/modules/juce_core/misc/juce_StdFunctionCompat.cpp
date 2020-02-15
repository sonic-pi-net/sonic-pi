/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   Permission is granted to use this software under the terms of the ISC license
   http://www.isc.org/downloads/software-support-policy/isc-license/

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND ISC DISCLAIMS ALL WARRANTIES WITH REGARD
   TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
   FITNESS. IN NO EVENT SHALL ISC BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT,
   OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
   USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
   TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
   OF THIS SOFTWARE.

   -----------------------------------------------------------------------------

   To release a closed-source product which uses other parts of JUCE not
   licensed under the ISC terms, commercial licenses are available: visit
   www.juce.com for more information.

  ==============================================================================
*/

namespace juce
{

#if JUCE_UNIT_TESTS

namespace FunctionTestsHelpers
{
    static void incrementArgument (int& x) { x++; }
    static double multiply (double x, double a) noexcept { return a * x; }

    struct BigData
    {
        BigData()
        {
            for (auto i = 0; i < bigDataSize; ++i)
                content[i] = i + 1;
        }

        int sum() const
        {
            int result = 0;
            for (auto i = 0; i < bigDataSize; ++i)
                result += content[i];

            return result;
        }

        static const int bigDataSize = 32,
                         bigDataSum = bigDataSize * (bigDataSize + 1) / 2;
        int content[bigDataSize];
    };

    struct FunctionObject
    {
        FunctionObject() {}

        FunctionObject (const FunctionObject& other)
        {
            bigData = new BigData (*other.bigData);
        }

        int operator()(int i) const { return bigData->sum() + i; }

        ScopedPointer<BigData> bigData { new BigData() };
    };
}

class FunctionTests  : public UnitTest
{
public:
    FunctionTests() : UnitTest ("Function", "Function") {}

    void runTest() override
    {
        FunctionTestsHelpers::BigData bigData;

        {
            beginTest ("Functions");

            std::function<void(int&)> f1 (FunctionTestsHelpers::incrementArgument);

            auto x = 0;
            f1 (x);
            expectEquals (x, 1);

            std::function<double(double, double)> f2 (FunctionTestsHelpers::multiply);
            expectEquals (6.0, f2 (2.0, 3.0));

        }

        {
            beginTest ("Function objects");
            std::function<int(int)> f1 = FunctionTestsHelpers::FunctionObject();
            expectEquals (f1 (5), FunctionTestsHelpers::BigData::bigDataSum + 5);
        }

        {
            beginTest ("Lambdas");

            std::function<int()> fStack ([]() { return 3; });
            expectEquals (fStack(), 3);

            std::function<int()> fHeap ([=]() { return bigData.sum(); });
            expectEquals (fHeap(), FunctionTestsHelpers::BigData::bigDataSum);
        }

        {
            beginTest ("Boolean");

            std::function<void(int&)> f1;

            if (f1)
                expect (false);

            std::function<int()> f2 ([]() { return 3; });

            if (! f2)
                expect (false);
        }

        std::function<int()> fEmpty;

        std::function<int()> fStack ([]() { return 3; });

        std::function<int()> fHeap ([=]() { return bigData.sum(); });

        {
            beginTest ("copy constructor");

            std::function<int()> f1 (fStack);
            expectEquals (f1(), 3);

            std::function<int()> f2 (fHeap);
            expectEquals (f2(), FunctionTestsHelpers::BigData::bigDataSum);

            std::function<int()> f3 (fEmpty);
            if (f3)
                expect (false);
        }

        {
            beginTest ("assignment");

            std::function<int()> f1;
            f1 = fStack;
            expectEquals (f1(), 3);

            std::function<int()> f2;
            f2 = fHeap;
            expectEquals (f2(), FunctionTestsHelpers::BigData::bigDataSum);

            f1 = fHeap;
            expectEquals (f1(), FunctionTestsHelpers::BigData::bigDataSum);

            f2 = fStack;
            expectEquals (f2(), 3);

            f1 = fEmpty;
            if (f1)
                expect (false);
        }

        {
            beginTest ("move constructor");

            ScopedPointer<std::function<int()>> fStackTmp (new std::function<int()> (fStack));
            std::function<int()> f1 (static_cast<std::function<int()>&&> (*fStackTmp));

            fStackTmp = nullptr;
            expectEquals (f1(), 3);

            ScopedPointer<std::function<int()>> fHeapTmp (new std::function<int()> (fHeap));
            std::function<int()> f2 (static_cast<std::function<int()>&&> (*fHeapTmp));
            if (*fHeapTmp)
                expect (false);

            fHeapTmp = nullptr;
            expectEquals (f2(), FunctionTestsHelpers::BigData::bigDataSum);

            ScopedPointer<std::function<int()>> fEmptyTmp (new std::function<int()>());
            std::function<int()> f3 (static_cast<std::function<int()>&&> (*fEmptyTmp));
            fEmptyTmp = nullptr;
            if (f3)
                expect (false);
        }

        {
            beginTest ("move assignment");

            std::function<int()> f1 (fHeap);
            ScopedPointer<std::function<int()>> fStackTmp (new std::function<int()> (fStack));
            f1 = static_cast<std::function<int()>&&> (*fStackTmp);

            fStackTmp = nullptr;
            expectEquals (f1(), 3);

            std::function<int()> f2 (fStack);
            ScopedPointer<std::function<int()>> fHeapTmp (new std::function<int()> (fHeap));
            f2 = static_cast<std::function<int()>&&> (*fHeapTmp);
            if (*fHeapTmp)
                expect (false);

            fHeapTmp = nullptr;
            expectEquals (f2(), FunctionTestsHelpers::BigData::bigDataSum);

            std::function<int()> f3 (fHeap);
            ScopedPointer<std::function<int()>> fEmptyTmp (new std::function<int()>());
            f3 = static_cast<std::function<int()>&&> (*fEmptyTmp);
            fEmptyTmp = nullptr;
            if (f3)
                expect (false);
        }

        {
            beginTest ("nullptr");

            std::function<int()> f1 (nullptr);
            if (f1)
                expect (false);

            std::function<int()> f2 ([]() { return 11; });
            f2 = nullptr;
            if (f2)
                expect (false);
        }

        {
            beginTest ("Swap");

            std::function<int()> f1;
            std::function<int()> f2 (fStack);
            f2.swap (f1);
            expectEquals (f1(), 3);
            if (f2)
                expect (false);

            std::function<int()> f3 (fHeap);
            f3.swap (f1);
            expectEquals (f3(), 3);
            expectEquals (f1(), FunctionTestsHelpers::BigData::bigDataSum);
        }
    }
};

static FunctionTests functionTests;

#endif

} // namespace juce
