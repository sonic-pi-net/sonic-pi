/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

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

RelativeTime::RelativeTime (const double secs) noexcept           : numSeconds (secs) {}
RelativeTime::RelativeTime (const RelativeTime& other) noexcept   : numSeconds (other.numSeconds) {}
RelativeTime::~RelativeTime() noexcept {}

//==============================================================================
RelativeTime RelativeTime::milliseconds (const int milliseconds) noexcept   { return RelativeTime (milliseconds * 0.001); }
RelativeTime RelativeTime::milliseconds (const int64 milliseconds) noexcept { return RelativeTime (milliseconds * 0.001); }
RelativeTime RelativeTime::seconds (double s) noexcept                      { return RelativeTime (s); }
RelativeTime RelativeTime::minutes (const double numberOfMinutes) noexcept  { return RelativeTime (numberOfMinutes * 60.0); }
RelativeTime RelativeTime::hours (const double numberOfHours) noexcept      { return RelativeTime (numberOfHours * (60.0 * 60.0)); }
RelativeTime RelativeTime::days (const double numberOfDays) noexcept        { return RelativeTime (numberOfDays  * (60.0 * 60.0 * 24.0)); }
RelativeTime RelativeTime::weeks (const double numberOfWeeks) noexcept      { return RelativeTime (numberOfWeeks * (60.0 * 60.0 * 24.0 * 7.0)); }

//==============================================================================
int64 RelativeTime::inMilliseconds() const noexcept { return (int64) (numSeconds * 1000.0); }
double RelativeTime::inMinutes() const noexcept     { return numSeconds / 60.0; }
double RelativeTime::inHours() const noexcept       { return numSeconds / (60.0 * 60.0); }
double RelativeTime::inDays() const noexcept        { return numSeconds / (60.0 * 60.0 * 24.0); }
double RelativeTime::inWeeks() const noexcept       { return numSeconds / (60.0 * 60.0 * 24.0 * 7.0); }

//==============================================================================
RelativeTime& RelativeTime::operator= (const RelativeTime& other) noexcept      { numSeconds = other.numSeconds; return *this; }

RelativeTime RelativeTime::operator+= (RelativeTime t) noexcept     { numSeconds += t.numSeconds; return *this; }
RelativeTime RelativeTime::operator-= (RelativeTime t) noexcept     { numSeconds -= t.numSeconds; return *this; }
RelativeTime RelativeTime::operator+= (const double secs) noexcept  { numSeconds += secs; return *this; }
RelativeTime RelativeTime::operator-= (const double secs) noexcept  { numSeconds -= secs; return *this; }

JUCE_API RelativeTime JUCE_CALLTYPE operator+ (RelativeTime t1, RelativeTime t2) noexcept  { return t1 += t2; }
JUCE_API RelativeTime JUCE_CALLTYPE operator- (RelativeTime t1, RelativeTime t2) noexcept  { return t1 -= t2; }

JUCE_API bool JUCE_CALLTYPE operator== (RelativeTime t1, RelativeTime t2) noexcept       { return t1.inSeconds() == t2.inSeconds(); }
JUCE_API bool JUCE_CALLTYPE operator!= (RelativeTime t1, RelativeTime t2) noexcept       { return t1.inSeconds() != t2.inSeconds(); }
JUCE_API bool JUCE_CALLTYPE operator>  (RelativeTime t1, RelativeTime t2) noexcept       { return t1.inSeconds() >  t2.inSeconds(); }
JUCE_API bool JUCE_CALLTYPE operator<  (RelativeTime t1, RelativeTime t2) noexcept       { return t1.inSeconds() <  t2.inSeconds(); }
JUCE_API bool JUCE_CALLTYPE operator>= (RelativeTime t1, RelativeTime t2) noexcept       { return t1.inSeconds() >= t2.inSeconds(); }
JUCE_API bool JUCE_CALLTYPE operator<= (RelativeTime t1, RelativeTime t2) noexcept       { return t1.inSeconds() <= t2.inSeconds(); }

//==============================================================================
static void translateTimeField (String& result, int n, const char* singular, const char* plural)
{
    result << TRANS (n == 1 ? singular : plural)
                .replace (n == 1 ? "1" : "2", String (n))
           << ' ';
}

String RelativeTime::getDescription (const String& returnValueForZeroTime) const
{
    if (numSeconds < 0.001 && numSeconds > -0.001)
        return returnValueForZeroTime;

    String result;
    result.preallocateBytes (32);

    if (numSeconds < 0)
        result << '-';

    int fieldsShown = 0;
    int n = std::abs ((int) inWeeks());
    if (n > 0)
    {
        translateTimeField (result, n, NEEDS_TRANS("1 week"), NEEDS_TRANS("2 weeks"));
        ++fieldsShown;
    }

    n = std::abs ((int) inDays()) % 7;
    if (n > 0)
    {
        translateTimeField (result, n, NEEDS_TRANS("1 day"), NEEDS_TRANS("2 days"));
        ++fieldsShown;
    }

    if (fieldsShown < 2)
    {
        n = std::abs ((int) inHours()) % 24;
        if (n > 0)
        {
            translateTimeField (result, n, NEEDS_TRANS("1 hr"), NEEDS_TRANS("2 hrs"));
            ++fieldsShown;
        }

        if (fieldsShown < 2)
        {
            n = std::abs ((int) inMinutes()) % 60;
            if (n > 0)
            {
                translateTimeField (result, n, NEEDS_TRANS("1 min"), NEEDS_TRANS("2 mins"));
                ++fieldsShown;
            }

            if (fieldsShown < 2)
            {
                n = std::abs ((int) inSeconds()) % 60;
                if (n > 0)
                {
                    translateTimeField (result, n, NEEDS_TRANS("1 sec"), NEEDS_TRANS("2 secs"));
                    ++fieldsShown;
                }

                if (fieldsShown == 0)
                {
                    n = std::abs ((int) inMilliseconds()) % 1000;
                    if (n > 0)
                        result << n << ' ' << TRANS ("ms");
                }
            }
        }
    }

    return result.trimEnd();
}

} // namespace juce
