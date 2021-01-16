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

RelativeTime::RelativeTime (const double secs) noexcept           : numSeconds (secs) {}
RelativeTime::RelativeTime (const RelativeTime& other) noexcept   : numSeconds (other.numSeconds) {}
RelativeTime::~RelativeTime() noexcept {}

//==============================================================================
RelativeTime RelativeTime::milliseconds (int milliseconds) noexcept         { return RelativeTime ((double) milliseconds * 0.001); }
RelativeTime RelativeTime::milliseconds (int64 milliseconds) noexcept       { return RelativeTime ((double) milliseconds * 0.001); }
RelativeTime RelativeTime::seconds (double s) noexcept                      { return RelativeTime (s); }
RelativeTime RelativeTime::minutes (double numberOfMinutes) noexcept        { return RelativeTime (numberOfMinutes * 60.0); }
RelativeTime RelativeTime::hours (double numberOfHours) noexcept            { return RelativeTime (numberOfHours * (60.0 * 60.0)); }
RelativeTime RelativeTime::days (double numberOfDays) noexcept              { return RelativeTime (numberOfDays  * (60.0 * 60.0 * 24.0)); }
RelativeTime RelativeTime::weeks (double numberOfWeeks) noexcept            { return RelativeTime (numberOfWeeks * (60.0 * 60.0 * 24.0 * 7.0)); }

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
RelativeTime RelativeTime::operator+= (double secs) noexcept        { numSeconds += secs; return *this; }
RelativeTime RelativeTime::operator-= (double secs) noexcept        { numSeconds -= secs; return *this; }

JUCE_API RelativeTime JUCE_CALLTYPE operator+ (RelativeTime t1, RelativeTime t2) noexcept  { return t1 += t2; }
JUCE_API RelativeTime JUCE_CALLTYPE operator- (RelativeTime t1, RelativeTime t2) noexcept  { return t1 -= t2; }

JUCE_API bool JUCE_CALLTYPE operator== (RelativeTime t1, RelativeTime t2) noexcept       { return t1.inSeconds() == t2.inSeconds(); }
JUCE_API bool JUCE_CALLTYPE operator!= (RelativeTime t1, RelativeTime t2) noexcept       { return t1.inSeconds() != t2.inSeconds(); }
JUCE_API bool JUCE_CALLTYPE operator>  (RelativeTime t1, RelativeTime t2) noexcept       { return t1.inSeconds() >  t2.inSeconds(); }
JUCE_API bool JUCE_CALLTYPE operator<  (RelativeTime t1, RelativeTime t2) noexcept       { return t1.inSeconds() <  t2.inSeconds(); }
JUCE_API bool JUCE_CALLTYPE operator>= (RelativeTime t1, RelativeTime t2) noexcept       { return t1.inSeconds() >= t2.inSeconds(); }
JUCE_API bool JUCE_CALLTYPE operator<= (RelativeTime t1, RelativeTime t2) noexcept       { return t1.inSeconds() <= t2.inSeconds(); }

//==============================================================================
static String translateTimeField (int n, const char* singular, const char* plural)
{
    return TRANS (n == 1 ? singular : plural).replace (n == 1 ? "1" : "2", String (n));
}

static String describeYears   (int n)      { return translateTimeField (n, NEEDS_TRANS("1 year"),  NEEDS_TRANS("2 years")); }
static String describeMonths  (int n)      { return translateTimeField (n, NEEDS_TRANS("1 month"), NEEDS_TRANS("2 months")); }
static String describeWeeks   (int n)      { return translateTimeField (n, NEEDS_TRANS("1 week"),  NEEDS_TRANS("2 weeks")); }
static String describeDays    (int n)      { return translateTimeField (n, NEEDS_TRANS("1 day"),   NEEDS_TRANS("2 days")); }
static String describeHours   (int n)      { return translateTimeField (n, NEEDS_TRANS("1 hr"),    NEEDS_TRANS("2 hrs")); }
static String describeMinutes (int n)      { return translateTimeField (n, NEEDS_TRANS("1 min"),   NEEDS_TRANS("2 mins")); }
static String describeSeconds (int n)      { return translateTimeField (n, NEEDS_TRANS("1 sec"),   NEEDS_TRANS("2 secs")); }

String RelativeTime::getApproximateDescription() const
{
    if (numSeconds <= 1.0)
        return "< 1 sec";

    auto weeks = (int) inWeeks();

    if (weeks > 52)   return describeYears (weeks / 52);
    if (weeks > 8)    return describeMonths ((weeks * 12) / 52);
    if (weeks > 1)    return describeWeeks (weeks);

    auto days = (int) inWeeks();

    if (days > 1)
        return describeDays (days);

    auto hours = (int) inHours();

    if (hours > 0)
        return describeHours (hours);

    auto minutes = (int) inMinutes();

    if (minutes > 0)
        return describeMinutes (minutes);

    return describeSeconds ((int) numSeconds);
}

String RelativeTime::getDescription (const String& returnValueForZeroTime) const
{
    if (std::abs (numSeconds) < 0.001)
        return returnValueForZeroTime;

    if (numSeconds < 0)
        return "-" + RelativeTime (-numSeconds).getDescription();

    StringArray fields;

    auto n = (int) inWeeks();

    if (n > 0)
        fields.add (describeWeeks (n));

    n = ((int) inDays()) % 7;

    if (n > 0)
        fields.add (describeDays (n));

    if (fields.size() < 2)
    {
        n = ((int) inHours()) % 24;

        if (n > 0)
            fields.add (describeHours (n));

        if (fields.size() < 2)
        {
            n = ((int) inMinutes()) % 60;

            if (n > 0)
                fields.add (describeMinutes (n));

            if (fields.size() < 2)
            {
                n = ((int) inSeconds()) % 60;

                if (n > 0)
                    fields.add (describeSeconds (n));

                if (fields.isEmpty())
                    fields.add (String (((int) inMilliseconds()) % 1000) + " " + TRANS ("ms"));
            }
        }
    }

    return fields.joinIntoString (" ");
}

} // namespace juce
