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

Uuid::Uuid()
{
    Random r;

    for (size_t i = 0; i < sizeof (uuid); ++i)
        uuid[i] = (uint8) (r.nextInt (256));

    // To make it RFC 4122 compliant, need to force a few bits...
    uuid[6] = (uuid[6] & 0x0f) | 0x40;
    uuid[8] = (uuid[8] & 0x3f) | 0x80;
}

Uuid::~Uuid() noexcept {}

Uuid::Uuid (const Uuid& other) noexcept
{
    memcpy (uuid, other.uuid, sizeof (uuid));
}

Uuid& Uuid::operator= (const Uuid& other) noexcept
{
    memcpy (uuid, other.uuid, sizeof (uuid));
    return *this;
}

bool Uuid::operator== (const Uuid& other) const noexcept    { return memcmp (uuid, other.uuid, sizeof (uuid)) == 0; }
bool Uuid::operator!= (const Uuid& other) const noexcept    { return ! operator== (other); }

bool Uuid::operator<  (const Uuid& other) const noexcept    { return compare (other) < 0; }
bool Uuid::operator>  (const Uuid& other) const noexcept    { return compare (other) > 0; }
bool Uuid::operator<= (const Uuid& other) const noexcept    { return compare (other) <= 0; }
bool Uuid::operator>= (const Uuid& other) const noexcept    { return compare (other) >= 0; }

int Uuid::compare (Uuid other) const noexcept
{
    for (size_t i = 0; i < sizeof (uuid); ++i)
        if (int diff = uuid[i] - (int) other.uuid[i])
            return diff > 0 ? 1 : -1;

    return 0;
}

Uuid Uuid::null() noexcept
{
    return Uuid ((const uint8*) nullptr);
}

bool Uuid::isNull() const noexcept
{
    for (auto i : uuid)
        if (i != 0)
            return false;

    return true;
}

String Uuid::getHexRegion (int start, int length) const
{
    return String::toHexString (uuid + start, length, 0);
}

String Uuid::toString() const
{
    return getHexRegion (0, 16);
}

String Uuid::toDashedString() const
{
    return getHexRegion (0, 4)
            + "-" + getHexRegion (4, 2)
            + "-" + getHexRegion (6, 2)
            + "-" + getHexRegion (8, 2)
            + "-" + getHexRegion (10, 6);
}

Uuid::Uuid (const String& uuidString)
{
    operator= (uuidString);
}

Uuid& Uuid::operator= (const String& uuidString)
{
    MemoryBlock mb;
    mb.loadFromHexString (uuidString);
    mb.ensureSize (sizeof (uuid), true);
    mb.copyTo (uuid, 0, sizeof (uuid));
    return *this;
}

Uuid::Uuid (const uint8* const rawData) noexcept
{
    operator= (rawData);
}

Uuid& Uuid::operator= (const uint8* const rawData) noexcept
{
    if (rawData != nullptr)
        memcpy (uuid, rawData, sizeof (uuid));
    else
        zeromem (uuid, sizeof (uuid));

    return *this;
}

uint32 Uuid::getTimeLow() const noexcept                  { return ByteOrder::bigEndianInt (uuid); }
uint16 Uuid::getTimeMid() const noexcept                  { return ByteOrder::bigEndianShort (uuid + 4); }
uint16 Uuid::getTimeHighAndVersion() const noexcept       { return ByteOrder::bigEndianShort (uuid + 6); }
uint8  Uuid::getClockSeqAndReserved() const noexcept      { return uuid[8]; }
uint8  Uuid::getClockSeqLow() const noexcept              { return uuid[9]; }
uint64 Uuid::getNode() const noexcept                     { return (((uint64) ByteOrder::bigEndianShort (uuid + 10)) << 32) + ByteOrder::bigEndianInt (uuid + 12); }

uint64 Uuid::hash() const noexcept
{
    uint64 result = 0;

    for (auto n : uuid)
        result = ((uint64) 101) * result + n;

    return result;
}

} // namespace juce
