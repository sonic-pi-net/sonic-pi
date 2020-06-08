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

MACAddress::MACAddress() noexcept
{
    zeromem (address, sizeof (address));
}

MACAddress::MACAddress (const MACAddress& other) noexcept
{
    memcpy (address, other.address, sizeof (address));
}

MACAddress& MACAddress::operator= (const MACAddress& other) noexcept
{
    memcpy (address, other.address, sizeof (address));
    return *this;
}

MACAddress::MACAddress (const uint8 bytes[6]) noexcept
{
    memcpy (address, bytes, sizeof (address));
}

MACAddress::MACAddress (StringRef addressString)
{
    MemoryBlock hex;
    hex.loadFromHexString (addressString);

    if (hex.getSize() == sizeof (address))
        memcpy (address, hex.getData(), sizeof (address));
    else
        zeromem (address, sizeof (address));
}

String MACAddress::toString() const
{
    return toString ("-");
}

String MACAddress::toString (StringRef separator) const
{
    String s;

    for (size_t i = 0; i < sizeof (address); ++i)
    {
        s << String::toHexString ((int) address[i]).paddedLeft ('0', 2);

        if (i < sizeof (address) - 1)
            s << separator;
    }

    return s;
}

int64 MACAddress::toInt64() const noexcept
{
    int64 n = 0;

    for (int i = (int) sizeof (address); --i >= 0;)
        n = (n << 8) | address[i];

    return n;
}

Array<MACAddress> MACAddress::getAllAddresses()
{
    Array<MACAddress> addresses;
    findAllAddresses (addresses);
    return addresses;
}

bool MACAddress::isNull() const noexcept                                { return toInt64() == 0; }

bool MACAddress::operator== (const MACAddress& other) const noexcept    { return memcmp (address, other.address, sizeof (address)) == 0; }
bool MACAddress::operator!= (const MACAddress& other) const noexcept    { return ! operator== (other); }

} // namespace juce
