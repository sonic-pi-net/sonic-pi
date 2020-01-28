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

IPAddress::IPAddress (bool IPv6) noexcept : isIPv6 (IPv6)
{
    for (int i = 0; i < 16; ++i)
        address[i] = 0;
}

IPAddress::IPAddress (const uint8 bytes[], bool IPv6) noexcept : isIPv6 (IPv6)
{
    for (int i = 0; i < (isIPv6 ? 16 : 4); ++i)
        address[i] = bytes[i];

    if (! isIPv6)
        zeroUnusedBytes();
}

IPAddress::IPAddress (const uint16 bytes[8]) noexcept : isIPv6 (true)
{
    ByteUnion temp;

    for (int i = 0; i < 8; ++i)
    {
        temp.combined = bytes[i];

        address[i * 2]     = temp.split[0];
        address[i * 2 + 1] = temp.split[1];
    }
}

IPAddress::IPAddress (uint8 a0, uint8 a1, uint8 a2, uint8 a3) noexcept : isIPv6 (false)
{
    address[0] = a0;  address[1] = a1;
    address[2] = a2;  address[3] = a3;

    zeroUnusedBytes();
}

IPAddress::IPAddress (uint16 a1, uint16 a2, uint16 a3, uint16 a4,
                      uint16 a5, uint16 a6, uint16 a7, uint16 a8) noexcept : isIPv6 (true)

{
    uint16 array[8] = { a1, a2, a3, a4, a5, a6, a7, a8 };

    ByteUnion temp;

    for (int i = 0; i < 8; ++i)
    {
        temp.combined = array[i];
        address[i * 2]     = temp.split[0];
        address[i * 2 + 1] = temp.split[1];
    }
}

IPAddress::IPAddress (uint32 n) noexcept : isIPv6 (false)
{
    address[0] = (n >> 24);
    address[1] = (n >> 16) & 255;
    address[2] = (n >> 8) & 255;
    address[3] = (n & 255);

    zeroUnusedBytes();
}

IPAddress::IPAddress (const String& adr)
{
    isIPv6 = adr.contains (":");

    if (! isIPv6)
    {
        StringArray tokens;
        tokens.addTokens (adr, ".", String());

        for (int i = 0; i < 4; ++i)
            address[i] = (uint8) tokens[i].getIntValue();
    }
    else
    {
        StringArray tokens;
        tokens.addTokens (adr.removeCharacters ("[]"), ":", String());

        if (tokens.contains (StringRef())) // if :: shorthand has been used
        {
            int idx = tokens.indexOf (StringRef());
            tokens.set (idx, "0");

            while (tokens.size() < 8)
                tokens.insert (idx, "0");
        }

        for (int i = 0; i < 8; ++i)
        {
            ByteUnion temp;
            temp.combined = (uint16) CharacterFunctions::HexParser<int>::parse (tokens[i].getCharPointer());

            address[i * 2]     = temp.split[0];
            address[i * 2 + 1] = temp.split[1];
        }
    }
}

String IPAddress::toString() const
{
    if (! isIPv6)
    {
        String s ((int) address[0]);

        for (int i = 1; i < 4; ++i)
            s << '.' << (int) address[i];

        return s;
    }

    String addressString;
    ByteUnion temp;

    temp.split[0] = address[0];
    temp.split[1] = address[1];

    addressString = String (String::toHexString (temp.combined));

    for (int i = 1; i < 8; ++i)
    {
        temp.split[0] = address[i * 2];
        temp.split[1] = address[i * 2 + 1];

        addressString << ':' << String (String::toHexString (temp.combined));
    }

    return getFormattedAddress (addressString);
}

IPAddress IPAddress::any (bool IPv6) noexcept           { return IPAddress (IPv6); }
IPAddress IPAddress::broadcast() noexcept               { return IPAddress (255, 255, 255, 255); }
IPAddress IPAddress::local (bool IPv6) noexcept         { return IPv6 ? IPAddress (0, 0, 0, 0, 0, 0, 0, 1)
                                                                      : IPAddress (127, 0, 0, 1); }

String IPAddress::getFormattedAddress (const String& unformattedAddress)
{
    jassert (unformattedAddress.contains (":") && ! unformattedAddress.contains ("::")); // needs to be an unformatted IPv6 address!

    String portString = unformattedAddress.fromFirstOccurrenceOf ("]", false, true);
    String addressString = unformattedAddress.dropLastCharacters (portString.length()).removeCharacters ("[]");

    StringArray tokens;
    tokens.addTokens (addressString, ":", String());

    int numZeros = 0;
    int numZerosTemp = 0;
    bool isFirst = false;
    bool isLast = false;

    for (int i = 0; i < tokens.size(); ++i)
    {
        String t = tokens.getReference (i);

        if (t.getHexValue32() == 0x0000)
        {
            ++numZeros;

            if (i == 0)
                isFirst = true;
            else if (i == tokens.size() - 1 && numZeros > numZerosTemp)
                isLast = true;

            if (t.length() > 1)
                addressString = addressString.replace (String::repeatedString ("0", t.length()), "0");

            if (isFirst && numZerosTemp != 0 && numZeros > numZerosTemp)
                isFirst = false;
        }
        else
        {
            addressString = addressString.replace (t, t.trimCharactersAtStart ("0").toLowerCase());

            if (numZeros > 0)
            {
                if (numZeros > numZerosTemp)
                    numZerosTemp = numZeros;

                numZeros = 0;
            }
        }
    }

    if (numZerosTemp > numZeros)
        numZeros = numZerosTemp;

    if (numZeros > 1)
    {
        if (numZeros == tokens.size())
            addressString = "::,";
        else
        {
            String zeroString = isFirst ? String ("0") + String::repeatedString (":0", numZeros - 1)
                                        : String::repeatedString (":0", numZeros);

            addressString = addressString.replaceFirstOccurrenceOf (zeroString, ":");

            if (isLast)
                addressString += String (":");
        }
    }

    if (portString.isNotEmpty())
        addressString = String ("[") + addressString + String ("]") + portString;

    return addressString;
}

bool IPAddress::operator== (const IPAddress& other) const noexcept
{
    for (int i = 0; i < (isIPv6 ? 16 : 4); ++i)
        if (address[i] != other.address[i])
            return false;

    return true;

}

bool IPAddress::operator!= (const IPAddress& other) const noexcept
{
    return ! operator== (other);
}

#if (! JUCE_WINDOWS) && (! JUCE_ANDROID)
static void addAddress (const sockaddr_in* addr_in, Array<IPAddress>& result)
{
    in_addr_t addr = addr_in->sin_addr.s_addr;

    if (addr != INADDR_NONE)
        result.addIfNotAlreadyThere (IPAddress (ntohl (addr)));
}

static void addAddress (const sockaddr_in6* addr_in, Array<IPAddress>& result)
{
    in6_addr addr = addr_in->sin6_addr;

    typedef union
    {
        uint16 combined;
        uint8 split[2];
    } ByteUnion;

    ByteUnion temp;
    uint16 arr[8];

    for (int i = 0; i < 8; ++i) // Swap bytes from network to host order
    {
        temp.split[0] = addr.s6_addr[i * 2 + 1];
        temp.split[1] = addr.s6_addr[i * 2];

        arr[i] = temp.combined;
    }

    IPAddress ip (arr);
    result.addIfNotAlreadyThere (ip);
}

void IPAddress::findAllAddresses (Array<IPAddress>& result, bool includeIPv6)
{
    struct ifaddrs *ifaddr, *ifa;

    if (getifaddrs (&ifaddr) == -1)
        return;

    for (ifa = ifaddr; ifa != nullptr; ifa = ifa->ifa_next)
    {
        if (ifa->ifa_addr == nullptr)
            continue;

        if      (ifa->ifa_addr->sa_family == AF_INET)                 addAddress ((const sockaddr_in*)  ifa->ifa_addr, result);
        else if (ifa->ifa_addr->sa_family == AF_INET6 && includeIPv6) addAddress ((const sockaddr_in6*) ifa->ifa_addr, result);
    }

    freeifaddrs (ifaddr);
}
#endif

} // namespace juce
