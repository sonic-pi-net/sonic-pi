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

//==============================================================================
/**
    Represents an IP address.
*/
class JUCE_API  IPAddress  final
{
public:
    //==============================================================================
    /** Populates a list of all the IP addresses that this machine is using. */
    static void findAllAddresses (Array<IPAddress>& results, bool includeIPv6 = false);

    //==============================================================================
    /** Creates a null address - 0.0.0.0 (IPv4) or ::, (IPv6)
        @param IPv6 if true indicates that this is an IPv6 address
    */
    IPAddress (bool IPv6 = false) noexcept;

    /** Creates an IPv4 or IPv6 address by reading 4 or 16 bytes from an array.
        @param bytes The array containing the bytes to read.
        @param IPv6 if true indicates that 16 bytes should be read instead of 4.
    */
    explicit IPAddress (const uint8 bytes[], bool IPv6 = false) noexcept;

    /** Creates an IPv6 address from an array of 8 16-bit integers
        @param bytes The array containing the bytes to read.
    */
    explicit IPAddress (const uint16 bytes[8]) noexcept;

    /** Creates an IPv4 address from 4 bytes. */
    IPAddress (uint8 address1, uint8 address2, uint8 address3, uint8 address4) noexcept;

    /** Creates an IPv6 address from 8 16-bit integers */
    IPAddress (uint16 address1, uint16 address2, uint16 address3, uint16 address4,
               uint16 address5, uint16 address6, uint16 address7, uint16 address8) noexcept;

    /** Creates an IPv4 address from a packed 32-bit integer, where the
        MSB is the first number in the address, and the LSB is the last.
    */
    explicit IPAddress (uint32 asNativeEndian32Bit) noexcept;

    /** Parses a string IP address of the form "1.2.3.4" (IPv4) or "1:2:3:4:5:6:7:8" (IPv6). */
    explicit IPAddress (const String& address);

    /** Returns a dot- or colon-separated string in the form "1.2.3.4" (IPv4) or "1:2:3:4:5:6:7:8" (IPv6). */
    String toString() const;

    /** Returns an IPv4 or IPv6 address meaning "any", equivalent to 0.0.0.0 (IPv4) or ::, (IPv6)  */
    static IPAddress any (bool IPv6 = false) noexcept;

    /** Returns an IPv4 address meaning "broadcast" (255.255.255.255) */
    static IPAddress broadcast() noexcept;

    /** Returns an IPv4 or IPv6 address meaning "localhost", equivalent to 127.0.0.1 (IPv4) or ::1 (IPv6) */
    static IPAddress local (bool IPv6 = false) noexcept;

    /** Returns a formatted version of the provided IPv6 address conforming to RFC 5952 with leading zeros suppressed,
        lower case characters, and double-colon notation used to represent contiguous 16-bit fields of zeros.

        @param unformattedAddress the IPv6 address to be formatted
    */
    static String getFormattedAddress (const String& unformattedAddress);

    bool operator== (const IPAddress& other) const noexcept;
    bool operator!= (const IPAddress& other) const noexcept;

    /** The elements of the IP address. */
    uint8 address[16];

    bool isIPv6;

private:
    /** Union used to split a 16-bit unsigned integer into 2 8-bit unsigned integers or vice-versa */
    typedef union
    {
        uint16 combined;
        uint8 split[2];
    } ByteUnion;

    /** Method used to zero the remaining bytes of the address array when creating IPv4 addresses */
    void zeroUnusedBytes()
    {
        for (int i = 4; i < 16; ++i)
            address[i] = 0;
    }
};

} // namespace juce
