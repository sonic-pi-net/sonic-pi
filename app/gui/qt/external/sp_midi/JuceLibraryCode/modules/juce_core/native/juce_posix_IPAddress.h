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

namespace
{
    static IPAddress makeAddress (const sockaddr_in6* addr_in)
    {
        if (addr_in == nullptr)
            return {};

        auto addr = addr_in->sin6_addr;

        IPAddressByteUnion temp;
        uint16 arr[8];

        for (int i = 0; i < 8; ++i) // Swap bytes from network to host order
        {
            temp.split[0] = addr.s6_addr[i * 2 + 1];
            temp.split[1] = addr.s6_addr[i * 2];

            arr[i] = temp.combined;
        }

        return IPAddress (arr);
    }

    static IPAddress makeAddress (const sockaddr_in* addr_in)
    {
        if (addr_in->sin_addr.s_addr == INADDR_NONE)
            return {};

        return IPAddress (ntohl (addr_in->sin_addr.s_addr));
    }

    struct InterfaceInfo
    {
        IPAddress interfaceAddress, broadcastAddress;
    };

    bool operator== (const InterfaceInfo& lhs, const InterfaceInfo& rhs)
    {
        return lhs.interfaceAddress == rhs.interfaceAddress
            && lhs.broadcastAddress == rhs.broadcastAddress;
    }

    bool populateInterfaceInfo (struct ifaddrs* ifa, InterfaceInfo& interfaceInfo)
    {
        if (ifa->ifa_addr != nullptr)
        {
            if (ifa->ifa_addr->sa_family == AF_INET)
            {
                auto interfaceAddressInfo = reinterpret_cast<sockaddr_in*> (ifa->ifa_addr);
                auto broadcastAddressInfo = reinterpret_cast<sockaddr_in*> (ifa->ifa_dstaddr);

                if (interfaceAddressInfo->sin_addr.s_addr != INADDR_NONE)
                {
                    interfaceInfo.interfaceAddress = makeAddress (interfaceAddressInfo);
                    interfaceInfo.broadcastAddress = makeAddress (broadcastAddressInfo);
                    return true;
                }
            }
            else if (ifa->ifa_addr->sa_family == AF_INET6)
            {
                interfaceInfo.interfaceAddress = makeAddress (reinterpret_cast<sockaddr_in6*> (ifa->ifa_addr));
                interfaceInfo.broadcastAddress = makeAddress (reinterpret_cast<sockaddr_in6*> (ifa->ifa_dstaddr));
                return true;
            }
        }

        return false;
    }

    Array<InterfaceInfo> getAllInterfaceInfo()
    {
        Array<InterfaceInfo> interfaces;
        struct ifaddrs* ifaddr = nullptr;

        if (getifaddrs (&ifaddr) != -1)
        {
            for (auto* ifa = ifaddr; ifa != nullptr; ifa = ifa->ifa_next)
            {
                InterfaceInfo i;

                if (populateInterfaceInfo (ifa, i))
                    interfaces.addIfNotAlreadyThere (i);
            }

            freeifaddrs (ifaddr);
        }

        return interfaces;
    }
}

void IPAddress::findAllAddresses (Array<IPAddress>& result, bool includeIPv6)
{
    for (auto& i : getAllInterfaceInfo())
        if (includeIPv6 || ! i.interfaceAddress.isIPv6)
            result.addIfNotAlreadyThere (i.interfaceAddress);
}

IPAddress IPAddress::getInterfaceBroadcastAddress (const IPAddress& interfaceAddress)
{
    for (auto& i : getAllInterfaceInfo())
        if (i.interfaceAddress == interfaceAddress)
            return i.broadcastAddress;

    return {};
}

} // namespace juce
