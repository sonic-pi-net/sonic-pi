//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef REMUX_ENDS_H
#define REMUX_ENDS_H

#include <algorithm>
#include <cstdint>
#include <utility>
#include <vector>

// When the remux to a standard MP4 has read everything. The source reader
// reports each stream's end on its own, and the audio and the video end at
// different times, so the read goes on until every stream it copies has
// ended. A stream that ends twice stops it too, so it cannot read for ever.
namespace SonicPi
{

class RemuxEnds
{
public:
    explicit RemuxEnds(std::vector<uint32_t> copied) : m_open(std::move(copied)) {}

    // The stream has ended. True when the read is over.
    bool ended(uint32_t stream)
    {
        if (std::find(m_ended.begin(), m_ended.end(), stream) != m_ended.end()) return true;
        m_ended.push_back(stream);
        m_open.erase(std::remove(m_open.begin(), m_open.end(), stream), m_open.end());
        return m_open.empty();
    }

private:
    std::vector<uint32_t> m_open;
    std::vector<uint32_t> m_ended;
};

} // namespace SonicPi

#endif // REMUX_ENDS_H
