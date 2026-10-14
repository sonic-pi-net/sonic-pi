//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "sharelink.h"

#include "qrcodegen/qrcodegen.hpp"

namespace SonicPi::ShareLink
{

using qrcodegen::QrCode;
using qrcodegen::QrSegment;

Qr qrCode(std::string_view link)
{
    // a digits link: what comes before the digits as bytes, the digits as digits
    std::vector<QrSegment> segments;
    const size_t at = link.find("#code=N");
    const size_t digits = at == std::string_view::npos ? std::string_view::npos : at + 8;
    if (digits != std::string_view::npos && digits < link.size()
        && link.find_first_not_of("0123456789", digits) == std::string_view::npos)
    {
        segments.push_back(QrSegment::makeBytes(std::vector<std::uint8_t>(link.begin(), link.begin() + digits)));
        segments.push_back(QrSegment::makeNumeric(std::string(link.substr(digits)).c_str()));
    }
    else
    {
        segments.push_back(QrSegment::makeBytes(std::vector<std::uint8_t>(link.begin(), link.end())));
    }
    // asked only for what fits the largest code (its data codewords at each level: 2334 at M, 2956 at L), so the
    // library is never asked for one it can't make, which it says by throwing: the web's build has no exceptions
    constexpr struct { QrCode::Ecc ecc; char level; int bits; } levels[] = {
        { QrCode::Ecc::MEDIUM, 'M', 2334 * 8 },
        { QrCode::Ecc::LOW, 'L', 2956 * 8 },
    };
    for (const auto& l : levels)
    {
        const int bits = QrSegment::getTotalBits(segments, QrCode::MAX_VERSION);
        if (bits < 0 || bits > l.bits)
            continue;
        const QrCode code = QrCode::encodeSegments(segments, l.ecc, QrCode::MIN_VERSION, QrCode::MAX_VERSION, -1, false);
        if (code.getVersion() > QrMaxVersion)
            continue;
        Qr qr;
        qr.version = code.getVersion();
        qr.level = l.level;
        qr.size = code.getSize();
        qr.dark.resize((size_t)qr.size * qr.size);
        for (int r = 0; r < qr.size; r++)
            for (int c = 0; c < qr.size; c++)
                qr.dark[(size_t)r * qr.size + c] = code.getModule(c, r);
        return qr;
    }
    return {};
}

} // namespace SonicPi::ShareLink
