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

#include <algorithm>
#include <array>
#include <cctype>
#include <cstring>
#include <memory>

#define ZSTD_STATIC_LINKING_ONLY   // the magicless frame format
#include "zstd/zstd.h"
#include <zlib.h>

namespace SonicPi::ShareLink
{

const Table& formatOne();   // sharelink_table1.cpp
const Table& format_2();    // sharelink_table2.cpp

const Table* table(char format)
{
    switch (format)
    {
    case '1': return &formatOne();
    case '2': return &format_2();
    default: return nullptr;
    }
}

// ── The token stream ───────────────────────────────────────────────────────
// Bytes 0x80–0xDF and 0xF0–0xFF are the 112 one-byte tokens, 0xE0–0xEF a newline followed by 0–15 indents of two
// spaces, 0x01 n a two-byte token, 0x02 hi lo a three-byte one, 0x03 b a source byte that would otherwise read as
// one of these; every other byte is the source's own. A token is matched whole: one that starts or ends with a
// word character doesn't match inside a longer word.

constexpr int OneByte = 112, TwoByte = 256;

static bool isWord(unsigned char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_';
}

Codec::Codec(Table table)
    : m_table(std::move(table)), m_byFirst(256)
{
    for (int k = 0; k < (int)m_table.vocab.size(); k++)
    {
        if (!m_table.vocab[k].empty())
            m_byFirst[(unsigned char)m_table.vocab[k][0]].push_back(k);
    }
    for (auto& tokens : m_byFirst)
    {
        std::stable_sort(tokens.begin(), tokens.end(),
                         [this](int a, int b) { return m_table.vocab[a].size() > m_table.vocab[b].size(); });
    }
}

Bytes Codec::pack(std::string_view src) const
{
    Bytes out;
    out.reserve(src.size());
    const size_t n = src.size();
    size_t i = 0;
    while (i < n)
    {
        const unsigned char c = (unsigned char)src[i];
        if (c == '\n')
        {
            size_t spaces = 0;
            while (spaces < 31 && i + 1 + spaces < n && src[i + 1 + spaces] == ' ')
                spaces++;
            if (spaces % 2 == 0)
            {
                out.push_back((std::uint8_t)(0xE0 + spaces / 2));
                i += 1 + spaces;
            }
            else
            {
                out.push_back('\n');
                i++;
            }
            continue;
        }
        int hit = -1;
        size_t len = 0;
        for (int k : m_byFirst[c])
        {
            const std::string& t = m_table.vocab[k];
            if (src.compare(i, t.size(), t) != 0)
                continue;
            if (isWord((unsigned char)t.front()) && i > 0 && isWord((unsigned char)src[i - 1]))
                continue;
            if (isWord((unsigned char)t.back()) && i + t.size() < n && isWord((unsigned char)src[i + t.size()]))
                continue;
            hit = k;
            len = t.size();
            break;
        }
        if (hit >= 0)
        {
            if (hit < 96)
                out.push_back((std::uint8_t)(0x80 + hit));
            else if (hit < OneByte)
                out.push_back((std::uint8_t)(0xF0 + hit - 96));
            else if (hit < OneByte + TwoByte)
                out.insert(out.end(), { 0x01, (std::uint8_t)(hit - OneByte) });
            else
                out.insert(out.end(), { 0x02, (std::uint8_t)((hit - OneByte - TwoByte) >> 8), (std::uint8_t)((hit - OneByte - TwoByte) & 255) });
            i += len;
            continue;
        }
        if (c >= 0x80 || c < 0x09)
            out.push_back(0x03);
        out.push_back(c);
        i++;
    }
    return out;
}

Read Codec::unpack(const Bytes& b) const
{
    Read r;
    const size_t n = b.size();
    auto token = [&](size_t k) {
        if (k >= m_table.vocab.size())
            return false;
        r.code += m_table.vocab[k];
        return true;
    };
    for (size_t i = 0; i < n;)
    {
        const std::uint8_t x = b[i];
        bool ok = true;
        if (x >= 0xF0) { ok = token(96 + x - 0xF0); i++; }
        else if (x >= 0xE0) { r.code += '\n'; r.code.append(2 * (x - 0xE0), ' '); i++; }
        else if (x >= 0x80) { ok = token(x - 0x80); i++; }
        else if (x == 0x01)
        {
            if (i + 1 >= n) { r.error = "the link's code is cut short"; return r; }
            ok = token(OneByte + b[i + 1]);
            i += 2;
        }
        else if (x == 0x02)
        {
            if (i + 2 >= n) { r.error = "the link's code is cut short"; return r; }
            ok = token(OneByte + TwoByte + (b[i + 1] << 8) + b[i + 2]);
            i += 3;
        }
        else if (x == 0x03)
        {
            if (i + 1 >= n) { r.error = "the link's code is cut short"; return r; }
            r.code += (char)b[i + 1];
            i += 2;
        }
        else { r.code += (char)x; i++; }
        if (!ok)
        {
            r.code.clear();
            r.error = "the link's code is not a program: it has a word no table has";
            return r;
        }
    }
    r.ok = true;
    return r;
}

// ── Compression, against the dictionary ───────────────────────────────────

// No program comes near this: past it a link is taken for a trick, not a program (a few KB of link can inflate to
// gigabytes).
constexpr size_t MaxProgram = 16 * 1024 * 1024;

// Format 1: raw DEFLATE (no zlib header: negative window bits), deflate's whole window, its most thorough search.
static Bytes deflateBytes(const Bytes& packed, const Bytes& dictionary)
{
    z_stream z{};
    if (deflateInit2(&z, 9, Z_DEFLATED, -15, 9, Z_DEFAULT_STRATEGY) != Z_OK)
        return {};
    std::unique_ptr<z_stream, int (*)(z_stream*)> end(&z, deflateEnd);
    if (!dictionary.empty())
        deflateSetDictionary(&z, dictionary.data(), (uInt)dictionary.size());
    Bytes out(deflateBound(&z, (uLong)packed.size()));
    z.next_in = const_cast<Bytef*>(packed.data());
    z.avail_in = (uInt)packed.size();
    z.next_out = out.data();
    z.avail_out = (uInt)out.size();
    if (deflate(&z, Z_FINISH) != Z_STREAM_END)
        return {};
    out.resize(z.total_out);
    return out;
}

static bool inflateBytes(const Bytes& bytes, const Bytes& dictionary, Bytes& packed, std::string& error)
{
    z_stream z{};
    if (inflateInit2(&z, -15) != Z_OK)
    {
        error = "the link's code could not be read here (no memory)";
        return false;
    }
    std::unique_ptr<z_stream, int (*)(z_stream*)> end(&z, inflateEnd);
    if (!dictionary.empty())
        inflateSetDictionary(&z, dictionary.data(), (uInt)dictionary.size());
    z.next_in = const_cast<Bytef*>(bytes.data());
    z.avail_in = (uInt)bytes.size();
    std::array<Bytef, 16384> chunk;
    for (;;)
    {
        z.next_out = chunk.data();
        z.avail_out = (uInt)chunk.size();
        const int status = inflate(&z, Z_NO_FLUSH);
        packed.insert(packed.end(), chunk.data(), chunk.data() + (chunk.size() - z.avail_out));
        if (packed.size() > MaxProgram)
        {
            error = "the link's code is too big to be a program";
            return false;
        }
        if (status == Z_STREAM_END)
            return true;
        if (status == Z_BUF_ERROR || (status == Z_OK && z.avail_in == 0 && z.avail_out != 0))
        {
            error = "the link's code is cut short";
            return false;
        }
        if (status != Z_OK)
        {
            error = std::string("the link's code is not a program: ") + (z.msg ? z.msg : "it does not inflate");
            return false;
        }
    }
}

// Format 2: zstd at level 19, one frame with nothing a link can do without — no magic number, no checksum, no
// dictionary id, no content size: the format character says what the bytes are, and base64 carries their length.
static Bytes zstdBytes(const Bytes& packed, const Bytes& dictionary)
{
    std::unique_ptr<ZSTD_CCtx, size_t (*)(ZSTD_CCtx*)> c(ZSTD_createCCtx(), ZSTD_freeCCtx);
    if (!c)
        return {};
    ZSTD_CCtx_setParameter(c.get(), ZSTD_c_compressionLevel, 19);
    ZSTD_CCtx_setParameter(c.get(), ZSTD_c_format, ZSTD_f_zstd1_magicless);
    ZSTD_CCtx_setParameter(c.get(), ZSTD_c_checksumFlag, 0);
    ZSTD_CCtx_setParameter(c.get(), ZSTD_c_dictIDFlag, 0);
    ZSTD_CCtx_setParameter(c.get(), ZSTD_c_contentSizeFlag, 0);
    if (ZSTD_isError(ZSTD_CCtx_loadDictionary(c.get(), dictionary.data(), dictionary.size())))
        return {};
    Bytes out(ZSTD_compressBound(packed.size()));
    const size_t n = ZSTD_compress2(c.get(), out.data(), out.size(), packed.data(), packed.size());
    if (ZSTD_isError(n))
        return {};
    out.resize(n);
    return out;
}

static bool unzstdBytes(const Bytes& bytes, const Bytes& dictionary, Bytes& packed, std::string& error)
{
    std::unique_ptr<ZSTD_DCtx, size_t (*)(ZSTD_DCtx*)> d(ZSTD_createDCtx(), ZSTD_freeDCtx);
    if (!d || ZSTD_isError(ZSTD_DCtx_loadDictionary(d.get(), dictionary.data(), dictionary.size())))
    {
        error = "the link's code could not be read here (no memory)";
        return false;
    }
    ZSTD_DCtx_setParameter(d.get(), ZSTD_d_format, ZSTD_f_zstd1_magicless);
    ZSTD_inBuffer in{ bytes.data(), bytes.size(), 0 };
    std::array<std::uint8_t, 16384> chunk;
    for (;;)
    {
        ZSTD_outBuffer out{ chunk.data(), chunk.size(), 0 };
        const size_t status = ZSTD_decompressStream(d.get(), &out, &in);
        packed.insert(packed.end(), chunk.data(), chunk.data() + out.pos);
        if (packed.size() > MaxProgram)
        {
            error = "the link's code is too big to be a program";
            return false;
        }
        if (ZSTD_isError(status))
        {
            error = std::string("the link's code is not a program: ") + ZSTD_getErrorName(status);
            return false;
        }
        if (status == 0)   // the frame is whole
        {
            if (in.pos < in.size)
            {
                error = "the link's code is not a program: there are bytes after it";
                return false;
            }
            return true;
        }
        if (in.pos == in.size && out.pos < out.size)
        {
            error = "the link's code is cut short";
            return false;
        }
    }
}

Bytes Codec::encode(std::string_view source) const
{
    const Bytes packed = pack(source);
    return m_table.compression == Compression::Zstd ? zstdBytes(packed, m_table.dictionary)
                                                    : deflateBytes(packed, m_table.dictionary);
}

Read Codec::decode(const Bytes& bytes) const
{
    Read r;
    Bytes packed;
    const bool ok = m_table.compression == Compression::Zstd ? unzstdBytes(bytes, m_table.dictionary, packed, r.error)
                                                             : inflateBytes(bytes, m_table.dictionary, packed, r.error);
    return ok ? unpack(packed) : r;
}

// ── base64url, and decimal digits ──────────────────────────────────────────

std::string base64url(const Bytes& bytes)
{
    static const char* abc = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
    std::string out;
    out.reserve((bytes.size() * 4 + 2) / 3);
    for (size_t i = 0; i < bytes.size(); i += 3)
    {
        const size_t k = std::min<size_t>(3, bytes.size() - i);
        std::uint32_t v = bytes[i] << 16;
        if (k > 1) v |= bytes[i + 1] << 8;
        if (k > 2) v |= bytes[i + 2];
        out += abc[(v >> 18) & 63];
        out += abc[(v >> 12) & 63];
        if (k > 1) out += abc[(v >> 6) & 63];
        if (k > 2) out += abc[v & 63];
    }
    return out;
}

// As a browser's atob reads it: either alphabet, padding optional, white space between the characters ignored, and
// a length that leaves one character over refused.
bool fromBase64(std::string_view text, Bytes& out)
{
    out.clear();
    std::uint32_t acc = 0;
    int bits = 0, chars = 0;
    size_t i = 0;
    for (; i < text.size(); i++)
    {
        const char c = text[i];
        int v;
        if (c >= 'A' && c <= 'Z') v = c - 'A';
        else if (c >= 'a' && c <= 'z') v = c - 'a' + 26;
        else if (c >= '0' && c <= '9') v = c - '0' + 52;
        else if (c == '+' || c == '-') v = 62;
        else if (c == '/' || c == '_') v = 63;
        else if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f') continue;
        else if (c == '=') break;
        else return false;
        acc = (acc << 6) | v;
        bits += 6;
        chars++;
        if (bits >= 8)
        {
            bits -= 8;
            out.push_back((std::uint8_t)((acc >> bits) & 255));
        }
    }
    for (; i < text.size(); i++)
    {
        if (text[i] != '=' && text[i] != ' ' && text[i] != '\t' && text[i] != '\n' && text[i] != '\r' && text[i] != '\f')
            return false;
    }
    return chars % 4 != 1;
}

// Seven bytes to seventeen digits, the last few bytes to as few digits as hold them; a length of digits names its
// bytes, so the tail needs no marker.
static constexpr std::array<size_t, 8> Tail = { 0, 3, 5, 8, 10, 13, 15, 17 };   // k bytes → digits

std::string toDigits(const Bytes& bytes)
{
    std::string out;
    for (size_t i = 0; i < bytes.size(); i += 7)
    {
        const size_t k = std::min<size_t>(7, bytes.size() - i);
        std::uint64_t n = 0;
        for (size_t j = 0; j < k; j++)
            n = (n << 8) | bytes[i + j];
        std::string piece = std::to_string(n);
        out.append(Tail[k] - piece.size(), '0');
        out += piece;
    }
    return out;
}

bool fromDigits(std::string_view s, Bytes& out, std::string& error)
{
    out.clear();
    const size_t tail = s.size() % 17;
    const auto at = std::find(Tail.begin(), Tail.end(), tail);
    if (at == Tail.end() || !std::all_of(s.begin(), s.end(), [](char c) { return c >= '0' && c <= '9'; }))
    {
        error = "the link's digits are cut short";
        return false;
    }
    for (size_t i = 0; i < s.size();)
    {
        const size_t len = s.size() - i >= 17 ? 17 : tail;
        const size_t k = std::find(Tail.begin(), Tail.end(), len) - Tail.begin();
        std::uint64_t n = 0;
        for (size_t j = 0; j < len; j++)
            n = n * 10 + (std::uint64_t)(s[i + j] - '0');
        if (k < 8 && (n >> (8 * k)) != 0)
        {
            error = "the link's digits are not a program";
            return false;
        }
        for (size_t j = k; j-- > 0;)
            out.push_back((std::uint8_t)((n >> (8 * j)) & 255));
        i += len;
    }
    return true;
}

// ── Links ──────────────────────────────────────────────────────────────────

static const Codec* codecFor(char format)
{
    static const Codec one(formatOne());
    static const Codec two(format_2());
    switch (format)
    {
    case '1': return &one;
    case '2': return &two;
    default: return nullptr;
    }
}

// The program's bytes in whichever of WriteFormats makes the fewest (base64 and digits both go by the bytes), and
// that format; the first of them at a tie.
static std::pair<char, Bytes> shortest(std::string_view code)
{
    std::pair<char, Bytes> best;
    for (const char format : WriteFormats)
    {
        Bytes bytes = codecFor(format)->encode(code);
        if (!best.first || bytes.size() < best.second.size())
            best = { format, std::move(bytes) };
    }
    return best;
}

std::string encode(std::string_view code)
{
    const auto [format, bytes] = shortest(code);
    return std::string(1, format) + base64url(bytes);
}

std::string encodeDigits(std::string_view code)
{
    const auto [format, bytes] = shortest(code);
    return std::string("N") + format + toDigits(bytes);
}

Read decode(std::string_view link)
{
    Read r;
    const size_t at = link.find("#code=");
    std::string_view s = at == std::string_view::npos ? link : link.substr(at + 6);
    while (!s.empty() && std::strchr(" \t\r\n", s.front())) s.remove_prefix(1);
    while (!s.empty() && std::strchr(" \t\r\n", s.back())) s.remove_suffix(1);
    if (s.empty())
    {
        r.error = "the link has no code in it";
        return r;
    }
    const bool digits = s[0] == 'N';
    const char format = digits ? (s.size() > 1 ? s[1] : '\0') : s[0];
    const Codec* codec = codecFor(format);
    if (!codec)
    {
        r.error = std::string("a link from a newer Sonic Pi (format \"") + (format ? std::string(1, format) : std::string()) + "\")";
        return r;
    }
    Bytes bytes;
    if (digits)
    {
        if (!fromDigits(s.substr(2), bytes, r.error))
            return r;
    }
    else if (!fromBase64(s.substr(1), bytes))
    {
        // every character one a link has, and one too many or too few: the end was lost
        const bool alphabet = std::all_of(s.begin() + 1, s.end(), [](char c) {
            return std::isalnum((unsigned char)c) || std::strchr("-_+/= \t\r\n", c);
        });
        r.error = alphabet ? "the link's code is cut short" : "the link's code has characters no link has";
        return r;
    }
    return codec->decode(bytes);
}

} // namespace SonicPi::ShareLink
