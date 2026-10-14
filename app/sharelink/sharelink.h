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

#pragma once

// A program, or a whole set of buffers, carried in a link: #code= then one character naming the format, then the
// program packed and compressed, in base64url; or, for a QR code, "N", the format, then the same bytes as decimal
// digits, which fit a QR code's numeric mode. The link is the whole of it: nothing is stored anywhere.
//
// One codec for every Sonic Pi: the native app compiles this, and the web's page runs it compiled to wasm
// (app/web/app/src/share.js), so a link either writes the other reads, and both write the same link.
//
// A format packs the source as a token stream (its table: Sonic Pi's vocabulary, its functions, synths, fx,
// samples, opts and Ruby's words, as one, two or three bytes each; a newline with its indent as one) and compresses
// that against a preset dictionary of the token stream Sonic Pi programs share most. Format 1 deflates (raw DEFLATE,
// RFC 1951) against 32 KB of it; format 2, zstd against 512 KB, for links a sixth shorter. A table, once shipped, is
// fixed for its format character: a new table is a new character, and every link stays readable. The whole format
// is written down in app/web/docs/share-link-format.md.

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace SonicPi::ShareLink
{

using Bytes = std::vector<std::uint8_t>;

// The formats links are written in — whichever makes the shorter link for a program, format 1 for a line or two
// (zstd's frame costs a few bytes more than deflate's), format 2 past that — and every format this version knows is
// read.
constexpr char WriteFormats[] = { '2', '1' };

// How a format compresses its token stream: raw DEFLATE (zlib) or zstd, each against the table's dictionary.
enum class Compression : std::uint8_t
{
    Deflate,
    Zstd
};

// A format's table: its character, its vocabulary (token k is vocab[k]), and its compression with the dictionary it
// starts from.
struct Table
{
    char format = 0;
    std::vector<std::string> vocab;
    Compression compression = Compression::Deflate;
    Bytes dictionary;
};

// The table a format was written with, or nullptr for a format this version does not know.
const Table* table(char format);

// What reading gave: the code (UTF-8), or what was wrong with it.
struct Read
{
    bool ok = false;
    std::string code;
    std::string error;
};

// A table's codec: the source as a token stream, and that stream compressed against the table's dictionary.
class Codec
{
public:
    explicit Codec(Table table);

    Bytes pack(std::string_view source) const;
    Read unpack(const Bytes& stream) const;
    Bytes encode(std::string_view source) const;
    Read decode(const Bytes& bytes) const;

private:
    Table m_table;
    std::vector<std::vector<int>> m_byFirst;   // a byte → the tokens starting with it, longest first
};

// The link's code (what follows "#code="), as Share writes it: the format, then the program in base64url, in
// whichever of WriteFormats is shorter for it.
std::string encode(std::string_view code);
// The same for a QR code: "N", the format, then the program's bytes as decimal digits.
std::string encodeDigits(std::string_view code);
// A link read back: a whole link (anything with "#code=" in it) or its code alone.
Read decode(std::string_view link);

// A link as a QR code (sharelink_qr.cpp): the address up to the program in the byte mode, and the program's digits
// (encodeDigits's form) in the numeric mode, which holds a third more. Medium error correction while it fits (a phone
// reads a marked screen or a creased page), low when only that will hold it; none past QrMaxVersion (117×117
// modules), which a phone no longer reads off a screen: then the link or the file, not a code. Native and the web's
// page show the same code for the same link.
constexpr int QrMaxVersion = 25;
struct Qr
{
    int version = 0;          // 0: too big for a code a phone can read
    char level = 0;           // 'M' or 'L'
    int size = 0;             // modules a side
    std::vector<bool> dark;   // row by row
    bool isDark(int row, int col) const { return dark[(size_t)row * size + col]; }
};
Qr qrCode(std::string_view link);

// The pieces, for the web's tests and scripts/build-share-table.mjs
std::string base64url(const Bytes& bytes);
bool fromBase64(std::string_view text, Bytes& out);   // base64 or base64url, padded or not
std::string toDigits(const Bytes& bytes);
bool fromDigits(std::string_view digits, Bytes& out, std::string& error);

} // namespace SonicPi::ShareLink
