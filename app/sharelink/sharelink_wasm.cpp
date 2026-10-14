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

// The codec as the web's page calls it (app/web/app/src/share.js), compiled to wasm by
// app/web/scripts/build-runtime.sh. Every answer goes out through one buffer: its bytes at sl_out_ptr(), sl_out_len()
// long, good until the next call. A call that can fail answers 1, its result in the buffer, or 0, why not in it.

#include "sharelink.h"

#include <map>
#include <memory>

using namespace SonicPi::ShareLink;

static std::string out;
static std::map<int, std::unique_ptr<Codec>> codecs;   // made from tables of the caller's (scripts/build-share-table.mjs)
static int nextCodec = 1;

static int answer(const Read& r)
{
    out = r.ok ? r.code : r.error;
    return r.ok ? 1 : 0;
}

static void answer(const Bytes& bytes)
{
    out.assign(bytes.begin(), bytes.end());
}

extern "C" {

const char* sl_out_ptr() { return out.data(); }
int sl_out_len() { return (int)out.size(); }

// a link's code for a program: as text (base64url), or as digits for a QR code
void sl_encode(const char* code, int len, int digits)
{
    out = digits ? encodeDigits(std::string_view(code, len)) : encode(std::string_view(code, len));
}

// a link, or its code alone, read back
int sl_decode(const char* link, int len)
{
    return answer(decode(std::string_view(link, len)));
}

// a link as a QR code: its version (0: too big for one), and in the buffer its level ('M' or 'L'), its size, then
// a byte a module, row by row, 1 dark
int sl_qr(const char* link, int len)
{
    const Qr qr = qrCode(std::string_view(link, len));
    out.clear();
    if (!qr.version)
        return 0;
    out += qr.level;
    out += (char)qr.size;
    for (bool d : qr.dark)
        out += (char)(d ? 1 : 0);
    return qr.version;
}

void sl_to_digits(const std::uint8_t* bytes, int len)
{
    out = toDigits(Bytes(bytes, bytes + len));
}

int sl_from_digits(const char* digits, int len)
{
    Bytes bytes;
    std::string error;
    if (!fromDigits(std::string_view(digits, len), bytes, error))
    {
        out = error;
        return 0;
    }
    answer(bytes);
    return 1;
}

// a format's table, as JSON: {"version", "vocab", "compression" ("deflate" or "zstd"), "dictionary" (base64)}; 0 for a
// format this version doesn't know
int sl_table_json(int format)
{
    const Table* t = table((char)format);
    if (!t)
        return 0;
    auto quoted = [](const std::string& s) {
        std::string q = "\"";
        for (char c : s)
        {
            if (c == '"' || c == '\\') q += '\\';
            q += c;
        }
        return q + "\"";
    };
    std::string b64 = base64url(t->dictionary);
    for (char& c : b64) c = c == '-' ? '+' : c == '_' ? '/' : c;
    while (b64.size() % 4) b64 += '=';
    out = "{\"version\":" + quoted(std::string(1, t->format)) + ",\"vocab\":[";
    for (size_t i = 0; i < t->vocab.size(); i++)
        out += (i ? "," : "") + quoted(t->vocab[i]);
    out += "],\"compression\":" + quoted(t->compression == Compression::Zstd ? "zstd" : "deflate")
         + ",\"dictionary\":" + quoted(b64) + "}";
    return 1;
}

// a codec from any table: its vocabulary a token a line (no token has a newline), its dictionary's bytes, and its
// compression (0 deflate, 1 zstd)
int sl_codec_new(const char* vocab, int vocabLen, const std::uint8_t* dictionary, int dictionaryLen, int compression)
{
    Table t;
    t.compression = compression == 1 ? Compression::Zstd : Compression::Deflate;
    std::string_view v(vocab, vocabLen);
    for (size_t at = 0; at <= v.size();)
    {
        const size_t nl = v.find('\n', at);
        t.vocab.emplace_back(v.substr(at, nl == std::string_view::npos ? std::string_view::npos : nl - at));
        if (nl == std::string_view::npos) break;
        at = nl + 1;
    }
    t.dictionary.assign(dictionary, dictionary + dictionaryLen);
    codecs[nextCodec] = std::make_unique<Codec>(std::move(t));
    return nextCodec++;
}

void sl_codec_free(int codec) { codecs.erase(codec); }

void sl_codec_pack(int codec, const char* source, int len) { answer(codecs.at(codec)->pack(std::string_view(source, len))); }
void sl_codec_encode(int codec, const char* source, int len) { answer(codecs.at(codec)->encode(std::string_view(source, len))); }
int sl_codec_unpack(int codec, const std::uint8_t* bytes, int len) { return answer(codecs.at(codec)->unpack(Bytes(bytes, bytes + len))); }
int sl_codec_decode(int codec, const std::uint8_t* bytes, int len) { return answer(codecs.at(codec)->decode(Bytes(bytes, bytes + len))); }

} // extern "C"
