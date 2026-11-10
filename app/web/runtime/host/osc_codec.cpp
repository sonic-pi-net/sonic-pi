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

#include "osc_codec.h"

#include <cmath>
#include <cstdio>
#include <cstring>

// clockwork's vendored oscpack (clockwork/src/vendor/oscpack)
#include <osc/OscException.h>
#include <osc/OscOutboundPacketStream.h>
#include <osc/OscReceivedElements.h>

namespace SonicPi::osc
{

namespace
{

// The arguments from `it` up to `end`, or to the array's end when `inArray`.
void readArgs(::osc::ReceivedMessageArgumentIterator& it, const ::osc::ReceivedMessageArgumentIterator& end,
              std::vector<Value>& out, bool inArray, int depth)
{
    for (; it != end; ++it)
    {
        const auto& a = *it;
        if (a.IsArrayEnd())
        {
            if (inArray) return;
            continue;   // a stray end: skipped
        }
        Value v;
        if (a.IsArrayBegin())
        {
            if (depth > 16) throw ::osc::Exception("arrays nested too deep");
            v.type = Value::Array;
            ++it;
            readArgs(it, end, v.a, true, depth + 1);   // leaves `it` on the array's end
            if (it == end) { out.push_back(std::move(v)); return; }
        }
        else if (a.IsInt32()) v = Value::int32(a.AsInt32Unchecked());
        else if (a.IsInt64()) v = Value::int64(a.AsInt64Unchecked());
        else if (a.IsFloat()) v = Value::flt(a.AsFloatUnchecked());
        else if (a.IsDouble()) v = Value::dbl(a.AsDoubleUnchecked());
        else if (a.IsString()) v = Value::str(a.AsStringUnchecked());
        else if (a.IsSymbol()) v = Value::str(a.AsSymbolUnchecked());
        else if (a.IsBlob())
        {
            const void* data = nullptr;
            ::osc::osc_bundle_element_size_t size = 0;
            a.AsBlobUnchecked(data, size);
            const auto* p = static_cast<const uint8_t*>(data);
            v = Value::blob(std::vector<uint8_t>(p, p + size));
        }
        else if (a.IsBool()) v = Value::boolean(a.AsBoolUnchecked());
        else if (a.IsNil() || a.IsInfinitum()) v = Value::nil();
        else if (a.IsTimeTag()) { v.type = Value::TimeTag; v.d = timetagToNtp(a.AsTimeTagUnchecked()); }
        else if (a.IsChar()) v = Value::int32(a.AsCharUnchecked());
        else continue;   // a colour, a MIDI message: nothing the host reads
        out.push_back(std::move(v));
    }
}

Message messageOf(const ::osc::ReceivedMessage& m)
{
    Message out(m.AddressPattern());
    auto it = m.ArgumentsBegin();
    readArgs(it, m.ArgumentsEnd(), out.args, false, 0);
    return out;
}

void flatten(const ::osc::ReceivedBundle& b, Packet& out)
{
    for (auto e = b.ElementsBegin(); e != b.ElementsEnd(); ++e)
    {
        if (e->IsBundle()) flatten(::osc::ReceivedBundle(*e), out);
        else out.messages.push_back(messageOf(::osc::ReceivedMessage(*e)));
    }
}

// Bytes a message needs on the wire, with room to spare: oscpack writes
// into a buffer the caller sizes.
size_t upperBound(const Value& v)
{
    switch (v.type)
    {
    case Value::String: return v.s.size() + 5;
    case Value::Blob: return v.b.size() + 8;
    case Value::Array:
    {
        size_t n = 2;
        for (const auto& x : v.a) n += upperBound(x);
        return n;
    }
    default: return 8;
    }
}

size_t upperBound(const Message& m)
{
    size_t n = m.address.size() + 5 + m.args.size() + 5 + 16;
    for (const auto& v : m.args) n += upperBound(v);
    return n;
}

void write(::osc::OutboundPacketStream& s, const Value& v)
{
    switch (v.type)
    {
    case Value::Int32: s << static_cast<::osc::int32>(v.i); break;
    case Value::Int64: s << static_cast<::osc::int64>(v.i); break;
    case Value::Float: s << static_cast<float>(v.d); break;
    case Value::Double: s << v.d; break;
    case Value::TimeTag: s << ::osc::TimeTag(ntpToTimetag(v.d)); break;
    case Value::String: s << v.s.c_str(); break;
    case Value::Blob: s << ::osc::Blob(v.b.data(), static_cast<::osc::osc_bundle_element_size_t>(v.b.size())); break;
    case Value::True: s << true; break;
    case Value::False: s << false; break;
    case Value::Nil: s << ::osc::OscNil; break;
    case Value::Array:
        s << ::osc::BeginArray;
        for (const auto& x : v.a) write(s, x);
        s << ::osc::EndArray;
        break;
    }
}

} // namespace

std::string Value::inspect() const
{
    char buf[64];
    switch (type)
    {
    case Int32:
    case Int64:
        snprintf(buf, sizeof buf, "%lld", static_cast<long long>(i));
        return buf;
    case Float:
    case Double:
    {
        if (std::isnan(d)) return "NaN";
        if (std::isinf(d)) return d > 0 ? "Infinity" : "-Infinity";
        for (int p = 1; p <= 17; p++)   // the shortest decimal that reads back the same
        {
            snprintf(buf, sizeof buf, "%.*g", p, d);
            if (std::strtod(buf, nullptr) == d) break;
        }
        std::string s = buf;
        if (s.find_first_of(".eE") == std::string::npos) s += ".0";
        return s;
    }
    case String:
        return "\"" + s + "\"";
    case Blob:
        return "<blob " + std::to_string(b.size()) + ">";
    case True:
        return "true";
    case False:
        return "false";
    case Nil:
        return "nil";
    case TimeTag:
        snprintf(buf, sizeof buf, "%.6f", d);
        return buf;
    case Array:
    {
        std::string out = "[";
        for (size_t k = 0; k < a.size(); k++)
        {
            if (k) out += ", ";
            out += a[k].inspect();
        }
        return out + "]";
    }
    }
    return "";
}

std::string Message::strArg(size_t n, const std::string& def) const
{
    const Value* v = arg(n);
    if (!v) return def;
    if (v->type == Value::String) return v->s;
    if (v->type == Value::Nil) return def;
    return v->inspect();
}

int64_t Message::intArg(size_t n, int64_t def) const
{
    const Value* v = arg(n);
    if (!v) return def;
    if (v->isNumber()) return v->integer();
    if (v->type == Value::True || v->type == Value::False) return v->i;
    return def;
}

double Message::numArg(size_t n, double def) const
{
    const Value* v = arg(n);
    return v && v->isNumber() ? v->number() : def;
}

bool decodeMessage(const uint8_t* bytes, size_t len, Message& out)
{
    try
    {
        ::osc::ReceivedPacket p(reinterpret_cast<const char*>(bytes), static_cast<::osc::osc_bundle_element_size_t>(len));
        if (p.IsBundle()) return false;
        out = messageOf(::osc::ReceivedMessage(p));
        return true;
    }
    catch (const ::osc::Exception&)
    {
        return false;
    }
}

bool decode(const uint8_t* bytes, size_t len, Packet& out)
{
    out = Packet();
    try
    {
        ::osc::ReceivedPacket p(reinterpret_cast<const char*>(bytes), static_cast<::osc::osc_bundle_element_size_t>(len));
        if (p.IsBundle())
        {
            ::osc::ReceivedBundle b(p);
            out.bundle = true;
            out.timeTag = timetagToNtp(b.TimeTag());
            flatten(b, out);
        }
        else
        {
            out.messages.push_back(messageOf(::osc::ReceivedMessage(p)));
        }
        return true;
    }
    catch (const ::osc::Exception&)
    {
        return false;
    }
}

std::vector<uint8_t> encode(const Message& m)
{
    std::vector<char> buf(upperBound(m) + 64);
    try
    {
        ::osc::OutboundPacketStream s(buf.data(), buf.size());
        s << ::osc::BeginMessage(m.address.c_str());
        for (const auto& v : m.args) write(s, v);
        s << ::osc::EndMessage;
        return std::vector<uint8_t>(s.Data(), s.Data() + s.Size());
    }
    catch (const ::osc::Exception&)
    {
        return {};
    }
}

uint64_t ntpToTimetag(double ntp)
{
    if (ntp <= 0.0) return 1; // OSC's "immediately"
    const double secs = std::floor(ntp);
    uint64_t lo = uint64_t((ntp - secs) * 4294967296.0);
    if (lo > 4294967295ull) lo = 4294967295ull;
    return (uint64_t(secs) << 32) | lo;
}

double timetagToNtp(uint64_t tag)
{
    if (tag <= 1) return 0.0;
    return double(tag >> 32) + double(uint32_t(tag)) / 4294967296.0;
}

std::vector<uint8_t> schedule(double ntp, const Message& inner)
{
    Message m("/clockwork/schedule");
    m.add(Value::int64(int64_t(ntpToTimetag(ntp))));
    m.add(Value::blob(encode(inner)));
    return encode(m);
}

void retime(std::vector<uint8_t>& b, double secs)
{
    // the timetag is bytes 8..16 of a bundle, big-endian; rewritten in place
    if (b.size() < 16 || std::memcmp(b.data(), "#bundle", 8) != 0) return;
    uint64_t tag = 0;
    for (int k = 0; k < 8; k++) tag = (tag << 8) | b[8 + k];
    if (tag <= 1) return;   // immediate stays immediate
    const uint64_t moved = ntpToTimetag(timetagToNtp(tag) + secs);
    for (int k = 0; k < 8; k++) b[8 + k] = uint8_t(moved >> (56 - 8 * k));
}

} // namespace SonicPi::osc
