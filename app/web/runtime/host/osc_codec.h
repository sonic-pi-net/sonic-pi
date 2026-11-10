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

// OSC as the host reads and writes it: a value type for a decoded message,
// over oscpack (clockwork's vendored copy, already linked in with the engine).
// The GUI's own wire format stays on oscpkt (OscHandler); this is for the
// runtime's records and the engine's replies, which carry doubles, 64-bit
// ints, nil and arrays that oscpkt does not read.
#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace SonicPi::osc
{

struct Value
{
    enum Type
    {
        Int32,
        Int64,
        Float,
        Double,
        String,
        Blob,
        True,
        False,
        Nil,
        Array,
        TimeTag
    };
    Type type = Nil;
    int64_t i = 0;          // Int32, Int64, True/False (1/0)
    double d = 0.0;         // Float, Double, TimeTag (NTP seconds)
    std::string s;          // String
    std::vector<uint8_t> b; // Blob
    std::vector<Value> a;   // Array

    static Value int32(int32_t v) { Value x; x.type = Int32; x.i = v; return x; }
    static Value int64(int64_t v) { Value x; x.type = Int64; x.i = v; return x; }
    static Value flt(float v) { Value x; x.type = Float; x.d = v; return x; }
    static Value dbl(double v) { Value x; x.type = Double; x.d = v; return x; }
    static Value str(std::string v) { Value x; x.type = String; x.s = std::move(v); return x; }
    static Value blob(std::vector<uint8_t> v) { Value x; x.type = Blob; x.b = std::move(v); return x; }
    static Value boolean(bool v) { Value x; x.type = v ? True : False; x.i = v ? 1 : 0; return x; }
    static Value nil() { return Value(); }

    bool isNumber() const { return type == Int32 || type == Int64 || type == Float || type == Double; }
    bool isInt() const { return type == Int32 || type == Int64; }
    double number() const { return isInt() ? double(i) : d; }
    int64_t integer() const { return isInt() ? i : int64_t(d); }
    // What the GUI shows for a value, Ruby's way: 60, 1.5, "a string"
    std::string inspect() const;
};

struct Message
{
    std::string address;
    std::vector<Value> args;

    Message() = default;
    explicit Message(std::string addr) : address(std::move(addr)) {}
    Message& add(Value v) { args.push_back(std::move(v)); return *this; }
    Message& addInt(int32_t v) { return add(Value::int32(v)); }
    Message& addFloat(float v) { return add(Value::flt(v)); }
    Message& addStr(std::string v) { return add(Value::str(std::move(v))); }

    const Value* arg(size_t n) const { return n < args.size() ? &args[n] : nullptr; }
    std::string strArg(size_t n, const std::string& def = "") const;
    int64_t intArg(size_t n, int64_t def = 0) const;
    double numArg(size_t n, double def = 0.0) const;
};

// One OSC packet (a message, or a bundle holding messages). A bundle's
// messages are flattened into `messages` in order; its timetag is `timeTag`.
struct Packet
{
    bool bundle = false;
    double timeTag = 0.0;
    std::vector<Message> messages;
};

// Reads a packet. False when the bytes are not OSC.
bool decode(const uint8_t* bytes, size_t len, Packet& out);
// Reads one message (not a bundle). False when it is a bundle or malformed.
bool decodeMessage(const uint8_t* bytes, size_t len, Message& out);

// Writes a message.
std::vector<uint8_t> encode(const Message& m);
// A clockwork verb the engine holds and carries out at `ntp` seconds:
// /clockwork/schedule h:timetag b:message (web/sonic_pi.js oscSchedule).
std::vector<uint8_t> schedule(double ntp, const Message& inner);

// A bundle's timetag moved on by `secs`, in place (an immediate one left as it is).
void retime(std::vector<uint8_t>& bundle, double secs);

// NTP seconds <-> an OSC timetag's 64 bits (0 or less: "immediately", 1).
uint64_t ntpToTimetag(double ntp);
double timetagToNtp(uint64_t tag);

} // namespace SonicPi::osc
