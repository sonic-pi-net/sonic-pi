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

// gui.log is written by several threads through one cout. On 2026-11-14 a
// session restart left lines like "API[API] [INFO ] : Startinzing..." in it:
// two threads' text spliced character by character. A log that can read
// like that cannot be trusted when it matters, so every line in the file
// must be one thread's whole line, stamped once at its start.

#include <catch2/catch_test_macros.hpp>

#include <api/timestamp_line_buf.h>

#include <map>
#include <ostream>
#include <regex>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

using SonicPi::TimestampLineBuf;

namespace
{
const int kThreads = 8;
const int kLines = 300;

// Everything written, split into lines.
std::vector<std::string> linesOf(const std::string& text)
{
    std::vector<std::string> lines;
    std::string line;
    std::istringstream in(text);
    while (std::getline(in, line))
        lines.push_back(line);
    return lines;
}
} // namespace

TEST_CASE("Lines written in pieces from many threads come out whole and stamped once", "[gui-log]")
{
    std::stringbuf file;
    TimestampLineBuf buf(&file);
    std::ostream out(&buf);

    std::vector<std::thread> threads;
    for (int t = 0; t < kThreads; ++t)
    {
        threads.emplace_back([&out, t] {
            for (int n = 0; n < kLines; ++n)
                out << "[T" << t << "] line " << n << " of " << kLines << std::endl;
        });
    }
    for (auto& th : threads)
        th.join();

    const auto lines = linesOf(file.str());
    REQUIRE(lines.size() == static_cast<size_t>(kThreads * kLines));

    const std::regex shape(R"(^\[\d\d:\d\d:\d\d\.\d\d\d\] \[T(\d)\] line (\d+) of 300$)");
    std::map<int, int> seenPerThread;
    for (const auto& line : lines)
    {
        std::smatch m;
        INFO("line: " << line);
        REQUIRE(std::regex_match(line, m, shape));
        ++seenPerThread[std::stoi(m[1])];
    }
    for (int t = 0; t < kThreads; ++t)
        CHECK(seenPerThread[t] == kLines);
}

TEST_CASE("A chunk holding several lines is stamped on every line", "[gui-log]")
{
    std::stringbuf file;
    TimestampLineBuf buf(&file);
    std::ostream out(&buf);

    out << "first\nsecond\n" << std::flush;

    const auto lines = linesOf(file.str());
    REQUIRE(lines.size() == 2);
    const std::regex shape(R"(^\[\d\d:\d\d:\d\d\.\d\d\d\] (first|second)$)");
    CHECK(std::regex_match(lines[0], shape));
    CHECK(std::regex_match(lines[1], shape));
}

TEST_CASE("A flush writes the partial line as it stands, and its rest is not stamped again", "[gui-log]")
{
    std::stringbuf file;
    TimestampLineBuf buf(&file);
    std::ostream out(&buf);

    out << "half a" << std::flush;
    CHECK(std::regex_match(file.str(), std::regex(R"(^\[\d\d:\d\d:\d\d\.\d\d\d\] half a$)")));

    out << " line\nnext\n" << std::flush;
    const auto lines = linesOf(file.str());
    REQUIRE(lines.size() == 2);
    CHECK(std::regex_match(lines[0], std::regex(R"(^\[\d\d:\d\d:\d\d\.\d\d\d\] half a line$)")));
    CHECK(std::regex_match(lines[1], std::regex(R"(^\[\d\d:\d\d:\d\d\.\d\d\d\] next$)")));
}
