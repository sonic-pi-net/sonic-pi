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


#include "files.h"

#include <algorithm>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <sstream>

namespace fs = std::filesystem;

namespace SonicPi::files
{

bool isSoundFile(const std::string& name)
{
    const auto dot = name.rfind('.');
    if (dot == std::string::npos) return false;
    std::string ext = name.substr(dot + 1);
    std::transform(ext.begin(), ext.end(), ext.begin(), [](unsigned char c) { return char(std::tolower(c)); });
    // the set Sonic Pi has always globbed for in a folder (SampleLoader#ls_samples)
    return ext == "wav" || ext == "wave" || ext == "aif" || ext == "aiff" || ext == "flac" || ext == "mp3" || ext == "ogg" || ext == "oga";
}

std::string runtimePath(const std::string& path)
{
    std::string p = path;
    std::replace(p.begin(), p.end(), '\\', '/');
    return p;
}

bool isDirectory(const std::string& path)
{
    std::error_code ec;
    return fs::is_directory(fs::path(path), ec);
}

bool isFile(const std::string& path)
{
    std::error_code ec;
    return fs::is_regular_file(fs::path(path), ec);
}

std::vector<std::string> soundFilesIn(const std::string& dir, bool recursive)
{
    std::vector<std::string> out;
    std::error_code ec;
    if (!fs::is_directory(fs::path(dir), ec)) return out;
    auto take = [&](const fs::directory_entry& e) {
        if (e.is_regular_file(ec) && isSoundFile(e.path().filename().string())) out.push_back(runtimePath(e.path().string()));
    };
    if (recursive)
        for (const auto& e : fs::recursive_directory_iterator(fs::path(dir), ec)) take(e);
    else
        for (const auto& e : fs::directory_iterator(fs::path(dir), ec)) take(e);
    std::sort(out.begin(), out.end());
    return out;
}

std::vector<uint8_t> readFile(const std::string& path)
{
    std::ifstream f(path, std::ios::binary);
    if (!f) return {};
    return std::vector<uint8_t>((std::istreambuf_iterator<char>(f)), std::istreambuf_iterator<char>());
}

std::string readTextFile(const std::string& path)
{
    std::ifstream f(path, std::ios::binary);
    if (!f) return "";
    std::stringstream ss;
    ss << f.rdbuf();
    return ss.str();
}

} // namespace SonicPi::files
