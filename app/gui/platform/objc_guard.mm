//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2026 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#import "objc_guard.h"

#include "macos.h"

#include <iostream>
#include <sstream>

namespace SonicPi
{
namespace objc
{
namespace
{
std::string describe(const char* what, NSException* e)
{
    std::ostringstream ss;
    ss << "[objc] exception in " << (what ? what : "?") << ": "
       << [[e name] UTF8String] << ": "
       << (e.reason ? [e.reason UTF8String] : "(no reason)");
    NSArray<NSString*>* frames = [e callStackSymbols];
    const NSUInteger shown = frames.count < 12 ? frames.count : 12;
    for (NSUInteger i = 0; i < shown; i++)
        ss << "\n    " << [frames[i] UTF8String];
    return ss.str();
}

} // namespace

void uncaught(NSException* e)
{
    const std::string text = describe("an unguarded block (about to abort)", e);
    std::cout << text << std::endl;
    NSLog(@"%s", text.c_str());
}

void reportException(const char* what, NSException* e)
{
    const std::string text = describe(what, e);
    std::cout << text << std::endl;
    NSLog(@"%s", text.c_str());
}

} // namespace objc

void installUncaughtExceptionLog()
{
    NSSetUncaughtExceptionHandler(&objc::uncaught);
}
} // namespace SonicPi
