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

#ifndef OBJC_GUARD_H
#define OBJC_GUARD_H

#import <Foundation/Foundation.h>

// An Objective-C exception that escapes a block is fatal. On the main thread
// AppKit's event loop swallows it; on a dispatch queue — every system
// completion handler, every dispatch_async — nothing does, and the whole app
// aborts. Apple's frameworks throw for arguments they refuse (AVFoundation
// refused a 5120x2880 H.264 writer on 2026-10-06 and took Sonic Pi with it),
// so every block handed to a system API goes through here: an exception is
// reported to gui.log and the unified log, with its backtrace, and the block
// fails instead of the app. gui-tests lints the Objective-C++ sources for
// any block literal that does not.
namespace SonicPi
{
namespace objc
{
// Reports an exception caught in `what`: name, reason and the top of its
// backtrace, to gui.log (std::cout) and NSLog.
void reportException(const char* what, NSException* exception);

// Runs `body` now. An NSException is reported and swallowed; returns false
// when one was.
inline bool guard(const char* what, void (NS_NOESCAPE ^body)(void))
{
    @try
    {
        body();
        return true;
    }
    @catch (NSException* e)
    {
        reportException(what, e);
        return false;
    }
}

// A block literal lives on the stack until copied; ARC copies one that is
// returned, a file compiled without ARC (macos.mm) does not.
#if __has_feature(objc_arc)
#define SP_OBJC_HEAP_BLOCK(block) (block)
#else
#define SP_OBJC_HEAP_BLOCK(block) [[(block) copy] autorelease]
#endif

// The block to hand a system API: `block`, with an NSException inside it
// reported rather than fatal.
template <typename... Args>
void (^guarded(const char* what, void (^block)(Args...)))(Args...)
{
    return SP_OBJC_HEAP_BLOCK(^(Args... args) {
        @try
        {
            block(args...);
        }
        @catch (NSException* e)
        {
            reportException(what, e);
        }
    });
}

// As guarded, for a block that answers something: `fallback` is the answer
// when it threw.
template <typename R, typename... Args>
R (^guardedOr(const char* what, R (^block)(Args...), R fallback))(Args...)
{
    return SP_OBJC_HEAP_BLOCK(^R(Args... args) {
        @try
        {
            return block(args...);
        }
        @catch (NSException* e)
        {
            reportException(what, e);
            return fallback;
        }
    });
}
} // namespace objc
} // namespace SonicPi

#endif // OBJC_GUARD_H
