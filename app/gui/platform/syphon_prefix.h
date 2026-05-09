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
//
// Force-included via clang -include when compiling vendored Syphon-Framework
// sources. Substitutes Syphon's own Syphon_Prefix.pch and adds CoreVideo +
// IOSurface (Syphon's static-link build references those constants) and a
// no-op SYPHONLOG for non-DEBUG builds.

#ifdef __OBJC__
#import <Cocoa/Cocoa.h>
#import <CoreVideo/CoreVideo.h>
#import <IOSurface/IOSurface.h>
#import <Metal/Metal.h>

// Keep Syphon's internal diagnostics on regardless of build type. They
// fire only on errors (library-load failures, missing shader functions,
// nil textures, pipeline-create failures) — silencing them in release
// makes the publisher untraceable when something goes wrong.
#ifndef SYPHONLOG
#  define SYPHONLOG(format, ...) NSLog(@"SYPHON: %@: %@", NSStringFromClass([self class]), [NSString stringWithFormat:format, ##__VA_ARGS__])
#endif
#endif
