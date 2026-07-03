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

#pragma once

namespace SonicPi
{

// Local accessibility self-test (--selftest-accessibility), Windows edition.
// Drives the real UI Automation bridge — the layer Narrator/NVDA/JAWS talk
// to — and exits with 0 (pass) / 1 (fail). Counterpart of the macOS
// NSAccessibility self-test in platform/macos.mm.
int runAccessibilitySelfTest();

} // namespace SonicPi
