//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef CHROME_METRICS_H
#define CHROME_METRICS_H

// Height of the dock-chrome controls (the left help-tab strip's square side,
// the buffer selector tabs, the transport/Link button) so they share one
// scale and line up. Design pixels; scale with ScaleHeightForDPI at each
// use site.
namespace SonicPi
{
constexpr int kChromeControlDp = 34;
}

#endif // CHROME_METRICS_H
