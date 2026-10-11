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

#pragma once

#include <QDockWidget>

namespace SonicPi
{

// Whether a dock is on, by its own setting. Before the window is first shown,
// isVisible() is false for every widget in it, whatever it is set to: a state
// that follows a dock and reads that at startup reads every dock as off. The
// help state did, after the saved layout was restored; it took the help pane
// for hidden, put its 1px placeholder bar up beside it, and as the window
// appeared the row squeezed to the pane's minimum. The bar came down a moment
// later, the squeeze stayed, and the pane came back a sliver on every launch.
inline bool dockShown(const QDockWidget* dock)
{
    return dock && !dock->isHidden();
}

} // namespace SonicPi
