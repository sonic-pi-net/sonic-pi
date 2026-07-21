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

// Widget-level conveniences over the type scale in dpi.h — the sizes and the
// zoom curve live there so ScalePxInStyleSheet can resolve its `small` /
// `medium` / … keywords from the same table. This header adds the parts that
// need QFont/QWidget, kept out of dpi.h because that one is included almost
// everywhere.
//
// app.qss deliberately carries no font-size: a stylesheet font-size silently
// beats setFont(), which is what made per-pane text zoom impossible to apply
// uniformly. Two rules survive by necessity — QDockWidget::title and
// QTableWidget#linkPeersTable QHeaderView::section are sub-controls, which
// have no widget to set a font on. They pull their px from FontRolePx() via
// SonicPiTheme::reloadStylesheet() so they stay on this scale.
// (stylesheet_invariants.test.cpp pins that.)

#pragma once

#include "dpi.h"

#include <QApplication>
#include <QFont>
#include <QWidget>

inline QFont RoleFont(FontRole role, double scale = 1.0)
{
    QFont f = QApplication::font();
    f.setPixelSize(FontRolePx(role, scale));
    return f;
}

inline void ApplyFontRole(QWidget* w, FontRole role, double scale = 1.0)
{
    if (w)
        w->setFont(RoleFont(role, scale));
}
