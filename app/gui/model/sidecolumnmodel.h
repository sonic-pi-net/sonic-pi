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

#ifndef SIDECOLUMNMODEL_H
#define SIDECOLUMNMODEL_H

// The column beside the code — the scope, log, cues and metronome panes —
// has one state of its own: with the code, or away. Each pane keeps its own
// setting (the View menu's); the column away hides them all without touching
// a setting, and back brings back the ones whose setting is on. The grip on
// the editor | column divider moves it (MainWindow::sideToggle), and
// MainWindow::applySideColumn derives the panes, the grip and the bar at the
// editor's right edge from it, as applyHelpPanel does for the help panel.
// Pure: gui-tests pins it.
namespace SonicPi
{
class SideColumnModel
{
public:
    bool away() const { return m_away; }

    // The grip: away, or back.
    bool toggle()
    {
        m_away = !m_away;
        return m_away;
    }

    // Whether a pane shows: its own setting, unless the column is away.
    bool paneVisible(bool setting) const { return setting && !m_away; }

    // What the controls derive, and nothing else.
    bool gripPointsLeft() const { return m_away; }    // the way back: the panes return from the right
    bool awayBarVisible() const { return m_away; }

private:
    bool m_away = false;
};
} // namespace SonicPi

#endif // SIDECOLUMNMODEL_H
