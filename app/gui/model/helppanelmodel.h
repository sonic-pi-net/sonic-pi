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

#ifndef HELPPANELMODEL_H
#define HELPPANELMODEL_H

// The help panel's one state: away, beside the code, or full size. The Help
// icon, the divider's grips, the menu items that open a pane, the double-click
// on the divider and focus mode all move it through these transitions — the
// web's (main.js: setPanel, setPanelFull, toggleBottom, growBottom) — and
// MainWindow::applyHelpPanel derives everything on screen from it: the dock,
// the editor's room, the icon, which grip shows and which way it points, the
// bar at the editor's foot. No control keeps an idea of the panel of its own,
// so none can disagree with another. Pure: gui-tests pins every transition.
namespace SonicPi
{
class HelpPanelModel
{
public:
    enum class State { Away, Beside, Full };

    // What the divider's hide grip does from here: the one source of its
    // tooltip and its accessible name, so neither can go stale.
    enum class GripAction { Hide, Show, Beside };

    State state() const { return m_state; }

    // The Help icon, the divider's down grip, the shortcut, a double-click on
    // the divider: one action. Full size steps back beside the code first,
    // beside the code goes away, away comes back beside the code.
    State toggle()
    {
        switch (m_state)
        {
        case State::Full:   m_state = State::Beside; break;
        case State::Beside: m_state = State::Away;   break;
        case State::Away:   m_state = State::Beside; break;
        }
        return m_state;
    }

    // The up grip: full size, from beside the code only (there is nothing to
    // make full while the panel is away).
    State full()
    {
        if (m_state == State::Beside) m_state = State::Full;
        return m_state;
    }

    // A pane asked for (a docs tab, the cards, the logs, the welcome screen):
    // the panel beside the code if it was away; a full-size panel keeps the
    // room, the reader is reading at length.
    State show()
    {
        if (m_state == State::Away) m_state = State::Beside;
        return m_state;
    }

    // Away, whatever it was.
    State away()
    {
        m_state = State::Away;
        return m_state;
    }

    // A drag on the divider: up while the panel is away reveals it, as the
    // web's drawer divider does; down from full size gives the editor its
    // room back. Beside the code either way.
    State beside()
    {
        m_state = State::Beside;
        return m_state;
    }

    // The divider dragged to the foot puts the panel away, as the web's does
    // within 70px of it. The panel is never shorter than its minimum, so the
    // snap height is that or the 70px, whichever is more: a panel dragged as
    // short as it goes is being put away, not kept squashed.
    static int snapHeight(int seventyPx, int minimumHeight)
    {
        return seventyPx > minimumHeight ? seventyPx : minimumHeight;
    }
    static bool dragEndsAway(int height, int snapHeight) { return height <= snapHeight; }

    // Focus mode: the panel goes away for the code, and comes back beside it
    // afterwards — not full size, which is not remembered, as on the web.
    void enterFocus()
    {
        m_beforeFocus = m_state;
        m_state = State::Away;
    }
    State leaveFocus()
    {
        m_state = m_beforeFocus == State::Away ? State::Away : State::Beside;
        return m_state;
    }

    // What the controls derive, and nothing else.
    bool panelVisible() const { return m_state != State::Away; }
    bool editorVisible() const { return m_state != State::Full; }
    bool iconLit() const { return panelVisible(); }
    bool hideGripPointsUp() const { return m_state == State::Away; }   // the way back
    GripAction hideGripAction() const
    {
        switch (m_state)
        {
        case State::Away:   return GripAction::Show;
        case State::Full:   return GripAction::Beside;
        case State::Beside: break;
        }
        return GripAction::Hide;
    }
    bool fullGripVisible() const { return m_state == State::Beside; }   // not away, not once full
    bool awayBarVisible() const { return m_state == State::Away; }

private:
    State m_state = State::Away;
    State m_beforeFocus = State::Away;
};
} // namespace SonicPi

#endif // HELPPANELMODEL_H
