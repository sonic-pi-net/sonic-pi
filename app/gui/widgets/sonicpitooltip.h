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

#include <QWidget>
#include <QPointer>
#include <QTimer>
#include <QTextDocument>
#include <QVariantAnimation>

class SonicPiTheme;

// Replacement for stock QToolTip. Instead of a flat, unstyled box at the
// mouse cursor, tips are drawn as a themed bubble anchored to the control
// they describe: centred below it (above when there's no room), with a
// caret pointing at the control, rounded corners, a soft shadow and a
// short fade-in.
//
// Content is structured rather than a single text blob:
//   - body:     the widget's regular toolTip() text (plain or rich text;
//               plain text is word-wrapped automatically — no manual \n
//               needed)
//   - title:    optional bold heading, from the widget property "tipTitle"
//   - shortcut: optional key-cap chip, from the widget property
//               "tipShortcut" (display string, e.g. "⇧⌘L"). Toolbar
//               buttons don't need the property: the chip is derived from
//               their QAction's shortcut automatically.
class SonicPiToolTip : public QWidget
{
    Q_OBJECT

public:
    explicit SonicPiToolTip(SonicPiTheme* theme);

    // Show (or move) the tip. anchorGlobal is the rect of the control in
    // global coordinates; the bubble is placed against it and the caret
    // points at its centre.
    void showTip(const QRect& anchorGlobal, const QString& title,
                 const QString& body, const QString& shortcut);
    void hideTip();

    bool isShowing(const QString& body) const { return isVisible() && body == m_body; }

protected:
    void paintEvent(QPaintEvent* event) override;

private:
    void layoutContent();
    void place(const QRect& anchorGlobal);

    SonicPiTheme* m_theme;

    QString m_title;
    QString m_body;
    QString m_shortcut;

    QTextDocument m_titleDoc;
    QTextDocument m_bodyDoc;

    // metrics computed per show (DPI-scaled)
    int m_padX = 0, m_padY = 0, m_radius = 0;
    int m_caretW = 0, m_caretH = 0, m_shadow = 0;
    int m_chipW = 0, m_chipH = 0;
    QRect m_bubble;        // bubble rect in widget coords (excludes caret + shadow)
    int m_caretX = 0;      // caret tip x in widget coords
    bool m_below = true;   // bubble below the anchor (caret on top edge)

    QVariantAnimation m_fade;
};

// Application-wide event filter that routes every QEvent::ToolTip to
// SonicPiToolTip, so all existing setToolTip() call sites get the new
// popup without modification. Also handles:
//   - per-item tips in item views (QTreeWidget / QListWidget etc.) and
//     per-tab tips in QTabBar, anchored to the item/tab rect
//   - keyboard accessibility: tabbing onto a control shows its tip after
//     a short delay (WCAG 1.4.13 — hover-only help excludes keyboard
//     users); Esc dismisses
//   - dismissal on click, key press, scroll, focus loss, window move and
//     app deactivation
//
// Installs itself on qApp at construction.
class SonicPiToolTipManager : public QObject
{
    Q_OBJECT

public:
    explicit SonicPiToolTipManager(SonicPiTheme* theme, QObject* parent = nullptr);
    ~SonicPiToolTipManager();

protected:
    bool eventFilter(QObject* obj, QEvent* event) override;

private:
    struct Tip
    {
        QString title;
        QString body;
        QString shortcut;
    };

    // Walk up from w (like Qt's own tooltip propagation) to the first
    // widget with a tooltip; fills tip and returns that widget, or
    // nullptr if nothing along the chain has one.
    static QWidget* resolveTip(QWidget* w, Tip& tip);

    // Resolve + anchor + show for a widget under the pointer at globalPos.
    // Returns true when handled (tip shown, or deliberately suppressed —
    // e.g. hovering a group box's body rather than its title); false if
    // nothing along the parent chain has a tip.
    bool showResolvedTip(QWidget* w, const QPoint& globalPos);

    void showTip(QWidget* anchorWidget, const Tip& tip, const QRect& anchorGlobal, bool cursorAnchored);
    void hideTip();
    void onFocusTipTimer();
    void onReshowTimer();

    SonicPiToolTip* m_tip;
    QPointer<QWidget> m_anchorWidget;  // widget the visible tip belongs to
    bool m_cursorAnchored = false;     // anchored to the cursor (large widget), not the widget rect
    QPoint m_cursorAnchor;             // global pos of the cursor anchor
    QTimer m_focusTipTimer;
    QPointer<QWidget> m_focusCandidate;
    // Clicks hide the tip and Qt sends no new ToolTip event until the
    // pointer moves — so after a release the tip (with any refreshed
    // state, e.g. a toggled control) is re-shown if the pointer is still
    // resting on the same control.
    QTimer m_reshowTimer;
    QPointer<QWidget> m_reshowCandidate;
};
