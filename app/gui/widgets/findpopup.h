//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef FINDPOPUP_H
#define FINDPOPUP_H

#include <QWidget>
#include <QColor>

class QLineEdit;
class QLabel;
class QToolButton;
class QPropertyAnimation;

// A find bar floated over the editor's top-right corner: type-to-search with
// every match highlighted in the buffer, a live "n of m" counter, and
// Enter/Shift+Enter (or F3 / Ctrl+S / Ctrl+R) to cycle through matches.
// The editor owns the searching and highlighting (SonicPiScintilla::
// refreshFind); this widget is just the chrome. Unlike the completion popup
// it deliberately takes focus — the query field is where you type.
class FindPopup : public QWidget
{
    Q_OBJECT
public:
    explicit FindPopup(QWidget* parent);

    // An editor-coloured pill with a strong full-contrast ring (the metro
    // pills' white-ring-on-dark language; near-black on light themes) — it
    // must read instantly over code in every theme without being garish.
    // accentText is the legible text colour on an accent fill
    // (SonicPiTheme::contrastingText) for the checked Aa toggle.
    void applyTheme(const QColor& surface, const QColor& surfaceText,
                    const QColor& border, const QColor& accent,
                    const QColor& accentText);

    // Show, slide in, focus the field and select-all. A null seed keeps the
    // query shared across buffers/openings (like every modern editor); a
    // non-null seed (the editor's selection) replaces it.
    void open(const QString& seed);
    void closePopup();
    bool isOpen() const { return isVisible(); }
    bool editHasFocus() const;

    QString query() const;
    // Aa toggle: force exact case. Off = smart-case (an all-lowercase query
    // matches any case; a capital anywhere makes it exact).
    bool forceCase() const;

    // current is 1-based. total 0 = query matched nothing (shows "No results");
    // total -1 = empty query (counter hidden).
    void setMatchStatus(int current, int total);

    // Anchor to the parent viewport's top-right corner.
    void reposition();

    // Track the editor zoom (delta from the default zoom level) so the bar
    // stays legible when the editor is blown up for a projector.
    void setZoom(int delta);

signals:
    void queryChanged(const QString& query);
    void nextRequested();
    void prevRequested();
    // abortToOrigin: Ctrl+G (emacs isearch abort) returns the caret to where
    // the search started; Escape/close keep it on the current match.
    void closeRequested(bool abortToOrigin);

protected:
    void paintEvent(QPaintEvent*) override;   // rounded pill background + border
    bool eventFilter(QObject* obj, QEvent* ev) override;

private:
    void restyle();
    qreal zoomScale() const;
    void applyZoomMetrics();   // size + icon/font metrics at the current zoom

    int m_zoom = 0;

    QLineEdit* m_edit = nullptr;
    QLabel* m_icon = nullptr;
    QLabel* m_count = nullptr;
    QToolButton* m_prev = nullptr;
    QToolButton* m_next = nullptr;
    QToolButton* m_case = nullptr;
    QToolButton* m_close = nullptr;
    QPropertyAnimation* m_slide = nullptr;

    QColor m_bg = QColor(30, 30, 30);
    QColor m_border = QColor(127, 127, 127);
    QColor m_fg = QColor(220, 220, 220);
    QColor m_accent = QColor(0xff, 0x14, 0x93);
    QColor m_accentText = QColor(255, 255, 255);
    bool m_noMatches = false;   // tints the border/counter as a quiet alert
    bool m_editFocused = false; // accent focus ring (house docsFilter language)

    // The Aa toggle is shared across all editors (the query itself is shared
    // by SonicPiScintilla), so find behaves as one feature, not ten.
    static bool s_forceCase;
};

#endif // FINDPOPUP_H
