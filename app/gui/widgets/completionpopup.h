//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef COMPLETIONPOPUP_H
#define COMPLETIONPOPUP_H

#include <QWidget>
#include <QList>
#include <QColor>
#include <QIcon>
#include <QModelIndex>

#include "utils/scintilla_api.h"

class QLabel;
class QListView;
class QStandardItemModel;
class QTextBrowser;
class QToolButton;
class QWidget;
class QPropertyAnimation;
class NotePiano;       // mini keyboard for note completions (defined in the .cpp)
class RangeSlider;     // value-picker for bounded opts (defined in the .cpp)
class OptIllustration; // live diagram for a bounded opt (defined in the .cpp)

// A modern, frameless code-completion popup: one row per candidate showing a
// kind badge, the name, and a dimmed one-line summary. It never steals focus
// from the editor — the editor keeps handling keystrokes and forwards
// navigation/accept keys here via moveSelection()/currentText().
class CompletionPopup : public QWidget
{
    Q_OBJECT
public:
    // Roles carried by each model row.
    enum Roles {
        KindRole = Qt::UserRole + 1,
        SummaryRole,
        InsertRole,
        NoteRole,      // MIDI note for kind=="note", or the tonic for chord/scale
        DocRole,       // full docstring (markdown)
        UsageRole,     // canonical calling form (the detail pane's code card)
        IntervalsRole  // chord/scale semitone offsets from the tonic (QList<int>)
    };

    explicit CompletionPopup(QWidget* parent = nullptr);

    // Match the editor's theme so the popup is legible on any colour scheme.
    void applyTheme(const QColor& bg, const QColor& fg,
                    const QColor& selBg, const QColor& selFg);

    // Show (or refresh) the popup with the given candidates. caretTopLeft is the
    // caret position in the parent editor's coordinates and lineHeight its line
    // height; the popup sits just below that line (or above if there's no room).
    // Returns false (and hides) if there is nothing to show.
    bool showItems(const QList<CompletionItem>& items, const QPoint& caretTopLeft,
                   int lineHeight, int preferNote = -1);

    // Match the editor's current (zoomed) code font.
    void setItemFont(const QFont& font, double docPointSize);

    // When false, the popup is a plain word list (no docstring/piano/slider panes).
    void setShowHelp(bool on) { m_showHelp = on; }

    void moveSelection(int delta);   // +1/-1 row, or +/- page (slider: linear nudge)
    QString currentText() const;     // insert text of the selected row ("" if none)
    // A screen-reader phrase for the highlighted row (e.g. "prophet, synth, 1 of 5")
    // or the slider's value text. Empty when there's nothing selected.
    QString currentAnnouncement() const;
    // The selected row's full docstring as plain text (for on-demand SR reading).
    QString currentDoc() const;
    bool isShowing() const;
    bool isSliderMode() const { return m_sliderMode; }  // value picker vs list
    void hidePopup();

    // The name column's x (relative to a row's left), after the kind badge, so
    // names line up regardless of badge width. Used by the delegate.
    int nameColumnX() const { return m_nameColX; }
    // Selection + text colours, read straight by the delegate. Setting a stylesheet
    // re-resolves the widget palette, so QPalette::Highlight can't be relied on;
    // these are the theme values applyTheme() was last given.
    QColor selectionBg() const { return m_selBg; }
    QColor selectionFg() const { return m_selFg; }
    QColor textColor()   const { return m_text; }
    QColor backgroundColor() const { return m_bg; }
    // Row whose inline ▶ glyph the pointer is over (-1: none) — sample rows
    // paint the glyph hot when it's theirs. Read by the delegate.
    int playHoverRow() const { return m_playHoverRow; }

protected:
    void paintEvent(QPaintEvent*) override;   // rounded background + border
    void resizeEvent(QResizeEvent*) override; // keeps the close button pinned top-right
    // Swaps the close cross to its high-contrast variant on hover: a
    // stylesheet-styled QToolButton doesn't reliably use QIcon::Active.
    bool eventFilter(QObject* obj, QEvent* ev) override;

signals:
    // A piano key was clicked — the editor should insert the current selection.
    void accepted();
    // The close button was clicked — the editor should cancel completion
    // exactly as it does for Escape (revert the preview, then hide).
    void dismissRequested();
    // The previewed text changed (list navigation, note selection, slider drag):
    // the editor live-previews `text` in the buffer in place of the typed word.
    void previewChanged(const QString& text);
    // The "Docs" button was clicked — the editor should open the help pane for
    // this keyword (same as C-i over the word).
    void docsRequested(const QString& name);
    // A piano key was clicked on a synth/fx row — run `code` to audition the
    // highlighted instrument at that pitch (the docs pane's preview UX).
    void auditionRequested(const QString& code);
    // The highlighted entry changed via explicit navigation: the editor relays
    // `text` to the screen reader so suggestions are spoken while focus (and the
    // typing echo) stay in the editor. Emitted on arrow/page/slider moves only —
    // never on per-keystroke refiltering, so it doesn't talk over typing.
    void announceRequested(const QString& text);

private:
    void announceSelection();     // emit announceRequested() for the current row
    // Height of the strip reserved above all regions for the close button.
    int closeGlyphPx() const;
    void resizeToContents();
    void computeColumns();
    void updateDetail();          // refresh the docstring/piano for the current row
    void restyleDetailHeader();   // re-style the title/usage card (theme or font change)
    bool selectNote(int midi);    // highlight the (number) item for a MIDI note
    // Resize the popup to (w, h): instant on first show, otherwise a quick tween
    // so the popup grows/shrinks smoothly instead of snapping between shapes.
    void setPopupSize(int w, int h);

    int m_nameColX = 0;

    // Last summary+doc rendered into the detail pane, so navigation/filtering that
    // lands on the same row's content doesn't re-parse the whole docstring.
    QString m_detailKey;

    // Current docstring font size (pt). The detail pane is stylesheet-governed, so
    // its font is driven via a stylesheet font-size (QWidget::setFont is ignored on
    // styled widgets), updated on zoom independently of the list font.
    int m_docPointSize = -1;

    // When a piano key with no matching list row is clicked, the MIDI note to
    // insert (currentText() returns this); cleared once a real row is selected.
    QString m_noteOverride;

    // Per-session layout state, so width doesn't jitter while filtering/navigating.
    bool m_hasDetail = false;   // this list has docstrings → reserve the detail pane
    bool m_noteMode = false;    // this list is notes → show the piano
    bool m_chordMode = false;   // this list is chords/scales → piano shows their notes
    bool m_auditionMode = false; // synth/fx row → piano clicks audition the instrument
    bool m_sliderMode = false;  // this is a bounded opt → show the value slider
    bool m_showHelp = true;     // show the docstring/piano/slider helper panes
    int m_sessionListW = 0;     // grow-only list width for the current session

    QListView* m_view = nullptr;
    QStandardItemModel* m_model = nullptr;
    QWidget* m_detailPane = nullptr;     // right-hand column: docstring + Docs button
    QLabel* m_detailTitle = nullptr;     // the row's one-line summary, accent header
    QLabel* m_usageCard = nullptr;       // canonical calling form in a rounded code card
    QTextBrowser* m_detail = nullptr;    // scrollable rich-text docstring (right of the list); QTextBrowser for in-doc anchor jumps
    QToolButton* m_docsButton = nullptr; // "Docs ↗" — opens the help pane for the row
    QToolButton* m_playButton = nullptr; // "▶ Play" — auditions the highlighted sample
    // Audition `:name` through the shared preview path (real-time, no log).
    void auditionSample(const QString& name);
    // The inline ▶ glyph's rect for a sample row (viewport coords), mirroring
    // the delegate's paint math; null rect for non-sample rows.
    QRect samplePlayRect(const QModelIndex& idx) const;
    int m_playHoverRow = -1;
    QToolButton* m_closeButton = nullptr; // "×" top-right — dismiss, same as Escape
    int m_closeIconPx = -1;               // last icon size rendered (avoids re-render per tween frame)
    // Last global pointer position seen over the list. Hover only follows
    // REAL movement — not the popup appearing under a parked cursor, nor rows
    // scrolling beneath it — so the selection (and its live preview) can't
    // change without user intent.
    QPoint m_lastHoverGlobal = QPoint(-1, -1);
    // Apex of the "safe triangle" to the detail pane: where the selection was
    // last set. Crossing rows inside the triangle (pointer en route to the
    // docs/keyboard) must not switch the selection.
    QPoint m_hoverAnchorGlobal = QPoint(-1, -1);
    bool inDetailSafeTriangle(const QPoint& g) const;
    QIcon m_closeIconNormal;              // muted cross (resting)
    QIcon m_closeIconHover;               // high-contrast cross (on the accent pill)
    // Re-render the close glyph (a painted cross, so it centres exactly —
    // the × text glyph sits low in its font box) at `side` px.
    void updateCloseIcon(int side);
    NotePiano* m_piano = nullptr;        // mini keyboard (bottom, for notes)
    RangeSlider* m_rangeSlider = nullptr; // value slider (for bounded opts)
    OptIllustration* m_optIllo = nullptr; // live diagram beneath the slider
    bool m_hasIllo = false;               // the current opt has an illustration
    OptIllustration* m_shapeIllo = nullptr; // waveform/curve shape in the detail pane
    QString m_enumIllo;                   // "wave"/"curve" for the current enum list
    QPropertyAnimation* m_sizeAnim = nullptr; // tweens the popup between shapes
    QSize m_targetSize;                  // the size the popup is animating toward
    QColor m_bg = QColor(30, 30, 30);    // painted background (rounded)
    QColor m_border = QColor(127, 127, 127); // painted border
    QColor m_selBg = QColor(0xff, 0x14, 0x93);   // selection fill (theme accent)
    QColor m_selFg = QColor(255, 255, 255);      // selected-row text
    QColor m_text  = QColor(220, 220, 220);      // normal row text
};

#endif // COMPLETIONPOPUP_H
