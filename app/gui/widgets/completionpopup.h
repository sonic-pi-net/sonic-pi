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

#include "utils/scintilla_api.h"

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
    void sliderNudgeLog(int steps);  // slider: logarithmic (proportional) nudge
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

protected:
    void paintEvent(QPaintEvent*) override;   // rounded background + border

signals:
    // A piano key was clicked — the editor should insert the current selection.
    void accepted();
    // The previewed text changed (list navigation, note selection, slider drag):
    // the editor live-previews `text` in the buffer in place of the typed word.
    void previewChanged(const QString& text);
    // The "Docs" button was clicked — the editor should open the help pane for
    // this keyword (same as C-i over the word).
    void docsRequested(const QString& name);
    // The highlighted entry changed via explicit navigation: the editor relays
    // `text` to the screen reader so suggestions are spoken while focus (and the
    // typing echo) stay in the editor. Emitted on arrow/page/slider moves only —
    // never on per-keystroke refiltering, so it doesn't talk over typing.
    void announceRequested(const QString& text);

private:
    void announceSelection();     // emit announceRequested() for the current row
    void resizeToContents();
    void computeColumns();
    void updateDetail();          // refresh the docstring/piano for the current row
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
    bool m_sliderMode = false;  // this is a bounded opt → show the value slider
    bool m_showHelp = true;     // show the docstring/piano/slider helper panes
    int m_sessionListW = 0;     // grow-only list width for the current session

    QListView* m_view = nullptr;
    QStandardItemModel* m_model = nullptr;
    QWidget* m_detailPane = nullptr;     // right-hand column: docstring + Docs button
    QTextBrowser* m_detail = nullptr;    // scrollable rich-text docstring (right of the list); QTextBrowser for in-doc anchor jumps
    QToolButton* m_docsButton = nullptr; // "Docs ↗" — opens the help pane for the row
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
};

#endif // COMPLETIONPOPUP_H
