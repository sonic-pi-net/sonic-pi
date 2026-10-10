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

#ifndef CODECARD_H
#define CODECARD_H

#include <QFrame>
#include <QHash>
#include <QIcon>
#include <QPoint>
#include <QPointer>
#include <QSet>
#include <QString>
#include <QStringList>

#include "dpi.h"
#include "utils/tablericons.h"

class QLabel;
class QPlainTextEdit;
class QPushButton;
class QScrollArea;
class QVBoxLayout;
class QWidget;
class CardScope;
class TutProseText;
class SonicPiTheme;

namespace SonicPi
{
class SonicPiAPI;
}

// A code card: a titled piece of code that plays, drawn as the web draws it
// (app/web/app/src/ui/card.js) for its quickstart, docs and tutorial. An accent
// header with the title and the card's actions; the code, which Edit makes
// yours to change; a footer with a description, what the program puts and any
// error, beside Play and Stop — each a disc in a ring, the left channel round
// Play and the right round Stop.
//
// What the card does is its own: its actions, its editing, its keyboard, being
// dragged onto the editor, lighting the line that sounded, its rings while it
// plays. What it is part of is its host's: where it sits and how big, which
// card is next, what Play and Stop do to the runs the host keeps. Hover is the
// host's to drive (hoverAt), because a card can slide beneath a pointer that
// never moved.
class CodeCard : public QFrame
{
    Q_OBJECT
public:
    enum Action { Edit = 1, Reset = 2, Copy = 4, Add = 8, Drag = 16, Open = 32 };

    struct Spec
    {
        QString title;
        QString code;                        // as written
        QString blurb;                       // rich text; may be empty
        int actions = Edit | Reset | Copy | Add | Drag;
        bool runnable = true;                // a fragment to read is still: no transport
        // The code takes the caret and selection, so a page's reading path
        // runs through it; in a deck the card is one stop and drags whole.
        bool readable = false;
        unsigned int scopeSlot = 0;          // the scope-stream slot its runs draw on
        QString key;                         // where an edit is kept; empty keeps none
    };

    // The sizes a host lays its cards out to, in pixels. Zero is "as the
    // content needs": a deck gives every card one size, a page lets each be
    // as tall as its code.
    struct Metrics
    {
        int width = 0;
        int codeBodyHeight = 0;
        int footerHeight = 0;
        int scopeSide = 0;
        int blurbWidth = 0;
        int codeFontPx = 0;
        int titlePx = 0;
        double zoomFactor = 1.0;             // the host's, which padding grows with
    };

    CodeCard(const Spec& spec, const Metrics& metrics, SonicPiTheme* theme,
             QWidget* parent = nullptr);

    const Spec& spec() const { return m_spec; }
    const QString& title() const { return m_spec.title; }
    // The code as it stands: as edited, or as written.
    QString code() const { return m_code; }
    bool isEdited() const { return m_code != m_spec.code; }
    bool isEditing() const { return m_editor != nullptr; }
    // What lands in the editor, by every route: the code with blank lines
    // round it. The title rides on the drag image, not in the editor.
    QString payload() const { return QStringLiteral("\n%1\n\n").arg(m_code); }
    // The live_loops the code defines, so a host can see one card take a
    // loop over from another.
    QSet<QString> loopNames() const;

    // Playing: Stop is live, the border lit, and the rings draw the card's
    // slot from the api.
    void setPlaying(bool playing, SonicPi::SonicPiAPI* api);
    bool isPlaying() const { return m_playing; }
    // Waiting on the engine to start the run: an arc goes round Play.
    void setBooting(bool booting);
    // The program's error, in the footer; empty clears it.
    void setError(const QString& message);
    // What the program puts: the last few lines show.
    void appendOutput(const QString& line);
    void clearOutput();

    // A run's line, counted from 1; the first is the with_fx :scope_out the
    // code is wrapped in, so the card's own lines start at 2.
    void flashLine(int runLine);

    // Where the pointer is, and whether the card may light for it at all (on
    // the host, meaningfully visible). Lights the border, swaps the glyph of
    // the button beneath, lights the ring, and projects Add's preview.
    void hoverAt(const QPoint& globalPos, bool eligible);

    // The body's height after all: the room a host has for it (an example
    // fitted to its page), which the code scrolls within.
    void setCodeBodyHeight(int px);

    QPushButton* playButton() const { return m_play; }
    QPushButton* stopButton() const { return m_stop; }
    QScrollArea* body() const { return m_body; }
    // The code as it reads, while it isn't being edited.
    TutProseText* codeText() const { return m_text; }

    static constexpr int kOutputLines = 3;
    // How tall `lines` lines of code are at this size, as a card lays them out:
    // a deck gives every card's body room for its longest.
    static int codeLinesHeight(int lines, int codeFontPx);

signals:
    // Play: run the code (again, while it plays: the new run takes over).
    void playRequested();
    // Stop: every run the card started.
    void stopRequested();
    void insertRequested(const QString& title, const QString& code);
    void copyRequested(const QString& title, const QString& code);
    void openRequested(const QString& title, const QString& code);
    void insertPreviewRequested(const QString& title, const QString& code);
    void insertPreviewCleared();
    void announceRequested(const QString& message);
    void dragEnded();
    // Left/Right on the card: the previous or next card, the host's to find.
    void stepRequested(int delta);
    // Pressed and released where it was pressed: not the start of a drag.
    void clicked();
    void editedChanged(bool edited);

protected:
    bool eventFilter(QObject* obj, QEvent* event) override;
    void resizeEvent(QResizeEvent* event) override;

private:
    // The code goes into the editor at the cursor from the keyboard (I, the
    // context menu) when the card has Add, or a drag handle: Insert is the
    // pointer-only handle's keyboard equivalent.
    bool insertable() const { return m_spec.actions & (Add | Drag); }
    // Padding and spacing grow with the host's zoom, as its type does.
    UiScale uiScale() const { return UiScale(m_m.zoomFactor); }
    QPushButton* makeButton(TablerIcons::Glyph glyph, int iconPx, int w, int h);
    void setButtonGlyph(QPushButton* b, TablerIcons::Glyph glyph);
    QWidget* makeScopeBox(QWidget* parent, int side, bool stop);
    QPixmap disc(bool stop, bool hover, bool enabled, int d) const;
    void refreshDiscs();
    void refreshRings();
    void renderLines();
    // The code's text as every card dresses it (and codeLinesHeight measures).
    static void dressCodeText(TutProseText* text, int codeFontPx);
    void fitBody();
    void setEditing(bool editing);
    void setCode(const QString& code);
    void remember();
    void refreshFooter();
    QPixmap dragPixmap() const;
    void startDrag();
    void showHoverIcon(QPushButton* b);
    void restyle();

    Spec m_spec;
    Metrics m_m;
    SonicPiTheme* m_theme;
    QString m_code;
    bool m_playing = false;
    bool m_booting = false;
    bool m_hover = false;
    QString m_error;
    QStringList m_output;

    QVBoxLayout* m_layout = nullptr;
    QScrollArea* m_body = nullptr;
    QWidget* m_codeBlock = nullptr;
    TutProseText* m_text = nullptr;
    QPlainTextEdit* m_editor = nullptr;
    QWidget* m_footer = nullptr;
    QLabel* m_blurb = nullptr;
    QLabel* m_errorLabel = nullptr;
    QLabel* m_outputLabel = nullptr;
    CardScope* m_playScope = nullptr;
    CardScope* m_stopScope = nullptr;
    QPushButton* m_play = nullptr;
    QPushButton* m_stop = nullptr;
    QPushButton* m_edit = nullptr;
    QPushButton* m_reset = nullptr;
    QPushButton* m_add = nullptr;
    // Each icon button's glyph at rest and under the pointer.
    QHash<QPushButton*, QIcon> m_iconNormal;
    QHash<QPushButton*, QIcon> m_iconHover;
    QPointer<QPushButton> m_hoverButton;
    // Pressing here and moving drags the card; pressing and letting go is a
    // click.
    QSet<QObject*> m_dragHandles;
    QObject* m_dragSource = nullptr;
    QPoint m_pressPos;
    QPoint m_pressGlobal;
};

#endif // CODECARD_H
