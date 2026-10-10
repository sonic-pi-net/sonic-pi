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

#ifndef TUTORIALPANE_H
#define TUTORIALPANE_H

#include <QFrame>
#include <QIcon>
#include <QUrl>
#include <QVector>

#include <memory>

#include "utils/tutorialdocs.h"

class QGridLayout;
class QHBoxLayout;
class QLabel;
class QPushButton;
class QScrollArea;
class QVBoxLayout;
class SonicPiTheme;
class TutDial;
class TutSelectionGroup;

namespace SonicPi
{
class SonicPiAPI;
}

// Native replacement for the QTextBrowser docs pane: a scrollable column of
// heading/prose/code blocks parsed from the tutorial markdown. Code blocks
// are lightweight syntax-highlighted labels with play/stop/copy controls;
// play runs the snippet as its own job so stop only stops that snippet.
class TutorialPane : public QFrame
{
    Q_OBJECT
public:
    explicit TutorialPane(SonicPiTheme* theme, QWidget* parent = nullptr);

    // The jukebox scope reads an isolated scope-buffer slot straight from the
    // engine shm; the pane needs the API handle to fetch a reader per play.
    void setAudioApi(std::shared_ptr<SonicPi::SonicPiAPI> api);

    // imagesRoot is the on-disk dir that image paths are relative to
    // (rootPath()/etc/doc/images). Empty prev/next titles hide that button.
    void loadChapter(const SonicPi::TutorialChapter& chapter, const QString& imagesRoot,
                     const QString& prevTitle, const QString& nextTitle);

    // An example (the Examples tab): one card, as the web's Examples page
    // has it, its edit kept under `key`.
    void showExamplePage(const QString& key, const QString& title, const QString& code,
                         const QString& blurb);

    // Interactive FX/synth pages: icon + doc + per-opt dials (true per-synth
    // defaults) that live-regenerate a playable snippet
    void showInstrumentPage(bool isFx, const SonicPi::InstrumentPage& page);
    // Samples group page: one playable snippet per sample
    void showSampleGroupPage(const SonicPi::SampleGroup& group);
    // Lang reference page: usage, doc, runnable examples
    void showLangPage(const SonicPi::LangPage& page);

    // Play the page's first card (or snippet); false if there is none
    bool playFirstSnippet();

    // Land keyboard/screen-reader focus on the page's content: the Examples
    // editor when that page is showing, else the first readable block. Called
    // by MainWindow when a menu/list action navigates here, and internally
    // after a rebuild that destroyed the focused widget (without this, focus
    // falls back to the window title bar and a screen-reader user is dumped
    // at the top of the interface).
    void focusContent();

    void applyTheme();
    // The pane owns its zoom (A-/A+, persisted) — deliberately independent
    // of the per-buffer editor zoom
    int userZoom() const { return m_userZoom; }
    void setUserZoom(int zoom);
    // Font multiplier the current zoom step works out to; the docs nav lists
    // track the same figure so the whole help tab scales as one.
    double fontScale() const { return m_fontScale; }

    // The A-/A+ text-size buttons, packed in a standalone widget so the help
    // dock can host them at the foot of its tab rail.
    QWidget* zoomControls() const;

    // Keyboard scrolling (docScrollUp/Down shortcuts): one scroll step
    void scrollStep(int direction);

    // Copy the pane's current selection (prose/code group, or the Examples
    // editor when focused). The app-wide Copy action routes here when focus
    // is inside the pane, so Cmd/Ctrl+C copies what the user selected in the
    // docs rather than the editor buffer.
    void copySelection();

    // Wire these to QtAPIClient's RunStartedReceived/RunEndedReceived
    void runStarted(int jobId, const QString& workspace);
    void runEnded(int jobId);
    // A card's sounding line, what its run puts and the error that ended it
    // (QtAPIClient's FlashReceived, RunOutputReceived, RunErrorReceived).
    void flashLine(const QString& workspace, int line);
    void runOutput(int jobId, const QString& text);
    void runError(int jobId, const QString& message, int line);

protected:
    // Instrument pages are playable: Space toggles the demo, QWERTY piano
    // keys (a w s e d f t g y h u j k o l p) play notes up from the note dial
    void keyPressEvent(QKeyEvent* event) override;
    // Hover recolour for the zoom -/+ glyph buttons.
    bool eventFilter(QObject* obj, QEvent* event) override;
    // An example's card keeps fitting the pane.
    void resizeEvent(QResizeEvent* event) override;

signals:
    // scopeTap wraps the run in an fx_scope_out tap so the jukebox scope can
    // show only this run's audio (used by the Examples page play button).
    void runRequested(const QString& code, const QString& workspace, bool silent = false,
                      bool scopeTap = false);
    void stopJobRequested(int jobId);
    void loadRequested(const QString& code); // load into the current editor buffer
    // A card's code into the editor, as the Cards tab's go: at the cursor
    // (Add), previewed there while Add is hovered, or dragged; and onto the
    // clipboard.
    void insertRequested(const QString& title, const QString& code);
    void insertPreviewRequested(const QString& title, const QString& code);
    void insertPreviewCleared();
    void dragEnded();
    void copyRequested(const QString& title, const QString& code);
    void announceRequested(QString msg);
    void navigateRequested(int delta); // -1 previous chapter, +1 next
    void linkClicked(const QUrl& url);
    // A-/A+ moved the pane's zoom: the help dock's nav lists follow it, and
    // MainWindow persists the new step.
    void zoomChanged(int zoom);

private:
    struct Snippet
    {
        QFrame* frame = nullptr;
        class TutProseText* codeView = nullptr;
        QPushButton* play = nullptr;
        QPushButton* stop = nullptr;
        QPushButton* copy = nullptr;
        QGridLayout* codeArea = nullptr; // grid hosting the code + corner controls
        bool realTime = false; // run with use_real_time, like the piano keys
        QString code;
        QString workspace;
        int jobId = -1;
    };

    // Zoom-aware pixel metrics. Every content size is laid out through these
    // rather than the bare DPI helpers, so a page built at 2x text gets 2x
    // padding, column widths and glyphs to sit in — scaling the font alone is
    // what left labels overlapping their neighbours.
    int sx(int px) const;
    int sy(int px) const;

    // Re-render whatever page is showing at the current zoom, keeping the
    // scroll position and the playground's dial values. Layout geometry is
    // baked in at build time, so a zoom step has to rebuild, not just restyle.
    void redisplayCurrentPage();
    // Container margins/spacing that live outside the page build.
    void applyShellSizing();

    // Emits announceRequested unless this is a zoom rebuild of the same page.
    void announcePage(const QString& title);

    void rebuild();
    void clearContent();
    void beginPage();
    void endPage(const QString& announceTitle);
    void applySizing();
    void applyContentTheme();
    void regenerateInstrumentCode();
    // html overrides the default highlight rendering (the playground snippet
    // embeds editable-number anchors).
    void setSnippetCode(int index, const QString& code, const QString& html = QString());
    // Inline editor for one opt value, wired to its dial (from code anchors).
    void openOptEditor(const QString& optName, const QPoint& globalPos);
    void addHeading(int level, const QString& text);
    void addProse(const QString& richText);
    void addList(const SonicPi::TutorialBlock& block);
    void addImage(const SonicPi::TutorialBlock& block);
    // `into` nests the snippet inside another card (the instrument playground)
    // instead of appending it to the page column. `transportInto` relocates
    // the play/stop/copy buttons into an external row (the playground's
    // trigger row) instead of a strip under the code.
    void addSnippet(const QString& code, bool runnable = true, QVBoxLayout* into = nullptr,
                    QHBoxLayout* transportInto = nullptr);
    // A code block as a card, as the web's tutorial and docs have them: named
    // for the heading over it, numbered after the first under one heading.
    // Its code joins the page's reading path.
    class CodeCard* addCard(const QString& code, bool runnable, int actions,
                            const QString& blurb = QString(), const QString& key = QString());
    // The example's card, its code given the room the pane has.
    void fitExampleCard();
    // The px a stylesheet pt size comes to at the pane's zoom (the cards are
    // sized in px).
    int zoomedPtPx(int basePt) const;
    void addOptsGrid(const QVector<SonicPi::InstrumentOpt>& opts);
    void addNavFooter();
    // Caret plumbing for a prose/code block: continuous reading across
    // blocks and keeping the caret scrolled into view.
    void wireProse(class TutProseText* label);
    void focusAdjacentText(class TutProseText* from, int direction);
    void ensureGlobalRectVisible(const QRect& globalRect);
    // Re-focus the page content when the widget that held focus was
    // destroyed by a page (re)build.
    void restoreFocusAfterBuild();
    void setSnippetPlaying(Snippet& snippet, bool playing);
    void renderFxIcon(QLabel* iconLabel);
    void toggleDemo();
    void playKeyboardNote(int semitoneOffset);
    void shiftOctave(int delta);
    QString instrumentOpts() const; // ", opt: val" for the non-default dials (excl. note)
    QString proseColoured(const QString& richText) const;

    SonicPiTheme* m_theme = nullptr;
    QScrollArea* m_scroll = nullptr;
    QWidget* m_content = nullptr;
    QVBoxLayout* m_column = nullptr;

    std::shared_ptr<SonicPi::SonicPiAPI> m_spAPI;

    // Enough of the last page's source to rebuild it on a zoom step (all
    // cheap value types; the chapter lives in m_chapter as before).
    enum class PageKind
    {
        None,
        Chapter,
        Example,
        Instrument,
        SampleGroup,
        Lang
    };
    PageKind m_pageKind = PageKind::None;
    // Set while redisplayCurrentPage() re-runs a page build: the content is
    // unchanged, so it must not re-announce the title to the screen reader.
    bool m_redisplaying = false;
    SonicPi::InstrumentPage m_instrumentPage;
    SonicPi::SampleGroup m_sampleGroupPage;
    SonicPi::LangPage m_langPage;
    QString m_exampleKey;
    QString m_exampleTitle;
    QString m_exampleCode;
    QString m_exampleBlurb;
    class CodeCard* m_exampleCard = nullptr;

    SonicPi::TutorialChapter m_chapter;
    QVector<TutDial*> m_dials;
    QVector<class TutProseText*> m_proseLabels;
    // Every caret-navigable block (prose, code, opt docs) in page order —
    // the path continuous reading follows across block boundaries.
    QVector<class TutProseText*> m_readingOrder;
    // Set by clearContent when the focused widget is about to be destroyed.
    bool m_restoreFocus = false;
    std::shared_ptr<TutSelectionGroup> m_selGroup;
    class TutPiano* m_piano = nullptr;
    QLabel* m_octaveLabel = nullptr;
    int m_octave = 0; // keyboard octave shift, persists across pages
    bool m_pageIsFx = false;
    QString m_pageName;
    QString m_imagesRoot;
    QString m_prevTitle;
    QString m_nextTitle;
    QVector<Snippet> m_snippets;
    // The page's cards' runs, one card at a time; a run outlives its card, so
    // a zoom's rebuild finds it playing.
    class CardDeck* m_deck = nullptr;
    QString m_pageId;                 // names the page's cards' workspaces
    QString m_cardSection;            // the heading the next card is named for
    QHash<QString, int> m_cardCounts; // cards so far under each heading
    int m_cardIndex = 0;              // cards so far on the page
    class ZoomBar* m_zoomBar = nullptr; // shared A-/A+ bar, hosted at the foot of the help's tab rail
    SonicPi::CodeColours m_codeColours;
    QIcon m_playIcon;
    QIcon m_stopIcon;
    QIcon m_copyIcon;
    QIcon m_copiedIcon; // check-mark flash after a successful copy
    QHash<QString, QWidget*> m_optRows; // opt name → its doc-table row, for jump links
    QWidget* m_optIndex = nullptr;      // the quick-index panel; row names jump back to it
    int m_pianoBaseNote = 52; // page-default note the QWERTY keys offset from
    int m_demoNote = 50;      // FX demo's played note; follows the piano
    int m_userZoom = 0;      // pane zoom steps from A-/A+ (persisted as a pref)
    double m_fontScale = 1.0;
    // Scroll position held across a zoom rebuild, restored once the layout has
    // settled. Kept as pane state rather than captured per-rebuild: a second
    // zoom step arriving before the restore fires would otherwise read the
    // freshly-reset scrollbar and capture 0, throwing the position away.
    double m_pendingScrollFrac = -1.0;
    bool m_scrollRestoreQueued = false;
    int m_workspaceSeq = 0;
};

#endif // TUTORIALPANE_H
