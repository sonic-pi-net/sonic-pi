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
class SonicPiLexer;
class SonicPiScintilla;
class SonicPiTheme;
class TutDial;
class TutScope;
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
    TutorialPane(SonicPiLexer* lexer, SonicPiTheme* theme, QWidget* parent = nullptr);

    // The jukebox scope reads an isolated scope-buffer slot straight from the
    // engine shm; the pane needs the API handle to fetch a reader per play.
    void setAudioApi(std::shared_ptr<SonicPi::SonicPiAPI> api);

    // imagesRoot is the on-disk dir that image paths are relative to
    // (rootPath()/etc/doc/images). Empty prev/next titles hide that button.
    void loadChapter(const SonicPi::TutorialChapter& chapter, const QString& imagesRoot,
                     const QString& prevTitle, const QString& nextTitle);

    // Full-file playable code page (Examples tab): one persistent read-only
    // editor instance so long examples get real lexing + scrolling
    void showCodePage(const QString& title, const QString& code);

    // Interactive FX/synth pages: icon + doc + per-opt dials (true per-synth
    // defaults) that live-regenerate a playable snippet
    void showInstrumentPage(bool isFx, const SonicPi::InstrumentPage& page);
    // Samples group page: one playable snippet per sample
    void showSampleGroupPage(const SonicPi::SampleGroup& group);
    // Lang reference page: usage, doc, runnable examples
    void showLangPage(const SonicPi::LangPage& page);

    // Trigger the first snippet's play button; false if there is none
    bool playFirstSnippet();

    void applyTheme();
    // The pane owns its zoom (A-/A+, persisted) — deliberately independent
    // of the per-buffer editor zoom
    int userZoom() const { return m_userZoom; }
    void setUserZoom(int zoom);

    // The A-/A+ text-size buttons, packed in a standalone widget so the help
    // dock can host them in its title row beside the HELP title.
    QWidget* zoomControls() const;

    // Keyboard scrolling (docScrollUp/Down shortcuts): one scroll step
    void scrollStep(int direction);

    // Wire these to QtAPIClient's RunStartedReceived/RunEndedReceived
    void runStarted(int jobId, const QString& workspace);
    void runEnded(int jobId);

protected:
    // Instrument pages are playable: Space toggles the demo, QWERTY piano
    // keys (a w s e d f t g y h u j k o l p) play notes up from the note dial
    void keyPressEvent(QKeyEvent* event) override;
    // Hover recolour for the zoom -/+ glyph buttons.
    bool eventFilter(QObject* obj, QEvent* event) override;

signals:
    // scopeTap wraps the run in an fx_scope_out tap so the jukebox scope can
    // show only this run's audio (used by the Examples page play button).
    void runRequested(const QString& code, const QString& workspace, bool silent = false,
                      bool scopeTap = false);
    void stopJobRequested(int jobId);
    void loadRequested(const QString& code); // load into the current editor buffer
    void announceRequested(QString msg);
    void navigateRequested(int delta); // -1 previous chapter, +1 next
    void linkClicked(const QUrl& url);

private:
    struct Snippet
    {
        QFrame* frame = nullptr;
        class TutProseText* codeView = nullptr;
        QPushButton* play = nullptr;
        QPushButton* stop = nullptr;
        QPushButton* copy = nullptr;
        QGridLayout* codeArea = nullptr; // grid hosting the code + corner controls
        bool commentsAside = false; // display-only example: comments in a right column
        QString code;
        QString workspace;
        int jobId = -1;
    };

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
    // Two-column rendering for display-only examples: code left, comments
    // gathered in a muted right column (the old doc system's layout).
    QString exampleTableHtml(const QString& code) const;
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
    void addOptsGrid(const QVector<SonicPi::InstrumentOpt>& opts);
    void addNavFooter();
    void setSnippetPlaying(Snippet& snippet, bool playing);
    void ensureExampleEditor();
    void renderFxIcon(QLabel* iconLabel);
    void toggleDemo();
    void playKeyboardNote(int semitoneOffset);
    void shiftOctave(int delta);
    QString instrumentOpts() const; // ", opt: val" for the non-default dials (excl. note)
    QString proseColoured(const QString& richText) const;

    SonicPiLexer* m_lexer = nullptr;
    SonicPiTheme* m_theme = nullptr;
    QScrollArea* m_scroll = nullptr;
    QWidget* m_content = nullptr;
    QVBoxLayout* m_column = nullptr;

    // Examples full-file page (built once, reused)
    QWidget* m_examplePage = nullptr;
    QLabel* m_exampleTitle = nullptr;
    QFrame* m_exampleFrame = nullptr;
    QVBoxLayout* m_exampleFrameLayout = nullptr;
    SonicPiScintilla* m_exampleEditor = nullptr;
    QPushButton* m_examplePlay = nullptr; // jukebox transport: toggles play/stop
    QPushButton* m_exampleLoad = nullptr;
    TutScope* m_exampleScope = nullptr;   // live scope, visible only while playing
    std::shared_ptr<SonicPi::SonicPiAPI> m_spAPI;

    SonicPi::TutorialChapter m_chapter;
    QVector<TutDial*> m_dials;
    QVector<class TutProseText*> m_proseLabels;
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
    class ZoomBar* m_zoomBar = nullptr; // shared A-/A+ bar, hosted in the dock title row
    SonicPi::CodeColours m_codeColours;
    QIcon m_playIcon;
    QIcon m_stopIcon;
    QIcon m_exPlayIcon; // jukebox transport glyphs (contrasting, for accent fill)
    QIcon m_exStopIcon;
    QIcon m_copyIcon;
    QIcon m_copiedIcon; // check-mark flash after a successful copy
    QHash<QString, QWidget*> m_optRows; // opt name → its doc-table row, for jump links
    int m_pianoBaseNote = 52; // page-default note the QWERTY keys offset from
    int m_userZoom = 0;      // pane zoom steps from A-/A+ (persisted as a pref)
    double m_fontScale = 1.0;
    int m_workspaceSeq = 0;
};

#endif // TUTORIALPANE_H
