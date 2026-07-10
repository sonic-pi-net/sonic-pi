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

class QLabel;
class QPushButton;
class QScrollArea;
class QVBoxLayout;
class SonicPiLexer;
class SonicPiScintilla;
class SonicPiTheme;
class TutDial;
class TutSelectionGroup;

// Native replacement for the QTextBrowser docs pane: a scrollable column of
// heading/prose/code blocks parsed from the tutorial markdown. Code blocks
// are lightweight syntax-highlighted labels with play/stop/copy controls;
// play runs the snippet as its own job so stop only stops that snippet.
class TutorialPane : public QFrame
{
    Q_OBJECT
public:
    TutorialPane(SonicPiLexer* lexer, SonicPiTheme* theme, QWidget* parent = nullptr);

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

    // Keyboard scrolling (docScrollUp/Down shortcuts): one scroll step
    void scrollStep(int direction);

    // Wire these to QtAPIClient's RunStartedReceived/RunEndedReceived
    void runStarted(int jobId, const QString& workspace);
    void runEnded(int jobId);

protected:
    // Instrument pages are playable: Space toggles the demo, QWERTY piano
    // keys (a w s e d f t g y h u j k o l p) play notes up from the note dial
    void keyPressEvent(QKeyEvent* event) override;

signals:
    void runRequested(const QString& code, const QString& workspace, bool silent = false);
    void stopJobRequested(int jobId);
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
    void setSnippetCode(int index, const QString& code);
    void addHeading(int level, const QString& text);
    void addProse(const QString& richText);
    void addList(const SonicPi::TutorialBlock& block);
    void addImage(const SonicPi::TutorialBlock& block);
    void addSnippet(const QString& code, bool runnable = true);
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
    QPushButton* m_examplePlay = nullptr;
    QPushButton* m_exampleStop = nullptr;
    QPushButton* m_exampleCopy = nullptr;

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
    QPushButton* m_zoomIn = nullptr;
    QPushButton* m_zoomOut = nullptr;
    SonicPi::CodeColours m_codeColours;
    QIcon m_playIcon;
    QIcon m_stopIcon;
    int m_userZoom = 0;      // pane zoom steps from A-/A+ (persisted as a pref)
    double m_fontScale = 1.0;
    int m_workspaceSeq = 0;
};

#endif // TUTORIALPANE_H
