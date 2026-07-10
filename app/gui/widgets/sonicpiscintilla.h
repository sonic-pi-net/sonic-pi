//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef SONICPISCINTILLA_H
#define SONICPISCINTILLA_H

#include "model/sonicpitheme.h"
#include "widgets/sonicpilog.h"
#include <QCheckBox>
#include <Qsci/qsciscintilla.h>
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
#include <QRecursiveMutex>
#endif

class SonicPiLexer;
class QSettings;
class CompletionPopup;
class QMenu;
class QContextMenuEvent;
class QWheelEvent;

class SonicPiScintilla : public QsciScintilla
{
    Q_OBJECT

public:
    SonicPiScintilla(SonicPiLexer* lexer, SonicPiTheme* theme, QString fileName, bool autoIndent);

    virtual QStringList apiContext(int pos, int& context_start,
        int& last_word_start);
    SonicPiTheme* theme;
    QString fileName;
    bool selectionMode;

    void redraw();

    // Completion popup state, for the Tab handler in MainWindow (so Tab accepts
    // the popup the same way Return does).
    bool completionActive() const;
    void acceptCompletionPopup();

    // The live Scintilla zoom level (points added on top of the base font size).
    int currentZoom();
    // The zoom level a fresh workspace starts at; dependent UI (error card,
    // docs example editor) sizes its designs against this.
    static constexpr int kDefaultZoom = 2;

signals:
    // Zoom changed via zoomFontIn/Out or Ctrl+wheel, so dependent UI (e.g. the
    // error card) can track the editor's effective font size.
    void zoomLevelChanged();
    void bufferNewlineAndIndent(int point_line, int point_index, int first_line, const std::string& code, const std::string& fileName);
    // The completion popup's "Docs" button was clicked — open help for this name.
    void docsRequested(const QString& name);
    // A completion suggestion to speak to the screen reader (MainWindow relays it
    // to its portable announce() helper). Focus stays in the editor throughout.
    void announceRequested(const QString& message);
    // Right-click menu is being built — MainWindow appends editor-wide actions
    // (Comment/Uncomment, Show Docs, Align) before it's shown.
    void extendContextMenu(QMenu* menu);

public slots:
    void cutLineFromPoint();
    void tabCompleteifList();
    void transposeChars();
    void setMark();
    void triggerCompletion();          // explicitly invoke completion (menu/shortcut), even when auto-completion is off
    void announceCompletionDetails();  // emit the selected item's full docstring for a screen reader
    void showCompletionDocs();         // show the popup (with docs) if hidden, then read the current item's doc
    void escapeAndCancelSelection();
    void copyClear();
    void hideLineNumbers();
    void showLineNumbers();
    void setLineErrorMarker(int lineNumber, bool isSyntaxError, const QString& errorToken, int colStart, int colEnd);
    void clearLineMarkers();
    void replaceLine(int lineNumber, QString newLine);
    void replaceLines(int lineStart, int lineFinish, QString newLines);
    void forwardLines(int numLines);
    void forwardOneLine();
    void backOneLine();
    void forwardTenLines();
    void backTenLines();
    void moveLineOrSelection(int numLines);
    void moveLineOrSelectionUp();
    void moveLineOrSelectionDown();
    int incLineNumWithinBounds(int linenum, int inc);
    void moveLines(int numLines);
    void deselect();
    void upcaseWordOrSelection();
    void downcaseWordOrSelection();
    void highlightCurrentLine();
    void unhighlightCurrentLine();
    void zoomFontIn();
    void zoomFontOut();
    void newLine();
    void replaceBuffer(QString content, int line, int index, int first_line);
    void newlineAndIndent();
    void completeListOrNewlineAndIndent();
    void setAutoIndentEnabled(bool enabled);
    void charRight();
    void charLeft();
    void deleteForward();
    void deleteBack();
    void lineStart();
    void lineEnd();
    void documentStart();
    void documentEnd();
    void wordRight();
    void wordLeft();
    void selectLineStart();
    void selectLineEnd();
    void selectWordRight();
    void selectWordLeft();
    void selectDocStart();
    void selectDocEnd();
    void centerCaret();
    void undo();
    void redo();
    void selectAll();
    void deleteWordRight();
    void deleteWordLeft();

    void sp_paste();
    void sp_cut();

    void showAutoCompletion(bool val);
    void setCompletionHelp(bool val);   // show docstring/piano/slider helper panes
    void setText(const QString& text);

private:
    // Custom completion popup driven from key events (replaces Scintilla's
    // built-in list so we can show kind badges + summaries per row).
    void updateCompletion(bool force = false);   // (re)show/refresh the popup; force ignores the auto-completion pref
    void acceptCompletion();   // insert the highlighted entry, replacing the partial
    CompletionPopup* m_completion = nullptr;
    bool m_completionEnabled = true;

    // Live preview: while the popup is open, the selected entry (or slider value)
    // is written into the buffer in place of the typed word. A list preview is
    // ephemeral (restore = the typed filter); a slider value is committed live
    // (restore = the current value). Escape always restores the original text.
    void applyPreview(const QString& sel);  // swap in `sel`
    void clearPreview();                     // restore m_pvRestore
    void restoreOriginal();                  // restore m_pvOriginal (Escape)
    void endPreview();                       // deactivate (leave buffer as-is)
    void replacePreviewSpan(const QString& text);  // guarded edit of the previewed span
    void popCompletionOnClick();             // open the keyboard/slider when clicking onto a note/chord/scale/opt value
    // The ", " separator (or "") a freshly-chosen argument needs from the
    // preceding one; sets `replaceStart` to where the replacement begins
    // (eating intervening whitespace when a separator is added).
    QString argSeparatorBefore(const QStringList& context, int wordStart, int& replaceStart);
    // Buffer pos at the end of the token the caret sits in — extends past the caret
    // over trailing token chars so a caret mid-word/number (`lpf: 7|0`) completes and
    // replaces the whole token, not just the part before the caret.
    int tokenEndForCaret(int pos);
    int m_pvStart = -1;        // buffer pos of the previewed word (-1 = inactive)
    int m_pvLen = 0;           // current length of the previewed text
    QString m_pvRestore;       // what clearPreview() puts back (filter / current value)
    QString m_pvOriginal;      // what Escape puts back (text before the popup opened)
    bool m_pvSlider = false;   // the active preview is a slider value
    bool m_pvGuard = false;    // suppress textChanged during our own preview edits
    bool m_pvLive = false;     // a preview is written into the buffer (vs merely armed)
    QString m_pvPrefix;        // separator prepended to the previewed selection (e.g. ", ")

    void addKeyBinding(QSettings& qs, int cmd, int key);
    void addOtherKeyBinding(QSettings& qs, int cmd, int key);
    void dragEnterEvent(QDragEnterEvent* pEvent);
    void dropEvent(QDropEvent* pEvent);
    void dragMoveEvent(QDragMoveEvent* event);
    void focusOutEvent(QFocusEvent* event) override;
    void mouseReleaseEvent(QMouseEvent* event) override;
    void contextMenuEvent(QContextMenuEvent* event) override;
    void wheelEvent(QWheelEvent* event) override;
    bool event(QEvent* evt);
    bool autoIndent;

    // The error line's gutter markers (margin washes + dot) are fixed-size RGBA
    // images, so they are rebuilt at the current line height whenever the zoom
    // changes. m_errorLine is the marked line, or -1 when none is shown.
    void applyErrorMarkers(int lineNumber);
    void refreshErrorMarkers();
    // Sets the symbol-margin gap (number-to-code) to ~1 char width so it tracks
    // the font/zoom; kept identical in every state so errors never shift the
    // code. Returns the width. Caller must hold the mutex.
    int updateErrorMarginWidth();
    int m_errorLine = -1;
    bool m_errorIsSyntax = false;  // colour markers blue for syntax, pink for runtime
    QString m_errorToken;          // identifier to underline when no exact span
    int m_errorColStart = -1;      // exact byte-column span of the token to
    int m_errorColEnd = -1;        // underline (error_highlight); -1 = none
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
    QRecursiveMutex* mutex;
#else
    QMutex* mutex;
#endif
};

#endif // SONICPISCINTILLA_H
