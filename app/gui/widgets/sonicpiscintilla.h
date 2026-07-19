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
#include "utils/flash_style.h"
#include "widgets/sonicpilog.h"
#include "api/audio/server_shm.hpp"
#include <QCheckBox>
#include <QHash>
#include <QVector>
#include <Qsci/qsciscintilla.h>
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
#include <QRecursiveMutex>
#endif

class SonicPiLexer;
class QSettings;
class CompletionPopup;
class FindPopup;
class EditorToolbar;
class QVariantAnimation;
class QMenu;
class QContextMenuEvent;
class QWheelEvent;

namespace SonicPi
{
class SonicPiAPI;
}

class SonicPiScintilla : public QsciScintilla
{
    Q_OBJECT

public:
    // keyBindings: pass a shared QSettings when constructing several editors
    // back-to-back (MainWindow builds ten) so the bindings ini is parsed once;
    // each ctor opens its own when null.
    SonicPiScintilla(SonicPiLexer* lexer, SonicPiTheme* theme, QString fileName, bool autoIndent,
                     QSettings* keyBindings = nullptr);

    virtual QStringList apiContext(int pos, int& context_start,
        int& last_word_start);
    SonicPiTheme* theme;
    QString fileName;
    bool selectionMode;

    void redraw();

    // Transient italic placeholder shown while the buffer is empty (like a text
    // field's placeholder); it isn't part of the document and vanishes as soon
    // as any real text is entered.
    void setPlaceholderText(const QString& text);

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
    // The popup's piano was clicked on a synth/fx row — run `code` to audition it.
    void auditionRequested(const QString& code);
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
    // Briefly pulse a line (e.g. the source line of a sound as it plays):
    // a translucent wash behind the line's code text and/or a small dot in
    // the gutter. line is 0-based; re-flashing extends the pulse rather than
    // stacking.
    void flashLine(int line, bool codeWash = true, bool gutterDot = false);
    // Strength of the code wash as a percentage (100 = opaque accent).
    void setFlashBrightness(int percent);
    // Snapshot the current line positions as edit-tracking marker handles (call
    // when the buffer is run). flashRunLine then maps a run-time line number to
    // its current line, so flashes stay correct as the code is edited live.
    void snapshotRunLines();
    void flashRunLine(int runLine, bool codeWash = true, bool gutterDot = false);
    // Pin a mini oscilloscope to a live_loop's header line (runLine is the
    // 0-based line at run time, mapped through the same edit-tracking anchors
    // as the flashes). Re-registering an existing name updates its line and
    // reader; endLiveLoopScope removes it when the loop dies.
    void setLiveLoopScope(const QString& name, int runLine, const shm_scope_stream_reader& reader);
    // Roll-mode (scrolling strip) vs the default triggered sweep; applies to
    // current and future loop scopes (preference: loop_scope_scroll).
    void setLiveLoopScopeScroll(bool scroll);
    // Engine API access for the inline scopes' sample-clock reads (audible-time
    // window alignment). Called once by MainWindow after boot wiring.
    void setAudioApi(SonicPi::SonicPiAPI* api);
    void endLiveLoopScope(const QString& name);
    void clearLiveLoopScopes();
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
    // Caret-relocation ops (menu/shortcut driven, so they bypass the popup's
    // key handling). Each dismisses an open completion popup first: typed
    // text stays, an un-committed preview is dropped.
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

    // In-buffer find (the floating FindPopup bar). showFind opens/refocuses
    // the bar seeded from the selection or the shared last query; pressing
    // the Find shortcut again while the bar is focused advances (isearch
    // style). Next/prev cycle with wrap-around. ("Match" suffix: QsciScintilla
    // already has a virtual findNext with a different signature.)
    void showFind();
    void findNextMatch();
    void findPrevMatch();
    // Close the bar and clear the highlights (Escape path — also called from
    // MainWindow::escapeWorkspaces). No-op when the bar is hidden.
    void closeFindPopup();

    // Optional floating edit toolbar (undo/redo, cut/copy/paste, find) in the
    // editor's top-right corner; the find bar takes the corner over while open.
    void setEditorToolbarEnabled(bool on);

    void showAutoCompletion(bool val);
    void setCompletionHelp(bool val);   // show docstring/piano/slider helper panes
    void setText(const QString& text);

    // Re-lay the empty-buffer welcome overlay (size tracks the zoom, so call
    // this after any zoomTo that doesn't route through zoomFontIn/Out).
    void updatePlaceholder();

private:
    class QLabel* m_placeholder = nullptr;
    QString m_placeholderText;

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
    // Dismiss an open completion popup the "click away" way: keep whatever the
    // user typed, drop an un-committed preview. No-op when the popup is hidden.
    void dismissCompletionKeepTyped();
    int m_pvStart = -1;        // buffer pos of the previewed word (-1 = inactive)
    int m_pvLen = 0;           // current length of the previewed text
    QString m_pvRestore;       // what clearPreview() puts back (filter / current value)
    QString m_pvOriginal;      // what Escape puts back (text before the popup opened)
    bool m_pvSlider = false;   // the active preview is a slider value
    bool m_pvGuard = false;    // suppress textChanged during our own preview edits
    bool m_keyEditGuard = false; // suppress the textChanged completion backstop while
                                 // the keypress path runs its own updateCompletion()
    bool m_pvLive = false;     // a preview is written into the buffer (vs merely armed)
    QString m_pvPrefix;        // separator prepended to the previewed selection (e.g. ", ")

    // Find state: match spans are byte positions kept in step with the query
    // (refreshFind re-runs on every keystroke and buffer edit). The query is
    // shared across all editors via s_lastFindQuery so find behaves as one
    // feature across buffer tabs.
    FindPopup* m_find = nullptr;
    QVector<int> m_findStarts, m_findEnds;
    int m_findCurrent = -1;         // index into m_findStarts (-1 = none)
    int m_findOrigin = -1;          // caret pos at open (Ctrl+G abort target)
    QVariantAnimation* m_findPulse = nullptr;   // current-match landing pulse
    static QString s_lastFindQuery;
    // interact: select + scroll to the current match (user-driven navigation);
    // false for buffer-edit refreshes, which must never move the caret.
    void refreshFind(bool interact);
    void setCurrentFindMatch(int idx, bool interact);
    void applyFindIndicatorColours();
    void closeFind(bool abortToOrigin);

    EditorToolbar* m_editorToolbar = nullptr;
    bool m_editorToolbarEnabled = false;
    // Visible = enabled and the find bar isn't occupying the corner.
    void updateEditorToolbarVisibility();

    void addKeyBinding(QSettings& qs, int cmd, int key);
    void addOtherKeyBinding(QSettings& qs, int cmd, int key);
    void dragEnterEvent(QDragEnterEvent* pEvent);
    void dropEvent(QDropEvent* pEvent);
    void dragMoveEvent(QDragMoveEvent* event);
    void dragLeaveEvent(QDragLeaveEvent* event);

    // Live preview of a text drag (quickstart cards): the payload is written
    // into the buffer at the pointer's line while the drag is over the
    // editor, so the drop shows exactly what it will produce. Kept out of
    // the undo history; the real insert happens on drop.
    void placeDropPreview(int bytePos, const QString& text, const QString& title = QString());
    void clearDropPreview();
    int m_dropPreviewPos = -1; // byte pos of the previewed text (-1 = none)
    int m_dropPreviewLen = 0;  // byte length of the previewed text
    QString m_dropPreviewText; // the previewed payload
    QWidget* m_dropPreviewBox = nullptr; // accent border overlay around the block

public:
    // Commit a still-live drop preview as a real (undoable) insert at the
    // previewed position.
    void finaliseDropPreview();
    // Quickstart insert-button hover preview: project the card code at the
    // cursor line (like a drag-over), then commit (finaliseDropPreview) on click
    // or drop (cancelInsertPreview) on unhover.
    void previewInsertAtCursor(const QString& text, const QString& title = QString());
    void cancelInsertPreview();
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

    // Live line-flash: a re-flash bumps the line's generation so only the last
    // pending timer clears the marker (rapid pulses extend, don't stack).
    QHash<int, int> m_flashGen;
    // Edit-tracking anchors captured at run time, indexed by run-time line: the
    // character position of each line's start, nudged on every insert/delete
    // (see the SCN_MODIFIED handler) so a run-time line maps to its current line
    // even after the code is edited live — including line splits, which plain
    // line-markers don't follow.
    QVector<int> m_runLinePos;
    bool m_inReplaceBuffer = false; // suppress incremental tracking during a full replace
    int m_flashAlpha = SonicPi::kFlashWashAlpha; // code-wash indicator alpha (setFlashBrightness)
    void applyFlashMarkerColours();
    void clearFlashWash(int line);
    void trackEditForFlash(int position, int modificationType, int length);
    int runLineToCurrent(int runLine);
    // live_loop mini scopes, keyed by loop name; one shared timer polls the
    // scope buffers and re-pins each widget to its (edit-tracked) header line.
    QHash<QString, class LiveLoopScopeWidget*> m_loopScopes;
    bool m_loopScopeScroll = false;
    QHash<QString, int> m_loopScopeLines;
    QTimer* m_loopScopeTimer = nullptr;
    SonicPi::SonicPiAPI* m_audioApi = nullptr;
    void positionLiveLoopScopes();
    void applyLoopScopeColours();
    // Remap flash anchors across a full-buffer replace (Return-triggered
    // re-indent, beautify) by diffing old vs new lines ignoring indentation, so
    // inserted/removed lines shift the anchors below them.
    void remapFlashAnchorsAcrossReplace(const QVector<int>& oldAnchorLines,
                                        const QStringList& oldStripped,
                                        const QStringList& newStripped);
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
    QRecursiveMutex* mutex;
#else
    QMutex* mutex;
#endif
};

#endif // SONICPISCINTILLA_H
