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

#include "profiler.h"
#include "sonicpiscintilla.h"
#include "dpi.h"
#include <QSettings>
#include <QShortcut>
#include <QDrag>
#include <QDragEnterEvent>
#include <QDropEvent>
#include <Qsci/qscicommandset.h>
#include <Qsci/qscilexer.h>
#include <QCheckBox>
#include <QRegularExpression>
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
#include <QRecursiveMutex>
#endif

SonicPiScintilla::SonicPiScintilla(SonicPiLexer *lexer, SonicPiTheme *theme, QString fileName, bool autoIndent)
  : QsciScintilla()
{
  setAcceptDrops(true);
  this->theme = theme;
  this->fileName = fileName;
  this->autoIndent = autoIndent;
  this->selectionMode = false;
  standardCommands()->clearKeys();
  standardCommands()->clearAlternateKeys();
  QString skey;
  QSettings settings(QSettings::IniFormat, QSettings::UserScope, "sonic-pi.net", "gui-keys-bindings");
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
  mutex = new QRecursiveMutex();
#else
  mutex = new QMutex(QMutex::Recursive);
#endif

#if defined(Q_OS_MAC)
  int SPi_CTRL = Qt::META;
  int SPi_META = Qt::CTRL;
#else
  int SPi_CTRL = Qt::CTRL;
  int SPi_META = Qt::ALT;
#endif

  // basic navigation
  addKeyBinding(settings, QsciCommand::PageDown, Qt::Key_PageDown);
  addKeyBinding(settings, QsciCommand::PageUp, Qt::Key_PageUp);

  addOtherKeyBinding(settings, QsciCommand::LineDown, Qt::Key_Down);
  addKeyBinding(settings, QsciCommand::LineDownExtend, Qt::Key_Down | Qt::SHIFT);
  addOtherKeyBinding(settings, QsciCommand::LineUp, Qt::Key_Up);
  addKeyBinding(settings, QsciCommand::LineUpExtend, Qt::Key_Up | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::CharRight, Qt::Key_F | SPi_CTRL);
  addOtherKeyBinding(settings, QsciCommand::CharRight, Qt::Key_Right);
  addKeyBinding(settings, QsciCommand::CharRightExtend, Qt::Key_Right | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::WordRight, Qt::Key_F | SPi_META);
  addOtherKeyBinding(settings, QsciCommand::WordRight, Qt::Key_Right | SPi_CTRL);
  addKeyBinding(settings, QsciCommand::WordRightExtend, Qt::Key_Right | SPi_CTRL | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::CharLeft, Qt::Key_B | SPi_CTRL);
  addOtherKeyBinding(settings, QsciCommand::CharLeft, Qt::Key_Left);
  addKeyBinding(settings, QsciCommand::CharLeftExtend, Qt::Key_Left | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::WordLeft, Qt::Key_B | SPi_META);
  addOtherKeyBinding(settings, QsciCommand::WordLeft, Qt::Key_Left | SPi_CTRL);
  addKeyBinding(settings, QsciCommand::WordLeftExtend, Qt::Key_Left | SPi_CTRL | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::Delete, Qt::Key_D | SPi_CTRL);
  addOtherKeyBinding(settings, QsciCommand::Delete, Qt::Key_Delete);

  addKeyBinding(settings, QsciCommand::DeleteBack, Qt::Key_H | SPi_CTRL);
  addOtherKeyBinding(settings, QsciCommand::DeleteBack, Qt::Key_Backspace);

  addKeyBinding(settings, QsciCommand::Home, Qt::Key_A | SPi_CTRL);
  addKeyBinding(settings, QsciCommand::VCHome, Qt::Key_Home);
  addKeyBinding(settings, QsciCommand::VCHomeExtend, Qt::Key_Home | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::DocumentStart, Qt::Key_Comma | Qt::SHIFT | SPi_META);
  addKeyBinding(settings, QsciCommand::DocumentStartExtend, Qt::Key_Home | SPi_CTRL | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::LineEnd, Qt::Key_E | SPi_CTRL);
  addOtherKeyBinding(settings, QsciCommand::LineEnd, Qt::Key_End);
  addKeyBinding(settings, QsciCommand::LineEndExtend, Qt::Key_End | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::DocumentEnd, Qt::Key_Greater | SPi_META);
  addOtherKeyBinding(settings, QsciCommand::DocumentEnd, Qt::Key_Period | Qt::SHIFT | SPi_META);
  addKeyBinding(settings, QsciCommand::DocumentEndExtend, Qt::Key_End | SPi_CTRL | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::Delete, Qt::Key_D | SPi_CTRL);
  addKeyBinding(settings, QsciCommand::VerticalCentreCaret, Qt::Key_L | SPi_CTRL);

  addKeyBinding(settings, QsciCommand::Backtab, Qt::Key_Tab | Qt::SHIFT);

  // copy paste
  addKeyBinding(settings, QsciCommand::SelectionCopy, Qt::Key_C | SPi_META);
  addOtherKeyBinding(settings, QsciCommand::SelectionCopy, Qt::Key_C | SPi_CTRL);
  addKeyBinding(settings, QsciCommand::SelectionCut, Qt::Key_X | SPi_META);


  addKeyBinding(settings, QsciCommand::Undo, Qt::Key_Z | SPi_META);
  addOtherKeyBinding(settings, QsciCommand::Undo, Qt::Key_Z | SPi_CTRL);
  addKeyBinding(settings, QsciCommand::Redo, Qt::Key_Z | Qt::SHIFT | SPi_META);
  addOtherKeyBinding(settings, QsciCommand::Redo, Qt::Key_Z | Qt::SHIFT | SPi_CTRL);
  addKeyBinding(settings, QsciCommand::SelectAll, Qt::Key_A | SPi_META);

  // delete word left and right
  addKeyBinding(settings, QsciCommand::DeleteWordLeft, Qt::Key_Backslash | SPi_META);
  addKeyBinding(settings, QsciCommand::DeleteWordLeft, Qt::Key_Backspace | SPi_META);
  addKeyBinding(settings, QsciCommand::DeleteWordRight, Qt::Key_D | SPi_META);

  standardCommands()->readSettings(settings);

  this->setMatchedBraceBackgroundColor(theme->color("MatchedBraceBackground"));
  this->setMatchedBraceForegroundColor(theme->color("MatchedBraceForeground"));

  setIndentationWidth(ScaleHeightForDPI(2));
  setIndentationGuides(true);
  setIndentationGuidesForegroundColor(theme->color("IndentationGuidesForeground"));
  setBraceMatching( SonicPiScintilla::SloppyBraceMatch);

  //TODO: add preference toggle for this:
  //this->setFolding(SonicPiScintilla::CircledTreeFoldStyle, 2);
  setCaretLineVisible(true);
  setCaretLineBackgroundColor(theme->color("CaretLineBackground"));
  setFoldMarginColors(theme->color("FoldMarginForeground"),theme->color("FoldMarginForeground"));
  setMarginLineNumbers(0, true);

  setMarginsBackgroundColor(theme->color("MarginBackground"));
  setMarginsForegroundColor(theme->color("MarginForeground"));
  setMarginsFont(QFont("Hack", 15, -1, true));
  setUtf8(true);
  setText("# Loading previous buffer contents. Please wait...");
  setLexer((QsciLexer *)lexer);

  markerDefine(QImage(":/images/marker-error.png").scaled(QSize(ScaleHeightForDPI(30), ScaleHeightForDPI(21))), 8);

  setMarkerBackgroundColor(theme->color("MarkerBackground"), 8);

  setAutoCompletionThreshold(1);
  setAutoCompletionSource(SonicPiScintilla::AcsAPIs);
  setAutoCompletionCaseSensitivity(false);

  setSelectionBackgroundColor(theme->color("SelectionBackground"));
  setSelectionForegroundColor(theme->color("SelectionForeground"));
  setCaretWidth(ScaleHeightForDPI(5));
  setCaretForegroundColor(theme->color("CaretForeground"));
  setEolMode(EolUnix);

  SendScintilla(SCI_SETWORDCHARS, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789:_?!");


}

void SonicPiScintilla::redraw(){
    SP_ZoneScopedN("Scintilla Redraw");
  mutex->lock();
  setMarginsBackgroundColor(theme->color("MarginBackground"));
  setMarginsForegroundColor(theme->color("MarginForeground"));
  setSelectionBackgroundColor(theme->color("SelectionBackground"));
  setSelectionForegroundColor(theme->color("SelectionForeground"));
  setCaretLineBackgroundColor(theme->color("CaretLineBackground"));
  setFoldMarginColors(theme->color("FoldMarginForeground"),theme->color("FoldMarginForeground"));
  setIndentationGuidesForegroundColor(theme->color("IndentationGuidesForeground"));
  setMatchedBraceBackgroundColor(theme->color("MatchedBraceBackground"));
  setMatchedBraceForegroundColor(theme->color("MatchedBraceForeground"));
  mutex->unlock();
}

void SonicPiScintilla::highlightCurrentLine(){
  mutex->lock();
  setCaretLineBackgroundColor(theme->color("SelectionBackground"));
  mutex->unlock();
}

void SonicPiScintilla::unhighlightCurrentLine(){
  mutex->lock();
  setCaretLineBackgroundColor(theme->color("CaretLineBackground"));
  mutex->unlock();
}

void SonicPiScintilla::hideLineNumbers(){
  mutex->lock();
  setMarginLineNumbers(0, false);
  setMarginWidth(0, "0");
  setMarginWidth(1, ScaleHeightForDPI(30));
  SendScintilla(SCI_HIDELINES);
  mutex->unlock();
}

void SonicPiScintilla::showLineNumbers(){
  mutex->lock();
  setMarginLineNumbers(0, true);
  setMarginWidth(0, "1000");
  setMarginWidth(1, ScaleHeightForDPI(30));
  SendScintilla(SCI_SHOWLINES);
  mutex->unlock();
}

void SonicPiScintilla::addOtherKeyBinding(QSettings &qs, int cmd, int key)
{
  mutex->lock();
  QString skey;
  QTextStream(&skey) << "/Scintilla/keymap/c" << cmd << "/alt";
  qs.setValue(skey, key);
  mutex->unlock();
}

void SonicPiScintilla::addKeyBinding(QSettings &qs, int cmd, int key)
{
  mutex->lock();
  QString skey;
  QTextStream(&skey) << "/Scintilla/keymap/c" << cmd << "/key";
  qs.setValue(skey, key);
  mutex->unlock();
}

void SonicPiScintilla::cutLineFromPoint()
{
  mutex->lock();
  int linenum, index;
  getCursorPosition(&linenum, &index);

  if (text(linenum).mid(index).contains(QRegularExpression("^\\s*\\n")))
  {
    setSelection(linenum, index, linenum + 1, 0);
    SendScintilla(SCI_CUT);
  } else
    {
      //  SendScintilla(SCI_CLEARSELECTIONS);
      int pos = SendScintilla(SCI_GETCURRENTPOS);

      SendScintilla(SCI_LINEEND);
      SendScintilla(SCI_SETANCHOR, pos);
      SendScintilla(SCI_CUT);
    }
  mutex->unlock();
}

void SonicPiScintilla::tabCompleteifList()
{
  mutex->lock();
  if(isListActive())
    {
      SendScintilla(QsciCommand::Tab);
    }
  mutex->unlock();
}

void SonicPiScintilla::transposeChars()
{
  mutex->lock();
  int linenum, index;
  getCursorPosition(&linenum, &index);
  setSelection(linenum, 0, linenum + 1, 0);
  int lineLength = selectedText().size();

  //transpose chars
  if(index > 0){
    if(index < (lineLength - 1)){
      index = index + 1;
    }
    setSelection(linenum, index - 2, linenum, index);
    QString text = selectedText();
    QChar a, b;
    a = text.at(0);
    b = text.at(1);
    QString replacement  = "";
    replacement.append(b);
    replacement.append(a);
    replaceSelectedText(replacement);
  }

  setCursorPosition(linenum, index);
  mutex->unlock();
}

void SonicPiScintilla::setMark()
{
  mutex->lock();
  int pos = SendScintilla(SCI_GETCURRENTPOS);
  SendScintilla(SCI_SETEMPTYSELECTION, pos);
  SendScintilla(SCI_SETSELECTIONMODE, 0);
  this->selectionMode = true;
  mutex->unlock();
}

void SonicPiScintilla::escapeAndCancelSelection()
{
  mutex->lock();
  int pos = SendScintilla(SCI_GETCURRENTPOS);
  SendScintilla(SCI_SETEMPTYSELECTION, pos);
  SendScintilla(SCI_CANCEL);
  this->selectionMode = false;
  mutex->unlock();
}

void SonicPiScintilla::deselect()
{
  mutex->lock();
  int pos = SendScintilla(SCI_GETCURRENTPOS);
  SendScintilla(SCI_SETEMPTYSELECTION, pos);
  this->selectionMode = false;
  mutex->unlock();
}

void SonicPiScintilla::copyClear()
{
  mutex->lock();
  QsciScintilla::copy();
  deselect();
  mutex->unlock();
}

void SonicPiScintilla::replaceLine(int lineNumber, QString newLine)
{
  mutex->lock();
  setSelection(lineNumber, 0, lineNumber + 1, 0);
  replaceSelectedText(newLine);
  mutex->unlock();
}

void SonicPiScintilla::replaceLines(int lineStart, int lineFinish, QString newLines)
{
  mutex->lock();
  setSelection(lineStart, 0, lineFinish + 1, 0);
  replaceSelectedText(newLines); mutex->unlock();
}

void SonicPiScintilla::forwardLines(int numLines) {
  mutex->lock();
  int idx;
  if(numLines > 0) {
    for (idx = 0 ; idx < numLines ; idx++) {
      if(selectionMode) {
        SendScintilla(SCI_LINEUPEXTEND);
      } else {
        SendScintilla(SCI_LINEUP);
      }
    }
  } else {
    for (idx = 0 ; idx > numLines ; idx--) {
      if(selectionMode) {
        SendScintilla(SCI_LINEDOWNEXTEND);
      } else {
        SendScintilla(SCI_LINEDOWN);
      }
    }
  }
  mutex->unlock();
}

void SonicPiScintilla::forwardOneLine() {
  forwardLines(1);
}

void SonicPiScintilla::backOneLine() {
  forwardLines(-1);
}

void SonicPiScintilla::forwardTenLines() {
  mutex->lock();
  forwardLines(10);
  mutex->unlock();
}

void SonicPiScintilla::backTenLines() {
  mutex->lock();
  forwardLines(-10);
  mutex->unlock();
}

void SonicPiScintilla::moveLineOrSelectionUp() {
  mutex->lock();
  moveLineOrSelection(-1);
  mutex->unlock();
}

void SonicPiScintilla::moveLineOrSelectionDown() {
  mutex->lock();
  moveLineOrSelection(1);
  mutex->unlock();
}

void SonicPiScintilla::moveLineOrSelection(int numLines) {
  mutex->lock();
  beginUndoAction();

  int linenum, cursor, origLinenum, origCursor;
  getCursorPosition(&linenum, &cursor);
  origLinenum = linenum;
  origCursor = cursor;

  bool hadSelectedText = hasSelectedText();


  if(!hadSelectedText) {
    setSelection(linenum, 0, linenum + 1, 0);
  }

  int lineFrom, indexFrom, lineTo, indexTo, lineOffset;
  getSelection(&lineFrom, &indexFrom, &lineTo, &indexTo);
  lineOffset = lineTo - origLinenum;
  linenum = lineFrom;

  QString selection = selectedText();

  if(selection[selection.length()-1] != '\n') {
    selection = selection + "\n";
    lineTo += 1;
    lineOffset += 1;
    indexTo = 0;
    replaceSelectedText("");
    setCursorPosition(linenum, 0);
    SendScintilla(SCI_DELETEBACK);
  } else {
    replaceSelectedText("");
  }
  setCursorPosition(linenum, 0);

  moveLines(numLines);

  getCursorPosition(&linenum, &cursor);
  setCursorPosition(linenum, 0);
  insert(selection);

  setCursorPosition(linenum + lineOffset, origCursor);

  int diffLine = lineTo - lineFrom;
  int diffIndex = indexTo - indexFrom;

  setSelection(linenum + diffLine, diffIndex, linenum, 0);

  endUndoAction();
  mutex->unlock();
}

QStringList SonicPiScintilla::apiContext(int pos, int &context_start,
					 int &last_word_start)
{
  QStringList context;
  // sampl|
  // sample |
  // chord :E3,|

  int linenum, cursor;
  getCursorPosition(&linenum, &cursor);
  QString line = text(linenum);
  line.truncate(cursor);
  context = line.split(QRegularExpression("[ ,(){}]+"));

  context_start = 0;
  last_word_start = pos;

  return context;
}

int SonicPiScintilla::incLineNumWithinBounds(int linenum, int inc) {
  mutex->lock();
  linenum += inc;
  int maxBufferIndex = lines() - 1;

  if(linenum < 0) {
    linenum = 0;
  }

  if(linenum > maxBufferIndex) {
    linenum = maxBufferIndex;
  }

  return linenum;
  mutex->unlock();
}

void SonicPiScintilla::moveLines(int numLines) {
  mutex->lock();
  if (numLines > 0)
  {
    for(int i = 0 ; i < numLines ; i++) {
      SendScintilla(SCI_LINEDOWN);
    }
  } else {
    for(int i = 0 ; i > numLines ; i--) {
      SendScintilla(SCI_LINEUP);
    }
  }
  mutex->unlock();
}

void SonicPiScintilla::upcaseWordOrSelection(){
  mutex->lock();
  if(hasSelectedText()) {
    SendScintilla(SCI_UPPERCASE);
  } else {
    setMark();
    SendScintilla(SCI_WORDRIGHT);
    SendScintilla(SCI_UPPERCASE);
    deselect();
  }
  mutex->unlock();
}


void SonicPiScintilla::downcaseWordOrSelection(){
  mutex->lock();
  if(hasSelectedText()) {
    SendScintilla(SCI_LOWERCASE);
  } else {
    setMark();
    SendScintilla(SCI_WORDRIGHT);
    SendScintilla(SCI_LOWERCASE);
    deselect();
  }
  mutex->unlock();
}

void SonicPiScintilla::setLineErrorMarker(int lineNumber){
  mutex->lock();

  markerDeleteAll(-1);
  markerAdd(lineNumber, 8);

  // Perhaps consider a more manual way of returning this functionality:
  // int currlinenum, index;
  // getCursorPosition(&currlinenum, &index);
  // if (lineNumber != currlinenum) {
  //   setCursorPosition(lineNumber, 0);
  // }

  mutex->unlock();
}

void SonicPiScintilla::clearLineMarkers(){
  mutex->lock();
  markerDeleteAll(-1);
  mutex->unlock();
}

void SonicPiScintilla::zoomFontIn() {
  mutex->lock();
  int zoom = property("zoom").toInt();
  zoom++;
  if (zoom > 20) zoom = 20;
  setProperty("zoom", QVariant(zoom));
  zoomTo(zoom);
  mutex->unlock();
}

void SonicPiScintilla::zoomFontOut() {
  mutex->lock();
  int zoom = property("zoom").toInt();
  zoom--;
  if (zoom < -10) zoom = -10;
  setProperty("zoom", QVariant(zoom));
  zoomTo(zoom);
  mutex->unlock();
}

void SonicPiScintilla::newLine() {
  mutex->lock();
  SendScintilla(QsciCommand::Newline);
  mutex->unlock();
}

void SonicPiScintilla::replaceBuffer(QString content, int line, int index, int first_line) {
  mutex->lock();
  beginUndoAction();
  insert(" ");
  SendScintilla(QsciCommand::Delete);
  selectAll();
  replaceSelectedText(content);
  setCursorPosition(line, index);
  setFirstVisibleLine(first_line);
  endUndoAction();
  mutex->unlock();
}

void SonicPiScintilla::completeListOrNewlineAndIndent(){
  mutex->lock();
  if(isListActive()) {
    tabCompleteifList();
  }
  else {
    if(autoIndent) {
      newlineAndIndent();
    } else {
      newLine();
    }
  }
  mutex->unlock();
}

void SonicPiScintilla::newlineAndIndent() {
  mutex->lock();
  int point_line, point_index, first_line;
  getCursorPosition(&point_line, &point_index);
  first_line = firstVisibleLine();

  std::string code = text().toStdString();

  emit bufferNewlineAndIndent(point_line, point_index, first_line, code, fileName.toStdString());
  mutex->unlock();
}

void SonicPiScintilla::dragEnterEvent(QDragEnterEvent *event) {
  mutex->lock();
  if (event->mimeData()->hasFormat("text/uri-list")) {
    event->acceptProposedAction();
  }
  mutex->unlock();
}

void SonicPiScintilla::dragMoveEvent(QDragMoveEvent *event) {
  mutex->lock();
  if (event->mimeData()->hasFormat("text/uri-list")) {
    event->acceptProposedAction();
  }
  mutex->unlock();
}

bool SonicPiScintilla::event(QEvent *evt) {
  mutex->lock();
  if (evt->type()==QEvent::KeyPress) {
    QKeyEvent* key = static_cast<QKeyEvent*>(evt);
    if (key->key() == Qt::Key_Return || key->key() == Qt::Key_Enter) {
      completeListOrNewlineAndIndent();
      return true;
    }
  }

  return QsciScintilla::event(evt);
  mutex->unlock();
}


void SonicPiScintilla::dropEvent(QDropEvent *dropEvent) {
  mutex->lock();
  if (dropEvent->mimeData()->hasFormat("text/uri-list")) {
    dropEvent->acceptProposedAction();
    QList<QUrl> urlList = dropEvent->mimeData()->urls();
    QString text;
    for (int i = 0; i < urlList.size(); ++i) {
      text += "\"" + urlList.at(i).toLocalFile() + "\"" + QLatin1Char('\n');
    }
    insert(text);
  }
  mutex->unlock();
}

void SonicPiScintilla::sp_paste() {
  mutex->lock();
  SendScintilla(QsciCommand::Paste);
  deselect();
  mutex->unlock();
}

void SonicPiScintilla::sp_cut() {
  mutex->lock();
  SendScintilla(QsciCommand::SelectionCut);
  deselect();
  mutex->unlock();
}

void SonicPiScintilla::showAutoCompletion(bool val) {
  if(val) {
      setAutoCompletionThreshold(1);
    } else {
    setAutoCompletionThreshold(-1);
  }
}

void SonicPiScintilla::setText(const QString &text) {
  SendScintilla(SCI_SETTEXT, ScintillaBytesConstData(textAsBytes(text)));
}

void SonicPiScintilla::setAutoIndentEnabled(bool enabled) {
  this->autoIndent = enabled;
}
