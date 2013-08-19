/****************************************************************************
**
** Copyright (C) 2004-2006 Trolltech ASA. All rights reserved.
**
** This file is part of the example classes of the Qt Toolkit.
**
** Licensees holding a valid Qt License Agreement may use this file in
** accordance with the rights, responsibilities and obligations
** contained therein.  Please consult your licensing agreement or
** contact sales@trolltech.com if any conditions of this licensing
** agreement are not clear to you.
**
** Further information about Qt licensing is available at:
** http://www.trolltech.com/products/qt/licensing.html or by
** contacting info@trolltech.com.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
****************************************************************************/

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

class QAction;
class QMenu;
class QsciScintilla;
class QProcess;
class QTextEdit;
class QsciLexerRuby;
class QString;

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QApplication &ref);

protected:
    void closeEvent(QCloseEvent *event);

private slots:
    void newFile();
    void runCode();
    void stopCode();
    void callInitScript();
    void stopRunningSynths();
    QString currentTabLabel();
    void open();
    bool save();
    bool saveAs();
    void about();
    void documentWasModified();
    void updateOutput();
    void updateError();
    void onExitCleanup();


private:

    void clearOutputPanels();
    void createActions();
    void createMenus();
    void createToolBars();
    void createStatusBar();
    void readSettings();
    void writeSettings();
    void killSynths();
    bool maybeSave();
    void loadFile(const QString &fileName, QsciScintilla* &text);
    bool saveFile(const QString &fileName, QsciScintilla* text);
    bool saveWorkspace(QsciScintilla* text);
    void setCurrentFile(const QString &fileName);
    QString strippedName(const QString &fullFileName);
    void ensureWorkspaces();
    void loadWorkspaces();
    void saveWorkspaces();
    QString workspaceFilename(QsciScintilla* text);

    QsciScintilla *textEdit;
    QsciScintilla *workspace1;
    QsciScintilla *workspace2;
    QsciScintilla *workspace3;
    QsciScintilla *workspace4;
    QsciScintilla *workspace5;
    QsciScintilla *workspace6;
    QsciScintilla *workspace7;
    QsciScintilla *workspace8;
    QTextEdit *outputPane;
    QsciScintilla *errorPane;

    QString curFile;
    QString groupName;

    QTabWidget *tabs;

    QsciLexerRuby *lexer;
    QProcess *runProcess;
    QMenu *fileMenu;
    QMenu *editMenu;
    QMenu *helpMenu;
    QToolBar *fileToolBar;
    QToolBar *editToolBar;
    QAction *runAct;
    QAction *stopAct;
    QAction *newAct;
    QAction *openAct;
    QAction *saveAct;
    QAction *saveAsAct;
    QAction *exitAct;
    QAction *cutAct;
    QAction *copyAct;
    QAction *pasteAct;
    QAction *aboutAct;
    QAction *aboutQtAct;
    QMap<QString, QString> *map;

};

#endif
