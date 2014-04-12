/****************************************************************************
**
** Copyright (c) 2013, 2014 Samuel Aaron (http://sam.aaron.name)
**
** Permission is hereby granted, free of charge, to any person obtaining a copy
** of this software and associated documentation files (the "Software"), to deal
** in the Software without restriction, including without limitation the rights
** to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
** copies of the Software, and to permit persons to whom the Software is
** furnished to do so, subject to the following conditions:
**
** The above copyright notice and this permission notice shall be included in
** all copies or substantial portions of the Software.
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
** IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
** FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
** AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
** LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
** OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
** THE SOFTWARE.
**
****************************************************************************/

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QDialog>
#include <QLabel>

class QAction;
class QMenu;
class QsciScintilla;
class QProcess;
class QTextEdit;
class SonicPiLexer;
class QString;

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QApplication &ref);

protected:
    void closeEvent(QCloseEvent *event);

private slots:
    void runCode();
    void stopCode();
    void stopRunningSynths();
    QString currentTabLabel();
    bool saveAs();
    void about();
    void help();
    void prefs();
    void documentWasModified();
    void onExitCleanup();
    void zoomFontIn();
    void zoomFontOut();


private:
    void startOSCListener();
    void clearOutputPanels();
    void createActions();
    void createToolBars();
    void createStatusBar();
    void readSettings();
    void writeSettings();
    void killSynths();
    void loadFile(const QString &fileName, QsciScintilla* &text);
    bool saveFile(const QString &fileName, QsciScintilla* text);
    bool saveWorkspace(QsciScintilla* text);
    void loadWorkspaces();
    void saveWorkspaces();
    QString workspaceFilename(QsciScintilla* text);
    //    void sendOSC((;

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
    QTextEdit *errorPane;

    QString curFile;
    QString groupName;

    QTabWidget *tabs;

    QProcess *runProcess;
    QProcess *serverProcess;
    QProcess *proxyProcess;

    SonicPiLexer *lexer;
    QMenu *fileMenu;
    QMenu *editMenu;
    QMenu *helpMenu;
    QToolBar *fileToolBar;
    QToolBar *supportToolBar;
    QToolBar *editToolBar;
    QAction *runAct;
    QAction *stopAct;
    QAction *saveAct;

    QAction *newAct;
    QAction *openAct;

    QAction *infoAct;
    QAction *prefsAct;
    QAction *helpAct;

    QAction *saveAsAct;
    QAction *exitAct;
    QAction *cutAct;
    QAction *copyAct;
    QAction *pasteAct;

    QAction *aboutQtAct;
    QMap<QString, QString> *map;

    QMainWindow *infoWindow;
    QMainWindow *prefsWindow;
    QLabel *imageLabel;

};

#endif
