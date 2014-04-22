//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, distribution,
// and distribution of modified versions of this work as long as this
// notice is included.
//++


#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QDialog>
#include <QLabel>
#include <QSplashScreen>
#include "oscpkt.hh"
#include "udp.hh"
#include <iostream>

class QAction;
class QMenu;
class QsciScintilla;
class QProcess;
class QTextEdit;
class SonicPiLexer;
class QString;
class QSlider;

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QApplication &ref, QSplashScreen &splash);

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
    void toggleRecording();
    void toggleRecordingOnIcon();
    void changeSystemVol();
    void setSystemAudioAuto();
    void setSystemAudioHeadphones();
    void setSystemAudioHDMI();

private:

    void initWorkspace(QsciScintilla* ws);
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
    void loadWorkspaces();
    void saveWorkspaces();
    std::string workspaceFilename(QsciScintilla* text);
    QsciScintilla* filenameToWorkspace(std::string filename);
    void sendOSC(oscpkt::Message m);

    bool cont_listening_for_osc;
    bool server_started;
    bool osc_incoming_port_open;
    bool is_recording;
    bool show_rec_icon_a;
    QTimer *rec_flash_timer;

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
    QToolBar *saveToolBar;
    QAction *runAct;
    QAction *stopAct;
    QAction *saveAct;
    QAction *recAct;

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

    QSlider *systemVol;

};

#endif
