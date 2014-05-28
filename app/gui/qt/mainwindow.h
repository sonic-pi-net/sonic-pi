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
#include <QCheckBox>
#include <QListWidgetItem>
#include <QListWidget>
#include <QProcess>
#include <QFuture>
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

struct help_page {
    QString title;
    QString filename;
};

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
    void beautifyCode();
    void stopRunningSynths();
    QString currentTabLabel();
    bool saveAs();
    void about();
    void help();
    void documentWasModified();
    void onExitCleanup();
    void zoomFontIn();
    void zoomFontOut();
    void toggleRecording();
    void toggleRecordingOnIcon();
    void changeSystemVol(int val);
    void setSystemAudioAuto();
    void setSystemAudioHeadphones();
    void setSystemAudioHDMI();
    void showPrefsPane();
    void updateDocPane(QListWidgetItem *cur, QListWidgetItem *prev);
    void serverError(QProcess::ProcessError error);
    void serverFinished(int exitCode, QProcess::ExitStatus exitStatus);
    void replaceBuffer(QString id, QString content);

private:

    void initWorkspace(QsciScintilla* ws);
    void startOSCListener();
    void clearOutputPanels();
    void createActions();
    void createToolBar();
    void createStatusBar();
    void readSettings();
    void writeSettings();
    void loadFile(const QString &fileName, QsciScintilla* &text);
    bool saveFile(const QString &fileName, QsciScintilla* text);
    void loadWorkspaces();
    void saveWorkspaces();
	std::string number_name(int);
    std::string workspaceFilename(QsciScintilla* text);
    QsciScintilla* filenameToWorkspace(std::string filename);
    void sendOSC(oscpkt::Message m);
    void initPrefsWindow();
    void initDocsWindow();
    void setHelpText(QListWidgetItem *item, const QString filename);
    void addHelpPage(QListWidget *nameList, struct help_page *helpPages,
                     int len);
    QListWidget *createHelpTab(QTextEdit *docPane, QString name);

    QFuture<void> osc_thread;

    bool cont_listening_for_osc;
    bool server_started;
    bool osc_incoming_port_open;
    bool is_recording;
    bool show_rec_icon_a;
    QTimer *rec_flash_timer;

    QsciScintilla *textEdit;
    static const int workspace_max = 8;
    QsciScintilla *workspaces[workspace_max];
    QTextEdit *outputPane;
    QTextEdit *errorPane;
    QWidget *prefsCentral;
    QTabWidget *docsCentral;
    QDockWidget *outputWidget;
    QDockWidget *prefsWidget;
    QDockWidget *docWidget;
    QTextEdit *langDocPane;
    QTextEdit *synthsDocPane;
    QTextEdit *fxDocPane;
    QTextEdit *samplesDocPane;
    QTextEdit *examplesDocPane;

    QTabWidget *tabs;

    QProcess *serverProcess;

    SonicPiLexer *lexer;

    QMenu *fileMenu;
    QMenu *editMenu;
    QMenu *helpMenu;

    QToolBar *toolBar;

    QAction *runAct;
    QAction *stopAct;
    QAction *saveAct;
    QAction *recAct;
    QAction *infoAct;
    QAction *prefsAct;
    QAction *helpAct;
    QAction *textIncAct;
    QAction *textDecAct;

    QAction *saveAsAct;
    QAction *exitAct;
    QAction *cutAct;
    QAction *copyAct;
    QAction *pasteAct;

    QCheckBox *print_output;
    QCheckBox *check_args;
    QCheckBox *clear_output_on_run;

    QAction *aboutQtAct;
    QMap<QString, QString> *map;

    QMainWindow *infoWindow;
    QMainWindow *docWindow;
    QLabel *imageLabel;
    QSlider *systemVol;

};

#endif
