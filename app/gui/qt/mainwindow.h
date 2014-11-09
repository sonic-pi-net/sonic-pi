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
#include <QShortcut>
#include <QSettings>
#include <QHash>
#include "oscpkt.hh"
#include "udp.hh"
#include <iostream>
#include <sstream>
#include <fstream>

class QAction;
class QMenu;
class QsciScintilla;
class QProcess;
class QTextEdit;
class QTextBrowser;
class SonicPiLexer;
class QString;
class QSlider;
class SonicPiAPIs;
class SonicPiScintilla;

struct help_page {
  QString title;
  QString keyword;
  QString filename;
};

struct help_entry {
  int pageIndex;
  int entryIndex;
};

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
#if defined(Q_OS_MAC)
    MainWindow(QApplication &ref, QMainWindow* splash);
#else
    MainWindow(QApplication &ref, QSplashScreen &splash);
#endif
protected:
    void closeEvent(QCloseEvent *event);
    void wheelEvent(QWheelEvent *event);

private slots:

    void unhighlightCode();
    void runCode();
    void update_mixer_invert_stereo();
    void update_mixer_force_mono();
    void stopCode();
    void beautifyCode();
    void reloadServerCode();
    void stopRunningSynths();
    void mixerInvertStereo();
    void mixerStandardStereo();
    void mixerMonoMode();
    void mixerStereoMode();
    void mixerLpfEnable(float freq);
    void mixerHpfEnable(float freq);
    void mixerHpfDisable();
    void mixerLpfDisable();
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
    void changeRPSystemVol(int val);
    void setRPSystemAudioAuto();
    void setRPSystemAudioHeadphones();
    void setRPSystemAudioHDMI();
    void showPrefsPane();
    void updateDocPane(QListWidgetItem *cur);
    void updateDocPane2(QListWidgetItem *cur, QListWidgetItem *prev);
    void serverError(QProcess::ProcessError error);
    void serverFinished(int exitCode, QProcess::ExitStatus exitStatus);
    void replaceBuffer(QString id, QString content);
    void tabNext();
    void tabPrev();
    void helpContext();

private:
    void addKeyBinding(QSettings &qs, int cmd, int key);
    void addOtherKeyBinding(QSettings &qs, int cmd, int key);
    void initWorkspace(SonicPiScintilla* ws);
    void startOSCListener();
    void clearOutputPanels();
    void createActions();
    void createToolBar();
    void createStatusBar();
    void readSettings();
    void writeSettings();
    void loadFile(const QString &fileName, SonicPiScintilla* &text);
    bool saveFile(const QString &fileName, SonicPiScintilla* text);
    void loadWorkspaces();
    void saveWorkspaces();
    std::string number_name(int);
    std::string workspaceFilename(SonicPiScintilla* text);
    SonicPiScintilla* filenameToWorkspace(std::string filename);
    void sendOSC(oscpkt::Message m);
    void initPrefsWindow();
    void initDocsWindow();
    void setHelpText(QListWidgetItem *item, const QString filename);
    void addHelpPage(QListWidget *nameList, struct help_page *helpPages,
                     int len);
    QListWidget *createHelpTab(QString name);
    QKeySequence cmdAltKey(char key);
    QKeySequence ctrlKey(char key);
    void setupAction(QAction *action, char key, QString tooltip,
		     const char *slot);

    QFuture<void> osc_thread;

    bool cont_listening_for_osc;
    bool server_started;
    bool osc_incoming_port_open;
    bool is_recording;
    bool show_rec_icon_a;
    QTimer *rec_flash_timer;

    SonicPiScintilla *textEdit;
    static const int workspace_max = 8;
    SonicPiScintilla *workspaces[workspace_max];
    QTextEdit *outputPane;
    QTextEdit *errorPane;
    QWidget *prefsCentral;
    QTabWidget *docsCentral;
    QDockWidget *outputWidget;
    QDockWidget *prefsWidget;
    QDockWidget *docWidget;
    QTextBrowser *docPane;

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
    QAction *textAlignAct;
    QAction *textIncAct1;
    QAction *textDecAct1;

    QAction *saveAsAct;
    QAction *exitAct;
    QAction *cutAct;
    QAction *copyAct;
    QAction *pasteAct;

    QShortcut *tabNextKey;
    QShortcut *tabPrevKey;
    QShortcut *textIncKey2;
    QShortcut *textDecKey2;
    QShortcut *reloadKey;

    QCheckBox *mixer_invert_stereo;
    QCheckBox *mixer_force_mono;
    QCheckBox *print_output;
    QCheckBox *check_args;
    QCheckBox *clear_output_on_run;

    QAction *aboutQtAct;
    QMap<QString, QString> *map;

    QTextBrowser *infoPane;
    QWidget *infoWidg;
    QTextEdit *startupPane;
    QLabel *imageLabel;
    QSlider *raspberryPiSystemVol;

    int currentLine;
    int currentIndex;

    QList<QListWidget *> helpLists;
    QHash<QString, help_entry> helpKeywords;
    std::ofstream stdlog;

    SonicPiAPIs *autocomplete;
};

#endif
