//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QDialog>
#include <QLabel>
#include <QSplashScreen>
#include <QCheckBox>
#include <QRadioButton>
#include <QListWidgetItem>
#include <QListWidget>
#include <QProcess>
#include <QFuture>
#include <QShortcut>
#include <QSettings>
#include <QHash>
#include <QTcpSocket>
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
class SonicPiServer;

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
    MainWindow(QApplication &ref, QSplashScreen* splash);
#endif
    void invokeStartupError(QString msg);
    SonicPiServer *sonicPiServer;
    enum {UDP=0, TCP=1};

protected:
    void closeEvent(QCloseEvent *event);
    void wheelEvent(QWheelEvent *event);

private slots:

    void unhighlightCode();
    void runCode();
    void update_mixer_invert_stereo();
    void update_mixer_force_mono();
    void update_check_updates();
    void enableCheckUpdates();
    void disableCheckUpdates();
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
    void serverStarted();
    void splashClose();
    void serverError(QProcess::ProcessError error);
    void serverFinished(int exitCode, QProcess::ExitStatus exitStatus);
    void startupError(QString msg);
    void replaceBuffer(QString id, QString content);
    void tabNext();
    void tabPrev();
    void helpContext();
    void resetErrorPane();
    void helpScrollUp();
    void helpScrollDown();
    void docScrollUp();
    void docScrollDown();
    void helpClosed(bool visible);

private:
    void startServer();
    void waitForServiceSync();
    void clearOutputPanels();
    void createShortcuts();
    void createToolBar();
    void createStatusBar();
    void createInfoPane();
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
    QString readFile(QString name);
    QString rootPath();

    void addUniversalCopyShortcuts(QTextEdit *te);

    QTcpSocket *clientSock;
    QFuture<void> osc_thread, server_thread;
    int protocol;

    bool startup_error_reported;
    bool is_recording;
    bool show_rec_icon_a;
    bool loaded_workspaces;
    QTimer *rec_flash_timer;

#ifdef Q_OS_MAC
    QMainWindow* splash;
#else
    QSplashScreen* splash;
#endif

    static const int workspace_max = 9;
    SonicPiScintilla *workspaces[workspace_max];
    QWidget *prefsCentral;
    QTabWidget *docsCentral;
    QTextEdit *outputPane;
    QTextEdit *errorPane;
    QDockWidget *outputWidget;
    QDockWidget *prefsWidget;
    QDockWidget *docWidget;
    QTextBrowser *docPane;
    bool hidingDocPane;

    QTabWidget *tabs;

    QProcess *serverProcess;

    SonicPiLexer *lexer;

    QMenu *fileMenu;
    QMenu *editMenu;
    QMenu *helpMenu;

    QToolBar *toolBar;

    QAction *recAct;

    QCheckBox *mixer_invert_stereo;
    QCheckBox *mixer_force_mono;
    QCheckBox *print_output;
    QCheckBox *check_args;
    QCheckBox *clear_output_on_run;
    QCheckBox *check_updates;

    QRadioButton *rp_force_audio_hdmi;
    QRadioButton *rp_force_audio_default;
    QRadioButton *rp_force_audio_headphones;
    QSlider *rp_system_vol;

    QAction *aboutQtAct;
    QMap<QString, QString> *map;

    QTextBrowser *infoPane;
    QWidget *infoWidg;
    QTextEdit *startupPane;
    QLabel *imageLabel;

    int currentLine;
    int currentIndex;

    QList<QListWidget *> helpLists;
    QHash<QString, help_entry> helpKeywords;
    std::streambuf *coutbuf;
    std::ofstream stdlog;

    SonicPiAPIs *autocomplete;
    QString sample_path, log_path;
};

#endif
