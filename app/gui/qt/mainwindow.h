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

#include <QSplitter>
#include <QDate>
#include <QMainWindow>
#include <QDialog>
#include <QLabel>
#include <QSplashScreen>
#include <QCheckBox>
#include <QRadioButton>
#include <QListWidgetItem>
#include <QListWidget>
#include <QVBoxLayout>
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
#include <QSignalMapper>

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
class SonicPiLog;
class SonicPiScintilla;
class SonicPiServer;

struct help_page {
  QString title;
  QString keyword;
  QString url;
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
    MainWindow(QApplication &ref, bool i18n, QMainWindow* splash);
#else
    MainWindow(QApplication &ref, bool i18n, QSplashScreen* splash);
#endif
    void invokeStartupError(QString msg);
    SonicPiServer *sonicPiServer;
    enum {UDP=0, TCP=1};
    QCheckBox *dark_mode;
    bool loaded_workspaces;

protected:
    void closeEvent(QCloseEvent *event);
    void wheelEvent(QWheelEvent *event);

private slots:
    void changeTab(int id);
    QString asciiArtLogo();
    void printAsciiArtLogo();
    void unhighlightCode();
    void runCode();
    void update_mixer_invert_stereo();
    void update_mixer_force_mono();
    void update_check_updates();
    void check_for_updates_now();
    void enableCheckUpdates();
    void disableCheckUpdates();
    void stopCode();
    void beautifyCode();
    void completeListOrIndentLine(QObject *ws);
    void indentCurrentLineOrSelection(SonicPiScintilla *ws);
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
    void toggleRecording();
    void toggleRecordingOnIcon();
    void changeRPSystemVol(int val);
    void setRPSystemAudioAuto();
    void setRPSystemAudioHeadphones();
    void setRPSystemAudioHDMI();
    void changeShowLineNumbers();
    void toggleDarkMode();
    void updateDarkMode();
    void showPrefsPane();
    void updateDocPane(QListWidgetItem *cur);
    void updateDocPane2(QListWidgetItem *cur, QListWidgetItem *prev);
    void serverStarted();
    void splashClose();
    void startupError(QString msg);
    void replaceBuffer(QString id, QString content, int line, int index, int first_line);
    void replaceLines(QString id, QString content, int first_line, int finish_line, int point_line, int point_index);
    void tabNext();
    void tabPrev();
    void helpContext();
    void resetErrorPane();
    void helpScrollUp();
    void helpScrollDown();
    void docScrollUp();
    void docScrollDown();
    void helpClosed(bool visible);
    void updateFullScreenMode();
    void toggleFullScreenMode();
    void updateFocusMode();
    void toggleFocusMode();
    void updateLogVisibility();
    void toggleLogVisibility();
    void updateTabsVisibility();
    void toggleTabsVisibility();
    void updateButtonVisibility();
    void toggleButtonVisibility();
    void setLineMarkerinCurrentWorkspace(int num);
    void setUpdateInfoText(QString t);
    void updateVersionNumber(QString version, int version_num, QString latest_version, int latest_version_num, QDate last_checked_date, QString platform);
    void requestVersion();
    void open_sonic_pi_net();
    void heartbeatOSC();

private:

    QSignalMapper *signalMapper;
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
    void refreshDocContent();
    void addHelpPage(QListWidget *nameList, struct help_page *helpPages,
                     int len);
    QListWidget *createHelpTab(QString name);
    QKeySequence metaKey(char key);
    QKeySequence shiftMetaKey(char key);
    QKeySequence ctrlMetaKey(char key);
    QKeySequence ctrlKey(char key);
    char int2char(int i);
    void setupAction(QAction *action, char key, QString tooltip,
		     const char *slot);
    QString tooltipStrShiftMeta(char key, QString str);
    QString tooltipStrMeta(char key, QString str);
    QString readFile(QString name);
    QString rootPath();

    void addUniversalCopyShortcuts(QTextEdit *te);

    QTcpSocket *clientSock;
    QFuture<void> osc_thread, server_thread;
    int protocol;

    bool focusMode;
    bool startup_error_reported;
    bool is_recording;
    bool show_rec_icon_a;
    QTimer *rec_flash_timer;

#ifdef Q_OS_MAC
    QMainWindow* splash;
#else
    QSplashScreen* splash;
#endif

    bool i18n;
    static const int workspace_max = 10;
    SonicPiScintilla *workspaces[workspace_max];
    QWidget *prefsCentral;
    QTabWidget *docsCentral;
    SonicPiLog *outputPane;
    QTextBrowser *errorPane;
    QDockWidget *outputWidget;
    QDockWidget *prefsWidget;
    QDockWidget *hudWidget;
    QDockWidget *docWidget;
    QTextBrowser *docPane;
//  QTextBrowser *hudPane;
    QWidget *mainWidget;
    bool hidingDocPane;
    bool restoreDocPane;

    QTabWidget *tabs;
    QTabWidget *prefTabs;

    QProcess *serverProcess;

    SonicPiLexer *lexer;

    QToolBar *toolBar;

    QAction *recAct;

    QCheckBox *mixer_invert_stereo;
    QCheckBox *mixer_force_mono;
    QCheckBox *print_output;
    QCheckBox *check_args;
    QCheckBox *clear_output_on_run;
    QCheckBox *log_cues;
    QCheckBox *show_line_numbers;
    QCheckBox *full_screen;
    QCheckBox *show_log;
    QCheckBox *show_buttons;
    QCheckBox *show_tabs;
    QCheckBox *check_updates;
    QPushButton *check_updates_now;
    QPushButton *visit_sonic_pi_net;
    QLabel *update_info;

    QRadioButton *rp_force_audio_hdmi;
    QRadioButton *rp_force_audio_default;
    QRadioButton *rp_force_audio_headphones;
    QSlider *rp_system_vol;

    QWidget *infoWidg;
    QList<QTextBrowser *> infoPanes;
    QTextEdit *startupPane;
    QVBoxLayout *mainWidgetLayout;

    QList<QListWidget *> helpLists;
    QHash<QString, help_entry> helpKeywords;
    std::streambuf *coutbuf;
#if defined(Q_OS_WIN) || defined(Q_OS_MAC)
    std::ofstream stdlog;
#endif

    SonicPiAPIs *autocomplete;
    QString sample_path, log_path;
    QString defaultTextBrowserStyle;

    QString version;
    int version_num;
    QString latest_version;
    int latest_version_num;

    QSplitter *docsplit;

    QLabel *versionLabel;

    QString guiID;

};

#endif
