//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
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
#include <QPixmap>
#include <QRadioButton>
#include <QListWidgetItem>
#include <QListWidget>
#include <QVBoxLayout>
#include <QProcess>
#include <QFuture>
#include <QShortcut>
#include <QSettings>
#include <QSet>
#include <QHash>
#include <QTcpSocket>
#include "oscpkt.hh"
#include "udp.hh"
#include <iostream>
#include <sstream>
#include <fstream>
#include <QSignalMapper>
#include "sonicpitheme.h"
#include "scope.h"
#include "oscsender.h"
#include <QComboBox>
#include "infowidget.h"

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
class SonicPiOSCServer;

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

    SonicPiOSCServer *sonicPiOSCServer;
    enum {UDP=0, TCP=1};
    QCheckBox *dark_mode;
    bool loaded_workspaces;

protected:
    void closeEvent(QCloseEvent *event);
    void wheelEvent(QWheelEvent *event);


public slots:
    void invokeStartupError(QString msg);

private slots:
    void addCuePath(QString path, QString val);
    void zoomInLogs();
    void zoomOutLogs();
    QString sonicPiHomePath();
    void updateLogAutoScroll();
    bool eventFilter(QObject *obj, QEvent *evt);
    void changeTab(int id);
    QString asciiArtLogo();
    void printAsciiArtLogo();
    void unhighlightCode();
    void runCode();
    void runBufferIdx(int idx);
    void update_mixer_invert_stereo();
    void update_mixer_force_mono();
    void update_check_updates();
    void check_for_updates_now();
    void enableCheckUpdates();
    void disableCheckUpdates();
    void stopCode();
    void beautifyCode();
    void completeSnippetListOrIndentLine(QObject *ws);
    void completeSnippetOrIndentCurrentLineOrSelection(SonicPiScintilla *ws);
    void toggleCommentInCurrentWorkspace();
    void toggleComment(SonicPiScintilla *ws);
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
    bool loadFile();
    bool saveAs();
    void about();
    void scope();
    void toggleScope();
    void toggleIcons();
    void help();
    void onExitCleanup();
    void toggleRecording();
    void toggleRecordingOnIcon();
    void changeSystemPreAmp(int val, int silent=0);
    void changeGUITransparency(int val);
    void changeShowLineNumbers();
    void toggleScope(QWidget* qw);
    void toggleLeftScope();
    void toggleRightScope();
    void toggleScopeAxes();
    void scopeVisibilityChanged();
    void toggleDarkMode();
    void updateDarkMode();
    void updatePrefsIcon();
    void togglePrefs();
    void updateDocPane(QListWidgetItem *cur);
    void updateDocPane2(QListWidgetItem *cur, QListWidgetItem *prev);
    void showWindow();
    void splashClose();
    void setMessageBoxStyle();
    void startupError(QString msg);
    void replaceBuffer(QString id, QString content, int line, int index, int first_line);
    void replaceBufferIdx(int buf_idx, QString content, int line, int index, int first_line);
    void replaceLines(QString id, QString content, int first_line, int finish_line, int point_line, int point_index);
    void tabNext();
    void tabPrev();
    void helpContext();
    void resetErrorPane();
    void helpScrollUp();
    void helpScrollDown();
    void docScrollUp();
    void docScrollDown();
    void helpVisibilityChanged();
    void updateFullScreenMode();
    void toggleFullScreenMode();
    void updateFocusMode();
    void toggleFocusMode();
    void toggleScopePaused();
    void updateLogVisibility();
    void updateIncomingOscLogVisibility();
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
    void zoomCurrentWorkspaceIn();
    void zoomCurrentWorkspaceOut();
    void showWelcomeScreen();
    void setupWindowStructure();
    void setupTheme();
    void escapeWorkspaces();
    void allJobsCompleted();
    void toggleMidi(int silent=0);
    void toggleOSCServer(int silent=0);
    void resetMidi();
    void honourPrefs();
    void updateMIDIInPorts(QString port_info);
    void updateMIDIOutPorts(QString port_info);

    void showError(QString msg);
    void showBufferCapacityError();
private:

    void checkPort(int port);
    QString osDescription();
    void setupLogPathAndRedirectStdOut();
    QSignalMapper *signalMapper;
    void startRubyServer();
    bool waitForServiceSync();
    void clearOutputPanels();
    void createShortcuts();
    void createToolBar();
    void createStatusBar();
    void createInfoPane();
    void createScopePane();
    void readSettings();
    void writeSettings();
    void loadFile(const QString &fileName, SonicPiScintilla* &text);
    bool saveFile(const QString &fileName, SonicPiScintilla* text);
    void loadWorkspaces();
    void saveWorkspaces();
    std::string number_name(int);
    std::string workspaceFilename(SonicPiScintilla* text);
    SonicPiScintilla* filenameToWorkspace(std::string filename);
    bool sendOSC(oscpkt::Message m);
    void initPrefsWindow();
    void initDocsWindow();
    void refreshDocContent();
    void addHelpPage(QListWidget *nameList, struct help_page *helpPages,
                     int len);
    QListWidget *createHelpTab(QString name);
    QKeySequence metaKey(char key);
    Qt::Modifier metaKeyModifier();
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
    int gui_listen_to_server_port, gui_send_to_server_port, server_listen_to_gui_port, server_send_to_gui_port, scsynth_port, scsynth_send_port, server_osc_cues_port, erlang_router_port, osc_midi_out_port, osc_midi_in_port;
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
    SonicPiLog *incomingPane;
    QTextBrowser *errorPane;
    QDockWidget *outputWidget;
    QDockWidget *incomingWidget;
    QDockWidget *prefsWidget;
    QDockWidget *hudWidget;
    QDockWidget *docWidget;
    QWidget *blankWidget;
    QWidget *outputWidgetTitle;
    QTextBrowser *docPane;
//  QTextBrowser *hudPane;
    QWidget *mainWidget;
    QDockWidget *scopeWidget;
    bool hidingDocPane;
    bool restoreDocPane;

    QTabWidget *tabs;
    QTabWidget *prefTabs;

    QProcess *serverProcess;

    SonicPiLexer *lexer;
    SonicPiTheme *theme;

    QToolBar *toolBar;

    QAction *runAct;
    QAction *stopAct;
    QAction *saveAsAct;
    QAction *loadFileAct;
    QAction *recAct;
    QAction *textIncAct;
    QAction *textDecAct;
    QAction *scopeAct;
    QAction *infoAct;
    QAction *helpAct;
    QAction *prefsAct;

    QCheckBox *mixer_invert_stereo;
    QCheckBox *mixer_force_mono;
    QCheckBox *print_output;
    QCheckBox *check_args;
    QCheckBox *clear_output_on_run;
    QCheckBox *log_cues;
    QCheckBox *log_auto_scroll;
    QCheckBox *enable_external_synths_cb;
    QCheckBox *synth_trigger_timing_guarantees_cb;
    QCheckBox *show_line_numbers;
    QCheckBox *auto_indent_on_run;
    QCheckBox *full_screen;
    QCheckBox *show_log;
    QCheckBox *show_incoming_osc_log;
    QCheckBox *show_buttons;
    QCheckBox *show_tabs;
    QCheckBox *check_updates;

    QComboBox *midi_default_channel_combo;
    QCheckBox *midi_enable_check;
    QCheckBox *osc_public_check;
    QCheckBox *osc_server_enabled_check;
    QCheckBox *pro_icons_check;

    QSignalMapper *scopeSignalMap;
//    QCheckBox *show_left_scope;
//    QCheckBox *show_right_scope;
    QCheckBox *show_scope_axes;
    QCheckBox *show_scopes;

    QPushButton *check_updates_now;
    QPushButton *visit_sonic_pi_net;
    QLabel *update_info;
    QLabel *midi_in_ports_label;
    QLabel *midi_out_ports_label;

    QSlider *system_vol_slider;
    QSlider *gui_transparency_slider;

    InfoWidget *infoWidg;
    QList<QTextBrowser *> infoPanes;
    QTextEdit *startupPane;
    QVBoxLayout *mainWidgetLayout;

    QList<QListWidget *> helpLists;
    QHash<QString, help_entry> helpKeywords;
    std::streambuf *coutbuf;
    std::ofstream stdlog;

    SonicPiAPIs *autocomplete;
    QString sample_path, log_path, sp_user_path, sp_user_tmp_path, ruby_server_path, ruby_path, server_error_log_path, server_output_log_path, gui_log_path, scsynth_log_path, init_script_path, exit_script_path, tmp_file_store, process_log_path, port_discovery_path, qt_app_theme_path, qt_browser_dark_css, qt_browser_light_css;
    QString defaultTextBrowserStyle;

    QString version;
    int version_num;
    QString latest_version;
    int latest_version_num;

    QSplitter *docsplit;

    QLabel *versionLabel;
    Scope* scopeInterface;
    QString guiID;
    bool homeDirWritable, tmpFileStoreAvailable;
    bool updated_dark_mode_for_help, updated_dark_mode_for_prefs;

    OscSender *oscSender;
    QSet<QString> cuePaths;

    QIcon pro_run_icon,
          pro_stop_icon,
          pro_save_icon,
          pro_load_icon,
          pro_rec_icon,
          pro_size_up_icon,
          pro_size_down_icon,
          pro_scope_bordered_icon,
          pro_scope_icon,
          pro_info_bordered_icon,
          pro_info_icon,
          pro_help_bordered_icon,
          pro_help_icon,
          pro_prefs_icon,
          pro_prefs_bordered_icon,
          pro_info_dark_bordered_icon,
          pro_info_dark_icon,
          pro_help_dark_bordered_icon,
          pro_help_dark_icon,
          pro_prefs_dark_bordered_icon,
          pro_prefs_dark_icon,
          pro_rec_b_icon,
          pro_rec_b_dark_icon,
          pro_load_dark_icon,
          pro_save_dark_icon,

          default_light_run_icon,
          default_light_stop_icon,
          default_light_save_icon,
          default_light_load_icon,
          default_light_rec_icon,
          default_light_rec_a_icon,
          default_light_rec_b_icon,
          default_light_size_up_icon,
          default_light_size_down_icon,
          default_light_scope_icon,
          default_light_scope_toggled_icon,
          default_light_info_icon,
          default_light_info_toggled_icon,
          default_light_help_icon,
          default_light_help_toggled_icon,
          default_light_prefs_icon,
          default_light_prefs_toggled_icon,

          default_dark_run_icon,
          default_dark_stop_icon,
          default_dark_save_icon,
          default_dark_load_icon,
          default_dark_rec_icon,
          default_dark_rec_a_icon,
          default_dark_rec_b_icon,
          default_dark_size_up_icon,
          default_dark_size_down_icon,
          default_dark_scope_icon,
          default_dark_scope_toggled_icon,
          default_dark_info_icon,
          default_dark_info_toggled_icon,
          default_dark_help_icon,
          default_dark_help_toggled_icon,
          default_dark_prefs_icon,
          default_dark_prefs_toggled_icon;

};

#endif
