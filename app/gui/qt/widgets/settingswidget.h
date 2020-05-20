#ifndef SETTINGSWIDGET_H
#define SETTINGSWIDGET_H

#include "model/settings.h"

#include <QWidget>

class QSlider;
class QTabWidget;
class QBoxLayout;
class QGroupBox;
class QComboBox;
class QCheckBox;
class QPushButton;
class QLabel;
class QLineEdit;
class QButtonGroup;
class QSignalMapper;
class QVBoxLayout;

class SettingsWidget : public QWidget
{
    Q_OBJECT

public:
    SettingsWidget( int server_osc_cues_port, SonicPiSettings *piSettings, QWidget *parent = 0);
    ~SettingsWidget();

    void updateVersionInfo( QString info_string, QString visit, bool sonic_pi_net_visible, bool check_now_visible);
    void updateMidiInPorts( QString in );
    void updateMidiOutPorts( QString out );
    void updateScopeNames(std::vector<QString>);
    QSize sizeHint() const;

private slots:
    void update_mixer_invert_stereo();
    void update_mixer_force_mono();
    void toggleOscServer();
    void toggleMidi();
    void forceMidiReset();
    void changeMainVolume(int);
    void toggleLineNumbers();
    void toggleLog();
    void toggleCuesLog();
    void toggleButtons();
    void toggleFullScreen();
    void toggleTabs();
    void toggleLogAutoScroll();
    void updateColourTheme();
    void toggleScope();
    void toggleScopeLabels();
    void toggleScope( QWidget* qw );
    void openSonicPiNet();
    void toggleCheckUpdates();
    void checkForUpdatesNow();
    void updateSettings();
    void updateTransparency(int t);
    void settingsChanged();

signals:
    void mixerSettingsChanged();
    void oscSettingsChanged();
    void midiSettingsChanged();
    void resetMidi();
    void volumeChanged(int vol);
    void showLineNumbersChanged();
    void showLogChanged();
    void showCuesChanged();
    void showButtonsChanged();
    void showFullscreenChanged();
    void showTabsChanged();
    void logAutoScrollChanged();
    void themeChanged();
    void scopeChanged();
    void scopeLabelsChanged();
    void scopeChanged(QString name);
    void transparencyChanged(int t);
    void checkUpdatesChanged();
    void forceCheckUpdates();

private:
    SonicPiSettings* piSettings;
    int server_osc_cues_port;

    QTabWidget *prefTabs;

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
    QCheckBox* goto_buffer_shortcuts;
    QCheckBox *show_log;
    QCheckBox *show_cues;
    QCheckBox *show_buttons;
    QCheckBox *show_tabs;
    QCheckBox *check_updates;
    QCheckBox *studio_mode;

    QComboBox *midi_default_channel_combo;
    QCheckBox *midi_enable_check;
    QCheckBox *osc_public_check;
    QCheckBox *osc_server_enabled_check;

    QButtonGroup *colourModeButtonGroup;
    QCheckBox *lightModeCheck;
    QCheckBox *darkModeCheck;
    QCheckBox *lightProModeCheck;
    QCheckBox *darkProModeCheck;
    QCheckBox *highContrastModeCheck;

    QSignalMapper *scopeSignalMap;
    QCheckBox *show_scope_labels;
    QCheckBox *show_scopes;
    QVBoxLayout *scope_box_kinds_layout;

    QPushButton *check_updates_now;
    QPushButton *visit_sonic_pi_net;
    QPushButton *check_studio_hash;
    QLineEdit   *user_token;
    QLabel *update_info;
    QLabel *midi_in_ports_label;
    QLabel *midi_out_ports_label;

    QSlider *system_vol_slider;
    QSlider *gui_transparency_slider;

    // TODO
    bool i18n = true;
    QGroupBox* createAudioPrefsTab();
    QGroupBox* createIoPrefsTab();
    QGroupBox* createEditorPrefsTab();
    QGroupBox* createVisualizationPrefsTab();
    QGroupBox* createUpdatePrefsTab();

    QString tooltipStrShiftMeta(char key, QString str);

    void connectAll();
};

#endif
