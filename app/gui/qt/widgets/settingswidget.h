#ifndef SETTINGSWIDGET_H
#define SETTINGSWIDGET_H

#include "model/settings.h"

#include <QWidget>
#include <QLabel>
#include <QLineEdit>
#include <QCheckBox>
#include <QComboBox>
#include <QPushButton>
#include <QSignalMapper>
#include <QSlider>
#include <QTabWidget>
#include <QString>
#include <QGroupBox>

class SettingsWidget : public QWidget
{
    Q_OBJECT

public:
    SettingsWidget( QWidget *parent = 0);
    ~SettingsWidget();

    const SonicPiSettings& getSettings() const { return settings; }

private slots:
    void update_mixer_invert_stereo();
    void update_mixer_force_mono();
    void toggleOscServer();
    void toggleMidi();
    void forceMidiReset();
    void changeMainVolume(int);
    void toggleLineNumbers();
    void updateSettings();

signals:
    void mixerSettingsChanged();
    void oscSettingsChanged();
    void midiSettingsChanged();
    void resetMidi();
    void volumeChanged(int vol);
    void showLineNumbersChanged();

private:
    SonicPiSettings settings;
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
    QCheckBox *show_log;
    QCheckBox *show_incoming_osc_log;
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
    QCheckBox *show_scope_axes;
    QCheckBox *show_scopes;

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
};

#endif
