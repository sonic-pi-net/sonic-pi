#ifndef SETTINGS_H
#define SETTINGS_H

#include <QString>

class SonicPiSettings {
public:    
    enum Theme { LightMode, DarkMode, LightProMode, DarkProMode, HighContrastMode };

    // Audio Settings
    int main_volume;
    bool mixer_invert_stereo;
    bool mixer_force_mono;
    bool check_args;
    bool synth_trigger_timing_guarantees;
    bool enable_external_synths;

    // IOSettings
    bool osc_server_enabled;
    bool osc_public;
    bool midi_enabled;
    int midi_default_channel;
    QString midi_default_channel_str;

    // EditorSettings
    bool auto_indent_on_run;
    bool show_line_numbers;
    bool show_log;
    bool show_incoming_osc_log;
    bool show_buttons;
    bool show_tabs;
    bool full_screen;
    bool print_output;
    bool clear_output_on_run;
    bool log_cues;
    bool log_auto_scroll;

    Theme theme;

    // UpdateSettings;
    bool check_updates;
};
#endif
