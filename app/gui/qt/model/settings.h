#ifndef SETTINGS_H
#define SETTINGS_H

#include <QString>
#include <map>
#include "sonicpitheme.h"
class SonicPiSettings {
public:


    // Audio Settings
    int main_volume;
    bool mixer_invert_stereo;
    bool mixer_force_mono;
    bool check_args;
    bool synth_trigger_timing_guarantees;
    bool enable_external_synths;
    bool enable_scsynth_inputs;

    // IOSettings
    bool osc_server_enabled;
    bool osc_public;
    bool midi_enabled;
    int midi_default_channel;
    QString midi_default_channel_str;

    // EditorSettings
    QString language;
    bool auto_indent_on_run;
    bool show_line_numbers;
    bool show_log;
    bool show_cues;
    bool show_buttons;
    bool show_tabs;
    bool show_metro;
    bool full_screen;
    bool goto_buffer_shortcuts;
    bool log_synths;
    bool clear_output_on_run;
    bool log_cues;
    bool log_auto_scroll;
    int gui_transparency;
    bool show_autocompletion;
    bool show_context;
    SonicPiTheme::Style themeStyle;

    // UpdateSettings;
    bool check_updates;

    // Visualizer
    bool show_scopes;
    bool show_scope_labels;
    bool show_titles;
    bool hide_menubar_in_fullscreen;
    std::vector<QString> scope_names;
    void setScopeState(QString name, bool s) { active_scopes[name] = s; }
    bool isScopeActive(QString name) { return active_scopes[name]; }
private:
    std::map<QString, bool> active_scopes;

};
#endif
