#ifndef SETTINGS_H
#define SETTINGS_H

class SonicPiSettings {
public:    
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
};
#endif
