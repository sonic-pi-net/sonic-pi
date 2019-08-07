#ifndef SETTINGS_H
#define SETTINGS_H

class SonicPiSettings {
public:    
    // Audio Settings
    bool mixer_invert_stereo;
    bool mixer_force_mono;

    // IOSettings
    bool osc_server_enabled;
    bool osc_public;

    bool midi_enabled;
};
#endif
