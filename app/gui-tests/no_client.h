//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

#include "api/sonicpi_api.h"

// A client that hears nothing: what a SonicPiAPI needs to be built without a
// window behind it.
struct NoClient : SonicPi::IAPIClient
{
    void Report(const SonicPi::MessageInfo&) override {}
    void Status(const SonicPi::StatusInfo&) override {}
    void Cue(const SonicPi::CueInfo&) override {}
    void Midi(const SonicPi::MidiInfo&) override {}
    void Version(const SonicPi::VersionInfo&) override {}
    void AudioDataAvailable(SonicPi::ProcessedAudioPtr) override {}
    void Buffer(const SonicPi::BufferInfo&) override {}
    void ActiveLinks(const int) override {}
    void BPM(const double) override {}
    void Scsynth(const SonicPi::ScsynthInfo&) override {}
    void AudioDevices(const SonicPi::AudioDevicesInfo&) override {}
    void AudioInputDevices(const SonicPi::AudioInputDevicesInfo&) override {}
    void AudioDeviceConfig(const SonicPi::AudioDeviceConfigInfo&) override {}
    void SupersonicSetup(int, int) override {}
    void SpiderReady() override {}
};
