#pragma once


#include <array>
#include <QObject>
#include <QString>
#include <api/sonicpi_api.h>

#include "config.h"

class MainWindow;

Q_DECLARE_METATYPE(SonicPi::MessageInfo);
Q_DECLARE_METATYPE(SonicPi::CueInfo);
Q_DECLARE_METATYPE(SonicPi::ProcessedAudioPtr);
Q_DECLARE_METATYPE(SonicPi::StatusInfo);
Q_DECLARE_METATYPE(SonicPi::MidiInfo);
Q_DECLARE_METATYPE(SonicPi::VersionInfo);
Q_DECLARE_METATYPE(SonicPi::BufferInfo);
Q_DECLARE_METATYPE(SonicPi::ScsynthInfo);
Q_DECLARE_METATYPE(SonicPi::AudioDevicesInfo);
Q_DECLARE_METATYPE(SonicPi::AudioInputDevicesInfo);
Q_DECLARE_METATYPE(SonicPi::AudioDeviceConfigInfo);
// Qt 6 auto-registers metatypes for signal parameter types; an explicit
// Q_DECLARE_METATYPE for SonicPi::AudioSwitchOutcome causes a double-
// specialization compile error. The qRegisterMetaType call in
// qt_api_client.cpp's constructor is sufficient for queued-connection
// support.

namespace SonicPi
{

class QtAPIClient : public QObject, public SonicPi::IAPIClient
{
    Q_OBJECT
public:
    explicit QtAPIClient(MainWindow* pMainWindow);
    ~QtAPIClient();

    // IAPIClient
    // This is not thread safe; each of these calls is marshalled onto their 'Gui'
    // equivalents below, which are then on the Gui thread.
    virtual void Report(const SonicPi::MessageInfo& message) override;
    virtual void Cue(const SonicPi::CueInfo& info) override;
    virtual void AudioDataAvailable(SonicPi::ProcessedAudioPtr audio) override;
    virtual void Status(const SonicPi::StatusInfo& info) override;
    virtual void Midi(const SonicPi::MidiInfo& info) override;
    virtual void GamepadDevices(const std::string& devices) override;
    virtual void Version(const SonicPi::VersionInfo& info) override;
    virtual void Buffer(const BufferInfo& info) override;
    virtual void ActiveLinks(const int numLinks) override;
    virtual void BPM(const double bpm) override;
    virtual void Scsynth(const SonicPi::ScsynthInfo& scsynthInfo) override;
    virtual void AudioDevices(const SonicPi::AudioDevicesInfo& devicesInfo) override;
    virtual void AudioInputDevices(const SonicPi::AudioInputDevicesInfo& devicesInfo) override;
    virtual void AudioDeviceConfig(const SonicPi::AudioDeviceConfigInfo& configInfo) override;
    virtual void SupersonicSetup(int sampleRate, int bufferSize) override;
    virtual void SpiderReady() override;
    virtual void AudioSwitchDone(const SonicPi::AudioSwitchOutcome& outcome) override;

signals:
    void ConsumeAudioData(SonicPi::ProcessedAudioPtr audio);
    void UpdateNumActiveLinks(const int numLinks);
    void UpdateBPM(const double bpm);
    void AudioDevicesReceived(const SonicPi::AudioDevicesInfo& devicesInfo);
    void AudioInputDevicesReceived(const SonicPi::AudioInputDevicesInfo& devicesInfo);
    void AudioDeviceConfigReceived(const SonicPi::AudioDeviceConfigInfo& configInfo);
    void SupersonicSetupReceived(int sampleRate, int bufferSize);
    void SpiderReadyReceived();
    void AudioSwitchDoneReceived(const SonicPi::AudioSwitchOutcome& outcome);

public slots:
    virtual void ReportGui(const SonicPi::MessageInfo& message);
    virtual void CueGui(const SonicPi::CueInfo& info);
    virtual void StatusGui(const SonicPi::StatusInfo& info);
    virtual void MidiGui(const SonicPi::MidiInfo& info);
    virtual void GamepadDevicesGui(const QString& devices);
    virtual void VersionGui(const SonicPi::VersionInfo& info);
    virtual void BufferGui(const SonicPi::BufferInfo& info);
    virtual void ScsynthGui(const SonicPi::ScsynthInfo& scsynthInfo);
    virtual void AudioDevicesGui(const SonicPi::AudioDevicesInfo& devicesInfo);
    virtual void AudioInputDevicesGui(const SonicPi::AudioInputDevicesInfo& devicesInfo);
    virtual void AudioDeviceConfigGui(const SonicPi::AudioDeviceConfigInfo& configInfo);

private:
    std::array<int, 20> last_incoming_path_lens;
    MainWindow* m_pMainWindow = nullptr;
};

} // namespace SonicPi
