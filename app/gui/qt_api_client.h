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
Q_DECLARE_METATYPE(std::vector<SonicPi::TrackInfo>);
Q_DECLARE_METATYPE(std::vector<SonicPi::TrackParamInfo>);
Q_DECLARE_METATYPE(std::vector<SonicPi::TrackPluginInfo>);
Q_DECLARE_METATYPE(std::vector<std::string>);
Q_DECLARE_METATYPE(SonicPi::BufferInfo);
Q_DECLARE_METATYPE(SonicPi::ScsynthInfo);
Q_DECLARE_METATYPE(SonicPi::AudioDevicesInfo);
Q_DECLARE_METATYPE(SonicPi::AudioInputDevicesInfo);
Q_DECLARE_METATYPE(SonicPi::AudioDeviceTableInfo);
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
    virtual void Tracks(int laneBase, const std::vector<SonicPi::TrackInfo>& tracks) override;
    virtual void TrackState(int id, float gain, bool mute) override;
    virtual void TrackFolders(const std::vector<std::string>& extra,
                              const std::vector<std::string>& platform) override;
    virtual void LinkAudioChannels(const std::vector<SonicPi::LinkAudioChannelInfo>& channels) override;
    virtual void LinkAudioInputs(const std::vector<SonicPi::LinkAudioInputInfo>& inputs) override;
    virtual void TrackPlugins(uint32_t total, uint32_t offset,
                              const std::vector<SonicPi::TrackPluginInfo>& plugins) override;
    virtual void TrackParams(int handle, uint32_t total, uint32_t offset,
                             const std::vector<SonicPi::TrackParamInfo>& params) override;
    virtual void TrackParamEdit(int handle, uint32_t id, double normalized, bool own) override;
    virtual void TrackError(const std::string& verb, const std::string& detail, int handle) override;
    virtual void Version(const SonicPi::VersionInfo& info) override;
    virtual void Buffer(const BufferInfo& info) override;
    virtual void ActiveLinks(const int numLinks) override;
    virtual void BPM(const double bpm) override;
    virtual void MixerSettings(double drive, double outputVolume) override;
    virtual void Scsynth(const SonicPi::ScsynthInfo& scsynthInfo) override;
    virtual void AudioDevices(const SonicPi::AudioDevicesInfo& devicesInfo) override;
    virtual void AudioInputDevices(const SonicPi::AudioInputDevicesInfo& devicesInfo) override;
    virtual void AudioDeviceTable(const SonicPi::AudioDeviceTableInfo& table) override;
    virtual void AudioDeviceConfig(const SonicPi::AudioDeviceConfigInfo& configInfo) override;
    virtual void SupersonicSetup(int sampleRate, int bufferSize) override;
    virtual void SpiderReady() override;
    virtual void AudioSwitchDone(const SonicPi::AudioSwitchOutcome& outcome) override;
    virtual void AudioDeviceReopenReply(bool accepted, const std::string& reason) override;
    virtual void RunStarted(int jobId, const std::string& workspace) override;
    virtual void RunEnded(int jobId) override;
    virtual void Flash(const std::string& workspace, int line) override;
    virtual void LiveLoopScope(int jobId, const std::string& name, const std::string& workspace,
                               int line, int scopeNum) override;
    virtual void LiveLoopScopeEnded(int jobId, const std::string& name) override;

signals:
    void ConsumeAudioData(SonicPi::ProcessedAudioPtr audio);
    void UpdateNumActiveLinks(const int numLinks);
    void UpdateBPM(const double bpm);
    void MixerSettingsReceived(double drive, double outputVolume);
    void AudioDevicesReceived(const SonicPi::AudioDevicesInfo& devicesInfo);
    void AudioInputDevicesReceived(const SonicPi::AudioInputDevicesInfo& devicesInfo);
    void AudioDeviceConfigReceived(const SonicPi::AudioDeviceConfigInfo& configInfo);
    void SupersonicSetupReceived(int sampleRate, int bufferSize);
    void SpiderReadyReceived();
    void AudioSwitchDoneReceived(const SonicPi::AudioSwitchOutcome& outcome);
    void AudioDeviceReopenReplyReceived(bool accepted, const QString& reason);
    void RunStartedReceived(int jobId, const QString& workspace);
    void RunEndedReceived(int jobId);
    void FlashReceived(const QString& workspace, int line);
    void LiveLoopScopeReceived(int jobId, const QString& name, const QString& workspace,
                               int line, int scopeNum);
    void LiveLoopScopeEndedReceived(int jobId, const QString& name);

public:
    // Last StartupError text received from the server (e.g. "SuperSonic
    // Audio Server Boot Error\n…"). MainWindow reads this when the boot
    // poll gives up so the boot-error dialog names the actual cause
    // instead of the generic "unable to connect to the Ruby server".
    // GUI-thread only (written by the marshalled ReportGui slot).
    QString GetStartupErrorText() const { return m_startupErrorText; }

public slots:
    virtual void ReportGui(const SonicPi::MessageInfo& message);
    virtual void CueGui(const SonicPi::CueInfo& info);
    virtual void StatusGui(const SonicPi::StatusInfo& info);
    virtual void MidiGui(const SonicPi::MidiInfo& info);
    virtual void GamepadDevicesGui(const QString& devices);
    virtual void TracksGui(int laneBase, const std::vector<SonicPi::TrackInfo>& tracks);
    virtual void TrackStateGui(int id, float gain, bool mute);
    virtual void TrackFoldersGui(const std::vector<std::string>& extra,
                                 const std::vector<std::string>& platform);
    virtual void LinkAudioChannelsGui(const std::vector<SonicPi::LinkAudioChannelInfo>& channels);
    virtual void LinkAudioInputsGui(const std::vector<SonicPi::LinkAudioInputInfo>& inputs);
    virtual void TrackPluginsGui(unsigned int total, unsigned int offset,
                                 const std::vector<SonicPi::TrackPluginInfo>& plugins);
    virtual void TrackParamsGui(int handle, unsigned int total, unsigned int offset,
                                const std::vector<SonicPi::TrackParamInfo>& params);
    virtual void TrackParamEditGui(int handle, unsigned int id, double normalized, bool own);
    virtual void TrackErrorGui(const QString& verb, const QString& detail, int handle);
    virtual void VersionGui(const SonicPi::VersionInfo& info);
    virtual void BufferGui(const SonicPi::BufferInfo& info);
    virtual void ScsynthGui(const SonicPi::ScsynthInfo& scsynthInfo);
    virtual void AudioDevicesGui(const SonicPi::AudioDevicesInfo& devicesInfo);
    virtual void AudioInputDevicesGui(const SonicPi::AudioInputDevicesInfo& devicesInfo);
    virtual void AudioDeviceTableGui(const SonicPi::AudioDeviceTableInfo& table);
    virtual void AudioDeviceConfigGui(const SonicPi::AudioDeviceConfigInfo& configInfo);

private:
    std::array<int, 20> last_incoming_path_lens;
    MainWindow* m_pMainWindow = nullptr;
    QString m_startupErrorText;
};

} // namespace SonicPi
