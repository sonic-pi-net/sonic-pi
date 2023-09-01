#pragma once

#include <api/logger.h>
#include <api/sonicpi_api.h>
#include <deque>
#include <memory>

#include <mutex>

class SPClient : public SonicPi::IAPIClient
{
public:
    virtual void Report(const SonicPi::MessageInfo& message) override;
    virtual void Cue(const SonicPi::CueInfo& info) override;
    virtual void AudioDataAvailable(const SonicPi::ProcessedAudio& audio) override;
    virtual void Status(const SonicPi::StatusInfo& info) override;
    virtual void Midi(const SonicPi::MidiInfo& info) override;
    virtual void Version(const SonicPi::VersionInfo& info) override;
    virtual void Buffer(const SonicPi::BufferInfo& info) override;
    virtual void ActiveLinks(const int numLinks) override;
    virtual void BPM(const double bpm) override;
    virtual void Scsynth(const SonicPi::ScsynthInfo& scsynthInfo) override;

private:
    void UpdateLog(const std::string& log);

};

struct SPData
{
    std::shared_ptr<SonicPi::SonicPiAPI> spApi;
    std::shared_ptr<SPClient> spClient;
    std::deque<std::string> log;

    // temp
    std::mutex guiMutex;

    bool exit = false;
};
extern SPData sonic;

void start_sonic_pi();
bool sync_sonic_pi();
void stop_sonic_pi();
void show_sonic_pi();
void cleanup_ui();
