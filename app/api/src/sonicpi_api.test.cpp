#include <cassert>
#include <iostream>
#include <catch2/catch_test_macros.hpp>

#include <api/sonicpi_api.h>
#include <api/logger.h>

#include "config.h"

using namespace SonicPi;
using namespace std::chrono;

class APIClient : public IAPIClient
{
    virtual void Report(const MessageInfo& message) override
    {
        /*std::cout << "[TestClient] : MultiMessage" << std::endl;
        for (auto& arg : message.messages)
        {
            std::cout << "[TestClient] :    " << arg.s << std::endl;
        }*/
    }
    virtual void Cue(const CueInfo& info) override
    {
        std::cout << "[TestClient] : " << info.time << " " << info.id << " " << info.address << " " << info.args << std::endl;
    }
    virtual void AudioDataAvailable(const ProcessedAudio& audio) override
    {
        //std::cout << "[TestClient] : Audio Data" ;
    }

    // Inherited via IAPIClient
    virtual void Status(const StatusInfo& info) override
    {
        std::cout << info.id << " " << int(info.type);
    }
    virtual void Midi(const MidiInfo& info) override
    {
        std::cout << info.portInfo;
    }
    virtual void Version(const VersionInfo& info) override
    {
        std::cout << info.version;
    }

    virtual void Buffer(const BufferInfo& info) override
    {
        std::cout << info.content;
    }

    virtual void ActiveLinks(const int numLinks) override
    {
        std::cout << numLinks;
    }

    virtual void BPM(const double bpm) override
    {
        std::cout << bpm;
    }

    virtual void Scsynth(const ScsynthInfo& scsynthInfo) override
    {
        std::cout << scsynthInfo.text;
    }


};

TEST_CASE("Init", "API")
{
    APIClient client;
    SonicPiAPI api(&client, APIProtocol::UDP, LogOption::File);

    REQUIRE(api.Init("bad-path") == APIInitResult::TerminalError);
    REQUIRE(api.Init(".") == APIInitResult::TerminalError);

    // Pass the valid path to the root sonic pi folder
    APIInitResult executeResult = api.Init(fs::path(APP_ROOT) / "..");
    if (executeResult == APIInitResult::Successful)
    {
        if (api.Boot() == APIBootResult::Successful)
        {
            if (api.WaitUntilReady())
            {
                api.TestAudio();
                std::this_thread::sleep_for(3s);
            }
        }
    }

    api.Shutdown();

    // Get the logs after shutdown because then cout is not redirected...
    std::cout << "Logs: " << std::endl
              << api.GetLogs() << std::flush;

    // Require this later to ensure logs are dumped
    //REQUIRE(executeResult == true);
}
