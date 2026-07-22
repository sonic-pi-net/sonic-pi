#include <cassert>
#include <cstdlib>
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
    virtual void AudioDataAvailable(ProcessedAudioPtr audio) override
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

    virtual void AudioDevices(const AudioDevicesInfo& devicesInfo) override
    {
    }

    virtual void AudioInputDevices(const AudioInputDevicesInfo& devicesInfo) override
    {
    }

    virtual void AudioDeviceConfig(const AudioDeviceConfigInfo& configInfo) override
    {
    }

    virtual void SupersonicSetup(int sampleRate, int bufferSize) override
    {
    }

    virtual void SpiderReady() override
    {
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

namespace
{

void SetHomeEnv(const char* value)
{
#if defined(WIN32)
    _putenv_s("SONIC_PI_HOME", value);
#else
    if (*value)
        setenv("SONIC_PI_HOME", value, 1);
    else
        unsetenv("SONIC_PI_HOME");
#endif
}

} // namespace

// A home directory containing non-ASCII characters must be usable end to
// end: every layer that handles the path as narrow bytes has to agree on
// the encoding (UTF-8), or the writability probe and the created
// directories end up referring to differently-named locations.
TEST_CASE("Init with non-ASCII home dir", "API")
{
    // "sptest-é-ü" as explicit UTF-8 bytes, so the assertion doesn't
    // depend on this source file's execution charset.
    const std::string dirName = "sptest-\xC3\xA9-\xC3\xBC";
    fs::path home = fs::temp_directory_path() / dirName;

    // Sweep every sptest-* entry, mangled leftovers included, so the
    // sibling scan below starts from a known-clean state.
    for (const auto& entry : fs::directory_iterator(home.parent_path()))
    {
        if (entry.path().filename().string().rfind("sptest-", 0) == 0)
        {
            fs::remove_all(entry.path());
        }
    }
    fs::create_directories(home);
    SetHomeEnv(home.string().c_str());

    APIClient client;
    SonicPiAPI api(&client, APIProtocol::UDP, LogOption::File);
    APIInitResult result = api.Init(fs::path(APP_ROOT) / "..");
    api.Shutdown();

    SetHomeEnv("");

    REQUIRE(result == APIInitResult::Successful);

    // The probe must have created the log tree under the real directory,
    // not under a mangled sibling.
    REQUIRE(fs::exists(home / ".sonic-pi" / "log"));

    fs::path parent = home.parent_path();
    for (const auto& entry : fs::directory_iterator(parent))
    {
        auto name = entry.path().filename().string();
        if (name.rfind("sptest-", 0) == 0)
        {
            REQUIRE(name == dirName);
        }
    }

    fs::remove_all(home);
}
