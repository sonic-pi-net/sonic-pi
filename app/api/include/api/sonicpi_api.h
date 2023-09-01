#pragma once

#include <atomic>
#include <fstream>
#include <map>
#include <string>
#include <thread>
#include <kissnet.hpp>
#include <mutex>
#include <api/osc/osc_pkt.hh>


#if 0
#ifdef __unix__
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem::v1;
#else
#include <filesystem>
namespace fs = std::filesystem;
#endif
#else
#include <ghc/filesystem.hpp>
namespace fs = ghc::filesystem;
#endif

#include <chrono>

namespace reproc
{
class process;
}

namespace SonicPi
{

class AudioProcessor;
struct ProcessedAudio;
class OscSender;
class OscServer;

enum class APIInitResult
{
    Successful,
    TerminalError,
    HomePathNotWritableError
};

enum class APIBootResult
{
    Successful,
    TerminalError,
    ScsynthBootError,
};

enum class BootDaemonInitResult
{
    Successful,
    TerminalError,
    ScsynthBootError
};

enum class SonicPiPath
{
    RootPath,            // Sonic Pi Application root
    HomePath,            // User-writable home directory root (parent of UserPath).
    UserPath,            // User-writable folder for config/saves etc.
    RubyPath,            // Path to ruby executable
    BootDaemonPath,      // Path to the Boot Daemon script
    FetchUrlPath,        // Path to the fetch url script
    SamplePath,          // Path to the samples folder
    LogPath,             // Base log folder
    SpiderServerLogPath, // Log file for Spider Server output
    BootDaemonLogPath,   // Log file for Boot Daemon output
    TauLogPath,          // Log file for Tau IO Server output
    SCSynthLogPath,      // Log file for SuperCollider scsynth's output
    GUILogPath,          // Log file for GUI
    ClearLogsPath,       // Path to Ruby script for clearing log dir
    ConfigPath,          // Base config folder
    AudioSettingsConfigPath  // Path to toml config file for audio settings
};

// NOTE: These port names returned by ruby; they match the symbols and cannot be changed.
enum class SonicPiPortId
{
    Invalid,
    daemon,
    gui_listen_to_spider,
    gui_send_to_spider,
    scsynth,
    tau_osc_cues,
    tau,
    phx_http
};

// Log output of the API to the log files or the console?
enum class LogOption
{
    File,
    Console
};

using TimePoint = std::chrono::time_point<std::chrono::high_resolution_clock>;

struct CueInfo
{
    std::string time;
    std::string address;
    int id;
    std::string args;
    uint64_t index;
    TimePoint arrivalTime;
};

// This is the processed audio data from the thread
struct ProcessedAudio
{
    std::vector<float> m_spectrum[2];
    std::vector<float> m_spectrumQuantized[2];
    std::vector<float> m_samples[2];
    std::vector<float> m_monoSamples;
};

enum class MessageType
{
    StartupError,
    RuntimeError,
    SyntaxError,
    Message,
    Info,
    InfoText,
    Multi
};

struct MessageData
{
    std::string text;
    int style = 0;
};

struct MessageInfo : MessageData
{
    MessageType type;
    int jobId = 0;
    std::string threadName;
    std::string runtime;
    std::string backtrace;
    int line = 0;
    std::string errorLineString;
    std::string lineNumString;

    std::vector<MessageData> multi;
};

 struct ScsynthInfo
 {
   std::string text;
 };

enum class MidiType
{
    Out,
    In
};

struct MidiInfo
{
    MidiType type;
    std::string portInfo;
};

enum class StatusType
{
    Ack,
    AllComplete,
    Exited
};

struct StatusInfo
{
    StatusType type;
    std::string id;
};

struct VersionInfo
{
    std::string version;
    int num;
    std::string latestVersion;
    int latestVersionNum;
    int lastCheckedDay;
    int lastCheckedMonth;
    int lastCheckedYear;
    std::string platform;
};

enum class BufferType
{
    Replace,
    ReplaceIndex,
    ReplaceLines,
    RunIndex
};

struct BufferInfo
{
    BufferType type;
    std::string id;
    int bufferIndex;
    std::string content;
    int line;
    int index;
    int lineNumber;

    // replace-lines
    int startLine;
    int finishLine;
    int pointLine;
    int pointIndex;
};

// Callbacks from the API into your application/UI.
// These are not on the UI thread, and marshalling is left up to the client
struct IAPIClient
{
    virtual void Report(const MessageInfo& info) = 0;
    virtual void Status(const StatusInfo& info) = 0;
    virtual void Cue(const CueInfo& info) = 0;
    virtual void Midi(const MidiInfo& info) = 0;
    virtual void Version(const VersionInfo& info) = 0;
    virtual void AudioDataAvailable(const ProcessedAudio& audio) = 0;
    virtual void Buffer(const BufferInfo& info) = 0;
    virtual void ActiveLinks(const int numLinks) = 0;
    virtual void BPM(const double bpm) = 0;
    virtual void Scsynth(const ScsynthInfo& scsynthInfo) = 0;
};

// Always UDP
enum class APIProtocol
{
    UDP = 0,
    TCP = 1
};

struct APISettings
{
    bool logSynths = true;
    bool logCues = false;
    bool checkArgs = true;
    bool enableExternalSynths = true;
    bool timingGuarantees = false;
    int defaultMidiChannel = -1;

    void Preprocess(std::string& code) const
    {
        if (!logSynths)
        {
            code = "use_debug false #__nosave__ set by Qt GUI user preferences.\n" + code;
        }

        if (!logCues)
        {
            code = "use_cue_logging false #__nosave__ set by Qt GUI user preferences.\n" + code;
        }

        if (checkArgs)
        {
            code = "use_arg_checks true #__nosave__ set by Qt GUI user preferences.\n" + code;
        }

        if (enableExternalSynths)
        {
            code = "use_external_synths true #__nosave__ set by Qt GUI user preferences.\n" + code;
        }

        if (timingGuarantees)
        {
            code = "use_timing_guarantees true #__nosave__ set by Qt GUI user preferences.\n" + code;
        }

        code = "use_midi_defaults channel: \"" + (defaultMidiChannel== -1 ? "*" : std::to_string(defaultMidiChannel)) + "\" #__nosave__ set by Qt GUI user preferences.\n" + code;
    }
};

class SonicPiAPI
{
public:
    explicit SonicPiAPI(IAPIClient* pClient, APIProtocol protocol = APIProtocol::UDP, LogOption logOption = LogOption::File);
    virtual ~SonicPiAPI();

    // Start the ruby server, connect the ports, find the paths.
    virtual APIInitResult Init(const fs::path& rootPath);
    virtual APIBootResult Boot(bool noScsynthInputs = false);

    virtual void StartClearLogsScript();

    virtual void RestartTau();

    virtual bool LinkEnable();
    virtual bool LinkDisable();

    virtual bool SetLinkBPM(double bpm);
    virtual void SetGlobalTimeWarp(double time);

    // Wait for the server to be in a good state
    virtual bool PingUntilServerCreated();
    virtual bool WaitUntilReady();

    // Shut down the API, close the server, ports, osc, etc.
    virtual void Shutdown();

    // Helper to send a buffer string to Ruby
    virtual void Run(const std::string& buffer, const std::string& text);

    // Stop all music
    virtual void Stop();

    virtual void BufferNewLineAndIndent(int point_line, int point_index, int first_line, const std::string& code, const std::string& fileName);

    // ** Audio processor

    // Enable audio processing
    virtual void AudioProcessor_Enable(bool enable);

    // Enable FFT generation
    virtual void AudioProcessor_EnableFFT(bool enable);

    // Set Max FFT buckets to generate
    virtual void AudioProcessor_SetMaxFFTBuckets(uint32_t buckets);

    // Client has used last audio data
    virtual void AudioProcessor_ConsumedAudio();

    std::string GetLogs();
    std::string GetScsynthLog();

    const int GetGuid() const;

    virtual bool TestAudio();

    // Get a unique path
    virtual const fs::path& GetPath(SonicPiPath piPath);

    // Get a port value
    virtual const int& GetPort(SonicPiPortId port);

    virtual bool SendOSC(oscpkt::Message m);
    virtual bool TauSendOSC(oscpkt::Message m);

    virtual void LoadWorkspaces();

    virtual void SaveWorkspaces(const std::map<uint32_t, std::string>& workspaces);

    virtual uint32_t MaxWorkspaces() const;

    virtual bool SaveAndRunBuffer(const std::string& name, const std::string& code);

    virtual const APISettings& GetSettings() const;
    virtual void SetSettings(const APISettings& settings);

private:
    fs::path FindHomePath() const;

    BootDaemonInitResult StartBootDaemon(bool noScsynthInputs);
    bool StartOscServer();
    void StopOscServer();

    bool InitializePaths(const fs::path& root);
    void EnsurePathsAreCanonical();

    std::error_code RunProcess(const std::vector<std::string>& args, std::string* pOutput = nullptr);
    std::shared_ptr<reproc::process> StartProcess(const std::vector<std::string>& args);



private:

    std::map<SonicPiPath, fs::path> m_paths;
    std::map<SonicPiPortId, int> m_ports;

    bool m_homeDirWriteable = false;
    std::streambuf* m_coutbuf = nullptr;
    std::ofstream m_stdlog;

    std::shared_ptr<reproc::process> m_bootDaemonProcess;

    std::mutex m_osc_mtx;
    bool m_shutdown_engaged = false;
    std::atomic<bool> m_keep_alive = { true };
    LogOption m_logOption;


    std::thread m_oscServerThread;
    std::thread m_pingerThread;
    std::thread m_bootDaemonSockPingLoopThread;

    std::shared_ptr<OscServer> m_spOscSpiderServer;
    std::shared_ptr<OscSender> m_spOscSpiderSender;
    std::shared_ptr<OscSender> m_spOscDaemonSender;
    std::shared_ptr<OscSender> m_spOscTauSender;
    std::shared_ptr<AudioProcessor> m_spAudioProcessor;
    int m_token;

    IAPIClient* m_pClient = nullptr;
    APIProtocol m_protocol = APIProtocol::UDP;

    enum State
    {
        Reset,
        Initializing,
        Invalid,
        Created,
        Error
    };
    State m_state = State::Reset;
    uint64_t m_startServerTime;
    APISettings m_settings;
};

inline uint64_t timer_start()
{
    using namespace std::chrono;
    return duration_cast<microseconds>(high_resolution_clock::now().time_since_epoch()).count();
}

inline float timer_stop(uint64_t start)
{
    using namespace std::chrono;
    auto now = duration_cast<microseconds>(high_resolution_clock::now().time_since_epoch()).count();
    return float((now - start) / 1000000.0);
}

inline float time_to_float_seconds(const TimePoint& pt)
{
    return std::chrono::duration_cast<std::chrono::milliseconds>(pt.time_since_epoch()).count() / 1000.0f;
}

} // namespace SonicPi
