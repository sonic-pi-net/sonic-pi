#pragma once

#include <atomic>
#include <fstream>
#include <map>
#include <memory>
#include <string>
#include <thread>
#include <vector>
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

// shm_audio_buffer is exposed in the global namespace via using-declarations
// inside the header; include it here so SonicPiAPI's accessor signature has
// the full type. (A forward declaration would create a distinct global type
// that wouldn't match the namespaced original.)
#include "api/audio/shm_audio_buffer.hpp"
// server_shm.hpp exposes ring_view / node_tree_view (used by the SuperSonic
// observability accessors below). It does not depend on this header, so the
// include is acyclic.
#include "api/audio/server_shm.hpp"

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
};

enum class BootDaemonInitResult
{
    Successful,
    TerminalError
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
    SCSynthLogPath,      // Log file for SuperCollider scsynth's output
    SuperSonicLogPath,   // Log file for SuperSonic audio engine
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
    tau_osc_cues
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
    // Per-bucket display values (ballistics applied: instant attack,
    // timed release) and slowly-falling peak-hold markers. Buckets are
    // log-spaced in frequency between m_spectrumFreqMin/Max.
    std::vector<float> m_spectrumQuantized[2];
    std::vector<float> m_spectrumPeaks[2];
    float m_spectrumFreqMin = 30.0f;
    float m_spectrumFreqMax = 20000.0f;
    std::vector<float> m_samples[2];
    std::vector<float> m_monoSamples;
};

// Immutable per-frame snapshot shared between the audio-processor thread
// and the GUI without further copying.
using ProcessedAudioPtr = std::shared_ptr<const ProcessedAudio>;

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
    // Byte-column span of the offending token on the error line; -1 when unavailable.
    int errorColStart = -1;
    int errorColEnd = -1;
    std::string errorLineString;
    std::string lineNumString;

    std::vector<MessageData> multi;
};

 struct ScsynthInfo
 {
   std::string text;
   int sampleRate = 0;
   int bufferSize = 0;
   std::vector<int> availableSampleRates;
   std::vector<int> availableBufferSizes;
   std::vector<std::string> availableDrivers;
   std::string currentDriver;
 };

struct AudioDevicesInfo {
    std::vector<std::string> devices;
    // Driver type (e.g. "ASIO", "Windows Audio", "DirectSound") for each
    // device, parallel to `devices`. Same length when present. Used by
    // the GUI to filter the Output dropdown by selected driver.
    std::vector<std::string> deviceTypes;
    std::string currentDevice;
    std::string mode;
    int sampleRate = 0;
};

struct AudioInputDevicesInfo {
    std::vector<std::string> devices;
    // Parallel driver-type array, see AudioDevicesInfo::deviceTypes.
    std::vector<std::string> deviceTypes;
    std::string currentDevice;
};

// Per-driver device table (/supersonic/device-table): the same devices as
// AudioDevicesInfo/AudioInputDevicesInfo but grouped by driver and NOT
// deduped across drivers, so a client can render any driver's device list
// directly. Absent entirely when the engine predates the message — clients
// then fall back to filtering the flat lists by deviceTypes.
struct AudioDeviceTableInfo {
    struct DriverDevices {
        std::string driver;                 // exact JUCE type name
        std::vector<std::string> outputs;
        std::vector<std::string> inputs;
    };
    std::string currentDriver;
    std::string intendedDriver;             // "" = no pending pick
    std::vector<DriverDevices> drivers;
};

struct AudioDeviceConfigInfo {
    int sampleRate = 0;
    int bufferSize = 0;
    int outputChannels = 0;
    int inputChannels = 0;
    // DSP→speaker delay reported by the device; 0 when the engine predates
    // the field. Visualisations delay by this to align with audible sound.
    int outputLatencySamples = 0;
    std::vector<int> availableSampleRates;
    std::vector<int> availableBufferSizes;
    std::vector<std::string> availableDrivers;
    std::string currentDriver;
    // The engine's pending driver pick (switchDriver with no openable
    // device yet); empty when nothing is pending. hasIntendedDriver is
    // false when the report predates the field (older engine) — the GUI
    // then falls back to inferring pending intent locally.
    std::string intendedDriver;
    bool hasIntendedDriver = false;
};

// Carries the truthful outcome of a debounced /supersonic/devices/switch
// from the engine. Two failure shapes are surfaced separately:
//   - success == false: the entire switch failed (no device opened, or
//     the engine rolled back). `error` carries the engine/JUCE message
//     verbatim. `actualOutput` and `actualInput` reflect whatever the
//     engine fell back to.
//   - success == true with inputUnavailable == true: the output opened
//     fine but the requested input couldn't be opened — the engine
//     fell back to output-only. `inputUnavailableReason` carries
//     JUCE's verbatim error for the input open. `actualInput` is empty.
// The GUI shows a modal carrying the verbatim JUCE message and reverts
// the affected dropdown. Engine does not enrich, translate, or
// speculate about cause.
struct AudioSwitchOutcome {
    bool        success            = false;
    std::string requestedOutput;
    std::string requestedInput;
    std::string actualOutput;
    std::string actualInput;
    std::string error;                  // top-level swap error
    bool        inputUnavailable   = false;
    std::string inputUnavailableReason; // JUCE's verbatim input-open error
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
    // Connected game-controller list, one device per line as
    // "enabled<TAB>name" (empty = none). Default no-op so non-GUI
    // consumers don't need to react.
    virtual void GamepadDevices(const std::string& devices) {}
    virtual void Version(const VersionInfo& info) = 0;
    virtual void AudioDataAvailable(ProcessedAudioPtr audio) = 0;
    virtual void Buffer(const BufferInfo& info) = 0;
    virtual void ActiveLinks(const int numLinks) = 0;
    virtual void BPM(const double bpm) = 0;
    virtual void Scsynth(const ScsynthInfo& scsynthInfo) = 0;
    virtual void AudioDevices(const AudioDevicesInfo& devicesInfo) = 0;
    virtual void AudioInputDevices(const AudioInputDevicesInfo& devicesInfo) = 0;
    // Per-driver device table; only fires on engines that broadcast
    // /supersonic/device-table. Default no-op so non-GUI consumers (and
    // older clients) don't need to react.
    virtual void AudioDeviceTable(const AudioDeviceTableInfo& /*table*/) {}
    virtual void AudioDeviceConfig(const AudioDeviceConfigInfo& configInfo) = 0;
    virtual void SupersonicSetup(int sampleRate, int bufferSize) = 0;
    virtual void SpiderReady() = 0;
    // Per-job run lifecycle (/run/started, /run/ended). Default no-op
    // so non-GUI consumers don't need to react.
    virtual void RunStarted(int /*jobId*/, const std::string& /*workspace*/) {}
    virtual void RunEnded(int /*jobId*/) {}
    // Transient editor line flash (e.g. a live_loop header pulsing each
    // iteration). workspace names the buffer, line is 1-based. Default no-op.
    virtual void Flash(const std::string& /*workspace*/, int /*line*/) {}
    // A live_loop started or was re-run: its audio is tapped into scope
    // buffer scopeNum and its header sits at line (1-based) in workspace.
    // Ended fires when the loop's thread dies. Default no-ops.
    virtual void LiveLoopScope(int /*jobId*/, const std::string& /*name*/,
                               const std::string& /*workspace*/, int /*line*/,
                               int /*scopeNum*/) {}
    virtual void LiveLoopScopeEnded(int /*jobId*/, const std::string& /*name*/) {}
    // Truthful outcome of a debounced device-switch — see
    // AudioSwitchOutcome. Default no-op so non-GUI consumers don't
    // need to react.
    virtual void AudioSwitchDone(const AudioSwitchOutcome& /*outcome*/) {}
};

// Always UDP
enum class APIProtocol
{
    UDP = 0,
    TCP = 1
};

struct LogSource
{
    std::string name;
    fs::path path;
    bool hasLivePanel = false;
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

    // Rotate the previous session's logs into the history dir and
    // truncate them ready for this run.
    virtual void CycleLogs();

    virtual bool LinkEnable();
    virtual bool LinkDisable();

    // SuperSonic network visibility: 0=Off, 1=LoopbackOnly, 2=NetworkWide.
    // Master gate for Link mesh + (when publish is on) Link Audio.
    enum class LinkVisibility { Off = 0, LoopbackOnly = 1, NetworkWide = 2 };
    virtual bool SetLinkVisibility(LinkVisibility mode);

    // Link Audio publish opt-in. Default off; channels aren't advertised
    // or sent until the user enables this.
    virtual bool SetLinkAudioPublish(bool enabled);

    // Identifier broadcast to other Link peers (default "SuperSonic";
    // Sonic Pi sets it to "Sonic Pi" on boot).
    virtual bool SetLinkPeerName(const std::string& name);

    virtual bool SetLinkBPM(double bpm);
    virtual void SetGlobalTimeWarp(double time);

    // Wait for the server to be in a good state
    virtual bool PingUntilServerCreated();
    virtual bool WaitUntilReady();

    // Non-blocking snapshots of the boot state WaitUntilReady blocks on.
    virtual bool IsServerReady();
    virtual bool HasServerErrored();

    // Shut down the API, close the server, ports, osc, etc.
    virtual void Shutdown();

    // Helper to send a buffer string to Ruby
    virtual void Run(const std::string& buffer, const std::string& text);

    // Run a code string as its own job without touching any buffer.
    // The workspace tags the job so /run/started can be matched back.
    // silent skips run logging and the spider's pre-run GC (for low-latency
    // one-shot previews such as keyboard/audition notes).
    virtual void RunCode(const std::string& code, const std::string& workspace, bool silent = false);

    // Stop all music
    virtual void Stop();

    // Stop a single job by id (from RunStarted)
    virtual void StopJob(int jobId);

    virtual void BufferNewLineAndIndent(int point_line, int point_index, int first_line, const std::string& code, const std::string& fileName);

    // ** Audio processor

    // Enable audio processing
    virtual void AudioProcessor_Enable(bool enable);

    // Enable FFT generation
    virtual void AudioProcessor_EnableFFT(bool enable);

    // Set Max FFT buckets to generate
    virtual void AudioProcessor_SetMaxFFTBuckets(uint32_t buckets);

    // Engine sample rate — used to map FFT bins to frequencies for the
    // log-spaced spectrum buckets
    virtual void AudioProcessor_SetSampleRate(int sampleRate);

    // Force the audio processor to reconnect to the scope shared memory.
    // Call after a cold-swap device change so the scope picks up the
    // freshly-allocated scope buffer from the rebuilt World.
    virtual void AudioProcessor_ResetConnection();

    // Client has used last audio data
    virtual void AudioProcessor_ConsumedAudio();

    std::vector<LogSource> GetLogSources();

    // Direct pointer to a slot in the cross-process shm_audio_buffer
    // array. Used by the session recorder to read the master output mix
    // (slot 0) while a supersonic-audio-out synth is feeding it. Returns
    // nullptr if the audio processor hasn't been initialised.
    virtual shm_audio_buffer* AudioProcessor_GetAudioBufferSlot(unsigned int slot);

    // Reader onto scope stream slot `scope_num`. Slot 0 is the master scope
    // (full mix); higher slots are independent fx_scope_out taps (inline
    // live-loop scopes, cards, the Examples jukebox). Invalid reader if the
    // audio processor hasn't connected. Combine with AudioProcessor_GetSampleClock
    // to window the stream on what the listener is currently hearing.
    virtual shm_scope_stream_reader AudioProcessor_GetScopeReader(unsigned int scope_num);

    // Seqlock-consistent snapshot of the engine's sample clock (sample
    // position ↔ wall-clock DAC time). `valid` is false until the engine has
    // published and while disconnected.
    virtual sample_clock_view AudioProcessor_GetSampleClock();

    // Flat pointer to the engine's PerformanceMetrics region (a block of
    // contiguous uint32 fields) in the cross-process shm mapping, or
    // nullptr if the audio processor hasn't connected yet. Consumed by
    // the GUI metrics panel, which reads the fields by index each tick.
    virtual const std::atomic<uint32_t>* AudioProcessor_GetMetrics();

    // Passive views onto the engine's OSC/debug rings and node-tree mirror,
    // for the SuperSonic observability panel. Empty when not connected.
    virtual ring_view AudioProcessor_GetInRing();
    virtual ring_view AudioProcessor_GetOutRing();
    virtual ring_view AudioProcessor_GetDebugRing();
    virtual node_tree_view AudioProcessor_GetNodeTree();
    virtual native_stats AudioProcessor_GetNativeStats();
    virtual bool AudioProcessor_HasNativeStats();

    std::string GetLogs();

    const int GetGuid() const;

    virtual bool TestAudio();

    // Get a unique path
    virtual const fs::path& GetPath(SonicPiPath piPath);

    // Get a port value
    virtual const int& GetPort(SonicPiPortId port);

    virtual bool SendOSC(oscpkt::Message m);
    virtual bool SendDaemonOSC(oscpkt::Message m);
    virtual bool SupersonicSendOSC(oscpkt::Message m);
    virtual void RequestAudioDevices();
    virtual int GetToken() const;

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
    // Line-stamping wrapper around m_stdlog's buffer (see TimestampLineBuf)
    std::unique_ptr<std::streambuf> m_stampbuf;

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
    std::shared_ptr<OscSender> m_spOscSupersonicSender;
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
    // Atomic: written by the pinger/boot thread, polled from the GUI thread
    // (IsServerReady/HasServerErrored and WaitUntilReady's sleep loop).
    std::atomic<State> m_state = State::Reset;
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
