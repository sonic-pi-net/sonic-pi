#include <algorithm>
#include <array>
#include <cassert>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <future>
#include <mutex>
#include <string>

#include <reproc++/drain.hpp>
#include <reproc++/reproc.hpp>
#include <reproc++/run.hpp>
#include <sago/platform_folders.h>
#include <kissnet.hpp>

#include <api/file_utils.h>
#include <api/logger.h>
#include <api/process_utils.h>
#include <api/string_utils.h>

#include <api/osc/osc_sender.h>
#include <api/osc/udp.hh>
#include <api/osc/udp_osc_server.h>

#include <api/audio/audio_processor.h>
#include <api/scope_exit.h>
#include <api/sonicpi_api.h>

#ifndef __APPLE__
#include <crossguid/guid.hpp>
#endif

using namespace std::chrono;
using namespace oscpkt;

namespace SonicPi
{

#ifdef DEBUG
Logger logger{ LT::DBG };
#else
Logger logger{ LT::INFO };
#endif

namespace
{
const uint32_t ProcessWaitMilliseconds = 10000;
const uint32_t KillProcessMilliseconds = 2000;
const uint32_t TerminateProcessMilliseconds = 2000;

template <typename T>
std::map<T, T> vector_convert_to_pairs(const std::vector<T>& vals)
{
    std::map<T, T> pairs;
    auto itrFirst = vals.begin();
    while (itrFirst != vals.end())
    {
        auto itrSecond = itrFirst + 1;
        if (itrSecond != vals.end())
        {
            pairs[*itrFirst] = *itrSecond;
        }

        itrFirst = ++itrSecond;
    };
    return pairs;
}

} // namespace

SonicPiAPI::SonicPiAPI(IAPIClient* pClient, APIProtocol protocol, LogOption logOption)
    : m_pClient(pClient)
    , m_protocol(protocol)
    , m_logOption(logOption)
{
    assert(m_pClient);
}

fs::path SonicPiAPI::FindHomePath() const
{
    auto pszHome = std::getenv("SONIC_PI_HOME");
    if (pszHome != nullptr)
    {
        return pszHome;
    }

#if defined(WIN32)
    auto usrprofHome = std::getenv("USERPROFILE");
    if (usrprofHome != nullptr)
    {
        return fs::path(usrprofHome);
    }

    auto homeDrive = std::getenv("HOMEDRIVE");
    auto homePath = std::getenv("HOMEPATH");
    if ((homeDrive  != nullptr) && (homePath != nullptr))
    {
        return fs::path(homeDrive) / homePath;
    }
#endif

    auto home = std::getenv("HOME");
    if (home != nullptr)
    {
        return fs::path(home);
    }

    return fs::path(sago::getDocumentsFolder()).parent_path();
}

std::error_code SonicPiAPI::RunProcess(const std::vector<std::string>& args, std::string* pOutput)
{
    assert(!args.empty());

    // Wait for process, then terminate it
    reproc::stop_actions stop = {
        { reproc::stop::wait, reproc::milliseconds(ProcessWaitMilliseconds) },
        { reproc::stop::terminate, reproc::milliseconds(TerminateProcessMilliseconds) },
        { reproc::stop::kill, reproc::milliseconds(KillProcessMilliseconds) }
    };

    reproc::options options;
    options.stop = stop;

    reproc::process proc;
    std::error_code ec = proc.start(args, options);
    if (ec == std::errc::no_such_file_or_directory)
    {
        std::ostringstream str;
        str << "RunProcess - Program Not Found";
        if (!args.empty())
            str << " : " << args[0];
        LOG(ERR, str.str());
        return ec;
    }
    else if (ec)
    {
        LOG(ERR, "RunProcess - " << ec.message());
        return ec;
    }

    if (pOutput)
    {
        reproc::sink::string sink(*pOutput);
        ec = reproc::drain(proc, sink, sink);
        if (ec)
        {
            LOG(ERR, "RunProcess Draining - " << ec.message());
            return ec;
        }
    }

    // Call `process::stop` manually so we can access the exit status.
    int status = 0;
    std::tie(status, ec) = proc.stop(options.stop);
    if (ec)
    {
        LOG(ERR, "RunProcess - " << ec.message());
        return ec;
    }
    return ec;
}

std::shared_ptr<reproc::process> SonicPiAPI::StartProcess(const std::vector<std::string>& args)
{

    assert(!args.empty());

    auto spProcess = std::make_shared<reproc::process>();

    // auto yoProc = reproc::process();

    std::error_code ec = spProcess->start(args);
    LOG(INFO, "Started...");

    if (!ec)
    {
        // Success
        auto status = spProcess->wait(0s);
        LOG(DBG, "Process OK - " << status.second.message());
        return spProcess;
    }

    if (ec == std::errc::no_such_file_or_directory)
    {
        std::ostringstream str;
        str << "StartProcess - Program Not Found";
        if (!args.empty())
            str << " : " << args[0];
        LOG(ERR, str.str());

    }
    else if (ec)
    {
        LOG(ERR, "StartProcess - " << ec.message());
    }

    // Something went wrong. We've already logged an error
    // message, but also reset the shared pointer so that
    // the caller can see there's a problem too.
    spProcess.reset();
    return spProcess;
}

BootDaemonInitResult SonicPiAPI::StartBootDaemon(bool noScsynthInputs)
{
    LOG(INFO, "Launching Sonic Pi Boot Daemon:");

    std::string output;
    std::vector<std::string> args;

    args.push_back(GetPath(SonicPiPath::RubyPath).string());
    args.push_back(GetPath(SonicPiPath::BootDaemonPath).string());

    if(noScsynthInputs) {
      args.push_back("--no-scsynth-inputs");
    }

    std::ostringstream str;
    for (auto& arg : args)
    {
        str << arg << " ";
    }
    LOG(INFO, "Args: " << str.str());

    m_bootDaemonProcess = StartProcess(args);

    if (!m_bootDaemonProcess)
    {
        MessageInfo message;
        message.type = MessageType::StartupError;
        message.text = "The Boot Daemon could not be started!";

        m_pClient->Report(message);

        LOG(ERR, "Failed to start Boot Daemon!");
        return BootDaemonInitResult::TerminalError;
    }
    LOG(INFO, "Reading Boot Daemon output...");

    // We need a mutex along with `output` to prevent the main thread and
    // background thread from modifying `output` at the same time (`std::string`
    // is not thread safe).
    uint8_t buffer[4096];
    auto res = m_bootDaemonProcess->read(reproc::stream::out, buffer, sizeof(buffer));

    int bytes_read = (int)res.first;
    std::error_code ec = res.second;

    if(ec || bytes_read < 0) {
      if(ec) {
        LOG(ERR, "Error reading ports via Boot Daemon STDOUT. Bytes read: "  + std::to_string(bytes_read) + " Error code: " << ec);

      } else {
        LOG(ERR, "Failed to read ports via Boot Daemon STDOUT. Bytes read: " + std::to_string(bytes_read));

    }
      return BootDaemonInitResult::TerminalError;
    }

    std::string input_str(buffer, buffer + bytes_read);
      input_str = string_trim(input_str);

    if(input_str.find("SuperCollider Audio Server Boot Error") == 0) {
      LOG(ERR, "SuperCollider Audio Server boot error detected");
      return BootDaemonInitResult::ScsynthBootError;
    } else {
      auto daemon_stdout = string_split(input_str, " ");
      std::transform(daemon_stdout.begin(), daemon_stdout.end(), daemon_stdout.begin(), [](std::string& val) { return string_trim(val); });

    for(int i = 0 ; i < daemon_stdout.size() ; i ++) {
      LOG(INFO, "daemon_stdout: " + daemon_stdout[i]);
    }

    if(daemon_stdout.size() != 8) {
      LOG(ERR, "\nError. Was expecting 7 port numbers and a token from the Daemon Booter. Got: " + input_str + "\n");
      return BootDaemonInitResult::TerminalError;
    }

    m_ports[SonicPiPortId::daemon] = std::stoi(daemon_stdout[0]);
    m_ports[SonicPiPortId::gui_listen_to_spider] = std::stoi(daemon_stdout[1]);
    m_ports[SonicPiPortId::gui_send_to_spider] = std::stoi(daemon_stdout[2]);
    m_ports[SonicPiPortId::scsynth] = std::stoi(daemon_stdout[3]);
    m_ports[SonicPiPortId::tau_osc_cues] = std::stoi(daemon_stdout[4]);
    m_ports[SonicPiPortId::tau] = std::stoi(daemon_stdout[5]);
    m_ports[SonicPiPortId::phx_http] = std::stoi(daemon_stdout[6]);
    m_token = std::stoi(daemon_stdout[7]);

    LOG(INFO, "Setting up OSC sender to Spider on port " << m_ports[SonicPiPortId::gui_send_to_spider]);
    m_spOscSpiderSender    = std::make_shared<OscSender>(m_ports[SonicPiPortId::gui_send_to_spider]);

    LOG(INFO, "Setting up OSC sender to Daemon on port " << m_ports[SonicPiPortId::daemon]);
    m_spOscDaemonSender = std::make_shared<OscSender>(m_ports[SonicPiPortId::daemon]);

    LOG(INFO, "Setting up OSC sender to Tau on port " << m_ports[SonicPiPortId::tau]);
    m_spOscTauSender       = std::make_shared<OscSender>(m_ports[SonicPiPortId::tau]);
    LOG(INFO, "Setting up Boot Daemon keep alive loop");
    m_bootDaemonSockPingLoopThread = std::thread([&]() {
      while(m_keep_alive.load())
      {
        LOG(DBG, "SND keep_alive");
        Message msg("/daemon/keep-alive");
        msg.pushInt32(m_token);
        m_spOscDaemonSender->sendOSC(msg);
        LOG(DBG, "SND keep_alive sent");
        std::this_thread::sleep_for(4s);
      }
      LOG(INFO, "Keep Alive Thread Loop has completed.");
    });

    m_startServerTime = timer_start();
      return BootDaemonInitResult::Successful;
    }
}

SonicPiAPI::~SonicPiAPI()
{
    LOG(INFO, "API deconstructor initiating shutdown... ");
    Shutdown();
}

void SonicPiAPI::RestartTau()
{

    LOG(INFO, "Asking Daemon to restart Tau ");
    Message msg("/daemon/restart-tau");
    msg.pushInt32(m_token);
    m_spOscDaemonSender->sendOSC(msg);
    return;
}


bool SonicPiAPI::LinkEnable()
{
    Message msg("/link-enable");
    bool res = TauSendOSC(msg);
    if (!res)
    {
        return false;
    }
    return true;
}

bool SonicPiAPI::SetLinkBPM(double bpm)
{
    Message msg("/link-set-tempo");
    msg.pushFloat((float) bpm);
    bool res = TauSendOSC(msg);
    if (!res)
    {
        return false;
    }
    return true;
}

bool SonicPiAPI::LinkDisable()
{
    Message msg("/link-disable");
    bool res = TauSendOSC(msg);
    if (!res)
    {
        return false;
    }
    return true;
}

void SonicPiAPI::Shutdown()
{
    LOG(INFO, "Initiating shutdown...");
    std::lock_guard<std::mutex> lg(m_osc_mtx);

    if(m_shutdown_engaged)
    {
      LOG(INFO, "Shutdown already initiated...");
      return;
    }

    LOG(INFO, "Shutting down now...");

    m_shutdown_engaged = true;

    switch(m_state)
    {
    case State::Reset :
      LOG(INFO, "Shutting down with state: Reset");
      break;
    case State::Initializing :
      LOG(INFO, "Shutting down with state: Initializing");
      break;
    case State::Invalid :
      LOG(INFO, "Shutting down with state: Invalid");
      break;
    case State::Created :
      LOG(INFO, "Shutting down with state: Created");
      break;
    case State::Error :
      LOG(INFO, "Shutting down with state: Error");
      break;
    default :
      LOG(INFO, "Shutting down with unknown state!! Warning!");
    }

    if (m_state == State::Created || m_state == State::Invalid || m_state == State::Initializing)
    {
        LOG(INFO, "Resetting audio processor...");
        m_spAudioProcessor.reset();

        LOG(INFO, "Stopping OSC server...");
        StopOscServer();

        LOG(INFO, "Stopping daemon keep alive loop");
        m_keep_alive.store(false);
    }


    m_state = State::Reset;
    LOG(INFO, "API State set to: Reset...");
    LOG(INFO, "Waiting for Daemon keep alive loop to have stopped...");
    LOG(INFO, "Sending /daemon/exit to daemon's kill switch with token " << std::to_string(m_token)) ;
    Message msg("/daemon/exit");
    msg.pushInt32(m_token);
    m_spOscDaemonSender->sendOSC(msg);

    m_bootDaemonSockPingLoopThread.join();
    m_pingerThread.join();

    LOG(INFO, "API Shutdown complete...");

    if (m_coutbuf)
    {
        std::cout.rdbuf(m_coutbuf); // reset to stdout before exiting
        m_coutbuf = nullptr;
    }
}

bool SonicPiAPI::StartOscServer()
{
    if (m_protocol == APIProtocol::UDP)

      {
          auto listenPort = GetPort(SonicPiPortId::gui_listen_to_spider);

          m_spOscSpiderServer = std::make_shared<OscServerUDP>(m_pClient, std::make_shared<OscHandler>(m_pClient), listenPort);
          m_oscServerThread = std::thread([&]() {
              m_spOscSpiderServer->start();
              LOG(DBG, "Osc Server Thread Exiting");
        });
    }
    else
    {
        // TODO: TCP
        //sonicPiOSCServer = new SonicPiTCPOSCServer(this, handler);
        //sonicPiOSCServer->start();
    }
    return true;
}

void SonicPiAPI::SetGlobalTimeWarp(double time)
{
    Message msg("/set-global-timewarp");
    msg.pushInt32(m_token);
    msg.pushFloat((float) time);
    SendOSC(msg);
    return;
}

bool SonicPiAPI::SendOSC(Message m)
{

    if (WaitUntilReady())
    {
        bool res = m_spOscSpiderSender->sendOSC(m);
        if (!res)
        {
            LOG(ERR, "Could Not Send OSC to Spider");
            return false;
        }
        return true;
    }

    return false;
}

bool SonicPiAPI::TauSendOSC(Message m)
{

    if (WaitUntilReady())
    {
        bool res = m_spOscTauSender->sendOSC(m);
        if (!res)
        {
            LOG(ERR, "Could Not Send OSC to Tau");
            return false;
        }
        return true;
    }

    return false;
}

bool SonicPiAPI::WaitUntilReady()
{
    if (m_state == State::Created)
    {
        return true;
    }

    int num_tries = 60;
    while (m_state != State::Created && num_tries > 0)
    {
        num_tries--;
        if (m_state == State::Error)
        {
            LOG(ERR, "Oh no, Spider Server got to an Error State whilst starting...");
            return false;
        }
        LOG(INFO, "Waiting Until Ready... " + std::to_string(num_tries));
        std::this_thread::sleep_for(1s);
    }

    if (num_tries < 1)
    {
        return false;
    } else {
        return true;
    }
}


bool SonicPiAPI::PingUntilServerCreated()
{
    LOG(INFO, "Pinging Spider Server until a response is received...");
    std::lock_guard<std::mutex> lg(m_osc_mtx);

    if (m_state == State::Created)
    {
        LOG(ERR, "Error! No need to ping server as it's already created!");
        return true;
    }

    if (m_state != State::Initializing)
    {
        LOG(ERR, "API is not in the initialisation state. Error!");
        return false;
    }

    int timeout = 60;
    LOG(INFO, "Waiting for Sonic Pi Spider Server to respond...");
    while (m_keep_alive.load() && m_spOscSpiderServer->waitForServer() && timeout-- > 0)
    {
        std::this_thread::sleep_for(1s);
        LOG(INFO, ".");
        if (m_spOscSpiderServer->isIncomingPortOpen())
        {
            Message msg("/ping");
            msg.pushInt32(m_token);
            msg.pushStr("QtClient/1/hello");

            //bypass ::SendOSC as that needs to wait until ready
            //which is precisely what this message is attempting
            //to figure out!
            m_spOscSpiderSender->sendOSC(msg);
        }
    }

    if (!m_spOscSpiderServer->isServerStarted())
    {
        MessageInfo message;
        message.type = MessageType::StartupError;
        message.text = "Critical error! Could not connect to Sonic Pi Server.";

        m_pClient->Report(message);
        m_state = State::Error;
        return false;
    }
    else
    {
        auto time = timer_stop(m_startServerTime);
        LOG(INFO, "Sonic Pi Server connection established in " << time << "s");

        // Create the audio processor
        m_spAudioProcessor = std::make_shared<AudioProcessor>(m_pClient, GetPort(SonicPiPortId::scsynth));

        // All good
        m_state = State::Created;
        LOG(INFO, "API State set to: Created...");

        return true;
    }
}

// Initialize the API with the sonic pi root path (the folder containing the app folder)
APIInitResult SonicPiAPI::Init(const fs::path& root)
{
    m_token = -1;
    m_osc_mtx.lock();

    if (m_state == State::Created)
    {
        MessageInfo message;
        message.type = MessageType::StartupError;
        message.text = "Call shutdown before Init!";

        m_pClient->Report(message);
        LOG(ERR, "Call shutdown before init!");
        m_state = State::Error;
        m_osc_mtx.unlock();
        return APIInitResult::TerminalError;
    }

    if (!fs::exists(root))
    {
        MessageInfo message;
        message.type = MessageType::StartupError;
        message.text = "Could not find root path: " + root.string();

        m_pClient->Report(message);
        m_state = State::Error;
        m_osc_mtx.unlock();
        return APIInitResult::TerminalError;
    }

    if (!InitializePaths(root))
    {
      // oh no, something went wrong :-(
      m_osc_mtx.unlock();
      return APIInitResult::TerminalError;
    }

    // Make the log folder and check we can write to it.
    // This is /usr/home/.sonic-pi/log
    m_homeDirWriteable = true;
    auto logPath = GetPath(SonicPiPath::LogPath);
    if (!fs::exists(logPath))
    {
        std::error_code err;
        if (!fs::create_directories(logPath, err))
        {
            m_homeDirWriteable = false;
        }
        else
        {
            std::ofstream fstream(logPath / ".writeTest");
            if (!fstream.is_open())
            {
                m_homeDirWriteable = false;
            }
        }
    }

    if (m_homeDirWriteable) {

    } else {
      LOG(INFO, "Home dir not writable ");
      return APIInitResult::HomePathNotWritableError;
    }

    EnsurePathsAreCanonical();
    m_osc_mtx.unlock();
    return APIInitResult::Successful;
}

APIBootResult SonicPiAPI::Boot(bool noScsynthInputs)
{
    m_osc_mtx.lock();

    // Setup redirection of log from this app to our log file
    // stdout into ~/.sonic-pi/log/gui.log
    if (m_homeDirWriteable && (m_logOption == LogOption::File))
    {
        m_coutbuf = std::cout.rdbuf();
        m_stdlog.open(m_paths[SonicPiPath::GUILogPath].string().c_str());
        std::cout.rdbuf(m_stdlog.rdbuf());
    }

    StartClearLogsScript();

    LOG(INFO, "Starting...");

    // Start again, shutdown if we fail init
    m_state = State::Invalid;
    auto exitScope = sg::make_scope_guard([&]() {
        if (m_state == State::Invalid)
        {
            LOG(INFO, "Init failure, calling shutdown");
            Shutdown();
        }
    });



    LOG(INFO, "Log Path: " + GetPath(SonicPiPath::LogPath).string());
    m_state = State::Initializing;
    // Start the Boot Daemon
    BootDaemonInitResult boot_daemon_res = StartBootDaemon(noScsynthInputs);

    if (boot_daemon_res != BootDaemonInitResult::Successful)
    {
        LOG(INFO, "Attempting to start Boot Daemon failed....";)
        m_osc_mtx.unlock();
        if (boot_daemon_res == BootDaemonInitResult::ScsynthBootError) {
            return APIBootResult::ScsynthBootError;
        } else {
            return APIBootResult::TerminalError;
        }
    }

    // Start the OSC Server
    if(!StartOscServer())
    {
        LOG(INFO, "Attempting to start OSC Server failed....");
        m_osc_mtx.unlock();
        return APIBootResult::TerminalError;
    }

    LOG(INFO, "API Init Started...");


    LOG(INFO, "API State set to: Initializing...");

    m_osc_mtx.unlock();

    LOG(INFO, "Going to start pinging server...");
    m_pingerThread = std::thread([&]() {
        PingUntilServerCreated();
    });

    return APIBootResult::Successful;
}

bool SonicPiAPI::InitializePaths(const fs::path& root)
{
    // sanitise and set app root path
    m_paths[SonicPiPath::RootPath] = fs::canonical(fs::absolute(root));

    // Sonic pi home directory
    m_paths[SonicPiPath::HomePath] = FindHomePath();
    m_paths[SonicPiPath::UserPath] = FindHomePath() / ".sonic-pi";

    // Set path to Ruby executable (system dependent)
#if defined(WIN32)
    m_paths[SonicPiPath::RubyPath] = m_paths[SonicPiPath::RootPath] / "app/server/native/ruby/bin/ruby.exe";
#else
    m_paths[SonicPiPath::RubyPath] = m_paths[SonicPiPath::RootPath] / "app/server/native/ruby/bin/ruby";
#endif
    if (!fs::exists(m_paths[SonicPiPath::RubyPath]))
    {
        m_paths[SonicPiPath::RubyPath] = "ruby";
    }

    // Set Ruby script paths
    m_paths[SonicPiPath::BootDaemonPath]      = m_paths[SonicPiPath::RootPath] / "app/server/ruby/bin/daemon.rb";
    m_paths[SonicPiPath::FetchUrlPath]        = m_paths[SonicPiPath::RootPath] / "app/server/ruby/bin/fetch-url.rb";
    m_paths[SonicPiPath::ClearLogsPath]        = m_paths[SonicPiPath::RootPath] / "app/server/ruby/bin/clear-logs.rb";



    // Set Log paths
    m_paths[SonicPiPath::LogPath] = m_paths[SonicPiPath::UserPath] / "log";
    m_paths[SonicPiPath::SpiderServerLogPath] = m_paths[SonicPiPath::LogPath] / "spider.log";
    m_paths[SonicPiPath::BootDaemonLogPath]   = m_paths[SonicPiPath::LogPath] / "daemon.log";
    m_paths[SonicPiPath::TauLogPath]          = m_paths[SonicPiPath::LogPath] / "tau.log";
    m_paths[SonicPiPath::SCSynthLogPath]      = m_paths[SonicPiPath::LogPath] / "scsynth.log";
    m_paths[SonicPiPath::GUILogPath]          = m_paths[SonicPiPath::LogPath] / "gui.log";

    // Set built-in samples path
    m_paths[SonicPiPath::SamplePath] = m_paths[SonicPiPath::RootPath] / "etc/samples/";

    // Set Config paths
    m_paths[SonicPiPath::ConfigPath]              = m_paths[SonicPiPath::UserPath] / "config";
    m_paths[SonicPiPath::AudioSettingsConfigPath] = m_paths[SonicPiPath::ConfigPath] / "audio-settings.toml";

    // Sanity check for script existence
    const auto checkPaths = std::vector<SonicPiPath>{ SonicPiPath::FetchUrlPath, SonicPiPath::BootDaemonPath, SonicPiPath::ClearLogsPath  };
    for (const auto& check : checkPaths)
    {
        if (!fs::exists(m_paths[check]))
        {
            MessageInfo message;
            message.type = MessageType::StartupError;
            message.text = "Could not find script path: " + m_paths[check].string();

            m_pClient->Report(message);
            return false;
        }
    }

    return true;
}

void SonicPiAPI::EnsurePathsAreCanonical()
{
    std::for_each(m_paths.begin(), m_paths.end(), [this](auto& entry) {
        if (fs::exists(entry.second))
        {
            entry.second = fs::canonical(entry.second);
        }
        return entry;
    });
}

bool SonicPiAPI::TestAudio()
{
    // Just play a chord
    auto fileName = "d:/pi.rb";
    Message msg("/save-and-run-buffer");
    msg.pushInt32(m_token);
    msg.pushStr(fileName);
    msg.pushStr("play_chord [:c4, :e4, :g4]");
    msg.pushStr(fileName);
    bool res = SendOSC(msg);
    return res;
}

void SonicPiAPI::StopOscServer()
{
    // /*
    // * TODO: TCP
    // if(m_protocol == APIProtocol::TCP){
    //     clientSock->close();
    // }
    // */

    // Stop the osc server and hence the osc thread
    if (m_spOscSpiderServer)
    {
        LOG(DBG, "Stopping Osc Server...");
        m_spOscSpiderServer->stop();
    }

    // The server should have closed the osc channel; therefore we can join the thread
    if (m_oscServerThread.joinable())
    {
        LOG(DBG, "Waiting for Osc Server Thread...");
        m_oscServerThread.join();
        LOG(DBG, "Osc Server Thread done");
    }
    else
    {
        LOG(DBG, "Osc Server thread has already stopped");
    }
    m_spOscSpiderSender.reset();
}


const fs::path& SonicPiAPI::GetPath(SonicPiPath piPath)
{
    return m_paths[piPath];
}

const int& SonicPiAPI::GetPort(SonicPiPortId port)
{
    return m_ports[port];
}

std::string SonicPiAPI::GetScsynthLog()
{
    auto logs = std::vector<fs::path>{GetPath(SonicPiPath::SCSynthLogPath)};

    std::ostringstream str;
    for (auto& log : logs)
    {
        if (fs::exists(log))
        {
            auto contents = string_trim(file_read(log));
            if (!contents.empty())
            {
              str << contents;
            }
        }
    }
    return str.str();
}

std::string SonicPiAPI::GetLogs()
{
    auto logs = std::vector<fs::path>{ GetPath(SonicPiPath::SpiderServerLogPath),
        GetPath(SonicPiPath::BootDaemonLogPath),
        GetPath(SonicPiPath::TauLogPath),
        GetPath(SonicPiPath::SCSynthLogPath),
        GetPath(SonicPiPath::GUILogPath) };

    std::ostringstream str;
    for (auto& log : logs)
    {
        if (fs::exists(log))
        {
            auto contents = string_trim(file_read(log));
            if (!contents.empty())
            {
                str << string_trim(log.filename(), "\"") << ":" << std::endl
                    << contents << std::endl
                    << std::endl;
            }
        }
    }
    return str.str();
}

void SonicPiAPI::AudioProcessor_SetMaxFFTBuckets(uint32_t buckets)
{
    if (m_spAudioProcessor)
    {
        m_spAudioProcessor->SetMaxBuckets(buckets);
    }
}

void SonicPiAPI::AudioProcessor_Enable(bool enable)
{
    if (m_spAudioProcessor)
    {
        m_spAudioProcessor->Enable(enable);
    }
}

void SonicPiAPI::AudioProcessor_EnableFFT(bool enable)
{
    if (m_spAudioProcessor)
    {
        m_spAudioProcessor->EnableFFT(enable);
    }
}


void SonicPiAPI::AudioProcessor_ConsumedAudio()
{
    if (m_spAudioProcessor)
    {
        m_spAudioProcessor->SetConsumed(true);
    }
}


const int SonicPiAPI::GetGuid() const
{
    return m_token;
}

void SonicPiAPI::BufferNewLineAndIndent(int point_line, int point_index, int first_line, const std::string& code, const std::string& fileName)
{
    Message msg("/buffer-newline-and-indent");
    msg.pushInt32(m_token);
    msg.pushStr(fileName);
    msg.pushStr(code);
    msg.pushInt32(point_line);
    msg.pushInt32(point_index);
    msg.pushInt32(first_line);
    SendOSC(msg);
}

void SonicPiAPI::Run(const std::string& buffer, const std::string& text)
{
    Message msg("/save-and-run-buffer");
    msg.pushInt32(m_token);
    msg.pushStr(buffer);
    msg.pushStr(text);
    msg.pushStr(buffer);
    bool res = SendOSC(msg);
}

void SonicPiAPI::Stop()
{
    Message msg("/stop-all-jobs");
    msg.pushInt32(m_token);
    SendOSC(msg);
}

uint32_t SonicPiAPI::MaxWorkspaces() const
{
    return 10;
}

void SonicPiAPI::LoadWorkspaces()
{
    for (uint32_t i = 0; i < MaxWorkspaces(); i++)
    {
        Message msg("/load-buffer");
        msg.pushInt32(m_token);
        std::string s = "workspace_" + string_number_name(i);
        msg.pushStr(s);
        SendOSC(msg);
    }
}

void SonicPiAPI::SaveWorkspaces(const std::map<uint32_t, std::string>& workspaces)
{
    LOG(INFO, "Saving workspaces");

    for (uint32_t i = 0; i < MaxWorkspaces(); i++)
    {
        auto itrSpace = workspaces.find(i);
        if (itrSpace != workspaces.end())
        {
            Message msg("/save-buffer");
            msg.pushInt32(m_token);
            std::string s = "workspace_" + string_number_name(i);
            msg.pushStr(s);
            msg.pushStr(itrSpace->second);
            SendOSC(msg);
        }
    }

}

bool SonicPiAPI::SaveAndRunBuffer(const std::string& name, const std::string& text)
{
    std::string code = text;
    m_settings.Preprocess(code);

    Message msg("/save-and-run-buffer");
    msg.pushInt32(m_token);
    msg.pushStr(name);
    msg.pushStr(code);
    msg.pushStr(name);
    bool res = SendOSC(msg);
    if (!res)
    {
        return false;
    }
    return true;
}

const APISettings& SonicPiAPI::GetSettings() const
{
    return m_settings;
}

void SonicPiAPI::SetSettings(const APISettings& settings)
{
    m_settings = settings;
}


void SonicPiAPI::StartClearLogsScript()
{
    std::string output;
    std::vector<std::string> args;

    args.push_back(GetPath(SonicPiPath::RubyPath).string());
    args.push_back(GetPath(SonicPiPath::ClearLogsPath).string());

    std::ostringstream str;
    for (auto& arg : args)
    {
        str << arg << " ";
    }
    LOG(INFO, "Args: " << str.str());

    auto proc =  StartProcess(args);
}

} // namespace SonicPi
