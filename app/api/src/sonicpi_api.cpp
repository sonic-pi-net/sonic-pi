#include <algorithm>
#include <array>
#include <cassert>
#include <iomanip>
#include <iostream>
#include <sstream>

#include <reproc++/drain.hpp>
#include <reproc++/reproc.hpp>
#include <reproc++/run.hpp>
#include <sago/platform_folders.h>

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

// Compiler doesn't support magic enum on linux, so we
// can't reflect the enum; just get here instead
// Note the conversion from '_' to '-'
SonicPiPortId GetPortId(const std::string id)
{
    if (id == "gui-listen-to-server")
        return SonicPiPortId::gui_listen_to_server;
    else if (id == "gui-send-to-server")
        return SonicPiPortId::gui_send_to_server;
    else if (id == "server-listen-to-gui")
        return SonicPiPortId::server_listen_to_gui;
    else if (id == "server-send-to-gui")
        return SonicPiPortId::server_send_to_gui;
    else if (id == "scsynth")
        return SonicPiPortId::scsynth;
    else if (id == "scsynth-send")
        return SonicPiPortId::scsynth_send;
    else if (id == "server-osc-cues")
        return SonicPiPortId::server_osc_cues;
    else if (id == "erlang-router")
        return SonicPiPortId::erlang_router;
    else if (id == "osc-midi-out")
        return SonicPiPortId::osc_midi_out;
    else if (id == "osc-midi-in")
        return SonicPiPortId::osc_midi_in;
    else if (id == "websocket")
        return SonicPiPortId::websocket;

    return SonicPiPortId::Invalid;
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
    fs::path homePath;
    auto pszHome = std::getenv("SONIC_PI_HOME");
    if (pszHome != nullptr)
    {
        homePath = fs::path(pszHome);
    }

    // Check for home path existence and if not, use user documents path
    if (!fs::exists(homePath))
    {
        homePath = fs::path(sago::getDocumentsFolder()).parent_path();
    }

    // Final attempt at getting the folder; try to create it if possible
    if (!fs::exists(homePath))
    {
        std::error_code err;
        if (!fs::create_directories(homePath, err))
        {
            // Didn't exist, and failed to create it
            return fs::path();
        }
    }
    return homePath;
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

std::shared_ptr<reproc::process> SonicPiAPI::StartProcess(const std::vector<std::string>& args, const std::string& outPath, const std::string& errPath)
{
    assert(!args.empty());

    reproc::options options;

    if (!outPath.empty())
    {
        options.redirect.out.path = outPath.c_str();
    }

    if (!errPath.empty())
    {
        options.redirect.err.path = errPath.c_str();
    }

    if (errPath.empty() && outPath.empty())
    {
        options.redirect.out.type = reproc::redirect::parent;
        options.redirect.err.type = reproc::redirect::parent;
    }

    auto spProcess = std::make_shared<reproc::process>();
    std::error_code ec = spProcess->start(args, options);

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

    // Fail case
    spProcess.reset();
    return spProcess;
}

bool SonicPiAPI::GetPorts()
{
    std::string portString;
    auto ec = RunProcess(std::vector<std::string>{
                             m_paths[SonicPiPath::RubyPath].string(),
                             m_paths[SonicPiPath::PortDiscoveryPath].string() },
        &portString);
    if (ec)
    {
        return false;
    }

    // Split
    auto ports = string_split(portString, "\r\n:");

    // Trim and replace - with _ because our enum uses underscores
    std::transform(ports.begin(), ports.end(), ports.begin(), [](std::string& val) { return string_trim(val); });

    // Map
    auto port_pair = vector_convert_to_pairs(ports);

    LOG(INFO, "Checking Ports: ");

    bool missingPort = false;
    for (auto& port : port_pair)
    {
        auto portId = GetPortId(port.first);
        if (portId == SonicPiPortId::Invalid)
        {
            LOG(ERR, "!Unrecognized Port - " << port.first);
            missingPort = true;
            continue;
        }

        auto portNum = std::stoi(port.second);
        m_ports[portId] = portNum;

        // Check we can open the port
        oscpkt::UdpSocket sock;
        sock.bindTo(portNum);
        if ((portNum < 1024) || (!sock.isOk()))
        {
            LOG(ERR, port.first << ":" << portNum << " [Not Available] ");
            missingPort = true;
        }
        else
        {
            LOG(INFO, port.first << ":" << portNum << " [OK] ");
        }
        sock.close();
    }

    if (missingPort)
    {
        LOG(ERR, "Critical Error. One or more ports is not available.");

        MessageInfo message;
        message.type = MessageType::StartupError;
        message.text = "One or more ports is not available. Is Sonic Pi already running? If not, please reboot your machine and try again.";

        m_pClient->Report(message);
        return false;
    }
    else
    {
        LOG(INFO, "All Ports OK!");
    }

    return true;
}

bool SonicPiAPI::StartRubyServer()
{
    std::vector<std::string> args;
    args.push_back(GetPath(SonicPiPath::RubyPath).string());
    args.push_back("--enable-frozen-string-literal");
    args.push_back("-E");
    args.push_back("utf-8");
    args.push_back(GetPath(SonicPiPath::RubyServerPath).string());

    if (m_protocol == APIProtocol::TCP)
    {
        args.push_back("-t");
    }
    else
    {
        args.push_back("-u");
    }

    args.push_back(std::to_string(GetPort(SonicPiPortId::server_listen_to_gui)));
    args.push_back(std::to_string(GetPort(SonicPiPortId::server_send_to_gui)));
    args.push_back(std::to_string(GetPort(SonicPiPortId::scsynth)));
    args.push_back(std::to_string(GetPort(SonicPiPortId::scsynth_send)));
    args.push_back(std::to_string(GetPort(SonicPiPortId::server_osc_cues)));
    args.push_back(std::to_string(GetPort(SonicPiPortId::erlang_router)));
    args.push_back(std::to_string(GetPort(SonicPiPortId::websocket)));

    LOG(INFO, "Launching Sonic Pi Runtime Server:");

    std::ostringstream str;
    for (auto& arg : args)
    {
        str << arg << " ";
    }
    LOG(INFO, "Args: " << str.str());

    if (m_homeDirWriteable && (m_logOption != LogOption::Console))
    {
        m_spRubyServer = StartProcess(args, GetPath(SonicPiPath::ServerOutputLogPath).string(), GetPath(SonicPiPath::ServerErrorLogPath).string());
    }
    else
    {
        m_spRubyServer = StartProcess(args);
    }

    if (!m_spRubyServer)
    {
        MessageInfo message;
        message.type = MessageType::StartupError;
        message.text = "The Sonic Pi Server could not be started!";

        m_pClient->Report(message);

        LOG(ERR, "Failed to start ruby server!");
        return false;
    }

    // This will raise the process to above normal priority, but only on windows
    raise_process_priority(m_spRubyServer->pid().first);

    // Register server pid for potential zombie clearing
    RunProcess(std::vector<std::string>{ GetPath(SonicPiPath::RubyPath).string(), GetPath(SonicPiPath::TaskRegisterPath).string(), std::to_string(m_spRubyServer->pid().first) });
    LOG(INFO, "Ruby server pid registered: " << m_spRubyServer->pid().first);

    m_startServerTime = timer_start();

    return true;
}

SonicPiAPI::~SonicPiAPI()
{
    Shutdown();
}

void SonicPiAPI::Shutdown()
{
    if (m_state == State::Created || m_state == State::Invalid || m_state == State::Initializing)
    {
        LOG(INFO, "Shutdown");

        m_spAudioProcessor.reset();

        StopServerAndOsc();

        RunCleanupScript();

        if (m_coutbuf)
        {
            std::cout.rdbuf(m_coutbuf); // reset to stdout before exiting
            m_coutbuf = nullptr;
        }
    }
    m_state = State::Start;
}

bool SonicPiAPI::StartOscServer()
{
    if (m_protocol == APIProtocol::UDP)
    {
        auto listenPort = GetPort(SonicPiPortId::gui_listen_to_server);

        m_spOscServer = std::make_shared<OscServerUDP>(m_pClient, std::make_shared<OscHandler>(m_pClient), listenPort);
        m_oscServerThread = std::thread([&]() {
            m_spOscServer->start();
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

bool SonicPiAPI::SendOSC(Message m)
{
    bool res = m_spOscSender->sendOSC(m);
    if (!res)
    {
        LOG(ERR, "Could Not Send OSC");
    }
    return res;
}

bool SonicPiAPI::WaitForServer()
{
    if (m_state == State::Created)
    {
        return true;
    }

    if (m_state != State::Initializing)
    {
        return false;
    }

    //QString contents;
    LOG(INFO, "Waiting for Sonic Pi Server to boot...");
    bool server_booted = false;
    if (!m_homeDirWriteable)
    {
        // we can't monitor the logs so hope for the best!
        std::this_thread::sleep_for(15s);
        server_booted = true;
    }
    else
    {
        // TODO: Is this really necessary?
        for (int i = 0; i < 30; i++)
        {
            auto contents = file_read(GetPath(SonicPiPath::ServerOutputLogPath));
            if (contents.find("Sonic Pi Server successfully booted.") != std::string::npos)
            {
                LOG(INFO, "Sonic Pi Server successfully booted.");
                server_booted = true;
                break;
            }
            else
            {
                std::cout << ".";
                std::this_thread::sleep_for(2s);
            }
            server_booted = true;
            break;
        }
    }

    if (!server_booted)
    {
        MessageInfo message;
        message.type = MessageType::StartupError;
        message.text = "Critical error! Could not boot Sonic Pi Server.";

        m_pClient->Report(message);
        return false;
    }

    int timeout = 60;
    LOG(INFO, "Waiting for Sonic Pi Server to respond...");
    while (m_spOscServer->waitForServer() && timeout-- > 0)
    {
        std::this_thread::sleep_for(1s);
        std::cout << ".";
        if (m_spOscServer->isIncomingPortOpen())
        {
            Message msg("/ping");
            msg.pushStr(m_guid);
            msg.pushStr("QtClient/1/hello");
            SendOSC(msg);
        }
    }
    std::cout << std::endl;

    if (!m_spOscServer->isServerStarted())
    {
        MessageInfo message;
        message.type = MessageType::StartupError;
        message.text = "Critical error! Could not connect to Sonic Pi Server.";

        m_pClient->Report(message);
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

        return true;
    }
}

// Initialize the API with the sonic pi root path (the folder containing the app folder)
bool SonicPiAPI::Init(const fs::path& root)
{
    if (m_state == State::Created)
    {
        MessageInfo message;
        message.type = MessageType::StartupError;
        message.text = "Call shutdown before Init!";

        m_pClient->Report(message);
        LOG(ERR, "Call shutdown before init!");
        return false;
    }

    // Start again, shutdown if we fail init
    m_state = State::Invalid;
    auto exitScope = sg::make_scope_guard([&]() {
        if (m_state == State::Invalid)
        {
            LOG(DBG, "Init failure, calling shutdown");
            Shutdown();
        }
    });

    // A new Guid for each initialization
#if defined(__APPLE__)
    m_guid = random_string(32);
#else
    m_guid = xg::newGuid().str();
#endif

    if (!fs::exists(root))
    {
        MessageInfo message;
        message.type = MessageType::StartupError;
        message.text = "Could not find root path: " + root.string();

        m_pClient->Report(message);
        return false;
    }

    auto homePath = FindHomePath();

    m_paths[SonicPiPath::RootPath] = fs::canonical(fs::absolute(root));

#if defined(WIN32)
    m_paths[SonicPiPath::RubyPath] = m_paths[SonicPiPath::RootPath] / "app/server/native/ruby/bin/ruby.exe";
#else
    m_paths[SonicPiPath::RubyPath] = m_paths[SonicPiPath::RootPath] / "app/server/native/ruby/bin/ruby";
#endif

    if (!fs::exists(m_paths[SonicPiPath::RubyPath]))
    {
        m_paths[SonicPiPath::RubyPath] = "ruby";
    }

    // Create script paths
    m_paths[SonicPiPath::RubyServerPath] = m_paths[SonicPiPath::RootPath] / "app/server/ruby/bin/sonic-pi-server.rb";
    m_paths[SonicPiPath::PortDiscoveryPath] = m_paths[SonicPiPath::RootPath] / "app/server/ruby/bin/port-discovery.rb";
    m_paths[SonicPiPath::FetchUrlPath] = m_paths[SonicPiPath::RootPath] / "app/server/ruby/bin/fetch-url.rb";
    m_paths[SonicPiPath::InitScriptPath] = m_paths[SonicPiPath::RootPath] / "app/server/ruby/bin/init-script.rb";
    m_paths[SonicPiPath::ExitScriptPath] = m_paths[SonicPiPath::RootPath] / "app/server/ruby/bin/exit-script.rb";
    m_paths[SonicPiPath::TaskRegisterPath] = m_paths[SonicPiPath::RootPath] / "app/server/ruby/bin/task-register.rb";

    // Sanity check on script existence
    const auto checkPaths = std::vector<SonicPiPath>{ SonicPiPath::RubyServerPath, SonicPiPath::PortDiscoveryPath, SonicPiPath::FetchUrlPath, SonicPiPath::InitScriptPath, SonicPiPath::ExitScriptPath, SonicPiPath::TaskRegisterPath };
    for (const auto& check : checkPaths)
    {
        if (!fs::exists(m_paths[SonicPiPath::RubyServerPath]))
        {
            MessageInfo message;
            message.type = MessageType::StartupError;
            message.text = "Could not find script path: " + m_paths[SonicPiPath::RubyServerPath].string();

            m_pClient->Report(message);
            return false;
        }
    }

    // Samples
    m_paths[SonicPiPath::SamplePath] = m_paths[SonicPiPath::RootPath] / "etc/samples";

    // Sonic pi home directory
    m_paths[SonicPiPath::UserPath] = homePath / ".sonic-pi";

    auto logPath = m_paths[SonicPiPath::UserPath] / "log";

    // Make the log folder and check we can write to it.
    // This is /usr/home/.sonic-pi/log
    m_homeDirWriteable = true;
    if (!fs::exists(logPath))
    {
        std::error_code err;
        if (!fs::create_directories(logPath, err))
        {
            m_homeDirWriteable = false;
            LOG(INFO, "Home dir not writable: " << err.message());
        }
        else
        {
            std::ofstream fstream(logPath / ".writeTest");
            if (!fstream.is_open())
            {
                m_homeDirWriteable = false;
                LOG(INFO, "Home dir not writable!");
            }
        }
    }

    // Our log paths
    m_paths[SonicPiPath::ServerErrorLogPath] = logPath / "server-errors.log";
    m_paths[SonicPiPath::ServerOutputLogPath] = logPath / "server-output.log";
    m_paths[SonicPiPath::ProcessLogPath] = logPath / "processes.log";
    m_paths[SonicPiPath::SCSynthLogPath] = logPath / "scsynth.log";

    // This is technically 'this' processes log path; but it is called gui log
    m_paths[SonicPiPath::GUILogPath] = logPath / "gui.log";

    std::for_each(m_paths.begin(), m_paths.end(), [this](auto& entry) {
        if (fs::exists(entry.second))
        {
            entry.second = fs::canonical(entry.second);
        }
        return entry;
    });

    // Setup redirection of log from this app to our log file
    // stdout into ~/.sonic-pi/log/gui.log
    if (m_homeDirWriteable && (m_logOption == LogOption::File))
    {
        m_coutbuf = std::cout.rdbuf();
        m_stdlog.open(m_paths[SonicPiPath::GUILogPath].string().c_str());
        std::cout.rdbuf(m_stdlog.rdbuf());
    }

    // Clear out old tasks from previous sessions if they still exist
    // in addition to clearing out the logs
    auto ec = RunProcess(std::vector<std::string>{
        m_paths[SonicPiPath::RubyPath].string(),
        m_paths[SonicPiPath::InitScriptPath].string() });
    if (ec)
    {
        return false;
    }

    LOG(INFO, "Welcome to Sonic Pi");
    LOG(INFO, "===================");

    if (!GetPorts())
    {
        return false;
    }

    if (!StartOscServer())
    {
        return false;
    }

    if (!StartRubyServer())
    {
        return false;
    }

    // Create the sender
    m_spOscSender = std::make_shared<OscSender>(GetPort(SonicPiPortId::gui_send_to_server));

    m_state = State::Initializing;

    LOG(INFO, "Init SonicPi Succeeded...");
    return true;
}

bool SonicPiAPI::TestAudio()
{
    // Just play a chord
    auto fileName = "d:/pi.rb";
    Message msg("/save-and-run-buffer");
    msg.pushStr(m_guid);
    msg.pushStr(fileName);
    msg.pushStr("play_chord [:c4, :e4, :g4]");
    msg.pushStr(fileName);
    bool res = SendOSC(msg);
    return res;
}

void SonicPiAPI::StopServerAndOsc()
{
    /*
    * TODO: TCP
    if(m_protocol == APIProtocol::TCP){
        clientSock->close();
    }
    */

    // Ask the server to exit
    if (!m_spRubyServer)
    {
        LOG(DBG, "Server process is not running.");
    }
    else
    {
        auto timer = timer_start();

        // Ask the server to exit
        // This happens really fast, and the process is typically gone before we get to the sleep below.
        LOG(INFO, "Asking server process to exit...");
        Message msg("/exit");
        msg.pushStr(m_guid);
        SendOSC(msg);

        // Reproc is having a hard time figuring out that the process has gone because
        // the ruby server is holding onto a socket and not cleaning it up.
        // We use a quick platform-independent check for exit instead, and just give it a second
        // for that check to succeed before doing reproc's wait/terminate/kill process which 
        // will inevitably succeed too; just with a longer delay.
        // We should fix the ruby layer and remove these extra checks
        std::this_thread::sleep_for(1s);

        // Quick check for it if it now gone
        if (process_running(m_spRubyServer->pid().first))
        {
            // OK, wait to stop it, terminate it, then till it.
            reproc::stop_actions stopAndKill = {
                { reproc::stop::wait, reproc::milliseconds(ProcessWaitMilliseconds) },
                { reproc::stop::terminate, reproc::milliseconds(TerminateProcessMilliseconds) },
                { reproc::stop::kill, reproc::milliseconds(KillProcessMilliseconds) }
            };
            m_spRubyServer->stop(stopAndKill);
        }

        LOG(INFO, "Server process gone in " << timer_stop(timer) << "s");
    }

    // Stop the osc server and hence the osc thread
    if (m_spOscServer)
    {
        LOG(DBG, "Stopping Osc Server...");
        m_spOscServer->stop();
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
    m_spOscSender.reset();
}

void SonicPiAPI::RunCleanupScript()
{
    if (fs::exists(GetPath(SonicPiPath::ExitScriptPath)))
    {
        // Ensure child processes are nuked if they didn't exit gracefully
        LOG(DBG, "Executing exit script: " << GetPath(SonicPiPath::ExitScriptPath));
        auto ret = RunProcess({ GetPath(SonicPiPath::RubyPath).string(), GetPath(SonicPiPath::ExitScriptPath).string() });
        if (ret)
        {
            LOG(ERR, "Failed to call exit: " << ret.message());
            return;
        }
    }
}

const fs::path& SonicPiAPI::GetPath(SonicPiPath piPath)
{
    return m_paths[piPath];
}

const int& SonicPiAPI::GetPort(SonicPiPortId port)
{
    return m_ports[port];
}

std::string SonicPiAPI::GetLogs()
{
    auto logs = std::vector<fs::path>{ GetPath(SonicPiPath::ServerOutputLogPath),
        GetPath(SonicPiPath::ServerErrorLogPath),
        GetPath(SonicPiPath::ProcessLogPath),
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
                str << log << ":" << std::endl
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


const std::string& SonicPiAPI::GetGuid() const
{
    return m_guid;
}

void SonicPiAPI::BufferNewLineAndIndent(int point_line, int point_index, int first_line, const std::string& code, const std::string& fileName, const std::string& id)
{
    Message msg("/buffer-newline-and-indent");
    msg.pushStr(id);
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
    msg.pushStr(m_guid);
    msg.pushStr(buffer);
    msg.pushStr(text);
    msg.pushStr(buffer);
    bool res = SendOSC(msg);
}

void SonicPiAPI::Stop()
{
    Message msg("/stop-all-jobs");
    msg.pushStr(m_guid);
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
        msg.pushStr(m_guid);
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
            msg.pushStr(m_guid);
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
    msg.pushStr(m_guid);
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

} // namespace SonicPi
