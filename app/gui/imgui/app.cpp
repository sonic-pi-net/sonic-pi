#include "app.h"
#include "config.h"
#include <cstdio>
#include <sstream>

#include "cue_window.h"
#include "edit_window.h"
#include "imgui.h"
#include "log_window.h"
#include "midi_window.h"
#include "scope_window.h"

#include "main_menu.h"

#include "api/string_utils.h"
using namespace SonicPi;

SPData sonic;

void SPClient::Report(const MessageInfo& info)
{
    if (info.type == MessageType::Info ||
        info.type == MessageType::Message)
    {
        log_window_add_log(info.style, info.text + "\n");
    }
    else if (info.type == MessageType::Multi)
    {
        std::ostringstream str;
        str << "{run: " << info.jobId << ", time: " << info.runtime;
        if (!info.threadName.empty())
        {
            str << ", thread: " << info.threadName;
        }
        str << "}" << std::endl;

        log_window_add_log(info.style, str.str());
        for (auto& arg : info.multi)
        {
            auto lines = string_split(arg.text, "\n\r");
            for (auto& line : lines)
            {
                log_window_add_log(arg.style, "  " + line + "\n");
            }
        }
        log_window_add_log(0, "\n");
    }
    edit_window_update(info);
}

void SPClient::Cue(const CueInfo& cue)
{
    cue_window_add_cue(cue);
}

void SPClient::AudioDataAvailable(const ProcessedAudio& audio)
{
    //std::cout << "[TestClient] : Audio Data" ;
    scope_window_add(audio);
}

void SPClient::Status(const StatusInfo& info)
{
    log_window_add_log(0, "Status: " + info.id + "\n");
}

void SPClient::Midi(const MidiInfo& info)
{
    if (info.type == MidiType::In)
    {
        midi_window_set_inputs(info.portInfo);
    }
    else
    {
        midi_window_set_outputs(info.portInfo);
    }
}

void SPClient::Version(const VersionInfo& info)
{
    log_window_add_log(0, info.version + "\n");
}

void SPClient::Buffer(const BufferInfo& info)
{
    // TODO: Fill buffer
    edit_window_update(info);
}

void SPClient::ActiveLinks(const int numLinks)
{
  // please implement me
}

void SPClient::BPM(const double bpm)
{
  // please implement me
}

void SPClient::Scsynth(const ScsynthInfo& scsynthInfo)
{
  // please implement me
}

void start_sonic_pi()
{
    sonic.spClient = std::make_shared<SPClient>();
    sonic.spApi = std::make_shared<SonicPiAPI>(sonic.spClient.get(), APIProtocol::UDP, LogOption::File);
    sonic.spApi->Init(fs::path(APP_INSTALL_ROOT) / "..");
    sonic.spApi->Boot();
}

bool sync_sonic_pi()
{
    bool ok = sonic.spApi->WaitUntilReady();
    if (ok)
    {
        sonic.spApi->AudioProcessor_Enable(true);
        sonic.spApi->AudioProcessor_EnableFFT(true);
        sonic.spApi->TestAudio();

        sonic.spApi->LoadWorkspaces();
    }
    return ok;
}

void stop_sonic_pi()
{
    if (sonic.spApi)
    {
        sonic.spApi->Shutdown();
    }
}

void begin_main_window()
{
    static bool opt_fullscreen = true;
    static bool opt_padding = false;
    static ImGuiDockNodeFlags dockspace_flags = ImGuiDockNodeFlags_None;
    static bool p_open = true;

    // We are using the ImGuiWindowFlags_NoDocking flag to make the parent window not dockable into,
    // because it would be confusing to have two docking targets within each others.
    ImGuiWindowFlags window_flags = ImGuiWindowFlags_MenuBar | ImGuiWindowFlags_NoDocking;
    if (opt_fullscreen)
    {
        ImGuiViewport* viewport = ImGui::GetMainViewport();
        ImGui::SetNextWindowPos(viewport->GetWorkPos());
        ImGui::SetNextWindowSize(viewport->GetWorkSize());
        ImGui::SetNextWindowViewport(viewport->ID);
        ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);
        ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);
        window_flags |= ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoMove;
        window_flags |= ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoNavFocus;
    }
    else
    {
        dockspace_flags &= ~ImGuiDockNodeFlags_PassthruCentralNode;
    }

    // When using ImGuiDockNodeFlags_PassthruCentralNode, DockSpace() will render our background
    // and handle the pass-thru hole, so we ask Begin() to not render a background.
    if (dockspace_flags & ImGuiDockNodeFlags_PassthruCentralNode)
        window_flags |= ImGuiWindowFlags_NoBackground;

    // Important: note that we proceed even if Begin() returns false (aka window is collapsed).
    // This is because we want to keep our DockSpace() active. If a DockSpace() is inactive,
    // all active windows docked into it will lose their parent and become undocked.
    // We cannot preserve the docking relationship between an active window and an inactive docking, otherwise
    // any change of dockspace/settings would lead to windows being stuck in limbo and never being visible.
    if (!opt_padding)
        ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0.0f, 0.0f));

    ImGui::Begin("DockSpace Demo", &p_open, window_flags);
    if (!opt_padding)
        ImGui::PopStyleVar();

    if (opt_fullscreen)
        ImGui::PopStyleVar(2);

    // DockSpace
    ImGuiIO& io = ImGui::GetIO();
    ImGuiID dockspace_id = ImGui::GetID("MyDockSpace");
    ImGui::DockSpace(dockspace_id, ImVec2(0.0f, 0.0f), dockspace_flags);

    show_main_menu();
}

void end_main_window()
{
    ImGui::End();
}

void show_sonic_pi()
{
    static bool open = true;

    begin_main_window();
    log_window_show(&open);
    edit_window_show();
    cue_window_show();
    scope_window_show();
    midi_window_show();

    end_main_window();
}

void cleanup_ui()
{
    edit_window_close();
}
