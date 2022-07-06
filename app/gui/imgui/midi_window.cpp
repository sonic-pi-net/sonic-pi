#include "imgui.h"

#include <map>
#include <deque>
#include <cmath>

#include "app.h"
#include "cue_window.h"

#include "api/string_utils.h"

using namespace std::chrono;
using namespace SonicPi;

std::vector<std::string> inputs;
std::vector<std::string> outputs;

void midi_window_show()
{
    std::scoped_lock lk(sonic.guiMutex);

    ImGui::SetNextWindowSize(ImVec2(200, 100), ImGuiCond_FirstUseEver);
    ImGui::Begin("Midi In");
    if (ImGui::BeginTable("Midi", 1))
    {

        for (auto& in : inputs)
        {
            ImGui::TableNextRow();
            ImGui::TableSetColumnIndex(0);
            ImGui::Text(in.c_str());
        }

        ImGui::EndTable();
    }

    ImGui::End();
    
    ImGui::Begin("Midi Out");
    if (ImGui::BeginTable("Midi", 1))
    {

        for (auto& out : outputs)
        {
            ImGui::TableNextRow();
            ImGui::TableSetColumnIndex(0);
            ImGui::Text(out.c_str());
        }

        ImGui::EndTable();
    }

    ImGui::End();
}

void midi_window_set_inputs(const std::string& in)
{
    std::scoped_lock lk(sonic.guiMutex);

    inputs = string_split(in, "\n\r");
}

void midi_window_set_outputs(const std::string& out)
{
    std::scoped_lock lk(sonic.guiMutex);
    outputs = string_split(out, "\n\r");
}
