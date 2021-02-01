#include "imgui.h"

#include <cmath>
#include <deque>
#include <map>

#include "app.h"
#include "color_utils.h"
#include "cue_window.h"

#include "api/sonicpi_api.h"

using namespace std::chrono;
using namespace SonicPi;

std::map<std::string, SonicPi::CueInfo> Cues;
std::vector<SonicPi::CueInfo*> Ordered;
uint64_t LastTime = 0;

void cue_window_show()
{
    ImGui::SetNextWindowSize(ImVec2(100, 100), ImGuiCond_FirstUseEver);
    ImGui::Begin("Cues");
    if (ImGui::BeginTable("Cues", 2))
    {

        auto now = time_to_float_seconds(std::chrono::high_resolution_clock::now());
        for (auto& cue : Ordered)
        {
            float timeSince = now - time_to_float_seconds(cue->arrivalTime);
            timeSince *= 2.0f;

            // TODO: Style
            ImVec4 targetColor = ImVec4(1.0f, 1.0f, 1.0f, 1.0f);
            ImVec4 highlightColor = ImVec4(1.0f, 0.1f, 0.1f, 1.0f);
            auto col = blend(highlightColor, targetColor, std::min(timeSince, 1.0f));

            ImGui::PushStyleColor(ImGuiCol_Text, col);
            ImGui::TableNextRow();
            ImGui::TableSetColumnIndex(0);
            ImGui::Text(cue->address.c_str());
            ImGui::TableSetColumnIndex(1);
            ImGui::Text(cue->args.c_str());
            ImGui::PopStyleColor(1);
        }

        ImGui::EndTable();
    }

    ImGui::End();
}

void cue_window_reset()
{
    std::scoped_lock lk(sonic.guiMutex);
    Cues.clear();
    Ordered.clear();
}

void cue_window_add_cue(const SonicPi::CueInfo& cue)
{
    std::scoped_lock lk(sonic.guiMutex);

    auto itrCue = Cues.find(cue.address);
    if (itrCue == Cues.end())
    {
        itrCue = Cues.insert(std::make_pair(cue.address, cue)).first;
        Ordered.push_back(&itrCue->second);
    }

    itrCue->second.args = cue.args;
    itrCue->second.arrivalTime = cue.arrivalTime;
    itrCue->second.id = cue.id;
    itrCue->second.index = cue.index;
    itrCue->second.time = cue.time;

    // Every couple of seconds, sort the queue list
    auto diff = timer_stop(LastTime);
    if (diff > 2.0f)
    {
        LastTime = timer_start();
        std::sort(Ordered.begin(), Ordered.end(), [](auto pCueLeft, auto pCueRight) {
            return (pCueLeft->arrivalTime > pCueRight->arrivalTime);
        });
    }
}
