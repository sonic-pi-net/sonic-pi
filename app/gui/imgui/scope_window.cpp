// Scope spectrum contributed by cmaughan from Rezonality project
#include "imgui.h"

#include <map>
#include <algorithm>
#include <deque>
#include <cmath>

#include "app.h"
#include "cue_window.h"

#include "api/sonicpi_api.h"

using namespace std::chrono;
using namespace SonicPi;
using namespace ImGui;

std::mutex dataMutex;
std::vector<float> spectrum[2];

void scope_window_show()
{
    std::scoped_lock lk(dataMutex);
    static bool show = true;
    
    ImGui::SetNextWindowSize(ImVec2(300, 100), ImGuiCond_FirstUseEver);
    if (ImGui::Begin("Scope", &show))
    {
        // Calculate a region for the scope to sit in
        const float border = 2.0f;
        ImVec4 region = ImVec4(GetCursorScreenPos().x - border, GetCursorScreenPos().y - border, GetContentRegionAvail().x + border * 2.0f, GetContentRegionAvail().y + border * 2.0f);

        sonic.spApi->AudioProcessor_SetMaxFFTBuckets(uint32_t(region.z / 4));

        // Center and scale for split height
        auto centerY = region.y + region.w * .5f;
        auto sampleScale = region.w / 2.0f;

        // Make a pixel margin between the buckets for a cleaner view
        auto numRects = float(spectrum[0].size());
        auto stepPerRect = std::ceil(region.z / numRects);
        auto margin = 1.0f;

        // ... but discard it if we have to
        if (stepPerRect < ((margin * 2) + 2.0f))
        {
            margin = 0.0f;
        }

        // Make sure we step enough to make a valid rect
        stepPerRect = std::max(margin * 2.0f + 1.0f, float(stepPerRect));

        auto pDraw = GetWindowDrawList();

        auto col1 = ImColor(1.0f, .2f, .2f, 1.0f);
        auto col2 = ImColor(0.2f, 1.0f, .2f, 1.0f);
        
        // Stereo
        for (uint32_t index = 0; index < numRects; index++)
        {
            float x1 = index * stepPerRect + region.x;

            // Draw left at the top, right at the bottom
            float size = spectrum[0][index] * sampleScale;
            float size2 = spectrum[1][index] * sampleScale;
            size = std::max(1.0f, size);
            size2 = std::max(1.0f, size2);

            pDraw->AddRectFilled(ImVec2(x1 + margin, centerY - margin - size), ImVec2(x1 + stepPerRect - margin * 2.0f, centerY - margin), col1);
            pDraw->AddRectFilled(ImVec2(x1 + margin, centerY + margin), ImVec2(x1 + stepPerRect - margin * 2.0f, size2 + centerY + margin), col2);
        }
        sonic.spApi->AudioProcessor_ConsumedAudio();
    }

    ImGui::End();

}

void scope_window_add(const ProcessedAudio& audio)
{
    std::scoped_lock lk(dataMutex);
    spectrum[0] = audio.m_spectrumQuantized[0];
    spectrum[1] = audio.m_spectrumQuantized[1];
}
