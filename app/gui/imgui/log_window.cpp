#include "imgui.h"

#include "app.h"
#include "log_window.h"

struct LogLine
{
    int style;
    std::string text;
};

struct ExampleAppLog
{
    std::deque<LogLine> Lines;
    bool AutoScroll; // Keep scrolling if already at the bottom.

    ExampleAppLog()
    {
        AutoScroll = true;
    }

    void AddLog(int color, const std::string& text)
    {
        std::lock_guard lk(sonic.guiMutex);
        Lines.push_back(LogLine{ color, text });
    }

    void Draw(const char* title, bool* p_open = NULL)
    {
        std::lock_guard lk(sonic.guiMutex);

        ImGui::SetNextWindowSize(ImVec2(200, 200), ImGuiCond_FirstUseEver);
        if (!ImGui::Begin(title, p_open))
        {
            ImGui::End();
            return;
        }

        auto contentSize = ImGui::GetContentRegionAvail().y;

        // Options menu
        if (ImGui::BeginPopup("Options"))
        {
            ImGui::Checkbox("Auto-scroll", &AutoScroll);
            ImGui::EndPopup();
        }

        // Main window
        if (ImGui::Button("Options"))
            ImGui::OpenPopup("Options");
        ImGui::SameLine();
        bool clear = ImGui::Button("Clear");
        ImGui::SameLine();
        bool copy = ImGui::Button("Copy");
        ImGui::SameLine();

        ImGui::Separator();
        ImGui::BeginChild("scrolling", ImVec2(0, 0), false, ImGuiWindowFlags_HorizontalScrollbar);

        if (clear)
        {
            Lines.clear();
        }
        if (copy)
            ImGui::LogToClipboard();

        ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
        {
            // The simplest and easy way to display the entire buffer:
            //   ImGui::TextUnformatted(buf_begin, buf_end);
            // And it'll just work. TextUnformatted() has specialization for large blob of text and will fast-forward
            // to skip non-visible lines. Here we instead demonstrate using the clipper to only process lines that are
            // within the visible area.
            // If you have tens of thousands of items and their processing cost is non-negligible, coarse clipping them
            // on your side is recommended. Using ImGuiListClipper requires
            // - A) random access into your data
            // - B) items all being the  same height,
            // both of which we can handle since we an array pointing to the beginning of each line of text.
            // When using the filter (in the block of code above) we don't have random access into the data to display
            // anymore, which is why we don't use the clipper. Storing or skimming through the search result would make
            // it possible (and would be recommended if you want to search through tens of thousands of entries).
            int style = 0;
            ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.0f, 1.0f, 1.0f, 1.0f));
            //ImGuiListClipper clipper;
            //clipper.Begin(int(Lines.size()));
            //while (clipper.Step())
            {
                //for (int line_no = clipper.DisplayStart; line_no < clipper.DisplayEnd; line_no++)
                for (auto& line : Lines)
                {
                    //auto& line = Lines[line_no];
                    if (style != line.style)
                    {
                        style = line.style;
                        ImGui::PopStyleColor(1);
                        switch (style)
                        {
                        case 0:
                            // TODO: Style
                            ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.0f, 1.0f, 1.0f, 1.0f));
                            break;
                        case 1:
                        default:
                            ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.0f, .2f, .2f, 1.0f));
                            break;
                        }
                    }
                    ImGui::TextUnformatted(line.text.c_str());
                }
            }
            ImGui::PopStyleColor(1);
            //clipper.End();
        }
        ImGui::PopStyleVar();

        auto scrollY = ImGui::GetScrollY();
        auto maxY = ImGui::GetScrollMaxY();
        scrollY += ImGui::GetFontSize();
       
        // Temp: Figure out why this doesn't work.
        if (AutoScroll && (scrollY > maxY))
            ImGui::SetScrollHereY(1.0f);

        ImGui::EndChild();
        ImGui::End();
    }
};

static ExampleAppLog appLog;

void log_window_add_log(int color, const std::string& str)
{
    appLog.AddLog(color, str.c_str());
}

// Demonstrate creating a simple log window with basic filtering.
void log_window_show(bool* p_open)
{
    // Actually call in the regular Log helper (which will Begin() into the same window as we just did)
    appLog.Draw("Log", p_open);
}
