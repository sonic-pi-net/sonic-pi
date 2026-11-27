// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// What a plugin of any format keeps for a host: its instance (instance.h), started at the host's rate; the editor's
// size and zoom; the page's storage (bridge.js: its buffers, its sets, its preferences); and all of that as the
// bytes a host saves in its project. The format's own shell (vst3/shell.cpp, au/shell.mm) holds one and speaks its
// host's API over it. The editor (lib.rs) is handed the same object as its `host`, and reaches the storage and the
// zoom through the sp_host_* functions at the end.
//
// Threads: the main thread's, but for the storage, which a host may ask for with the state on another (its lock),
// and instance(), which the audio thread reads after start().
#pragma once
#include <cstdint>
#include <functional>
#include <map>
#include <mutex>
#include <string>
#include <vector>

#include "instance.h"

class SpPlugin
{
public:
    // `format` is the plugin's format as the page names it ("VST", "AU"): a literal, kept by its pointer.
    explicit SpPlugin (const char* format) : mFormat (format) {}
    const char* format () const { return mFormat; }
    ~SpPlugin ();
    SpPlugin (const SpPlugin&) = delete;
    SpPlugin& operator= (const SpPlugin&) = delete;

    SpInstance* instance () const { return mInstance; }

    // The host said its rate and the most frames it will ask for at once: the instance starts the first time, and
    // keeps its runs when either changes later. Not while the host renders. False, with the reason, when the
    // instance could not start.
    bool start (double sampleRate, uint32_t maxFrames, std::string& why);

    // The state a host keeps in its project, every number a little-endian 32-bit integer: a magic word, the
    // version, the editor's width and height; from version 3 the page's zoom in percent; then, from version 2, the
    // page's storage, a count and each key and its value as a length and its UTF-8 bytes.
    //
    // `read` copies the next `bytes` of what the host kept to `out`, false when there are not that many. False
    // from readState for what is not a state of ours, and then nothing has changed.
    using Read = std::function<bool (void* out, uint32_t bytes)>;
    bool readState (const Read& read);
    std::vector<uint8_t> writeState ();
    // After a state was read, on the main thread: with the editor open, the page starts again on what came.
    void editorTakesState ();

    // The page's storage. A write of the armed buffers' key reaches the instance (sp_instance_armed), so a project
    // opened with no editor has them as it was saved.
    void storePut (std::string key, std::string value);
    void storeRemove (const std::string& key);
    void storeClear ();
    template <typename Each> void storeEach (Each each)
    {
        std::lock_guard<std::mutex> lk (mStoreMutex);
        for (const auto& [key, value] : mStore) each (key, value);
    }

    // The editor open now (lib.rs's), or null.
    void setEditor (void* editor) { mEditor = editor; }

    // The editor's size in points: its first, then the one it was left at. A size no person could have left it at
    // is not kept.
    static constexpr int32_t kSmallestWidth = 640, kSmallestHeight = 420;
    void editorSize (int32_t& width, int32_t& height) const { width = mEditorWidth; height = mEditorHeight; }
    void setEditorSize (int32_t width, int32_t height);
    // A size asked for from inside: the page's resize grip, for a host that gives the window no handles
    // (plugin-grip.js). The shell showing the editor says how it is done in its format (an Audio Unit sizes its own
    // view, a VST3 asks its host); with no editor shown there is no way, and nothing happens. A drag begins from the
    // size the editor has then, and each move is that plus how far the pointer has gone, no smaller than the
    // smallest. Main thread.
    using Resizer = std::function<void (int32_t width, int32_t height)>;
    void setResizer (Resizer resizer) { mResizer = std::move (resizer); }
    void resizeStart () { mDragWidth = mEditorWidth; mDragHeight = mEditorHeight; }
    void resizeBy (int32_t dx, int32_t dy);
    // The page's zoom in percent, one a browser has (lib.rs ZOOMS' range).
    int32_t zoom () const { return mZoom; }
    void setZoom (int32_t percent);

private:
    void armedFromStore ();

    const char* mFormat;
    SpInstance* mInstance = nullptr;
    double mRate = 0;
    uint32_t mMaxFrames = 0;
    int32_t mEditorWidth = 1100;
    int32_t mEditorHeight = 760;
    int32_t mZoom = 100;
    std::mutex mStoreMutex;
    std::map<std::string, std::string> mStore;
    void* mEditor = nullptr;
    Resizer mResizer;
    int32_t mDragWidth = 0, mDragHeight = 0;
};
