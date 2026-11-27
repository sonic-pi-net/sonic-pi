// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
#include <algorithm>
#include "plugin.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>

extern "C" {
void  sp_view_store_replaced (void* view);
void  sp_armed_from_store (SpInstance* instance, const char* json, size_t length);   // lib.rs: the armed buffers, from the page's storage
void  sp_host_note (const char* text);
const char* sp_own_path ();
}

namespace {

constexpr uint32_t kStateMagic = 0x6C705053;   // "SPpl", little-endian
constexpr int32_t kStateVersion = 3;
constexpr uint32_t kLongestText = 64u << 20;   // a key or a value no longer than any page writes, by far
// The buffers the host's transport starts and stops are the page's to say, in its storage (main.js ARMED_KEY).
constexpr const char* kArmedKey = "sp-daw-armed";

// <bundle>/Contents/Resources: synthdefs/, samples/, buffers/ and the page.
std::string resourcesDir ()
{
    // <bundle>/Contents/MacOS/<binary>, or on Windows <bundle>/Contents/<arch>-win/<binary>
    std::string path = sp_own_path ();
    for (int up = 0; up < 2; ++up) {
        const auto slash = path.find_last_of ("/\\");
        if (slash == std::string::npos) return "";
        path.resize (slash);
    }
    return path + "/Resources";
}

bool readText (const SpPlugin::Read& read, std::string& out)
{
    uint32_t bytes = 0;
    if (!read (&bytes, sizeof bytes) || bytes > kLongestText) return false;
    out.resize (bytes);
    return bytes == 0 || read (out.data (), bytes);
}

void put (std::vector<uint8_t>& out, const void* in, size_t bytes)
{
    const auto* from = static_cast<const uint8_t*> (in);
    out.insert (out.end (), from, from + bytes);
}

void putText (std::vector<uint8_t>& out, const std::string& text)
{
    const uint32_t bytes = uint32_t (text.size ());
    put (out, &bytes, sizeof bytes);
    put (out, text.data (), text.size ());
}

} // namespace

SpPlugin::~SpPlugin () { sp_instance_free (mInstance); }

bool SpPlugin::start (double sampleRate, uint32_t maxFrames, std::string& why)
{
    if (sampleRate <= 0 || maxFrames == 0) {
        why = "the host gave no sample rate or no buffer size";
        return false;
    }
    if (!mInstance) {
        const std::string resources = resourcesDir ();
        SpInstanceConfig config {};
        config.struct_bytes = sizeof config;
        config.sample_rate = sampleRate;
        config.max_frames = maxFrames;
        config.resources = resources.c_str ();
        char reason[512] = {};
        mInstance = sp_instance_new (&config, reason, sizeof reason);
        if (!mInstance) {
            why = reason;
            sp_host_note ((std::string ("Sonic Pi did not start: ") + reason).c_str ());
            return false;
        }
        armedFromStore ();
        // For a test in a real host, where nothing can press Run: the program in the file SONIC_PI_PLUGIN_RUN
        // names is run as the instance starts.
        if (const char* file = std::getenv ("SONIC_PI_PLUGIN_RUN")) {
            if (FILE* f = std::fopen (file, "rb")) {
                std::string code;
                char chunk[4096];
                for (size_t n; (n = std::fread (chunk, 1, sizeof chunk, f)) > 0;) code.append (chunk, n);
                std::fclose (f);
                sp_instance_run (mInstance, code.c_str (), 0);
            }
        }
    } else if (sampleRate != mRate || maxFrames != mMaxFrames) {
        sp_instance_reconfigure (mInstance, sampleRate, maxFrames);
    }
    mRate = sampleRate;
    mMaxFrames = maxFrames;
    return true;
}

bool SpPlugin::readState (const Read& read)
{
    int32_t words[4] = {};
    if (!read (words, sizeof words) || uint32_t (words[0]) != kStateMagic) return false;
    if (words[1] < 1 || words[1] > kStateVersion) return false;
    int32_t zoom = 100;
    if (words[1] >= 3 && !read (&zoom, sizeof zoom)) return false;
    std::map<std::string, std::string> store;
    if (words[1] >= 2) {
        uint32_t count = 0;
        if (!read (&count, sizeof count)) return false;
        for (uint32_t i = 0; i < count; ++i) {
            std::string key, value;
            if (!readText (read, key) || !readText (read, value)) return false;
            store[std::move (key)] = std::move (value);
        }
    }
    setEditorSize (words[2], words[3]);
    setZoom (zoom);
    {
        std::lock_guard<std::mutex> lk (mStoreMutex);
        mStore = std::move (store);
    }
    armedFromStore ();
    return true;
}

std::vector<uint8_t> SpPlugin::writeState ()
{
    std::vector<uint8_t> out;
    const int32_t words[5] = { int32_t (kStateMagic), kStateVersion, mEditorWidth, mEditorHeight, mZoom };
    put (out, words, sizeof words);
    std::lock_guard<std::mutex> lk (mStoreMutex);
    const uint32_t count = uint32_t (mStore.size ());
    put (out, &count, sizeof count);
    for (const auto& [key, value] : mStore) {
        putText (out, key);
        putText (out, value);
    }
    return out;
}

void SpPlugin::editorTakesState ()
{
    if (mEditor) sp_view_store_replaced (mEditor);
}

void SpPlugin::storePut (std::string key, std::string value)
{
    const bool armed = key == kArmedKey;
    {
        std::lock_guard<std::mutex> lk (mStoreMutex);
        mStore[std::move (key)] = std::move (value);
    }
    if (armed) armedFromStore ();
}

void SpPlugin::storeRemove (const std::string& key)
{
    {
        std::lock_guard<std::mutex> lk (mStoreMutex);
        mStore.erase (key);
    }
    if (key == kArmedKey) armedFromStore ();
}

void SpPlugin::storeClear ()
{
    {
        std::lock_guard<std::mutex> lk (mStoreMutex);
        mStore.clear ();
    }
    armedFromStore ();
}

// Told whenever the storage or the instance is new.
void SpPlugin::armedFromStore ()
{
    if (!mInstance) return;
    std::string value;
    {
        std::lock_guard<std::mutex> lk (mStoreMutex);
        if (const auto it = mStore.find (kArmedKey); it != mStore.end ()) value = it->second;
    }
    sp_armed_from_store (mInstance, value.data (), value.size ());
}

void SpPlugin::setEditorSize (int32_t width, int32_t height)
{
    // no smaller than the editor lets itself be made, no larger than a screen
    if (width < kSmallestWidth || height < kSmallestHeight || width > 16384 || height > 16384) return;
    mEditorWidth = width;
    mEditorHeight = height;
}

void SpPlugin::resizeBy (int32_t dx, int32_t dy)
{
    if (!mResizer || mDragWidth <= 0) return;
    const auto within = [] (int64_t v, int32_t least) { return int32_t (std::min<int64_t> (16384, std::max<int64_t> (least, v))); };
    mResizer (within (int64_t (mDragWidth) + dx, kSmallestWidth), within (int64_t (mDragHeight) + dy, kSmallestHeight));
}

void SpPlugin::setZoom (int32_t percent)
{
    if (percent >= 50 && percent <= 300) mZoom = percent;
}

// The plugin's format, the page's storage and its zoom, for lib.rs: `host` the plugin an editor belongs to (sp_view_attach's).
extern "C" void sp_host_store_put (void* host, const char* key, size_t keyLength, const char* value, size_t valueLength, int remove)
{
    auto* plugin = static_cast<SpPlugin*> (host);
    if (remove) plugin->storeRemove (std::string (key, keyLength));
    else plugin->storePut (std::string (key, keyLength), std::string (value, valueLength));
}
extern "C" void sp_host_store_clear (void* host) { static_cast<SpPlugin*> (host)->storeClear (); }
extern "C" void sp_host_resize_start (void* host) { static_cast<SpPlugin*> (host)->resizeStart (); }
extern "C" void sp_host_resize_by (void* host, int dx, int dy) { static_cast<SpPlugin*> (host)->resizeBy (dx, dy); }
extern "C" const char* sp_host_format (void* host) { return static_cast<SpPlugin*> (host)->format (); }
extern "C" int sp_host_zoom (void* host) { return static_cast<SpPlugin*> (host)->zoom (); }
extern "C" void sp_host_keep_zoom (void* host, int percent) { static_cast<SpPlugin*> (host)->setZoom (percent); }
extern "C" void sp_host_store_each (void* host, void (*each) (void*, const char*, size_t, const char*, size_t), void* ctx)
{
    static_cast<SpPlugin*> (host)->storeEach ([&] (const std::string& key, const std::string& value) {
        each (ctx, key.data (), key.size (), value.data (), value.size ());
    });
}
