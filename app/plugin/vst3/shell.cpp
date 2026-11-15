// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// Sonic Pi as a VST3 instrument: each instance its own Sonic Pi (instance.h),
// rendered from process(), with the web GUI as its editor (lib.rs). The COM
// shape follows rerezzed/src/vst3_shell.cpp: interface headers only, one
// component that is also its own controller.
#include "pluginterfaces/base/funknown.h"
#include "pluginterfaces/base/ibstream.h"
#include "pluginterfaces/base/ipluginbase.h"
#include "pluginterfaces/vst/ivstaudioprocessor.h"
#include "pluginterfaces/vst/ivstcomponent.h"
#include "pluginterfaces/vst/ivsteditcontroller.h"
#include "pluginterfaces/vst/ivstparameterchanges.h"
#include "pluginterfaces/vst/ivstprocesscontext.h"
#include "pluginterfaces/vst/vstspeaker.h"
#include "pluginterfaces/vst/vsttypes.h"
#include "pluginterfaces/gui/iplugview.h"

#include <CoreFoundation/CoreFoundation.h>

#include <atomic>
#include <map>
#include <mutex>
#include <string>
#include <cstring>
#include <string>

#include "instance.h"

using namespace Steinberg;
using namespace Steinberg::Vst;

extern "C" {
void* sp_view_attach (void* parent, unsigned width, unsigned height, SpInstance* instance, void* host);
void  sp_view_store_replaced (void* view);
void  sp_log_enable ();   // filelog.rs: a host has loaded the plugin, so its lines go to the user's log folder
void  sp_view_resize (void* view, unsigned width, unsigned height);
void  sp_view_detach (void* view);
int   sp_host_key (void* view, int down, int key, int code, int mods);
void  sp_host_note (const char* text);
int   sp_parent_ready (void* parent);
const char* sp_own_path ();
}

namespace {

static const DECLARE_UID (kSonicPiUID, 0x2F6C1D84, 0x7A3B4E19, 0x9C05D6E2, 0x41B8F07A);

// <bundle>/Contents/Resources: synthdefs/, samples/, buffers/ and the page.
std::string resourcesDir ()
{
    std::string path = sp_own_path ();   // <bundle>/Contents/MacOS/<binary>
    for (int up = 0; up < 2; ++up) {
        const auto slash = path.find_last_of ('/');
        if (slash == std::string::npos) return "";
        path.resize (slash);
    }
    return path + "/Resources";
}

inline bool sameIID (const void* a, const void* b)
{
    return std::memcmp (a, b, sizeof (TUID)) == 0;
}

inline void toStr128 (const char* s, String128 out)
{
    int i = 0;
    for (; s[i] && i < 127; ++i)
        out[i] = static_cast<char16> (static_cast<unsigned char> (s[i]));
    out[i] = 0;
}

class SonicPiProcessor;
SpInstance* instanceOf (SonicPiProcessor* processor);
void retainProcessor (SonicPiProcessor* processor);
void releaseProcessor (SonicPiProcessor* processor);
// The editor's size: the instance keeps the one it was left at, and the host's project keeps it with the state.
void editorSizeOf (SonicPiProcessor* processor, int32& width, int32& height);
void keepEditorSize (SonicPiProcessor* processor, int32 width, int32 height);
// The editor open now (lib.rs's), told when the host gives the plugin a state; null when none is.
void editorOpened (SonicPiProcessor* processor, void* editor);

class SonicPiView : public IPlugView
{
public:
    explicit SonicPiView (SonicPiProcessor* processor) : mRefCount (1), mProcessor (processor)
    {
        retainProcessor (processor);
        editorSizeOf (processor, mWidth, mHeight);   // opened at the size it was left at
    }
    virtual ~SonicPiView () { removed (); releaseProcessor (mProcessor); }

    tresult PLUGIN_API queryInterface (const TUID iid, void** obj) override
    {
        if (!obj) return kInvalidArgument;
        if (sameIID (iid, FUnknown_iid) || sameIID (iid, IPlugView_iid))
            *obj = static_cast<IPlugView*> (this);
        else { *obj = nullptr; return kNoInterface; }
        addRef ();
        return kResultOk;
    }
    uint32 PLUGIN_API addRef () override { return static_cast<uint32> (++mRefCount); }
    uint32 PLUGIN_API release () override
    {
        const uint32 rc = static_cast<uint32> (--mRefCount);
        if (rc == 0) delete this;
        return rc;
    }

    tresult PLUGIN_API isPlatformTypeSupported (FIDString type) override
    {
        return (type && std::strcmp (type, kPlatformTypeNSView) == 0) ? kResultTrue : kResultFalse;
    }

    tresult PLUGIN_API attached (void* parent, FIDString type) override
    {
        if (!parent || isPlatformTypeSupported (type) != kResultTrue) return kInvalidArgument;
        if (mParent) return kResultOk;
        mParent = parent;
        // wry needs the view to be in a window already, and hosts may attach first.
        if (!tryAttach ()) startRetry ();
        return kResultOk;
    }

    tresult PLUGIN_API removed () override
    {
        stopRetry ();
        if (mView) { editorOpened (mProcessor, nullptr); sp_view_detach (mView); mView = nullptr; }
        mParent = nullptr;
        return kResultOk;
    }

    tresult PLUGIN_API onWheel (float) override { return kResultFalse; }
    tresult PLUGIN_API onKeyDown (char16 key, int16 code, int16 mods) override
    {
        return sp_host_key (mView, 1, key, code, mods) ? kResultTrue : kResultFalse;
    }
    tresult PLUGIN_API onKeyUp (char16 key, int16 code, int16 mods) override
    {
        return sp_host_key (mView, 0, key, code, mods) ? kResultTrue : kResultFalse;
    }
    tresult PLUGIN_API onFocus (TBool state) override
    {
        sp_host_note (state ? "vst3 focus in" : "vst3 focus out");
        return kResultOk;
    }

    tresult PLUGIN_API getSize (ViewRect* size) override
    {
        if (!size) return kInvalidArgument;
        size->left = 0; size->top = 0;
        size->right = mWidth; size->bottom = mHeight;
        return kResultOk;
    }

    tresult PLUGIN_API onSize (ViewRect* newSize) override
    {
        if (!newSize) return kInvalidArgument;
        mWidth  = newSize->right - newSize->left;
        mHeight = newSize->bottom - newSize->top;
        if (mView) sp_view_resize (mView, (unsigned) mWidth, (unsigned) mHeight);
        keepEditorSize (mProcessor, mWidth, mHeight);
        // said, so the log shows who sized it: the person, or a host restoring a size of its own
        sp_host_note (("editor resized to " + std::to_string (mWidth) + "x" + std::to_string (mHeight)).c_str ());
        return kResultOk;
    }

    tresult PLUGIN_API setFrame (IPlugFrame*) override { return kResultOk; }
    tresult PLUGIN_API canResize () override { return kResultTrue; }

    tresult PLUGIN_API checkSizeConstraint (ViewRect* rect) override
    {
        if (!rect) return kInvalidArgument;
        if (rect->right - rect->left < 640) rect->right = rect->left + 640;
        if (rect->bottom - rect->top < 420) rect->bottom = rect->top + 420;
        return kResultTrue;
    }

private:
    // True once there is nothing left to wait for, whether or not it worked.
    bool tryAttach ()
    {
        if (!sp_parent_ready (mParent)) return false;
        mView = sp_view_attach (mParent, (unsigned) mWidth, (unsigned) mHeight, instanceOf (mProcessor), mProcessor);
        editorOpened (mProcessor, mView);
        return true;
    }

    static void retry (CFRunLoopTimerRef, void* info)
    {
        auto* self = static_cast<SonicPiView*> (info);
        if (self->mParent && !self->mView && self->tryAttach ()) self->stopRetry ();
    }

    void startRetry ()
    {
        sp_host_note ("attached before the view had a window; waiting");
        CFRunLoopTimerContext ctx = { 0, this, nullptr, nullptr, nullptr };
        mRetry = CFRunLoopTimerCreate (kCFAllocatorDefault, CFAbsoluteTimeGetCurrent () + 0.05,
                                       0.05, 0, 0, &retry, &ctx);
        if (mRetry) CFRunLoopAddTimer (CFRunLoopGetMain (), mRetry, kCFRunLoopCommonModes);
    }

    void stopRetry ()
    {
        if (!mRetry) return;
        CFRunLoopTimerInvalidate (mRetry);
        CFRelease (mRetry);
        mRetry = nullptr;
    }

    std::atomic<int32> mRefCount;
    SonicPiProcessor* mProcessor;   // held: it owns the instance the editor talks to
    void* mParent = nullptr;
    void* mView = nullptr;
    CFRunLoopTimerRef mRetry = nullptr;
    int32 mWidth = 0;
    int32 mHeight = 0;
};

class SonicPiProcessor : public IComponent, public IAudioProcessor, public IEditController,
                         public IProcessContextRequirements
{
public:
    SonicPiProcessor () : mRefCount (1) {}
    virtual ~SonicPiProcessor () { sp_instance_free (mInstance); }

    SpInstance* instance () const { return mInstance; }

    tresult PLUGIN_API queryInterface (const TUID iid, void** obj) override
    {
        if (!obj) return kInvalidArgument;
        if (sameIID (iid, FUnknown_iid) || sameIID (iid, IPluginBase_iid) || sameIID (iid, IComponent_iid))
            *obj = static_cast<IComponent*> (this);
        else if (sameIID (iid, IAudioProcessor_iid))
            *obj = static_cast<IAudioProcessor*> (this);
        else if (sameIID (iid, IEditController_iid))
            *obj = static_cast<IEditController*> (this);
        else if (sameIID (iid, IProcessContextRequirements_iid))
            *obj = static_cast<IProcessContextRequirements*> (this);
        else { *obj = nullptr; return kNoInterface; }
        addRef ();
        return kResultOk;
    }
    uint32 PLUGIN_API addRef () override { return static_cast<uint32> (++mRefCount); }
    uint32 PLUGIN_API release () override
    {
        const uint32 rc = static_cast<uint32> (--mRefCount);
        if (rc == 0) delete this;
        return rc;
    }

    tresult PLUGIN_API initialize (FUnknown*) override
    {
        sp_log_enable ();
        return kResultOk;
    }
    tresult PLUGIN_API terminate () override { return kResultOk; }

    // Unanswered so a host asks this object for IEditController.
    tresult PLUGIN_API getControllerClassId (TUID) override { return kNotImplemented; }
    tresult PLUGIN_API setIoMode (IoMode) override { return kResultOk; }

    int32 PLUGIN_API getBusCount (MediaType type, BusDirection dir) override
    {
        if (type == kAudio) return 1;   // a stereo output, and a stereo input for live_audio
        if (type == kEvent) return dir == kInput ? 1 : 0;
        return 0;
    }

    tresult PLUGIN_API getBusInfo (MediaType type, BusDirection dir, int32 index, BusInfo& bus) override
    {
        if (index != 0) return kInvalidArgument;
        std::memset (&bus, 0, sizeof (bus));
        bus.mediaType = type;
        bus.direction = dir;
        bus.busType   = kMain;
        bus.flags     = BusInfo::kDefaultActive;
        if (type == kAudio && dir == kOutput) {
            bus.channelCount = 2;
            toStr128 ("Out", bus.name);
            return kResultOk;
        }
        if (type == kAudio && dir == kInput) {
            // an instrument's input is a side input to its host, and one the host need not connect
            bus.channelCount = 2;
            bus.busType = kAux;
            toStr128 ("In", bus.name);
            return kResultOk;
        }
        if (type == kEvent && dir == kInput) {
            bus.channelCount = 1;
            toStr128 ("Event In", bus.name);
            return kResultOk;
        }
        return kInvalidArgument;
    }

    tresult PLUGIN_API getRoutingInfo (RoutingInfo&, RoutingInfo&) override { return kNotImplemented; }
    tresult PLUGIN_API activateBus (MediaType, BusDirection, int32 index, TBool) override
    {
        return index == 0 ? kResultOk : kInvalidArgument;
    }
    tresult PLUGIN_API setActive (TBool) override { return kResultOk; }
    // The state a host keeps in its project, the component's and the controller's alike (one object is both), every
    // number a little-endian 32-bit integer: a magic word, the version, the editor's width and height; from version 3
    // the page's zoom in percent; then, from version 2, the page's storage (bridge.js: its buffers, its sets, its
    // preferences), a count and each key and its value as a length and its UTF-8 bytes. With the editor open, the
    // page starts again on what came.
    tresult PLUGIN_API setState (IBStream* stream) override
    {
        int32 words[4] = {};
        if (!readAll (stream, words, sizeof words) || uint32 (words[0]) != kStateMagic) return kResultFalse;
        if (words[1] < 1 || words[1] > kStateVersion) return kResultFalse;
        int32 zoom = 100;
        if (words[1] >= 3 && !readAll (stream, &zoom, sizeof zoom)) return kResultFalse;
        std::map<std::string, std::string> store;
        if (words[1] >= 2) {
            uint32 count = 0;
            if (!readAll (stream, &count, sizeof count)) return kResultFalse;
            for (uint32 i = 0; i < count; ++i) {
                std::string key, value;
                if (!readText (stream, key) || !readText (stream, value)) return kResultFalse;
                store[std::move (key)] = std::move (value);
            }
        }
        setEditorSize (words[2], words[3]);
        setZoom (zoom);
        {
            std::lock_guard<std::mutex> lk (mStoreMutex);
            mStore = std::move (store);
        }
        if (mEditor) sp_view_store_replaced (mEditor);
        return kResultOk;
    }
    tresult PLUGIN_API getState (IBStream* stream) override
    {
        int32 words[5] = { int32 (kStateMagic), kStateVersion, mEditorWidth, mEditorHeight, mZoom };
        if (!writeAll (stream, words, sizeof words)) return kResultFalse;
        std::lock_guard<std::mutex> lk (mStoreMutex);
        const uint32 count = uint32 (mStore.size ());
        if (!writeAll (stream, &count, sizeof count)) return kResultFalse;
        for (const auto& [key, value] : mStore)
            if (!writeText (stream, key) || !writeText (stream, value)) return kResultFalse;
        return kResultOk;
    }

    // The page's storage (lib.rs): main thread's, and a host may ask for the state on another, so under the lock.
    void storePut (std::string key, std::string value)
    {
        std::lock_guard<std::mutex> lk (mStoreMutex);
        mStore[std::move (key)] = std::move (value);
    }
    void storeRemove (const std::string& key)
    {
        std::lock_guard<std::mutex> lk (mStoreMutex);
        mStore.erase (key);
    }
    void storeClear ()
    {
        std::lock_guard<std::mutex> lk (mStoreMutex);
        mStore.clear ();
    }
    template <typename Each> void storeEach (Each each)
    {
        std::lock_guard<std::mutex> lk (mStoreMutex);
        for (const auto& [key, value] : mStore) each (key, value);
    }
    void setEditor (void* editor) { mEditor = editor; }

    void editorSize (int32& width, int32& height) const { width = mEditorWidth; height = mEditorHeight; }
    int32 zoom () const { return mZoom; }
    // a zoom a browser has (lib.rs ZOOMS' range)
    void setZoom (int32 percent)
    {
        if (percent >= 50 && percent <= 300) mZoom = percent;
    }
    void setEditorSize (int32 width, int32 height)
    {
        // a size a person could have left it at: no smaller than checkSizeConstraint allows, no larger than a screen
        if (width < 640 || height < 420 || width > 16384 || height > 16384) return;
        mEditorWidth = width;
        mEditorHeight = height;
    }

    tresult PLUGIN_API setBusArrangements (SpeakerArrangement* inputs, int32 numIns,
                                           SpeakerArrangement* outputs, int32 numOuts) override
    {
        if (numIns > 1 || numOuts != 1) return kResultFalse;
        if (numIns == 1 && inputs[0] != SpeakerArr::kStereo) return kResultFalse;
        return outputs[0] == SpeakerArr::kStereo ? kResultTrue : kResultFalse;
    }

    tresult PLUGIN_API getBusArrangement (BusDirection dir, int32 index, SpeakerArrangement& arr) override
    {
        if (index != 0) return kInvalidArgument;
        (void) dir;   // the input and the output are both stereo
        arr = SpeakerArr::kStereo;
        return kResultOk;
    }

    tresult PLUGIN_API canProcessSampleSize (int32 size) override
    {
        return size == kSample32 ? kResultTrue : kResultFalse;
    }
    uint32 PLUGIN_API getLatencySamples () override { return 0; }
    uint32 PLUGIN_API getTailSamples () override { return 0; }
    // The instance starts the first time the host says its rate, and keeps its runs when the rate changes.
    tresult PLUGIN_API setupProcessing (ProcessSetup& setup) override
    {
        if (setup.symbolicSampleSize != kSample32 || setup.sampleRate <= 0) return kResultFalse;
        const uint32_t frames = setup.maxSamplesPerBlock > 0 ? (uint32_t) setup.maxSamplesPerBlock : 4096;
        if (!mInstance) {
            const std::string resources = resourcesDir ();
            SpInstanceConfig config {};
            config.struct_bytes = sizeof config;
            config.sample_rate = setup.sampleRate;
            config.max_frames = frames;
            config.resources = resources.c_str ();
            char why[512] = {};
            mInstance = sp_instance_new (&config, why, sizeof why);
            if (!mInstance) {
                sp_host_note ((std::string ("Sonic Pi did not start: ") + why).c_str ());
                return kResultFalse;
            }
        } else if (setup.sampleRate != mRate || frames != mMaxFrames) {
            sp_instance_reconfigure (mInstance, setup.sampleRate, frames);
        }
        mRate = setup.sampleRate;
        mMaxFrames = frames;
        return kResultOk;
    }
    tresult PLUGIN_API setProcessing (TBool) override { return kResultOk; }

    // What process() reads of the host's transport: a host fills in no more than it is asked for.
    uint32 PLUGIN_API getProcessContextRequirements () override
    {
        return kNeedTempo | kNeedTimeSignature | kNeedTransportState | kNeedProjectTimeMusic | kNeedBarPositionMusic;
    }

    tresult PLUGIN_API process (ProcessData& data) override
    {
        if (data.numOutputs < 1 || !data.outputs || !data.outputs[0].channelBuffers32 || data.numSamples <= 0)
            return kResultOk;
        AudioBusBuffers& out = data.outputs[0];
        if (mInstance) {
            // The host's transport at this buffer's first frame: its tempo, its time signature, whether it plays,
            // and where it is in its bar (from the bar line it last passed, where it says that).
            SpTransport transport {};
            transport.beat_in_bar = -1.0;
            transport.playing = -1;
            if (const ProcessContext* context = data.processContext) {
                if (context->state & ProcessContext::kTempoValid) transport.bpm = context->tempo;
                if (context->state & ProcessContext::kTimeSigValid) {
                    transport.meter_num = context->timeSigNumerator;
                    transport.meter_den = context->timeSigDenominator;
                }
                transport.playing = (context->state & ProcessContext::kPlaying) ? 1 : 0;
                if (context->state & ProcessContext::kProjectTimeMusicValid) {
                    const double fromBar = (context->state & ProcessContext::kBarPositionValid)
                                         ? context->projectTimeMusic - context->barPositionMusic
                                         : context->projectTimeMusic;
                    // a position before the project's start counts back from its first bar line
                    transport.beat_in_bar = fromBar >= 0.0 ? fromBar : 0.0;
                }
            }
            sp_instance_transport (mInstance, &transport);
            // the host's input, when it has connected one: what live_audio plays
            const AudioBusBuffers* in = (data.numInputs > 0 && data.inputs && data.inputs[0].channelBuffers32) ? &data.inputs[0] : nullptr;
            sp_instance_render (mInstance, out.channelBuffers32, (uint32_t) out.numChannels,
                                in ? in->channelBuffers32 : nullptr, in ? (uint32_t) in->numChannels : 0,
                                (uint32_t) data.numSamples);
            out.silenceFlags = 0;
            return kResultOk;
        }
        for (int32 channel = 0; channel < out.numChannels; ++channel)
            if (float* o = out.channelBuffers32[channel])
                std::memset (o, 0, sizeof (float) * static_cast<size_t> (data.numSamples));
        out.silenceFlags = (uint64 (1) << out.numChannels) - 1;
        return kResultOk;
    }

    tresult PLUGIN_API setComponentState (IBStream*) override { return kResultOk; }
    int32 PLUGIN_API getParameterCount () override { return 0; }
    tresult PLUGIN_API getParameterInfo (int32, ParameterInfo&) override { return kInvalidArgument; }
    tresult PLUGIN_API getParamStringByValue (ParamID, ParamValue, String128) override { return kInvalidArgument; }
    tresult PLUGIN_API getParamValueByString (ParamID, TChar*, ParamValue&) override { return kNotImplemented; }
    ParamValue PLUGIN_API normalizedParamToPlain (ParamID, ParamValue norm) override { return norm; }
    ParamValue PLUGIN_API plainParamToNormalized (ParamID, ParamValue plain) override { return plain; }
    ParamValue PLUGIN_API getParamNormalized (ParamID) override { return 0.0; }
    tresult PLUGIN_API setParamNormalized (ParamID, ParamValue) override { return kInvalidArgument; }
    tresult PLUGIN_API setComponentHandler (IComponentHandler*) override { return kResultOk; }

    IPlugView* PLUGIN_API createView (FIDString name) override
    {
        return (name && std::strcmp (name, ViewType::kEditor) == 0) ? new SonicPiView (this) : nullptr;
    }

private:
    static constexpr uint32 kStateMagic = 0x6C705053;   // "SPpl", little-endian
    static constexpr int32 kStateVersion = 3;
    static constexpr uint32 kLongestText = 64u << 20;   // a key or a value no longer than any page writes, by far

    static bool readAll (IBStream* stream, void* out, int32 bytes)
    {
        int32 got = 0;
        return stream && stream->read (out, bytes, &got) == kResultOk && got == bytes;
    }
    static bool writeAll (IBStream* stream, const void* in, int32 bytes)
    {
        int32 put = 0;
        return stream && stream->write (const_cast<void*> (in), bytes, &put) == kResultOk && put == bytes;
    }
    static bool readText (IBStream* stream, std::string& out)
    {
        uint32 bytes = 0;
        if (!readAll (stream, &bytes, sizeof bytes) || bytes > kLongestText) return false;
        out.resize (bytes);
        return bytes == 0 || readAll (stream, out.data (), int32 (bytes));
    }
    static bool writeText (IBStream* stream, const std::string& text)
    {
        const uint32 bytes = uint32 (text.size ());
        return writeAll (stream, &bytes, sizeof bytes) && (bytes == 0 || writeAll (stream, text.data (), int32 (bytes)));
    }

    std::atomic<int32> mRefCount;
    SpInstance* mInstance = nullptr;
    double mRate = 0;
    uint32_t mMaxFrames = 0;
    int32 mEditorWidth = 1100;   // the editor's first size, then the one it was left at (main thread's)
    int32 mEditorHeight = 760;
    int32 mZoom = 100;           // the page's zoom, in percent (main thread's)
    std::mutex mStoreMutex;
    std::map<std::string, std::string> mStore;   // the page's storage, kept with the state
    void* mEditor = nullptr;                     // the editor open now (main thread's)
};

SpInstance* instanceOf (SonicPiProcessor* processor) { return processor->instance (); }
void retainProcessor (SonicPiProcessor* processor) { processor->addRef (); }
void releaseProcessor (SonicPiProcessor* processor) { processor->release (); }
void editorSizeOf (SonicPiProcessor* processor, int32& width, int32& height) { processor->editorSize (width, height); }
void keepEditorSize (SonicPiProcessor* processor, int32 width, int32 height) { processor->setEditorSize (width, height); }
void editorOpened (SonicPiProcessor* processor, void* editor) { processor->setEditor (editor); }

// The page's storage, for lib.rs: `host` the plugin an editor belongs to (sp_view_attach's).
extern "C" void sp_host_store_put (void* host, const char* key, size_t keyLength, const char* value, size_t valueLength, int remove)
{
    auto* processor = static_cast<SonicPiProcessor*> (host);
    if (remove) processor->storeRemove (std::string (key, keyLength));
    else processor->storePut (std::string (key, keyLength), std::string (value, valueLength));
}
extern "C" void sp_host_store_clear (void* host) { static_cast<SonicPiProcessor*> (host)->storeClear (); }
// The page's zoom, kept with the state (lib.rs).
extern "C" int sp_host_zoom (void* host) { return static_cast<SonicPiProcessor*> (host)->zoom (); }
extern "C" void sp_host_keep_zoom (void* host, int percent) { static_cast<SonicPiProcessor*> (host)->setZoom (percent); }
extern "C" void sp_host_store_each (void* host, void (*each) (void*, const char*, size_t, const char*, size_t), void* ctx)
{
    static_cast<SonicPiProcessor*> (host)->storeEach ([&] (const std::string& key, const std::string& value) {
        each (ctx, key.data (), key.size (), value.data (), value.size ());
    });
}

class SonicPiFactory : public IPluginFactory2
{
public:
    tresult PLUGIN_API queryInterface (const TUID iid, void** obj) override
    {
        if (!obj) return kInvalidArgument;
        if (sameIID (iid, FUnknown_iid) || sameIID (iid, IPluginFactory_iid))
            *obj = static_cast<IPluginFactory*> (this);
        else if (sameIID (iid, IPluginFactory2_iid))
            *obj = static_cast<IPluginFactory2*> (this);
        else { *obj = nullptr; return kNoInterface; }
        return kResultOk;
    }
    uint32 PLUGIN_API addRef () override { return 1; }
    uint32 PLUGIN_API release () override { return 1; }

    tresult PLUGIN_API getFactoryInfo (PFactoryInfo* info) override
    {
        if (!info) return kInvalidArgument;
        std::memset (info, 0, sizeof (*info));
        std::strncpy (info->vendor, "Sonic Pi", PFactoryInfo::kNameSize - 1);
        std::strncpy (info->url, "https://sonic-pi.net", PFactoryInfo::kURLSize - 1);
        info->flags = PFactoryInfo::kUnicode;
        return kResultOk;
    }

    int32 PLUGIN_API countClasses () override { return 1; }

    tresult PLUGIN_API getClassInfo (int32 index, PClassInfo* info) override
    {
        if (!info || index != 0) return kInvalidArgument;
        std::memset (info, 0, sizeof (*info));
        std::memcpy (info->cid, kSonicPiUID, sizeof (TUID));
        std::strncpy (info->category, kVstAudioEffectClass, PClassInfo::kCategorySize - 1);
        std::strncpy (info->name, "Sonic Pi", PClassInfo::kNameSize - 1);
        info->cardinality = PClassInfo::kManyInstances;
        return kResultOk;
    }

    tresult PLUGIN_API getClassInfo2 (int32 index, PClassInfo2* info) override
    {
        if (!info) return kInvalidArgument;
        PClassInfo base;
        if (getClassInfo (index, &base) != kResultOk) return kInvalidArgument;
        std::memset (info, 0, sizeof (*info));
        std::memcpy (info->cid, base.cid, sizeof (TUID));
        info->cardinality = base.cardinality;
        std::strncpy (info->category, base.category, PClassInfo::kCategorySize - 1);
        std::strncpy (info->name, base.name, PClassInfo::kNameSize - 1);
        std::strncpy (info->subCategories, PlugType::kInstrument, PClassInfo2::kSubCategoriesSize - 1);
        std::strncpy (info->vendor, "Sonic Pi", PClassInfo2::kVendorSize - 1);
        std::strncpy (info->version, "0.1.0", PClassInfo2::kVersionSize - 1);
        std::strncpy (info->sdkVersion, kVstVersionString, PClassInfo2::kVersionSize - 1);
        return kResultOk;
    }

    tresult PLUGIN_API createInstance (FIDString cid, FIDString iid, void** obj) override
    {
        if (!obj) return kInvalidArgument;
        *obj = nullptr;
        if (!cid || !sameIID (cid, kSonicPiUID)) return kNoInterface;
        auto* processor = new SonicPiProcessor ();
        const tresult result = processor->queryInterface (iid, obj);
        processor->release ();
        return result == kResultOk ? kResultOk : kNoInterface;
    }
};

} // namespace

extern "C" void* sp_get_plugin_factory ()
{
    static SonicPiFactory factory;
    return static_cast<void*> (static_cast<IPluginFactory*> (&factory));
}
