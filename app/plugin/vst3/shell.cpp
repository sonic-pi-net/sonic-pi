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
#include "pluginterfaces/vst/ivstevents.h"
#include "pluginterfaces/vst/ivstmidicontrollers.h"
#include "pluginterfaces/vst/ivstparameterchanges.h"
#include "pluginterfaces/vst/ivstprocesscontext.h"
#include "pluginterfaces/vst/vstspeaker.h"
#include "pluginterfaces/vst/vsttypes.h"
#include "pluginterfaces/gui/iplugview.h"
#include "pluginterfaces/gui/iplugviewcontentscalesupport.h"

#ifdef __APPLE__
#include <CoreFoundation/CoreFoundation.h>
#endif
#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#endif

#include <atomic>
#include <cstring>
#include <string>

#include "plugin.h"

using namespace Steinberg;
using namespace Steinberg::Vst;

extern "C" {
void* sp_view_attach (void* parent, unsigned width, unsigned height, SpInstance* instance, void* host);
void  sp_log_enable ();   // filelog.rs: a host has loaded the plugin, so its lines go to the user's log folder
void  sp_view_resize (void* view, unsigned width, unsigned height);
void  sp_view_detach (void* view);
int   sp_host_key (void* view, int down, int key, int code, int mods);
void  sp_host_note (const char* text);
int   sp_parent_ready (void* parent);
}

namespace {

static const DECLARE_UID (kSonicPiUID, 0x2F6C1D84, 0x7A3B4E19, 0x9C05D6E2, 0x41B8F07A);

inline bool sameIID (const void* a, const void* b)
{
    return std::memcmp (a, b, sizeof (TUID)) == 0;
}

// A host's event as the MIDI message it stands for: a note on, a note off, or a key's pressure. VST3 hands a plugin
// nothing else as an event (a controller or a bend comes only as a parameter). False for any other event.
inline bool eventToMidi (const Event& e, uint8_t bytes[3])
{
    const auto seven = [] (float v) { return static_cast<uint8_t> (v <= 0.f ? 0 : v >= 1.f ? 127 : static_cast<int> (v * 127.f + 0.5f)); };
    switch (e.type) {
        case Event::kNoteOnEvent:
            bytes[0] = static_cast<uint8_t> (0x90 | (e.noteOn.channel & 0x0F));
            bytes[1] = static_cast<uint8_t> (e.noteOn.pitch & 0x7F);
            // a note on is never velocity 0, which in MIDI is a note off
            bytes[2] = seven (e.noteOn.velocity) ? seven (e.noteOn.velocity) : uint8_t (1);
            return true;
        case Event::kNoteOffEvent:
            bytes[0] = static_cast<uint8_t> (0x80 | (e.noteOff.channel & 0x0F));
            bytes[1] = static_cast<uint8_t> (e.noteOff.pitch & 0x7F);
            bytes[2] = seven (e.noteOff.velocity);
            return true;
        case Event::kPolyPressureEvent:
            bytes[0] = static_cast<uint8_t> (0xA0 | (e.polyPressure.channel & 0x0F));
            bytes[1] = static_cast<uint8_t> (e.polyPressure.pitch & 0x7F);
            bytes[2] = seven (e.polyPressure.pressure);
            return true;
        default:
            return false;
    }
}

// A MIDI message a program sent as the event a host takes for it, at its frame of the buffer. Notes and a key's
// pressure are events of their own; a controller, a program change, a channel's pressure and a bend go as the
// legacy MIDI event, which most hosts pass on. False for what VST3 has no event for (MIDI clock, start, stop).
inline bool midiToEvent (const SpMidi& m, Event& e)
{
    if (m.length < 2) return false;
    std::memset (&e, 0, sizeof e);
    e.busIndex = 0;
    e.sampleOffset = static_cast<int32> (m.frame);
    const int16 channel = static_cast<int16> (m.bytes[0] & 0x0F);
    const uint8_t a = m.bytes[1] & 0x7F, b = m.length > 2 ? (m.bytes[2] & 0x7F) : 0;
    const auto legacy = [&] (uint8 control, uint8_t value, uint8_t value2) {
        e.type = Event::kLegacyMIDICCOutEvent;
        e.midiCCOut.controlNumber = control;
        e.midiCCOut.channel = static_cast<int8> (channel);
        e.midiCCOut.value = static_cast<int8> (value);
        e.midiCCOut.value2 = static_cast<int8> (value2);
    };
    switch (m.bytes[0] & 0xF0) {
        case 0x90:
            if (b == 0) {   // a note on of velocity 0 is a note off
                e.type = Event::kNoteOffEvent;
                e.noteOff.channel = channel; e.noteOff.pitch = a; e.noteOff.velocity = 0.f; e.noteOff.noteId = -1;
                return true;
            }
            e.type = Event::kNoteOnEvent;
            e.noteOn.channel = channel; e.noteOn.pitch = a; e.noteOn.velocity = b / 127.f; e.noteOn.noteId = -1;
            return true;
        case 0x80:
            e.type = Event::kNoteOffEvent;
            e.noteOff.channel = channel; e.noteOff.pitch = a; e.noteOff.velocity = b / 127.f; e.noteOff.noteId = -1;
            return true;
        case 0xA0:
            e.type = Event::kPolyPressureEvent;
            e.polyPressure.channel = channel; e.polyPressure.pitch = a; e.polyPressure.pressure = b / 127.f; e.polyPressure.noteId = -1;
            return true;
        case 0xB0: legacy (a, b, 0); return true;
        case 0xC0: legacy (kCtrlProgramChange, a, 0); return true;
        case 0xD0: legacy (kAfterTouch, a, 0); return true;
        case 0xE0: legacy (kPitchBend, a, b); return true;
        default: return false;
    }
}

inline void toStr128 (const char* s, String128 out)
{
    int i = 0;
    for (; s[i] && i < 127; ++i)
        out[i] = static_cast<char16> (static_cast<unsigned char> (s[i]));
    out[i] = 0;
}

class SonicPiProcessor;
// What the processor keeps for its host (plugin.h): its instance, the editor's size, the page's storage.
SpPlugin* pluginOf (SonicPiProcessor* processor);
void retainProcessor (SonicPiProcessor* processor);
void releaseProcessor (SonicPiProcessor* processor);

// The editor's size is kept in points, as macOS has it, so a project's size is the same on either. A Windows host
// sizes a view in its window's pixels, which are the points times the window's scale: the host's, when it says it
// (IPlugViewContentScaleSupport), and until then the system's.
class SonicPiView : public IPlugView
#ifdef _WIN32
                  , public IPlugViewContentScaleSupport
#endif
{
public:
    explicit SonicPiView (SonicPiProcessor* processor) : mRefCount (1), mProcessor (processor)
    {
        retainProcessor (processor);
        pluginOf (processor)->editorSize (mWidth, mHeight);   // opened at the size it was left at
#ifdef _WIN32
        mScale = GetDpiForSystem () / 96.0;
#endif
    }
    virtual ~SonicPiView () { removed (); releaseProcessor (mProcessor); }

    tresult PLUGIN_API queryInterface (const TUID iid, void** obj) override
    {
        if (!obj) return kInvalidArgument;
        if (sameIID (iid, FUnknown_iid) || sameIID (iid, IPlugView_iid))
            *obj = static_cast<IPlugView*> (this);
#ifdef _WIN32
        else if (sameIID (iid, IPlugViewContentScaleSupport_iid))
            *obj = static_cast<IPlugViewContentScaleSupport*> (this);
#endif
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
#ifdef _WIN32
        return (type && std::strcmp (type, kPlatformTypeHWND) == 0) ? kResultTrue : kResultFalse;
#else
        return (type && std::strcmp (type, kPlatformTypeNSView) == 0) ? kResultTrue : kResultFalse;
#endif
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
        if (mView) { pluginOf (mProcessor)->setEditor (nullptr); sp_view_detach (mView); mView = nullptr; }
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
        size->right = px (mWidth); size->bottom = px (mHeight);
        return kResultOk;
    }

    tresult PLUGIN_API onSize (ViewRect* newSize) override
    {
        if (!newSize) return kInvalidArgument;
        mWidth  = pt (newSize->right - newSize->left);
        mHeight = pt (newSize->bottom - newSize->top);
        if (mView) sp_view_resize (mView, (unsigned) (newSize->right - newSize->left), (unsigned) (newSize->bottom - newSize->top));
        pluginOf (mProcessor)->setEditorSize (mWidth, mHeight);
        // said, so the log shows who sized it: the person, or a host restoring a size of its own
        sp_host_note (("editor resized to " + std::to_string (mWidth) + "x" + std::to_string (mHeight)).c_str ());
        return kResultOk;
    }

    tresult PLUGIN_API setFrame (IPlugFrame* frame) override { mFrame = frame; return kResultOk; }

#ifdef _WIN32
    // The host's scale for the window it shows the editor in (a monitor's DPI): the editor keeps its size in points
    // and asks the host for the pixels that now make.
    tresult PLUGIN_API setContentScaleFactor (ScaleFactor factor) override
    {
        if (factor <= 0 || factor == mScale) return kResultTrue;
        mScale = factor;
        sp_host_note (("content scale " + std::to_string (factor)).c_str ());
        ViewRect rect (0, 0, px (mWidth), px (mHeight));
        if (mFrame && mParent) mFrame->resizeView (this, &rect);
        return kResultTrue;
    }
#endif
    tresult PLUGIN_API canResize () override { return kResultTrue; }

    tresult PLUGIN_API checkSizeConstraint (ViewRect* rect) override
    {
        if (!rect) return kInvalidArgument;
        if (rect->right - rect->left < px (SpPlugin::kSmallestWidth)) rect->right = rect->left + px (SpPlugin::kSmallestWidth);
        if (rect->bottom - rect->top < px (SpPlugin::kSmallestHeight)) rect->bottom = rect->top + px (SpPlugin::kSmallestHeight);
        return kResultTrue;
    }

private:
    // Points to the host's units and back: the same on macOS, pixels on Windows.
    int32 px (int32 points) const { return static_cast<int32> (points * mScale + 0.5); }
    int32 pt (int32 units) const { return static_cast<int32> (units / mScale + 0.5); }

    // True once there is nothing left to wait for, whether or not it worked.
    bool tryAttach ()
    {
        if (!sp_parent_ready (mParent)) return false;
        SpPlugin* plugin = pluginOf (mProcessor);
        mView = sp_view_attach (mParent, (unsigned) px (mWidth), (unsigned) px (mHeight), plugin->instance (), plugin);
        plugin->setEditor (mView);
        return true;
    }

#ifdef __APPLE__
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
#else
    // A Windows host hands over a window that is already there (sp_parent_ready), so there is never a wait.
    void startRetry () { sp_host_note ("attached to a window that is not there"); }
    void stopRetry () {}
#endif

    std::atomic<int32> mRefCount;
    SonicPiProcessor* mProcessor;   // held: it owns the instance the editor talks to
    void* mParent = nullptr;
    void* mView = nullptr;
    IPlugFrame* mFrame = nullptr;   // the host's, while it shows the editor (not held: VST3's rule)
    double mScale = 1.0;            // the host's pixels to a point: 1 on macOS
#ifdef __APPLE__
    CFRunLoopTimerRef mRetry = nullptr;
#endif
    int32 mWidth = 0;
    int32 mHeight = 0;
};

class SonicPiProcessor : public IComponent, public IAudioProcessor, public IEditController,
                         public IProcessContextRequirements
{
public:
    SonicPiProcessor () : mRefCount (1) {}
    virtual ~SonicPiProcessor () = default;

    SpPlugin* plugin () { return &mPlugin; }

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

    int32 PLUGIN_API getBusCount (MediaType type, BusDirection) override
    {
        // A stereo output, and a stereo input for live_audio. A MIDI input and a MIDI output: Sonic Pi's port `daw`.
        // (Ableton Live refuses an instrument without a MIDI input: "plugin has instrument category, but no valid
        // event input bus".)
        return (type == kAudio || type == kEvent) ? 1 : 0;
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
        if (type == kEvent) {
            bus.channelCount = 16;
            toStr128 (dir == kInput ? "MIDI In" : "MIDI Out", bus.name);
            return kResultOk;
        }
        return kInvalidArgument;
    }

    tresult PLUGIN_API getRoutingInfo (RoutingInfo&, RoutingInfo&) override { return kNotImplemented; }
    tresult PLUGIN_API activateBus (MediaType type, BusDirection dir, int32 index, TBool) override
    {
        return index < getBusCount (type, dir) && index >= 0 ? kResultOk : kInvalidArgument;
    }
    tresult PLUGIN_API setActive (TBool) override { return kResultOk; }
    // The state a host keeps in its project, the component's and the controller's alike (one object is both): the
    // plugin's own bytes (plugin.h). With the editor open, the page starts again on what came.
    tresult PLUGIN_API setState (IBStream* stream) override
    {
        const auto read = [stream] (void* out, uint32_t bytes) {
            int32 got = 0;
            return stream && stream->read (out, int32 (bytes), &got) == kResultOk && got == int32 (bytes);
        };
        if (!mPlugin.readState (read)) return kResultFalse;
        mPlugin.editorTakesState ();
        return kResultOk;
    }
    tresult PLUGIN_API getState (IBStream* stream) override
    {
        std::vector<uint8_t> state = mPlugin.writeState ();
        int32 put = 0;
        return stream && stream->write (state.data (), int32 (state.size ()), &put) == kResultOk && put == int32 (state.size ())
               ? kResultOk : kResultFalse;
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
        std::string why;
        if (!mPlugin.start (setup.sampleRate, frames, why)) return kResultFalse;
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
        if (SpInstance* instance = mPlugin.instance ()) {
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
            sp_instance_transport (instance, &transport);
            // The host's notes, each for its frame of this buffer: cues at "/midi:daw:<channel>/note_on".
            if (data.inputEvents) {
                const int32 count = data.inputEvents->getEventCount ();
                for (int32 i = 0; i < count; ++i) {
                    Event e {};
                    uint8_t bytes[3] = {};
                    if (data.inputEvents->getEvent (i, e) != kResultOk || !eventToMidi (e, bytes)) continue;
                    const int32 at = e.sampleOffset < 0 ? 0 : e.sampleOffset >= data.numSamples ? data.numSamples - 1 : e.sampleOffset;
                    sp_instance_midi_in (instance, bytes, 3, static_cast<uint32_t> (at));
                }
            }
            // the host's input, when it has connected one: what live_audio plays
            const AudioBusBuffers* in = (data.numInputs > 0 && data.inputs && data.inputs[0].channelBuffers32) ? &data.inputs[0] : nullptr;
            sp_instance_render (instance, out.channelBuffers32, (uint32_t) out.numChannels,
                                in ? in->channelBuffers32 : nullptr, in ? (uint32_t) in->numChannels : 0,
                                (uint32_t) data.numSamples);
            // What a program sent to `daw` that came due in this buffer, to the host at its frame. Taken whether or
            // not the host has anywhere to put it, so that nothing waits to come out late.
            SpMidi sent[64];
            for (uint32_t n; (n = sp_instance_midi_out (instance, sent, 64)) > 0;) {
                for (uint32_t i = 0; i < n && data.outputEvents; ++i) {
                    Event e {};
                    if (midiToEvent (sent[i], e)) data.outputEvents->addEvent (e);
                }
            }
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
    std::atomic<int32> mRefCount;
    SpPlugin mPlugin { "VST" };
};

SpPlugin* pluginOf (SonicPiProcessor* processor) { return processor->plugin (); }
void retainProcessor (SonicPiProcessor* processor) { processor->addRef (); }
void releaseProcessor (SonicPiProcessor* processor) { processor->release (); }

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
        std::strncpy (info->version, SONIC_PI_PLUGIN_VERSION, PClassInfo2::kVersionSize - 1);   // Cargo.toml's, from build.rs
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
