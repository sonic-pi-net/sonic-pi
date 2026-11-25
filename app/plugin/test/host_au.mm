// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// Loads the Audio Unit the way a host does: makes one from the bundle's factory, sets its rate and its callbacks,
// renders it on a thread of its own, hands it a project's state, and puts its Cocoa view in an off-screen window.
// Nothing is played to a device: the rendered audio is kept and measured. Nothing is installed either: the
// component is registered for this process alone.
//
// What the instrument does once it runs is the plugin's own, checked through the VST3 (host.mm); here it is what
// an Audio Unit host asks of it: its properties, its render, the host's transport and MIDI each way, its state, and
// its view.
//
//   SONIC_PI_PLUGIN_LOG=<file> SONIC_PI_PLUGIN_PROBE=1 host_au <Sonic Pi.component>
#import <AppKit/AppKit.h>
#import <AudioToolbox/AudioToolbox.h>
#import <AudioUnit/AUCocoaUIView.h>
#import <CoreMIDI/CoreMIDI.h>
#import <WebKit/WebKit.h>
#include <dlfcn.h>

#include <algorithm>
#include <array>
#include <atomic>
#include <chrono>
#include <cmath>
#include <cstring>
#include <mutex>
#include <string>
#include <thread>
#include <vector>

#include "host_mac.h"

static const double kRate = 48000;
static const UInt32 kBlock = 512;
// the component, as the bundle's Info.plist has it (scripts/build.sh)
static const AudioComponentDescription kSonicPi = { kAudioUnitType_MusicDevice, 'SPpl', 'SnPi', 0, 0 };

struct MidiOut { uint8_t bytes[3]; uint16_t length; long long frame; };   // frame: counted from the first rendered

struct Unit {
    AudioUnit unit = nullptr;
    std::thread render;
    std::atomic<bool> quit { false };
    std::atomic<int> transport { -1 };   // the host's transport: -1 it says nothing, 0 stopped, 1 playing
    std::atomic<long long> frames { 0 };
    std::atomic<double> bpm { 60.0 };    // its tempo, and where it is in beats (the render thread moves it on)
    std::atomic<double> beat { 0.0 };
    std::mutex mutex;
    std::vector<float> heard;            // the left channel, every frame rendered
    std::vector<MidiOut> midiReceived;
    std::vector<std::array<UInt32, 4>> midiToSend;   // status, two data bytes, frame
};

// ── The host's side of the Audio Unit's callbacks: a transport at its tempo, four to the bar ──

static OSStatus beatAndTempo (void* userData, Float64* beat, Float64* tempo)
{
    Unit& u = *static_cast<Unit*> (userData);
    if (u.transport.load () < 0) return kAudioUnitErr_CannotDoInCurrentContext;
    if (beat) *beat = u.beat.load ();
    if (tempo) *tempo = u.bpm.load ();
    return noErr;
}

static OSStatus musicalTime (void* userData, UInt32* toNextBeat, Float32* numerator, UInt32* denominator, Float64* barStart)
{
    Unit& u = *static_cast<Unit*> (userData);
    if (u.transport.load () < 0) return kAudioUnitErr_CannotDoInCurrentContext;
    const double beat = u.beat.load ();
    if (toNextBeat) *toNextBeat = 0;
    if (numerator) *numerator = 4;
    if (denominator) *denominator = 4;
    if (barStart) *barStart = std::floor (beat / 4.0) * 4.0;
    return noErr;
}

static OSStatus transportState (void* userData, Boolean* playing, Boolean* changed, Float64* sample, Boolean* cycling, Float64* cycleStart, Float64* cycleEnd)
{
    Unit& u = *static_cast<Unit*> (userData);
    if (u.transport.load () < 0) return kAudioUnitErr_CannotDoInCurrentContext;
    if (playing) *playing = u.transport.load () == 1;
    if (changed) *changed = false;
    if (sample) *sample = double (u.frames.load ());
    if (cycling) *cycling = false;
    if (cycleStart) *cycleStart = 0;
    if (cycleEnd) *cycleEnd = 0;
    return noErr;
}

static OSStatus midiFromUnit (void* userData, const AudioTimeStamp*, UInt32, const MIDIPacketList* list)
{
    Unit& u = *static_cast<Unit*> (userData);
    std::lock_guard<std::mutex> lk (u.mutex);
    const MIDIPacket* packet = &list->packet[0];
    for (UInt32 i = 0; i < list->numPackets; ++i) {
        MidiOut out {};
        out.length = std::min<uint16_t> (packet->length, 3);
        std::memcpy (out.bytes, packet->data, out.length);
        out.frame = u.frames.load () + (long long) packet->timeStamp;
        u.midiReceived.push_back (out);
        packet = MIDIPacketNext (packet);
    }
    return noErr;
}

template <typename T> static OSStatus get (AudioUnit unit, AudioUnitPropertyID property, AudioUnitScope scope, T& value)
{
    UInt32 size = sizeof value;
    return AudioUnitGetProperty (unit, property, scope, 0, &value, &size);
}

template <typename T> static OSStatus set (AudioUnit unit, AudioUnitPropertyID property, AudioUnitScope scope, const T& value)
{
    return AudioUnitSetProperty (unit, property, scope, 0, &value, sizeof value);
}

// One render of `frames` on the calling thread, into buffers of the host's own or (`bringNone`) none.
static OSStatus renderOnce (Unit& u, UInt32 frames, bool bringNone, float** left = nullptr)
{
    static std::vector<float> l, r;
    l.assign (frames, 1.f);
    r.assign (frames, 1.f);
    struct { AudioBufferList list; AudioBuffer second; } two;
    two.list.mNumberBuffers = 2;
    two.list.mBuffers[0] = { 1, UInt32 (frames * sizeof (float)), bringNone ? nullptr : l.data () };
    two.list.mBuffers[1] = { 1, UInt32 (frames * sizeof (float)), bringNone ? nullptr : r.data () };
    AudioTimeStamp when {};
    when.mSampleTime = double (u.frames.load ());
    when.mFlags = kAudioTimeStampSampleTimeValid;
    AudioUnitRenderActionFlags flags = 0;
    const OSStatus result = AudioUnitRender (u.unit, &flags, &when, 0, frames, &two.list);
    if (left) *left = static_cast<float*> (two.list.mBuffers[0].mData);
    return result;
}

static void startRendering (Unit& u)
{
    u.render = std::thread ([&u] {
        std::vector<float> left (kBlock), right (kBlock);
        struct { AudioBufferList list; AudioBuffer second; } two;
        auto begin = std::chrono::steady_clock::now ();
        while (!u.quit.load ()) {
            {
                std::lock_guard<std::mutex> lk (u.mutex);
                for (const auto& m : u.midiToSend) MusicDeviceMIDIEvent (u.unit, m[0], m[1], m[2], m[3]);
                u.midiToSend.clear ();
            }
            two.list.mNumberBuffers = 2;
            two.list.mBuffers[0] = { 1, UInt32 (kBlock * sizeof (float)), left.data () };
            two.list.mBuffers[1] = { 1, UInt32 (kBlock * sizeof (float)), right.data () };
            AudioTimeStamp when {};
            when.mSampleTime = double (u.frames.load ());
            when.mFlags = kAudioTimeStampSampleTimeValid;
            AudioUnitRenderActionFlags flags = 0;
            AudioUnitRender (u.unit, &flags, &when, 0, kBlock, &two.list);
            {
                std::lock_guard<std::mutex> lk (u.mutex);
                const float* out = static_cast<const float*> (two.list.mBuffers[0].mData);
                u.heard.insert (u.heard.end (), out, out + kBlock);
            }
            u.beat.store (u.beat.load () + double (kBlock) * u.bpm.load () / 60.0 / kRate);
            const long long frames = u.frames.fetch_add (kBlock) + kBlock;
            std::this_thread::sleep_until (begin + std::chrono::microseconds ((long long) (double (frames) * 1e6 / kRate)));
        }
    });
}

static size_t heardSoFar (Unit& u)
{
    std::lock_guard<std::mutex> lk (u.mutex);
    return u.heard.size ();
}

static float peakSince (Unit& u, size_t from)
{
    std::lock_guard<std::mutex> lk (u.mutex);
    float peak = 0;
    for (size_t i = from; i < u.heard.size (); ++i) peak = std::max (peak, std::fabs (u.heard[i]));
    return peak;
}

static bool soundsWithin (Unit& u, size_t from, double seconds)
{
    NSDate* until = [NSDate dateWithTimeIntervalSinceNow:seconds];
    while ([until timeIntervalSinceNow] > 0) {
        if (peakSince (u, from) > 0.05f) return true;
        pump (0.05);
    }
    return peakSince (u, from) > 0.05f;
}

static float peakOver (Unit& u, double seconds)
{
    const size_t from = heardSoFar (u);
    pump (seconds);
    return peakSince (u, from);
}

static std::vector<MidiOut> midiWithin (Unit& u, size_t count, double seconds)
{
    NSDate* until = [NSDate dateWithTimeIntervalSinceNow:seconds];
    for (;;) {
        {
            std::lock_guard<std::mutex> lk (u.mutex);
            if (u.midiReceived.size () >= count || [until timeIntervalSinceNow] <= 0) return u.midiReceived;
        }
        pump (0.05);
    }
}

// ── A project's state, as the plugin writes one (core/plugin.h) ──

static void putWord (std::vector<uint8_t>& out, uint32_t word)
{
    const auto* bytes = reinterpret_cast<const uint8_t*> (&word);
    out.insert (out.end (), bytes, bytes + 4);
}

static void putText (std::vector<uint8_t>& out, const std::string& text)
{
    putWord (out, uint32_t (text.size ()));
    out.insert (out.end (), text.begin (), text.end ());
}

// An editor of `width` by `height` at `zoom` percent, and one key of the page's storage.
static std::vector<uint8_t> stateWith (int width, int height, int zoom, const std::string& key, const std::string& value)
{
    std::vector<uint8_t> out;
    for (uint32_t word : { 0x6C705053u, 3u, uint32_t (width), uint32_t (height), uint32_t (zoom), 1u }) putWord (out, word);
    putText (out, key);
    putText (out, value);
    return out;
}

static NSDictionary* classInfo (Unit& u)
{
    CFPropertyListRef info = nullptr;
    if (get (u.unit, kAudioUnitProperty_ClassInfo, kAudioUnitScope_Global, info) != noErr || !info) return nil;
    return CFBridgingRelease (info);
}

static std::vector<uint8_t> stateOf (Unit& u)
{
    NSData* data = classInfo (u)[@kAUPresetDataKey];
    const auto* bytes = static_cast<const uint8_t*> (data.bytes);
    return std::vector<uint8_t> (bytes, bytes + data.length);
}

// The Audio Unit handed `state` as a host hands back what it kept: its own class info, with those bytes in it.
static OSStatus restore (Unit& u, const std::vector<uint8_t>& state, NSDictionary* changes = nil)
{
    NSMutableDictionary* info = [classInfo (u) mutableCopy];
    info[@kAUPresetDataKey] = [NSData dataWithBytes:state.data () length:state.size ()];
    if (changes) [info addEntriesFromDictionary:changes];
    CFPropertyListRef plist = (__bridge CFPropertyListRef) info;
    return set (u.unit, kAudioUnitProperty_ClassInfo, kAudioUnitScope_Global, plist);
}

static int32_t wordAt (const std::vector<uint8_t>& state, size_t index)
{
    int32_t word = 0;
    if (state.size () >= (index + 1) * 4) std::memcpy (&word, state.data () + index * 4, 4);
    return word;
}

// ── The view ──

static NSView* makeView (Unit& u)
{
    AudioUnitCocoaViewInfo info {};
    if (get (u.unit, kAudioUnitProperty_CocoaUI, kAudioUnitScope_Global, info) != noErr) return nil;
    NSURL* bundle = CFBridgingRelease (info.mCocoaAUViewBundleLocation);
    NSString* name = CFBridgingRelease (info.mCocoaAUViewClass[0]);
    // by name in the bundle, as GarageBand's and Logic's host process looks: a class made as the program runs is not found
    Class factory = [[NSBundle bundleWithURL:bundle] classNamed:name];
    if (![factory conformsToProtocol:@protocol (AUCocoaUIBase)]) return nil;
    id<AUCocoaUIBase> made = [factory new];
    return [made uiViewForAudioUnit:u.unit withSize:NSZeroSize];
}

static OffscreenWindow* show (NSView* view)
{
    // A host puts the view in a window inside a run loop pass, which has a pool.
    @autoreleasepool {
        OffscreenWindow* window = [[OffscreenWindow alloc] initWithContentRect:NSMakeRect (-8000, -6000, view.frame.size.width, view.frame.size.height)
                                                                     styleMask:NSWindowStyleMaskBorderless
                                                                       backing:NSBackingStoreBuffered
                                                                         defer:NO];
        window.releasedWhenClosed = NO;
        [window orderFront:nil];
        [window.contentView addSubview:view];
        paintsOffscreen (window.contentView);
        [window orderOut:nil];   // WebKit looks again at whether its page is seen when the window comes back
        [window orderFront:nil];
        return window;
    }
}

int main (int argc, char** argv)
{
    if (argc != 2 || !getenv ("SONIC_PI_PLUGIN_LOG") || !getenv ("SONIC_PI_PLUGIN_PROBE")) {
        fprintf (stderr, "usage: SONIC_PI_PLUGIN_LOG=<file> SONIC_PI_PLUGIN_PROBE=1 host_au <Sonic Pi.component>\n");
        return 2;
    }
    gLog = @(getenv ("SONIC_PI_PLUGIN_LOG"));
    [NSFileManager.defaultManager removeItemAtPath:gLog error:nil];

    NSApplication* app = NSApplication.sharedApplication;
    app.activationPolicy = NSApplicationActivationPolicyAccessory;
    [app finishLaunching];

    // ── The component, from the bundle's binary by the factory its Info.plist names ──
    NSBundle* bundle = [NSBundle bundleWithPath:@(argv[1])];
    NSDictionary* declared = [bundle.infoDictionary[@"AudioComponents"] firstObject];
    const auto code = [] (NSString* four) -> UInt32 {
        return four.length == 4 ? UInt32 ([four characterAtIndex:0]) << 24 | UInt32 ([four characterAtIndex:1]) << 16
                                | UInt32 ([four characterAtIndex:2]) << 8 | UInt32 ([four characterAtIndex:3]) : 0;
    };
    check ("the bundle says it is an instrument, with the codes a project names it by",
           code (declared[@"type"]) == kSonicPi.componentType && code (declared[@"subtype"]) == kSonicPi.componentSubType
           && code (declared[@"manufacturer"]) == kSonicPi.componentManufacturer,
           [NSString stringWithFormat:@"%@ %@ %@", declared[@"type"], declared[@"subtype"], declared[@"manufacturer"]]);
    NSString* factoryName = declared[@"factoryFunction"];
    void* module = dlopen (bundle.executablePath.fileSystemRepresentation, RTLD_NOW | RTLD_LOCAL);
    if (!module) { fprintf (stderr, "%s\n", dlerror ()); return 1; }
    auto factory = (AudioComponentFactoryFunction) (factoryName ? dlsym (module, factoryName.UTF8String) : nullptr);
    check ("the bundle's binary exports the factory its Info.plist names", factory != nullptr, factoryName);
    if (!factory) { printf ("%d of %d checks failed\n", gFailures, gChecks); return 1; }
    AudioComponent component = AudioComponentRegister (&kSonicPi, CFSTR ("Sonic Pi: Sonic Pi"), 0, factory);

    Unit a;
    check ("a host makes an Audio Unit from it", component && AudioComponentInstanceNew (component, &a.unit) == noErr && a.unit);
    if (!a.unit) { printf ("%d of %d checks failed\n", gFailures, gChecks); return 1; }

    // ── What it tells a host of itself ──
    {
        UInt32 ins = 0, outs = 0, instruments = 9, listBytes = 9;
        AudioStreamBasicDescription format {};
        AUChannelInfo channels {};
        Float64 latency = -1;
        Boolean writable = false;
        const bool asked = get (a.unit, kAudioUnitProperty_ElementCount, kAudioUnitScope_Input, ins) == noErr
                        && get (a.unit, kAudioUnitProperty_ElementCount, kAudioUnitScope_Output, outs) == noErr
                        && get (a.unit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, format) == noErr
                        && get (a.unit, kAudioUnitProperty_SupportedNumChannels, kAudioUnitScope_Global, channels) == noErr
                        && get (a.unit, kAudioUnitProperty_Latency, kAudioUnitScope_Global, latency) == noErr
                        && get (a.unit, kMusicDeviceProperty_InstrumentCount, kAudioUnitScope_Global, instruments) == noErr
                        && AudioUnitGetPropertyInfo (a.unit, kAudioUnitProperty_ParameterList, kAudioUnitScope_Global, 0, &listBytes, &writable) == noErr;
        check ("it has a stereo output and a stereo input, of floats, a buffer to a channel",
               asked && ins == 1 && outs == 1 && channels.inChannels == 2 && channels.outChannels == 2
               && format.mChannelsPerFrame == 2 && format.mFormatID == kAudioFormatLinearPCM && format.mBitsPerChannel == 32
               && (format.mFormatFlags & kAudioFormatFlagIsFloat) && (format.mFormatFlags & kAudioFormatFlagIsNonInterleaved),
               [NSString stringWithFormat:@"in %u out %u, %u channels", (unsigned) ins, (unsigned) outs, (unsigned) format.mChannelsPerFrame]);
        check ("it adds no latency and has no parameters", asked && latency == 0 && listBytes == 0 && instruments == 0);
        AudioStreamBasicDescription mono = format;
        mono.mChannelsPerFrame = 1;
        check ("it refuses a format it does not render",
               set (a.unit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, mono) == kAudioUnitErr_FormatNotSupported);
        UInt32 nothing = 0;
        check ("and a property it has not",
               get (a.unit, kAudioUnitProperty_BypassEffect, kAudioUnitScope_Global, nothing) == kAudioUnitErr_InvalidProperty);
        CFArrayRef outputs = nullptr;
        const bool named = get (a.unit, kAudioUnitProperty_MIDIOutputCallbackInfo, kAudioUnitScope_Global, outputs) == noErr && outputs;
        check ("it has one MIDI output", named && CFArrayGetCount (outputs) == 1);
        if (outputs) CFRelease (outputs);
    }

    // ── Starting, and the render ──
    check ("it does not render before the host has started it", renderOnce (a, kBlock, false) == kAudioUnitErr_Uninitialized);
    HostCallbackInfo callbacks {};
    callbacks.hostUserData = &a;
    callbacks.beatAndTempoProc = beatAndTempo;
    callbacks.musicalTimeLocationProc = musicalTime;
    callbacks.transportStateProc = transportState;
    const AUMIDIOutputCallbackStruct midiOut = { midiFromUnit, &a };
    const bool setUp = set (a.unit, kAudioUnitProperty_SampleRate, kAudioUnitScope_Output, Float64 (kRate)) == noErr
                    && set (a.unit, kAudioUnitProperty_SampleRate, kAudioUnitScope_Input, Float64 (kRate)) == noErr
                    && set (a.unit, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Global, kBlock) == noErr
                    && set (a.unit, kAudioUnitProperty_HostCallbacks, kAudioUnitScope_Global, callbacks) == noErr
                    && set (a.unit, kAudioUnitProperty_MIDIOutputCallback, kAudioUnitScope_Global, midiOut) == noErr;
    check ("it takes the host's rate, its buffer size and its callbacks", setUp);
    check ("the host starts it", AudioUnitInitialize (a.unit) == noErr);
    check ("the rate cannot be changed while it runs",
           set (a.unit, kAudioUnitProperty_SampleRate, kAudioUnitScope_Output, Float64 (44100)) == kAudioUnitErr_Initialized);
    check ("it refuses more frames than the host said it would ask for",
           renderOnce (a, kBlock * 2, false) == kAudioUnitErr_TooManyFramesToProcess);
    {
        float* left = nullptr;
        const OSStatus result = renderOnce (a, kBlock, true, &left);
        check ("a host that brings no buffers is handed the Audio Unit's, with the audio in them",
               result == noErr && left && std::all_of (left, left + kBlock, [] (float v) { return v == 0.f; }));
    }
    startRendering (a);
    check ("it is silent before anything runs", peakOver (a, 0.5) < 1e-6);

    // ── A project's state: the editor's size, the page's storage, and in that the buffers armed for the transport ──
    NSDictionary* info = classInfo (a);
    const std::vector<uint8_t> fresh = stateOf (a);
    check ("its state is a property list with the component's codes and the plugin's own bytes",
           [info[@kAUPresetTypeKey] unsignedIntValue] == kSonicPi.componentType && [info[@kAUPresetSubtypeKey] unsignedIntValue] == kSonicPi.componentSubType
           && [info[@kAUPresetManufacturerKey] unsignedIntValue] == kSonicPi.componentManufacturer
           && uint32_t (wordAt (fresh, 0)) == 0x6C705053u && wordAt (fresh, 2) == 1100 && wordAt (fresh, 3) == 760,
           [NSString stringWithFormat:@"%zu bytes, editor %dx%d", fresh.size (), wordAt (fresh, 2), wordAt (fresh, 3)]);
    // A buffer armed for the host's transport: a note out to the host, then a wait for a note from it, which it plays.
    const std::string armed = "[{\"buffer\":2,\"group\":1002,\"parent\":999,\"fade\":0.05,\"code\":"
                              "\"midi_note_on 64, 100, channel: 3\\nn, v = sync \\\"/midi:daw:5/note_on\\\"\\nplay n\\n\"}]";
    const std::vector<uint8_t> project = stateWith (1200, 800, 115, "sp-daw-armed", armed);
    a.transport.store (0);
    check ("it takes the state a project kept", restore (a, project) == noErr);
    check ("and gives the same back", stateOf (a) == project);
    check ("what is not a state of its own is refused, and changes nothing",
           restore (a, { 'n', 'o', 'n', 's', 'e', 'n', 's', 'e' }) == kAudioUnitErr_InvalidPropertyValue && stateOf (a) == project);
    check ("as is another Audio Unit's",
           restore (a, fresh, @{ @kAUPresetSubtypeKey: @(0x61626364) }) == kAudioUnitErr_InvalidPropertyValue && stateOf (a) == project);
    check ("with the host stopped, an armed buffer does not run", peakOver (a, 0.5) < 1e-6 && midiWithin (a, 1, 0).empty ());

    // ── The host's transport, and MIDI each way ──
    size_t from = heardSoFar (a);
    a.transport.store (1);
    const std::vector<MidiOut> sent = midiWithin (a, 1, 5);
    check ("the host's Play runs the armed buffer, with no editor open: its MIDI note reaches the host",
           sent.size () == 1 && sent[0].length == 3 && sent[0].bytes[0] == 0x92 && sent[0].bytes[1] == 64 && sent[0].bytes[2] == 100,
           [NSString stringWithFormat:@"%zu messages", sent.size ()]);
    check ("a program waiting on a note from the host is silent until one comes", peakOver (a, 0.5) < 1e-6);
    from = heardSoFar (a);
    {
        std::lock_guard<std::mutex> lk (a.mutex);
        a.midiToSend.push_back ({ 0x94, 72, 100, 100 });
    }
    check ("a note from the host wakes it, and it plays through the host's render", soundsWithin (a, from, 3));
    a.transport.store (0);
    pump (1.5);

    // ── The host's tempo ──
    // Sonic Pi's own tempo is 60: at the host's 120 a beat is half a second, 24,000 frames. Measured from the second
    // note: the first of an armed run goes out up to a buffer late, the word of the host's Play having just come.
    {
        a.bpm.store (120.0);
        const std::string beats = "[{\"buffer\":2,\"group\":1002,\"parent\":999,\"fade\":0.05,\"code\":"
                                  "\"4.times do\\n  midi_note_on 70, 100, channel: 1\\n  sleep 1\\nend\\n\"}]";
        const bool taken = restore (a, stateWith (1200, 800, 115, "sp-daw-armed", beats)) == noErr;
        const auto notes = [&] {
            std::vector<long long> at;
            std::lock_guard<std::mutex> lk (a.mutex);
            for (const MidiOut& m : a.midiReceived) if (m.length == 3 && (m.bytes[0] & 0xF0) == 0x90 && m.bytes[1] == 70) at.push_back (m.frame);
            return at;
        };
        a.transport.store (1);
        NSDate* until = [NSDate dateWithTimeIntervalSinceNow:5];
        while (notes ().size () < 4 && [until timeIntervalSinceNow] > 0) pump (0.05);
        const std::vector<long long> at = notes ();
        const long long first = at.size () == 4 ? at[2] - at[1] : -1, second = at.size () == 4 ? at[3] - at[2] : -1;
        check ("a program's beats are the host's: at 120 beats a minute, notes a beat apart reach the host half a second apart",
               taken && std::llabs (first - 24000) <= 1 && std::llabs (second - 24000) <= 1,
               [NSString stringWithFormat:@"%zu notes, %lld then %lld frames apart", at.size (), first, second]);
        a.transport.store (0);
        pump (1.0);
    }

    // ── The view ──
    NSView* view = makeView (a);
    check ("it has a Cocoa view, at the size the project kept", view && view.frame.size.width == 1200 && view.frame.size.height == 800,
           view ? NSStringFromSize (view.frame.size) : @"no view");
    if (!view) { printf ("%d of %d checks failed\n", gFailures, gChecks); return 1; }
    OffscreenWindow* window = show (view);
    const bool ready = waitFor (1, @"\"t\":\"ready\"", 30);
    check ("in a window, the view loads the page", ready && [lines (1, @"\"t\":\"ready\"").lastObject containsString:@"Sonic Pi"],
           lines (1, @"\"t\":\"ready\"").lastObject);
    for (NSString* line in [lines (1, @" 404 ") arrayByAddingObjectsFromArray:lines (1, @"page error")]) printf ("      %s\n", line.UTF8String);
    check ("the page found every file it asked for, and raised no error",
           ready && lines (1, @" 404 ").count == 0 && lines (1, @"page error").count == 0
           && lines (1, @"\"t\":\"error\"").count == 0 && lines (1, @"\"t\":\"rejection\"").count == 0);
    WKWebView* web = nil;
    for (NSView* child in view.subviews) if ([child isKindOfClass:WKWebView.class]) web = (WKWebView*) child;
    {
        __block NSString* shown = nil;
        __block bool done = false;
        [web evaluateJavaScript:@"document.getElementById('status-version')?.textContent + ' | ' + document.getElementById('about-v-target')?.textContent"
              completionHandler:^(id value, NSError*) { shown = [value isKindOfClass:NSString.class] ? value : nil; done = true; }];
        for (int i = 0; i < 300 && !done; ++i) pump (0.01);
        NSString* own = [NSBundle bundleWithPath:@(argv[1])].infoDictionary[@"CFBundleShortVersionString"];
        NSString* want = [NSString stringWithFormat:@"^Sonic Pi v[0-9]+\\.[0-9]+ AU( [A-Za-z0-9]+)? \\| AU %@$", [NSRegularExpression escapedPatternForString:own ?: @"?"]];
        check ("the page says it is the Audio Unit: 'Sonic Pi vX.Y AU', and 'AU' with the bundle's version in About",
               shown && [shown rangeOfString:want options:NSRegularExpressionSearch].location != NSNotFound, shown);
    }
    [view setFrameSize:NSMakeSize (1300, 850)];
    pump (0.3);
    check ("a host that resizes the view resizes the page, and the project keeps the size",
           web && web.frame.size.width == 1300 && web.frame.size.height == 850 && wordAt (stateOf (a), 2) == 1300 && wordAt (stateOf (a), 3) == 850,
           [NSString stringWithFormat:@"page %@, state %dx%d", web ? NSStringFromSize (web.frame.size) : @"none", wordAt (stateOf (a), 2), wordAt (stateOf (a), 3)]);
    @autoreleasepool { [view removeFromSuperview]; }
    pump (0.3);
    check ("a view the host takes out of its window closes the page", lines (1, @"detached").count == 1);
    @autoreleasepool { [window.contentView addSubview:view]; }
    check ("and one put back opens it again", waitFor (2, @"\"t\":\"ready\"", 30));

    // A host that closes the Audio Unit before its view: the view still has something to close.
    a.quit = true;
    a.render.join ();
    check ("the host stops and closes the Audio Unit with its view still open",
           AudioUnitUninitialize (a.unit) == noErr && AudioComponentInstanceDispose (a.unit) == noErr);
    pump (0.5);
    @autoreleasepool {
        [view removeFromSuperview];
        [window close];
    }
    pump (0.5);
    check ("and the view closes after it", lines (2, @"detached").count == 1);

    printf ("%d of %d checks failed\n", gFailures, gChecks);
    return gFailures ? 1 : 0;
}
