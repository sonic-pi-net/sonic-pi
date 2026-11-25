// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// Sonic Pi as an Audio Unit (version 2) instrument, for the hosts that take no VST3 (Logic, GarageBand): each
// instance its own Sonic Pi (plugin.h), rendered from the host's render call, with the web GUI as its Cocoa view
// (lib.rs). The same instrument as vst3/shell.cpp, to another host's API: a stereo output, a stereo input for
// live_audio, the host's MIDI in and out as Sonic Pi's port `daw`, the host's tempo and transport, and the same
// bytes for its state.
//
// The Audio Unit API is spoken directly (AudioComponentPlugInInterface: a table of functions a host looks up by
// selector), with no SDK between. Compiled without ARC.
#import <AppKit/AppKit.h>
#import <AudioToolbox/AudioToolbox.h>
#import <AudioUnit/AUCocoaUIView.h>
#import <CoreMIDI/CoreMIDI.h>
#import <objc/message.h>
#import <objc/runtime.h>
#include <pthread.h>

#include <atomic>
#include <chrono>
#include <cstdio>
#include <cstring>
#include <memory>
#include <mutex>
#include <new>
#include <string>
#include <vector>

#include "plugin.h"

// What a host makes the view with (AUCocoaUIBase): an object of the class the Audio Unit names, asked for a view.
// A class the compiler makes, where the view's own is made as the program runs: a host looks it up by name in the
// bundle (-[NSBundle classNamed:], as GarageBand's and Logic's host process does), which finds only a class that
// lies in the bundle's binary. So its name is one every copy of this binary shares, and with two in a process (the
// VST3's bundle too) the first loaded has the class: the Audio Unit names that copy's bundle to its host
// (kAudioUnitProperty_CocoaUI), and the class asks the Audio Unit how its view is made (MakeView).
@interface SonicPiAUViewFactory : NSObject <AUCocoaUIBase>
@end

extern "C" {
void* sp_view_attach (void* parent, unsigned width, unsigned height, SpInstance* instance, void* host);
void  sp_view_resize (void* view, unsigned width, unsigned height);
void  sp_view_detach (void* view);
void  sp_host_note (const char* text);
void  sp_log_enable ();   // filelog.rs: a host has loaded the plugin, so its lines go to the user's log folder
const char* sp_own_path ();
}

namespace {

// A property of our own, for the Cocoa view, which the host makes from the Audio Unit alone: the plugin the view
// is for (a std::shared_ptr<SpPlugin>*). The view and its Audio Unit are always in one process.
// How a view is made for an Audio Unit of this binary's: the answer to a property of our own, a function and what
// to call it with. A view factory may be another copy's of this binary (SonicPiAUViewFactory), and so asks the
// Audio Unit it was given, which answers with its own binary's function.
struct MakeView {
    NSView* (*make) (void* context);
    void* context;
};
const AudioUnitPropertyID kSonicPiProperty_MakeView = 0x53506D56;   // 'SPmV'

// The most frames a render asks for until the host says (kAudioUnitProperty_MaximumFramesPerSlice): the number
// Apple's own Audio Units start with.
const UInt32 kFirstMaxFrames = 1156;

// A value the render thread reads while another thread replaces it: the reader takes a pointer and never waits or
// frees. Each value published is kept until the Audio Unit closes, which a host's few changes make a short list.
template <typename T> class Published
{
public:
    ~Published () { for (T* value : mEvery) delete value; }
    const T* read () const { return mNow.load (std::memory_order_acquire); }
    T copy ()
    {
        std::lock_guard<std::mutex> lk (mMutex);
        const T* now = mNow.load (std::memory_order_acquire);
        return now ? *now : T {};
    }
    template <typename Change> void change (Change change)
    {
        std::lock_guard<std::mutex> lk (mMutex);
        const T* now = mNow.load (std::memory_order_acquire);
        T* next = new T (now ? *now : T {});
        change (*next);
        mEvery.push_back (next);
        mNow.store (next, std::memory_order_release);
    }
    void publish (const T& value) { change ([&] (T& next) { next = value; }); }

private:
    std::atomic<T*> mNow { nullptr };
    std::mutex mMutex;
    std::vector<T*> mEvery;
};

// The host's MIDI on its way to the next render. A host hands a message over from its render thread, before the
// render it is for, or from any other thread (a key clicked on its screen): so many writers, none of which waits,
// and the render thread reads. A queue of numbered slots (Vyukov's bounded queue).
class MidiIn
{
public:
    struct Message { uint8_t bytes[3]; uint8_t length; uint32_t frame; };

    MidiIn () { for (uint32_t i = 0; i < kSlots; ++i) mSlots[i].turn.store (i, std::memory_order_relaxed); }

    // False when the queue is full: the message is dropped.
    bool put (const Message& message)
    {
        uint32_t at = mTail.load (std::memory_order_relaxed);
        for (;;) {
            Slot& slot = mSlots[at % kSlots];
            const int32_t ahead = int32_t (slot.turn.load (std::memory_order_acquire) - at);
            if (ahead == 0) {
                if (mTail.compare_exchange_weak (at, at + 1, std::memory_order_relaxed)) {
                    slot.message = message;
                    slot.turn.store (at + 1, std::memory_order_release);
                    return true;
                }
            } else if (ahead < 0) {
                return false;
            } else {
                at = mTail.load (std::memory_order_relaxed);
            }
        }
    }

    // The render thread's alone.
    bool take (Message& message)
    {
        Slot& slot = mSlots[mHead % kSlots];
        if (slot.turn.load (std::memory_order_acquire) != mHead + 1) return false;
        message = slot.message;
        slot.turn.store (mHead + kSlots, std::memory_order_release);
        ++mHead;
        return true;
    }

private:
    static constexpr uint32_t kSlots = 256;
    struct Slot { std::atomic<uint32_t> turn; Message message; };
    Slot mSlots[kSlots];
    std::atomic<uint32_t> mTail { 0 };
    uint32_t mHead = 0;
};

// Where the input's audio comes from: a callback the host gave, or another Audio Unit's output. Neither: silence.
struct InputSource
{
    AURenderCallbackStruct callback {};
    AudioUnitConnection connection {};
};

struct RenderNotify
{
    AURenderCallback proc = nullptr;
    void* userData = nullptr;
};

struct PropertyListener
{
    AudioUnitPropertyID property = 0;
    AudioUnitPropertyListenerProc proc = nullptr;
    void* userData = nullptr;
};

AudioStreamBasicDescription stereoAt (double rate)
{
    AudioStreamBasicDescription format {};
    format.mSampleRate = rate;
    format.mFormatID = kAudioFormatLinearPCM;
    format.mFormatFlags = kAudioFormatFlagsNativeFloatPacked | kAudioFormatFlagIsNonInterleaved;
    format.mBytesPerPacket = sizeof (float);
    format.mFramesPerPacket = 1;
    format.mBytesPerFrame = sizeof (float);
    format.mChannelsPerFrame = 2;
    format.mBitsPerChannel = 32;
    return format;
}

// The one format the instrument renders: two channels of 32-bit floats, each in a buffer of its own, at any rate.
bool isOurs (const AudioStreamBasicDescription& format)
{
    const AudioStreamBasicDescription ours = stereoAt (format.mSampleRate);
    return format.mSampleRate > 0 && format.mFormatID == ours.mFormatID && format.mFormatFlags == ours.mFormatFlags
        && format.mBytesPerFrame == ours.mBytesPerFrame && format.mFramesPerPacket == ours.mFramesPerPacket
        && format.mChannelsPerFrame == ours.mChannelsPerFrame && format.mBitsPerChannel == ours.mBitsPerChannel;
}

struct SonicPiAU
{
    AudioComponentPlugInInterface interface {};   // first: the host's handle to the instance is a pointer to this
    AudioComponentInstance component = nullptr;
    // shared with an open view, so that a host that closes the Audio Unit first leaves the view something to close
    std::shared_ptr<SpPlugin> plugin = std::make_shared<SpPlugin> ("AU");

    std::atomic<bool> initialized { false };
    AudioStreamBasicDescription outFormat = stereoAt (44100);
    AudioStreamBasicDescription inFormat = stereoAt (44100);
    UInt32 maxFrames = kFirstMaxFrames;
    std::atomic<OSStatus> lastRenderError { noErr };
    CFStringRef presetName = CFSTR ("Untitled");   // a constant, or one we hold

    Published<InputSource> input;
    Published<HostCallbackInfo> host;
    Published<AUMIDIOutputCallbackStruct> midiOut;
    Published<std::vector<RenderNotify>> notifies;
    MidiIn midiIn;
    // What the host has asked for since it last initialized, for the log: renders, their frames, and those for a
    // sample time already rendered. lastSampleTime is the render thread's.
    std::atomic<uint64_t> renders { 0 }, framesRendered { 0 }, repeats { 0 };
    Float64 lastSampleTime = -1;
    std::chrono::steady_clock::time_point since;
    std::mutex listenersMutex;
    std::vector<PropertyListener> listeners;

    // the render thread's: buffers for a host that brings none of its own, and for the input it is asked for
    std::vector<float> outSpare[2], inSpare[2];

    ~SonicPiAU () { CFRelease (presetName); }
};

SonicPiAU* auOf (void* self) { return static_cast<SonicPiAU*> (self); }

void changed (SonicPiAU* au, AudioUnitPropertyID property, AudioUnitScope scope, AudioUnitElement element)
{
    std::vector<PropertyListener> listeners;
    {
        std::lock_guard<std::mutex> lk (au->listenersMutex);
        listeners = au->listeners;
    }
    for (const PropertyListener& l : listeners)
        if (l.property == property) l.proc (l.userData, au->component, property, scope, element);
}

// ── The state, as an Audio Unit keeps one: a property list with the plugin's bytes in it ──

AudioComponentDescription describe (SonicPiAU* au)
{
    AudioComponentDescription description {};
    if (AudioComponent component = AudioComponentInstanceGetComponent (au->component))
        AudioComponentGetDescription (component, &description);
    return description;
}

void setNumber (CFMutableDictionaryRef dict, CFStringRef key, SInt32 value)
{
    CFNumberRef number = CFNumberCreate (kCFAllocatorDefault, kCFNumberSInt32Type, &value);
    CFDictionarySetValue (dict, key, number);
    CFRelease (number);
}

bool numberIs (CFDictionaryRef dict, CFStringRef key, SInt32 expected)
{
    const void* found = CFDictionaryGetValue (dict, key);
    SInt32 value = 0;
    return found && CFGetTypeID (found) == CFNumberGetTypeID ()
        && CFNumberGetValue (static_cast<CFNumberRef> (found), kCFNumberSInt32Type, &value) && value == expected;
}

CFPropertyListRef copyClassInfo (SonicPiAU* au)
{
    const AudioComponentDescription description = describe (au);
    CFMutableDictionaryRef dict = CFDictionaryCreateMutable (kCFAllocatorDefault, 0, &kCFTypeDictionaryKeyCallBacks,
                                                             &kCFTypeDictionaryValueCallBacks);
    setNumber (dict, CFSTR (kAUPresetVersionKey), 0);
    setNumber (dict, CFSTR (kAUPresetTypeKey), SInt32 (description.componentType));
    setNumber (dict, CFSTR (kAUPresetSubtypeKey), SInt32 (description.componentSubType));
    setNumber (dict, CFSTR (kAUPresetManufacturerKey), SInt32 (description.componentManufacturer));
    const std::vector<uint8_t> state = au->plugin->writeState ();
    CFDataRef data = CFDataCreate (kCFAllocatorDefault, state.data (), CFIndex (state.size ()));
    CFDictionarySetValue (dict, CFSTR (kAUPresetDataKey), data);
    CFRelease (data);
    CFDictionarySetValue (dict, CFSTR (kAUPresetNameKey), au->presetName);
    return dict;
}

void adoptPresetName (SonicPiAU* au, CFStringRef name)
{
    if (!name || CFGetTypeID (name) != CFStringGetTypeID ()) return;
    CFRetain (name);
    CFRelease (au->presetName);
    au->presetName = name;
}

OSStatus restoreClassInfo (SonicPiAU* au, CFPropertyListRef plist)
{
    if (!plist || CFGetTypeID (plist) != CFDictionaryGetTypeID ()) return kAudioUnitErr_InvalidPropertyValue;
    CFDictionaryRef dict = static_cast<CFDictionaryRef> (plist);
    const AudioComponentDescription description = describe (au);
    // another Audio Unit's state is not ours
    if (!numberIs (dict, CFSTR (kAUPresetVersionKey), 0)
        || !numberIs (dict, CFSTR (kAUPresetTypeKey), SInt32 (description.componentType))
        || !numberIs (dict, CFSTR (kAUPresetSubtypeKey), SInt32 (description.componentSubType))
        || !numberIs (dict, CFSTR (kAUPresetManufacturerKey), SInt32 (description.componentManufacturer)))
        return kAudioUnitErr_InvalidPropertyValue;
    if (const void* found = CFDictionaryGetValue (dict, CFSTR (kAUPresetDataKey))) {
        if (CFGetTypeID (found) != CFDataGetTypeID ()) return kAudioUnitErr_InvalidPropertyValue;
        CFDataRef data = static_cast<CFDataRef> (found);
        const uint8_t* bytes = CFDataGetBytePtr (data);
        const size_t length = size_t (CFDataGetLength (data));
        size_t at = 0;
        const auto read = [&] (void* out, uint32_t wanted) {
            if (wanted > length - at) return false;
            std::memcpy (out, bytes + at, wanted);
            at += wanted;
            return true;
        };
        if (!au->plugin->readState (read)) return kAudioUnitErr_InvalidPropertyValue;
        // the open editor's page starts again on what came, on the main thread, which is where the page is
        if (pthread_main_np ()) {
            au->plugin->editorTakesState ();
        } else {
            std::shared_ptr<SpPlugin> plugin = au->plugin;
            dispatch_async (dispatch_get_main_queue (), ^{ plugin->editorTakesState (); });
        }
    }
    adoptPresetName (au, static_cast<CFStringRef> (CFDictionaryGetValue (dict, CFSTR (kAUPresetNameKey))));
    return noErr;
}

// ── The Cocoa view: the page, in a view the host puts in a window of its own ──
//
// Its classes are made as the plugin first needs them, each named with this binary's own address. A class written
// with @implementation has one name in every copy of the plugin, and a process holds one class to a name: with
// this Audio Unit and the VST3 both loaded (they are two bundles, each with the binary), or two versions of
// either, a host asking for the view by name would be handed whichever copy's class loaded first.

struct ViewState
{
    std::shared_ptr<SpPlugin> plugin;
    void* editor = nullptr;   // lib.rs's, while the view is in a window
};

Ivar gViewState = nullptr;

ViewState* stateOf (id view)
{
    return *reinterpret_cast<ViewState**> (reinterpret_cast<uint8_t*> (view) + ivar_getOffset (gViewState));
}

void toSuper (id self, SEL selector)
{
    objc_super super = { self, [NSView class] };
    reinterpret_cast<void (*) (objc_super*, SEL)> (objc_msgSendSuper) (&super, selector);
}

void closeEditor (ViewState* state)
{
    if (!state || !state->editor) return;
    state->plugin->setEditor (nullptr);
    sp_view_detach (state->editor);
    state->editor = nullptr;
}

// In a window: the page is made (wry needs its parent to be in one). The editor holds the view from here until it
// leaves its window, so nothing is left to close when the view is freed.
void viewDidMoveToWindow (id self, SEL selector)
{
    toSuper (self, selector);
    NSView* view = self;
    ViewState* state = stateOf (self);
    if (!state || state->editor || !view.window) return;
    const NSSize size = view.bounds.size;
    state->editor = sp_view_attach (self, unsigned (size.width), unsigned (size.height), state->plugin->instance (), state->plugin.get ());
    state->plugin->setEditor (state->editor);
}

// The host closed the editor (an Audio Unit's view is told no other way): the page goes while its view is still
// in the window.
void viewWillMoveToWindow (id self, SEL selector, NSWindow* window)
{
    if (!window) closeEditor (stateOf (self));
    objc_super super = { self, [NSView class] };
    reinterpret_cast<void (*) (objc_super*, SEL, NSWindow*)> (objc_msgSendSuper) (&super, selector, window);
}

void viewSetFrameSize (id self, SEL selector, NSSize size)
{
    objc_super super = { self, [NSView class] };
    reinterpret_cast<void (*) (objc_super*, SEL, NSSize)> (objc_msgSendSuper) (&super, selector, size);
    ViewState* state = stateOf (self);
    if (!state || !state->editor) return;
    sp_view_resize (state->editor, unsigned (size.width), unsigned (size.height));
    state->plugin->setEditorSize (int32_t (size.width), int32_t (size.height));
    // said, so the log shows who sized it: the person, or a host restoring a size of its own
    sp_host_note (("editor resized to " + std::to_string (int (size.width)) + "x" + std::to_string (int (size.height))).c_str ());
}

void viewDealloc (id self, SEL selector)
{
    ViewState* state = stateOf (self);
    closeEditor (state);
    delete state;
    toSuper (self, selector);
}

void nameOfClass (char* out, size_t bytes, const char* what)
{
    std::snprintf (out, bytes, "SonicPi%s_%p", what, static_cast<const void*> (&gViewState));
}

Class viewClass ()
{
    static Class made = [] {
        char name[64];
        nameOfClass (name, sizeof name, "AUView");
        Class c = objc_allocateClassPair ([NSView class], name, 0);
        class_addIvar (c, "state", sizeof (ViewState*), uint8_t (__builtin_ctzl (alignof (ViewState*))), "^v");
        class_addMethod (c, @selector (viewDidMoveToWindow), reinterpret_cast<IMP> (viewDidMoveToWindow), "v@:");
        class_addMethod (c, @selector (viewWillMoveToWindow:), reinterpret_cast<IMP> (viewWillMoveToWindow), "v@:@");
        class_addMethod (c, @selector (setFrameSize:), reinterpret_cast<IMP> (viewSetFrameSize), "v@:{CGSize=dd}");
        class_addMethod (c, @selector (dealloc), reinterpret_cast<IMP> (viewDealloc), "v@:");
        objc_registerClassPair (c);
        gViewState = class_getInstanceVariable (c, "state");
        return c;
    }();
    return made;
}

// The view of an Audio Unit of this binary's (MakeView): `context` its plugin.
NSView* makeView (void* context)
{
    auto* held = static_cast<std::shared_ptr<SpPlugin>*> (context);
    int32_t width = 0, height = 0;
    (*held)->editorSize (width, height);   // opened at the size it was left at
    Class c = viewClass ();
    NSView* view = [[c alloc] initWithFrame:NSMakeRect (0, 0, width, height)];
    *reinterpret_cast<ViewState**> (reinterpret_cast<uint8_t*> (view) + ivar_getOffset (gViewState)) = new ViewState { *held };
    // a host that lets its window be resized sizes the view with it
    view.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
    return [view autorelease];
}

// <bundle>, from <bundle>/Contents/MacOS/<binary>: where a host finds the view's class.
CFURLRef copyBundleURL ()
{
    std::string path = sp_own_path ();
    for (int up = 0; up < 3; ++up) {
        const auto slash = path.find_last_of ('/');
        if (slash == std::string::npos) return nullptr;
        path.resize (slash);
    }
    return CFURLCreateFromFileSystemRepresentation (kCFAllocatorDefault, reinterpret_cast<const UInt8*> (path.data ()), CFIndex (path.size ()), true);
}

// ── Properties ──

// Where a property is: on the Audio Unit as a whole, or on its one input or one output.
enum class At { Whole, Input, Bus, WholeOrBus, AnyScope };

OSStatus placed (At at, AudioUnitScope scope, AudioUnitElement element)
{
    const bool whole = scope == kAudioUnitScope_Global;
    const bool bus = scope == kAudioUnitScope_Input || scope == kAudioUnitScope_Output;
    const bool scopeFits = at == At::Whole ? whole
                         : at == At::Input ? scope == kAudioUnitScope_Input
                         : at == At::Bus ? bus
                         : at == At::WholeOrBus ? (whole || bus)
                         : true;
    if (!scopeFits) return kAudioUnitErr_InvalidScope;
    if (at == At::AnyScope && !whole && !bus) return noErr;
    return element == 0 ? noErr : kAudioUnitErr_InvalidElement;
}

// A property's size and whether a host may set it; an error for one the instrument has not, or not there.
OSStatus propertyInfo (AudioUnitPropertyID property, AudioUnitScope scope, AudioUnitElement element, UInt32& size, bool& writable)
{
    At at = At::Whole;
    writable = false;
    switch (property) {
        case kAudioUnitProperty_ClassInfo:              size = sizeof (CFPropertyListRef); writable = true; break;
        case kAudioUnitProperty_MakeConnection:         size = sizeof (AudioUnitConnection); writable = true; at = At::Input; break;
        case kAudioUnitProperty_SetRenderCallback:      size = sizeof (AURenderCallbackStruct); writable = true; at = At::Input; break;
        case kAudioUnitProperty_SampleRate:             size = sizeof (Float64); writable = true; at = At::WholeOrBus; break;
        case kAudioUnitProperty_StreamFormat:           size = sizeof (AudioStreamBasicDescription); writable = true; at = At::WholeOrBus; break;
        case kAudioUnitProperty_ElementCount:           size = sizeof (UInt32); at = At::WholeOrBus; break;
        case kAudioUnitProperty_Latency:                size = sizeof (Float64); break;
        case kAudioUnitProperty_TailTime:               size = sizeof (Float64); break;
        case kAudioUnitProperty_SupportedNumChannels:   size = sizeof (AUChannelInfo); break;
        case kAudioUnitProperty_MaximumFramesPerSlice:  size = sizeof (UInt32); writable = true; break;
        case kAudioUnitProperty_LastRenderError:        size = sizeof (OSStatus); break;
        case kAudioUnitProperty_HostCallbacks:          size = sizeof (HostCallbackInfo); writable = true; break;
        case kAudioUnitProperty_ParameterList:          size = 0; at = At::AnyScope; break;
        case kAudioUnitProperty_PresentPreset:          size = sizeof (AUPreset); writable = true; break;
        case kAudioUnitProperty_CocoaUI:                size = sizeof (AudioUnitCocoaViewInfo); break;
        case kAudioUnitProperty_MIDIOutputCallbackInfo: size = sizeof (CFArrayRef); break;
        case kAudioUnitProperty_MIDIOutputCallback:     size = sizeof (AUMIDIOutputCallbackStruct); writable = true; break;
        case kMusicDeviceProperty_InstrumentCount:      size = sizeof (UInt32); break;
        case kSonicPiProperty_MakeView:                 size = sizeof (MakeView); break;
        default: return kAudioUnitErr_InvalidProperty;
    }
    return placed (at, scope, element);
}

OSStatus getPropertyInfo (void* self, AudioUnitPropertyID property, AudioUnitScope scope, AudioUnitElement element,
                          UInt32* outSize, Boolean* outWritable)
{
    (void) self;
    UInt32 size = 0;
    bool writable = false;
    const OSStatus result = propertyInfo (property, scope, element, size, writable);
    if (result != noErr) return result;
    if (outSize) *outSize = size;
    if (outWritable) *outWritable = writable;
    return noErr;
}

AudioStreamBasicDescription& formatAt (SonicPiAU* au, AudioUnitScope scope)
{
    return scope == kAudioUnitScope_Input ? au->inFormat : au->outFormat;   // the whole's is the output's
}

OSStatus getProperty (void* self, AudioUnitPropertyID property, AudioUnitScope scope, AudioUnitElement element,
                      void* out, UInt32* ioSize)
{
    SonicPiAU* au = auOf (self);
    UInt32 size = 0;
    bool writable = false;
    const OSStatus result = propertyInfo (property, scope, element, size, writable);
    if (result != noErr) return result;
    if (!ioSize) return kAudio_ParamError;
    if (!out) { *ioSize = size; return noErr; }   // asked only how large it is
    if (*ioSize < size) return kAudio_ParamError;
    *ioSize = size;
    switch (property) {
        case kAudioUnitProperty_ClassInfo:
            *static_cast<CFPropertyListRef*> (out) = copyClassInfo (au);   // the host's to release
            return noErr;
        case kAudioUnitProperty_SampleRate:
            *static_cast<Float64*> (out) = formatAt (au, scope).mSampleRate;
            return noErr;
        case kAudioUnitProperty_StreamFormat:
            *static_cast<AudioStreamBasicDescription*> (out) = formatAt (au, scope);
            return noErr;
        case kAudioUnitProperty_ElementCount:
            *static_cast<UInt32*> (out) = 1;
            return noErr;
        case kAudioUnitProperty_Latency:
        case kAudioUnitProperty_TailTime:
            *static_cast<Float64*> (out) = 0.0;
            return noErr;
        case kAudioUnitProperty_SupportedNumChannels:
            *static_cast<AUChannelInfo*> (out) = { 2, 2 };
            return noErr;
        case kAudioUnitProperty_MaximumFramesPerSlice:
            *static_cast<UInt32*> (out) = au->maxFrames;
            return noErr;
        case kAudioUnitProperty_LastRenderError:
            *static_cast<OSStatus*> (out) = au->lastRenderError.exchange (noErr);
            return noErr;
        case kAudioUnitProperty_HostCallbacks:
            *static_cast<HostCallbackInfo*> (out) = au->host.copy ();
            return noErr;
        case kAudioUnitProperty_ParameterList:
            return noErr;   // it has none: what a program does is the program's to say
        case kAudioUnitProperty_PresentPreset:
            CFRetain (au->presetName);   // the host's to release
            *static_cast<AUPreset*> (out) = { -1, au->presetName };
            return noErr;
        case kAudioUnitProperty_CocoaUI: {
            // the bundle whose binary has the class by its name: this one's, or the copy's loaded before it
            NSURL* owner = [NSBundle bundleForClass:NSClassFromString (@"SonicPiAUViewFactory")].bundleURL;
            CFURLRef bundle = owner ? CFURLRef (CFRetain ((CFURLRef) owner)) : copyBundleURL ();
            if (!bundle) return kAudioUnitErr_InvalidProperty;
            // both the host's to release
            *static_cast<AudioUnitCocoaViewInfo*> (out) = { bundle, { CFStringCreateWithCString (kCFAllocatorDefault, class_getName ([SonicPiAUViewFactory class]), kCFStringEncodingUTF8) } };
            return noErr;
        }
        case kAudioUnitProperty_MIDIOutputCallbackInfo: {
            // one output, Sonic Pi's port `daw`; the host's to release
            CFStringRef name = CFSTR ("MIDI Out");
            *static_cast<CFArrayRef*> (out) = CFArrayCreate (kCFAllocatorDefault, reinterpret_cast<const void**> (&name), 1, &kCFTypeArrayCallBacks);
            return noErr;
        }
        case kMusicDeviceProperty_InstrumentCount:
            *static_cast<UInt32*> (out) = 0;
            return noErr;
        case kSonicPiProperty_MakeView:
            *static_cast<MakeView*> (out) = { makeView, &au->plugin };
            return noErr;
        default:
            return kAudioUnitErr_InvalidProperty;   // one a host sets and does not read
    }
}

OSStatus setProperty (void* self, AudioUnitPropertyID property, AudioUnitScope scope, AudioUnitElement element,
                      const void* in, UInt32 inSize)
{
    SonicPiAU* au = auOf (self);
    UInt32 size = 0;
    bool writable = false;
    const OSStatus result = propertyInfo (property, scope, element, size, writable);
    if (result != noErr) return result;
    if (!writable) return kAudioUnitErr_PropertyNotWritable;
    // a host built against an older header gives fewer of the host callbacks; every other is its whole size
    const UInt32 least = property == kAudioUnitProperty_HostCallbacks ? UInt32 (sizeof (void*)) : size;
    if (!in || inSize < least) return kAudioUnitErr_InvalidPropertyValue;
    const bool running = au->initialized.load ();
    switch (property) {
        case kAudioUnitProperty_ClassInfo: {
            const OSStatus restored = restoreClassInfo (au, *static_cast<const CFPropertyListRef*> (in));
            if (restored == noErr) changed (au, kAudioUnitProperty_PresentPreset, kAudioUnitScope_Global, 0);
            return restored;
        }
        case kAudioUnitProperty_MakeConnection: {
            const AudioUnitConnection connection = *static_cast<const AudioUnitConnection*> (in);
            au->input.publish (InputSource { {}, connection });   // in place of a callback, as a callback is in place of it
            changed (au, property, scope, element);
            return noErr;
        }
        case kAudioUnitProperty_SetRenderCallback:
            au->input.publish (InputSource { *static_cast<const AURenderCallbackStruct*> (in), {} });
            changed (au, property, scope, element);
            return noErr;
        case kAudioUnitProperty_SampleRate: {
            const Float64 rate = *static_cast<const Float64*> (in);
            if (rate == formatAt (au, scope).mSampleRate) return noErr;
            if (running) return kAudioUnitErr_Initialized;
            if (!(rate > 0)) return kAudioUnitErr_InvalidPropertyValue;
            formatAt (au, scope).mSampleRate = rate;
            changed (au, property, scope, element);
            changed (au, kAudioUnitProperty_StreamFormat, scope, element);
            return noErr;
        }
        case kAudioUnitProperty_StreamFormat: {
            const AudioStreamBasicDescription format = *static_cast<const AudioStreamBasicDescription*> (in);
            if (!isOurs (format)) return kAudioUnitErr_FormatNotSupported;
            if (format.mSampleRate == formatAt (au, scope).mSampleRate) return noErr;
            if (running) return kAudioUnitErr_Initialized;
            formatAt (au, scope) = stereoAt (format.mSampleRate);
            changed (au, property, scope, element);
            return noErr;
        }
        case kAudioUnitProperty_MaximumFramesPerSlice: {
            const UInt32 frames = *static_cast<const UInt32*> (in);
            if (frames == au->maxFrames) return noErr;
            if (running) return kAudioUnitErr_Initialized;
            if (frames == 0) return kAudioUnitErr_InvalidPropertyValue;
            au->maxFrames = frames;
            changed (au, property, scope, element);
            return noErr;
        }
        case kAudioUnitProperty_HostCallbacks: {
            HostCallbackInfo callbacks {};
            std::memcpy (&callbacks, in, inSize < sizeof callbacks ? inSize : sizeof callbacks);
            au->host.publish (callbacks);
            return noErr;
        }
        case kAudioUnitProperty_PresentPreset: {
            const AUPreset preset = *static_cast<const AUPreset*> (in);
            if (preset.presetNumber >= 0) return kAudioUnitErr_InvalidPropertyValue;   // a factory preset: it has none
            adoptPresetName (au, preset.presetName);
            changed (au, property, scope, element);
            return noErr;
        }
        case kAudioUnitProperty_MIDIOutputCallback:
            au->midiOut.publish (*static_cast<const AUMIDIOutputCallbackStruct*> (in));
            return noErr;
        default:
            return kAudioUnitErr_PropertyNotWritable;
    }
}

OSStatus addPropertyListener (void* self, AudioUnitPropertyID property, AudioUnitPropertyListenerProc proc, void* userData)
{
    if (!proc) return kAudio_ParamError;
    SonicPiAU* au = auOf (self);
    std::lock_guard<std::mutex> lk (au->listenersMutex);
    au->listeners.push_back ({ property, proc, userData });
    return noErr;
}

void removeListeners (SonicPiAU* au, AudioUnitPropertyID property, AudioUnitPropertyListenerProc proc, const void* userData, bool anyUserData)
{
    std::lock_guard<std::mutex> lk (au->listenersMutex);
    auto& all = au->listeners;
    for (size_t i = all.size (); i-- > 0;)
        if (all[i].property == property && all[i].proc == proc && (anyUserData || all[i].userData == userData))
            all.erase (all.begin () + long (i));
}

OSStatus removePropertyListener (void* self, AudioUnitPropertyID property, AudioUnitPropertyListenerProc proc)
{
    removeListeners (auOf (self), property, proc, nullptr, true);
    return noErr;
}

OSStatus removePropertyListenerWithUserData (void* self, AudioUnitPropertyID property, AudioUnitPropertyListenerProc proc, void* userData)
{
    removeListeners (auOf (self), property, proc, userData, false);
    return noErr;
}

OSStatus addRenderNotify (void* self, AURenderCallback proc, void* userData)
{
    if (!proc) return kAudio_ParamError;
    auOf (self)->notifies.change ([&] (std::vector<RenderNotify>& all) { all.push_back ({ proc, userData }); });
    return noErr;
}

OSStatus removeRenderNotify (void* self, AURenderCallback proc, void* userData)
{
    auOf (self)->notifies.change ([&] (std::vector<RenderNotify>& all) {
        for (size_t i = all.size (); i-- > 0;)
            if (all[i].proc == proc && all[i].userData == userData) all.erase (all.begin () + long (i));
    });
    return noErr;
}

// It has no parameters: a host automates nothing of a program.
OSStatus getParameter (void*, AudioUnitParameterID, AudioUnitScope, AudioUnitElement, AudioUnitParameterValue*)
{
    return kAudioUnitErr_InvalidParameter;
}
OSStatus setParameter (void*, AudioUnitParameterID, AudioUnitScope, AudioUnitElement, AudioUnitParameterValue, UInt32)
{
    return kAudioUnitErr_InvalidParameter;
}
OSStatus scheduleParameters (void*, const AudioUnitParameterEvent*, UInt32 count)
{
    return count == 0 ? noErr : kAudioUnitErr_InvalidParameter;
}

// ── Starting, and rendering ──

// The instance starts the first time, at the output's rate, and keeps its runs when the host stops it to change
// the rate or the buffer and starts it again.
OSStatus initialize (void* self)
{
    SonicPiAU* au = auOf (self);
    if (au->initialized.load ()) return noErr;
    std::string why;
    if (!au->plugin->start (au->outFormat.mSampleRate, au->maxFrames, why)) return kAudioUnitErr_FailedInitialization;
    for (auto* spare : { &au->outSpare[0], &au->outSpare[1], &au->inSpare[0], &au->inSpare[1] }) spare->assign (au->maxFrames, 0.f);
    au->renders = 0; au->framesRendered = 0; au->repeats = 0;
    au->since = std::chrono::steady_clock::now ();
    char note[160];
    std::snprintf (note, sizeof note, "Audio Unit: %.0f Hz out, %.0f Hz in, up to %u frames", au->outFormat.mSampleRate, au->inFormat.mSampleRate, unsigned (au->maxFrames));
    sp_host_note (note);
    au->initialized.store (true);
    return noErr;
}

OSStatus uninitialize (void* self)
{
    SonicPiAU* au = auOf (self);
    if (!au->initialized.exchange (false)) return noErr;
    const double seconds = std::chrono::duration<double> (std::chrono::steady_clock::now () - au->since).count ();
    char note[200];
    std::snprintf (note, sizeof note, "Audio Unit: %llu renders of %llu frames in %.1f s (%.0f frames a second), %llu for a time already rendered",
                   (unsigned long long) au->renders.load (), (unsigned long long) au->framesRendered.load (), seconds,
                   seconds > 0 ? double (au->framesRendered.load ()) / seconds : 0.0, (unsigned long long) au->repeats.load ());
    sp_host_note (note);
    return noErr;
}

OSStatus reset (void*, AudioUnitScope, AudioUnitElement) { return noErr; }

// The host's transport at this render's first frame, from the callbacks it gave: its tempo, its time signature,
// whether it plays, and where it is in its bar. What a host does not say is left unsaid (instance.h).
SpTransport transportOf (SonicPiAU* au)
{
    SpTransport transport {};
    transport.beat_in_bar = -1.0;
    transport.playing = -1;
    const HostCallbackInfo* host = au->host.read ();
    if (!host) return transport;
    Float64 beat = 0, tempo = 0;
    const bool hasBeat = host->beatAndTempoProc && host->beatAndTempoProc (host->hostUserData, &beat, &tempo) == noErr;
    if (hasBeat && tempo > 0) transport.bpm = tempo;
    UInt32 toNextBeat = 0, denominator = 0;
    Float32 numerator = 0;
    Float64 barStart = 0;
    const bool hasBar = host->musicalTimeLocationProc
                     && host->musicalTimeLocationProc (host->hostUserData, &toNextBeat, &numerator, &denominator, &barStart) == noErr;
    if (hasBar) {
        transport.meter_num = int32_t (numerator);
        transport.meter_den = int32_t (denominator);
    }
    Boolean playing = false, stateChanged = false, cycling = false;
    Float64 sample = 0, cycleStart = 0, cycleEnd = 0;
    if (host->transportStateProc
        && host->transportStateProc (host->hostUserData, &playing, &stateChanged, &sample, &cycling, &cycleStart, &cycleEnd) == noErr)
        transport.playing = playing ? 1 : 0;
    if (hasBeat) {
        const double fromBar = hasBar ? beat - barStart : beat;
        // a position before the project's start counts back from its first bar line
        transport.beat_in_bar = fromBar >= 0.0 ? fromBar : 0.0;
    }
    return transport;
}

// The input's audio for this render, when the host has connected one: what live_audio plays. False with none.
bool pullInput (SonicPiAU* au, const AudioTimeStamp* when, UInt32 frames, const float* channels[2])
{
    const InputSource* source = au->input.read ();
    if (!source || (!source->callback.inputProc && !source->connection.sourceAudioUnit)) return false;
    struct { AudioBufferList list; AudioBuffer second; } two;
    two.list.mNumberBuffers = 2;
    for (int c = 0; c < 2; ++c)
        two.list.mBuffers[c] = { 1, UInt32 (frames * sizeof (float)), au->inSpare[c].data () };
    AudioUnitRenderActionFlags flags = 0;
    const OSStatus result = source->callback.inputProc
        ? source->callback.inputProc (source->callback.inputProcRefCon, &flags, when, 0, frames, &two.list)
        : AudioUnitRender (source->connection.sourceAudioUnit, &flags, when, source->connection.sourceOutputNumber, frames, &two.list);
    if (result != noErr) return false;
    // the source may have answered with buffers of its own
    for (int c = 0; c < 2; ++c) {
        if (!two.list.mBuffers[c].mData || two.list.mBuffers[c].mDataByteSize < frames * sizeof (float)) return false;
        channels[c] = static_cast<const float*> (two.list.mBuffers[c].mData);
    }
    return true;
}

// What a program sent to `daw` that came due in this render, to the host, each at its frame.
void sendMidiOut (SonicPiAU* au, SpInstance* instance, const AudioTimeStamp* when)
{
    const AUMIDIOutputCallbackStruct* to = au->midiOut.read ();
    SpMidi sent[64];
    // Taken whether or not the host has anywhere to put it, so that nothing waits to come out late.
    for (uint32_t n; (n = sp_instance_midi_out (instance, sent, 64)) > 0;) {
        if (!to || !to->midiOutputCallback) continue;
        // a packet list of our own making: a time (its frame in this render), a length and the bytes, one after another
        // (16 bytes a message, with the padding between packets, and the count before them)
        alignas (MIDIPacketList) uint8_t bytes[sizeof (MIDIPacketList) + 64 * 16];
        MIDIPacketList* list = reinterpret_cast<MIDIPacketList*> (bytes);
        list->numPackets = 0;
        MIDIPacket* packet = &list->packet[0];
        for (uint32_t i = 0; i < n; ++i) {
            if (sent[i].length == 0 || sent[i].length > 3) continue;
            packet->timeStamp = sent[i].frame;
            packet->length = sent[i].length;
            std::memcpy (packet->data, sent[i].bytes, sent[i].length);
            ++list->numPackets;
            packet = MIDIPacketNext (packet);
        }
        if (list->numPackets) to->midiOutputCallback (to->userData, when, 0, list);
    }
}

void tellRenderNotifies (SonicPiAU* au, AudioUnitRenderActionFlags flags, const AudioTimeStamp* when, UInt32 bus, UInt32 frames, AudioBufferList* data)
{
    const std::vector<RenderNotify>* all = au->notifies.read ();
    if (!all) return;
    for (const RenderNotify& notify : *all) {
        AudioUnitRenderActionFlags said = flags;
        notify.proc (notify.userData, &said, when, bus, frames, data);
    }
}

OSStatus renderInto (SonicPiAU* au, const AudioTimeStamp* when, UInt32 bus, UInt32 frames, AudioBufferList* data)
{
    if (!au->initialized.load ()) return kAudioUnitErr_Uninitialized;
    if (bus != 0) return kAudioUnitErr_InvalidElement;
    if (frames > au->maxFrames) return kAudioUnitErr_TooManyFramesToProcess;
    if (!when || !data || data->mNumberBuffers != 2) return kAudio_ParamError;
    au->renders.fetch_add (1, std::memory_order_relaxed);
    au->framesRendered.fetch_add (frames, std::memory_order_relaxed);
    if ((when->mFlags & kAudioTimeStampSampleTimeValid) && when->mSampleTime == au->lastSampleTime) au->repeats.fetch_add (1, std::memory_order_relaxed);
    au->lastSampleTime = when->mSampleTime;
    SpInstance* instance = au->plugin->instance ();
    float* out[2] = {};
    for (int c = 0; c < 2; ++c) {
        AudioBuffer& buffer = data->mBuffers[c];
        // a host that brings no buffer is handed ours
        if (!buffer.mData) buffer.mData = au->outSpare[c].data ();
        else if (buffer.mDataByteSize < frames * sizeof (float)) return kAudio_ParamError;
        buffer.mDataByteSize = UInt32 (frames * sizeof (float));
        out[c] = static_cast<float*> (buffer.mData);
    }
    const SpTransport transport = transportOf (au);
    sp_instance_transport (instance, &transport);
    // The host's notes, each for its frame of this render: cues at "/midi:daw:<channel>/note_on".
    for (MidiIn::Message message; au->midiIn.take (message);) {
        const uint32_t frame = frames == 0 ? 0 : message.frame >= frames ? frames - 1 : message.frame;
        sp_instance_midi_in (instance, message.bytes, message.length, frame);
    }
    const float* in[2] = {};
    const bool hasInput = pullInput (au, when, frames, in);
    sp_instance_render (instance, out, 2, hasInput ? in : nullptr, hasInput ? 2 : 0, frames);
    sendMidiOut (au, instance, when);
    return noErr;
}

OSStatus render (void* self, AudioUnitRenderActionFlags* ioFlags, const AudioTimeStamp* when, UInt32 bus, UInt32 frames, AudioBufferList* data)
{
    SonicPiAU* au = auOf (self);
    const AudioUnitRenderActionFlags flags = ioFlags ? *ioFlags : 0;
    tellRenderNotifies (au, flags | kAudioUnitRenderAction_PreRender, when, bus, frames, data);
    const OSStatus result = renderInto (au, when, bus, frames, data);
    if (result != noErr) au->lastRenderError.store (result);
    tellRenderNotifies (au, flags | kAudioUnitRenderAction_PostRender | (result != noErr ? kAudioUnitRenderAction_PostRenderError : 0),
                        when, bus, frames, data);
    return result;
}

// ── The host's MIDI ──

// One channel message, for `frame` of the next render. Anything else (system messages) is not carried.
OSStatus midiEvent (void* self, UInt32 status, UInt32 data1, UInt32 data2, UInt32 frame)
{
    const uint8_t kind = uint8_t (status & 0xF0);
    if (kind < 0x80 || kind > 0xE0) return noErr;
    MidiIn::Message message {};
    message.bytes[0] = uint8_t (status);
    message.bytes[1] = uint8_t (data1 & 0x7F);
    message.bytes[2] = uint8_t (data2 & 0x7F);
    message.length = (kind == 0xC0 || kind == 0xD0) ? 2 : 3;   // a program change and a channel's pressure are two bytes
    message.frame = frame;
    auOf (self)->midiIn.put (message);
    return noErr;
}

OSStatus sysEx (void*, const UInt8*, UInt32) { return noErr; }

// A host that plays notes without MIDI: a note on, on the channel its group names.
OSStatus startNote (void* self, MusicDeviceInstrumentID, MusicDeviceGroupID group, NoteInstanceID* outNote, UInt32 frame,
                    const MusicDeviceNoteParams* params)
{
    if (!params || params->argCount < 2) return kAudio_ParamError;
    const UInt32 pitch = UInt32 (params->mPitch) & 0x7F;
    const UInt32 velocity = params->mVelocity < 1 ? 1 : params->mVelocity > 127 ? 127 : UInt32 (params->mVelocity);
    if (outNote) *outNote = pitch;
    return midiEvent (self, 0x90 | (group & 0x0F), pitch, velocity, frame);
}

OSStatus stopNote (void* self, MusicDeviceGroupID group, NoteInstanceID note, UInt32 frame)
{
    return midiEvent (self, 0x80 | (group & 0x0F), note & 0x7F, 0, frame);
}

// ── The component ──

OSStatus open (void* self, AudioComponentInstance instance)
{
    auOf (self)->component = instance;
    sp_log_enable ();
    return noErr;
}

OSStatus close (void* self)
{
    delete auOf (self);
    return noErr;
}

AudioComponentMethod lookup (SInt16 selector)
{
    const auto method = [] (auto function) { return reinterpret_cast<AudioComponentMethod> (function); };
    switch (selector) {
        case kAudioUnitInitializeSelect:                         return method (initialize);
        case kAudioUnitUninitializeSelect:                       return method (uninitialize);
        case kAudioUnitGetPropertyInfoSelect:                    return method (getPropertyInfo);
        case kAudioUnitGetPropertySelect:                        return method (getProperty);
        case kAudioUnitSetPropertySelect:                        return method (setProperty);
        case kAudioUnitAddPropertyListenerSelect:                return method (addPropertyListener);
        case kAudioUnitRemovePropertyListenerSelect:             return method (removePropertyListener);
        case kAudioUnitRemovePropertyListenerWithUserDataSelect: return method (removePropertyListenerWithUserData);
        case kAudioUnitAddRenderNotifySelect:                    return method (addRenderNotify);
        case kAudioUnitRemoveRenderNotifySelect:                 return method (removeRenderNotify);
        case kAudioUnitGetParameterSelect:                       return method (getParameter);
        case kAudioUnitSetParameterSelect:                       return method (setParameter);
        case kAudioUnitScheduleParametersSelect:                 return method (scheduleParameters);
        case kAudioUnitRenderSelect:                             return method (render);
        case kAudioUnitResetSelect:                              return method (reset);
        case kMusicDeviceMIDIEventSelect:                        return method (midiEvent);
        case kMusicDeviceSysExSelect:                            return method (sysEx);
        case kMusicDeviceStartNoteSelect:                        return method (startNote);
        case kMusicDeviceStopNoteSelect:                         return method (stopNote);
        default:                                                 return nullptr;
    }
}

} // namespace

@implementation SonicPiAUViewFactory
- (unsigned) interfaceVersion { return 0; }
- (NSString*) description { return @"Sonic Pi"; }
- (NSView*) uiViewForAudioUnit:(AudioUnit) unit withSize:(NSSize) size
{
    MakeView how {};
    UInt32 bytes = sizeof how;
    if (AudioUnitGetProperty (unit, kSonicPiProperty_MakeView, kAudioUnitScope_Global, 0, &how, &bytes) != noErr || !how.make) return nil;
    return how.make (how.context);
}
@end

// What the bundle's Info.plist names as its factory (lib.rs exports it as SonicPiAUFactory): a new Audio Unit, not
// yet opened. Null when there is no memory for one.
extern "C" void* sp_au_factory (const AudioComponentDescription*)
{
    SonicPiAU* au = new (std::nothrow) SonicPiAU ();
    if (!au) return nullptr;
    au->interface.Open = open;
    au->interface.Close = close;
    au->interface.Lookup = lookup;
    au->interface.reserved = nullptr;
    return au;
}
