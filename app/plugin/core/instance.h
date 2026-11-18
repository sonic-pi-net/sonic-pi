// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// One Sonic Pi inside a plugin: an engine the host renders (SuperSonic's
// scsynth on Clockwork, attached rather than booted on a device), the front
// that loads samples and synthdefs for it, and the language runtime, driven
// as the desktop app drives it. Nothing here is shared with another
// instance in the process: each has its own engine, front, runtime and
// threads.
//
// Threads: render() is the host's audio thread's alone. The rest is any
// thread but that one: new() and free() block (the runtime boots, the
// threads end), the verbs return at once. What the instance says arrives on
// its own threads through the listener.
#pragma once
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct SpInstance SpInstance;

typedef struct SpInstanceConfig {
    uint32_t    struct_bytes;   /* sizeof(SpInstanceConfig) */
    double      sample_rate;
    uint32_t    max_frames;     /* the most frames one render will ask for */
    const char* resources;      /* synthdefs/, samples/, buffers/ (the bundle's Resources) */
    double      time_origin_ntp; /* Time is an origin + frames rendered / sample_rate, live and offline, and no
                                  * clock is read after the start. 0: live. The origin is the wall clock as the
                                  * instance starts, and the Sonic Pi code runs on a thread of its own, woken as
                                  * the audio reaches its time. Non-zero: offline, from this origin. Each render
                                  * first runs the Sonic Pi code due up to the end of its buffer. Render may then
                                  * block, so it is for offline rendering and tests, not live audio. */
} SpInstanceConfig;

/* NULL, with the reason in `why`, when it could not start. */
SpInstance* sp_instance_new(const SpInstanceConfig* config, char* why, uint32_t why_bytes);
void        sp_instance_free(SpInstance* instance);

/* How many synthdefs and samples the instance has asked for that have neither loaded nor failed. Any thread. */
uint32_t sp_instance_loading(SpInstance* instance);

/* The host's rate or largest buffer changed: not while render runs. 0 when the engine took it. */
int sp_instance_reconfigure(SpInstance* instance, double sample_rate, uint32_t max_frames);

/* `frames` frames into out[0..out_channels), from in[0..in_channels). The audio thread's. */
uint32_t sp_instance_render(SpInstance* instance, float* const* out, uint32_t out_channels,
                            const float* const* in, uint32_t in_channels, uint32_t frames);

/* The host's own transport at the next render's first frame, said before it on the audio thread. The instance's
 * tempo follows the host's, and (sp_instance_align_bars, on unless turned off) while the host plays its bars sit on
 * the host's, so a program's `link` waits for the host's bar line. */
typedef struct SpTransport {
    double  bpm;          /* its tempo; 0: it does not say */
    double  beat_in_bar;  /* where it is in its bar, in quarter notes; below 0: it does not say */
    int32_t playing;      /* 1 playing, 0 stopped; below 0: it does not say */
    int32_t meter_num;    /* its time signature; 0: it does not say */
    int32_t meter_den;
} SpTransport;
void sp_instance_transport(SpInstance* instance, const SpTransport* transport);
/* Whether the instance's bars follow the host's (1, as it starts) or only its tempo (0). Any thread. */
void sp_instance_align_bars(SpInstance* instance, int on);
/* The global time warp, in milliseconds: every sound from then on is that much later (or, below 0, earlier, by no
 * more than the schedule-ahead time allows). Any thread. */
void sp_instance_time_warp(SpInstance* instance, double ms);

/* A program as a new run, in `group` (0: none in particular). */
void sp_instance_run(SpInstance* instance, const char* code, int group);
/* Every run stops where it stands; what sounds fades out. */
void sp_instance_stop(SpInstance* instance);
/* A run (by its job) or a group stops, its sounds faded over `fade` seconds; the rest play on. */
void sp_instance_stop_run(SpInstance* instance, int job, double fade);
void sp_instance_stop_group(SpInstance* instance, int group, double fade);
/* A group sits under another: stopping the parent stops it too. */
void sp_instance_group_under(SpInstance* instance, int group, int parent);

/* What the instance says, each a kind and its bytes:
 *   SP_SAYS_RECORD  # one of the runtime's records for the page (OSC: /sonic-pi/<kind> ...)
 *   SP_SAYS_LOG     # a line of the instance's own for the System log's Host tab (UTF-8)
 *   SP_SAYS_STATE   1 byte: 1 while something runs, 0 once everything has ended
 *   SP_SAYS_RUN     4 bytes: a run's job id (int32), as it starts; -1 when it did not
 *   SP_SAYS_ERROR   a line the player must see (UTF-8): a sample or synth that would not load
 *   SP_SAYS_TABLE   the process table as it stands (doubles, the page's PROCESS_FIELDS a row), when it changed
 *   SP_SAYS_EARLIER what came before, for an editor that asked (sp_instance_earlier): the kind it was said as (1
 *                   byte), then the number and what was said, as above
 *   SP_SAYS_ENGINE  # a line of the engine's, for the SuperSonic tab (UTF-8)
 *   SP_SAYS_RUNTIME # a line of the runtime's, for the Runtime tab (UTF-8)
 *   SP_SAYS_LOADS   # what the runtime asked to load or free: address, number and name, tab-separated (UTF-8)
 *   SP_SAYS_TEMPO   two doubles: the tempo the instance now keeps (its host's), and 1 while the host plays, else 0
 *   SP_SAYS_WARP    one double: the global time warp in milliseconds as it stands, for an editor that asked
 *                   (sp_instance_earlier): the instance plays on without its editor, and keeps its warp
 * #: the message starts with its number, a little-endian u64, counting what this instance has said that way. */
enum { SP_SAYS_RECORD = 1, SP_SAYS_LOG = 2, SP_SAYS_STATE = 3, SP_SAYS_RUN = 4, SP_SAYS_ERROR = 5, SP_SAYS_TABLE = 6,
       SP_SAYS_EARLIER = 7, SP_SAYS_ENGINE = 8, SP_SAYS_RUNTIME = 9, SP_SAYS_LOADS = 10, SP_SAYS_TEMPO = 11, SP_SAYS_WARP = 12 };
typedef void (*SpInstanceListener)(void* ctx, uint32_t kind, const uint8_t* bytes, uint32_t size);

/* One listener at a time; NULL stops it. Returns once no call to the last one is under way. */
void sp_instance_listen(SpInstance* instance, SpInstanceListener listener, void* ctx);

/* What came before, to the listener as SP_SAYS_EARLIER: the last lines of the Log pane and the System log, the cues,
 * and the starts of the threads still running, in the order said. For an editor that has just opened. */
void sp_instance_earlier(SpInstance* instance);

/* The instance's number in this process (1, 2, ... as they are made), as the plugin's file log tags its lines. */
unsigned sp_instance_number(const SpInstance* instance);

/* A copy of the instance's engine for its page (Clockwork's clockwork_client_copy, read by js/lib/copy.js): `parts`
 * as CLOCKWORK_COPY_*, `wants` three u32 a want (source, slot, frames). The bytes written, or 0 when `out` holds
 * too few; with `out` NULL, the bytes it takes. On the page's thread, one call at a time. */
uint32_t sp_instance_copy(SpInstance* instance, uint32_t parts, const uint32_t* wants, uint32_t count,
                          void* out, uint32_t bytes);

#ifdef __cplusplus
}
#endif
