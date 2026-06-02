//  Shared memory interface to the SuperSonic engine (reader / consumer side).
//  Copyright (C) 2011 Tim Blechmann
//  Copyright (C) 2011 Jakob Leben
//  Copyright (C) 2026 SuperSonic contributors
//
//  Uses raw POSIX shm_open/mmap (Linux, macOS) or Win32 named file mappings
//  (Windows). The engine publishes a single self-describing segment; this
//  client locates every region purely from the segment header, so it needs no
//  compile-time knowledge of the arena layout and cannot drift out of sync
//  (the MAGIC value rejects an incompatible engine outright).

#pragma once

#include "shm_audio_buffer.hpp"

#include <string>
#include <cstring>
#include <stdexcept>
#include <atomic>
#include <cstdint>

#ifdef _WIN32
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#  ifndef NOMINMAX
#    define NOMINMAX
#  endif
#  include <windows.h>
#else
#  include <fcntl.h>
#  include <sys/mman.h>
#  include <sys/stat.h>
#  include <unistd.h>
#endif

namespace detail_server_shm {

using std::string;

static inline string make_shmem_name(unsigned int port_number) {
    return string("SuperSonic_") + std::to_string(port_number);
}

// ──── Platform shared memory primitives (reader subset) ─────────────────

struct shm_handle {
    void*  ptr  = nullptr;
    size_t size = 0;
#ifdef _WIN32
    HANDLE mapping = nullptr;
#else
    int    fd  = -1;
#endif
};

inline shm_handle shm_open_existing(const string& name) {
    shm_handle h;
#ifdef _WIN32
    std::wstring wname(name.begin(), name.end());
    h.mapping = OpenFileMappingW(FILE_MAP_ALL_ACCESS, FALSE, wname.c_str());
    if (!h.mapping)
        throw std::runtime_error("OpenFileMapping failed for " + name);
    h.ptr = MapViewOfFile(h.mapping, FILE_MAP_ALL_ACCESS, 0, 0, 0);
    if (!h.ptr) {
        CloseHandle(h.mapping);
        throw std::runtime_error("MapViewOfFile failed for " + name);
    }
    MEMORY_BASIC_INFORMATION info;
    VirtualQuery(h.ptr, &info, sizeof(info));
    h.size = info.RegionSize;
#else
    string posix_name = "/" + name;
    h.fd = ::shm_open(posix_name.c_str(), O_RDWR, 0);
    if (h.fd < 0)
        throw std::runtime_error("shm_open(open) failed for " + name);
    struct stat st;
    fstat(h.fd, &st);
    h.size = static_cast<size_t>(st.st_size);
    h.ptr = ::mmap(nullptr, h.size, PROT_READ | PROT_WRITE, MAP_SHARED, h.fd, 0);
    if (h.ptr == MAP_FAILED) {
        ::close(h.fd);
        throw std::runtime_error("mmap failed for " + name);
    }
#endif
    return h;
}

inline void shm_close(shm_handle& h) {
#ifdef _WIN32
    if (h.ptr)     UnmapViewOfFile(h.ptr);
    if (h.mapping) CloseHandle(h.mapping);
    h.mapping = nullptr;
#else
    if (h.ptr && h.ptr != MAP_FAILED) ::munmap(h.ptr, h.size);
    if (h.fd >= 0)                    ::close(h.fd);
    h.fd = -1;
#endif
    h.ptr = nullptr;
    h.size = 0;
}

// ──── Self-describing segment header ────────────────────────────────────
//
// Byte-for-byte mirror of supersonic/src/scsynth/common/server_shm.hpp. The
// engine fills in every region offset/geometry; we read them rather than
// hardcoding the layout. MAGIC must match — a stale engine is rejected.
//
// MAGIC 0x5C09E005: unified layout (segment == shared_memory.h arena blob;
// rings in-segment; scope fixed-inline). All offsets are relative to the
// arena blob base (segment + blob_offset).
// MAGIC 0x5C09E006: + Link metric fields (METRICS_SIZE 184→232).
struct shm_segment_header {
    static constexpr uint32_t MAGIC = 0x5C09E006;

    uint32_t magic;
    uint32_t blob_offset;
    uint32_t blob_size;

    uint32_t in_ring_offset;
    uint32_t in_ring_size;
    uint32_t out_ring_offset;
    uint32_t out_ring_size;
    uint32_t debug_ring_offset;
    uint32_t debug_ring_size;
    uint32_t control_offset;

    uint32_t metrics_offset;
    uint32_t metrics_field_count;

    uint32_t node_tree_offset;
    uint32_t node_tree_header_bytes;
    uint32_t node_tree_entry_bytes;
    uint32_t node_tree_max_nodes;

    uint32_t audio_offset;
    uint32_t audio_slot_count;
    uint32_t audio_slot_bytes;

    uint32_t scope_offset;
    uint32_t scope_max;
    uint32_t scope_header_bytes;
    uint32_t scope_slot_bytes;
    uint32_t scope_slot_header;
    uint32_t scope_frames;
    uint32_t scope_channels;

    uint32_t native_stats_offset;  // native-only live stats (synthdefs, buffers, buffer_bytes)
};

// Convenience: the metrics field count for callers that want a constant.
static constexpr size_t METRICS_FIELD_COUNT = 58;

// ──── Fixed-inline scope reader ─────────────────────────────────────────
//
// Reads the triple-buffered scope slot the engine writes (offsets only). The
// geometry (frames, channels, slot-header size) comes from the segment header,
// so the reader is layout-agnostic. Best-effort: reports new data when the
// published `stage` index advances.
class shm_scope_buffer_reader {
public:
    shm_scope_buffer_reader() = default;
    shm_scope_buffer_reader(uint8_t* slot, uint32_t frames, uint32_t channels,
                            uint32_t slot_header)
        : slot_(slot), frames_(frames), channels_(channels), slot_header_(slot_header) {}

    bool valid() {
        if (!slot_) return false;
        auto* state = reinterpret_cast<std::atomic<uint32_t>*>(slot_ + 0);
        return state->load(std::memory_order_acquire) == 1;
    }

    unsigned int channels() {
        if (!slot_) return 0;
        return *reinterpret_cast<uint32_t*>(slot_ + 4);
    }

    unsigned int max_frames() { return frames_; }

    bool pull(unsigned int& frames) {
        if (!valid()) return false;
        auto* stage = reinterpret_cast<std::atomic<int32_t>*>(slot_ + 8);
        int s = stage->load(std::memory_order_acquire);
        if (s == last_stage_)
            return false;
        last_stage_ = s;
        frames = frames_;
        return true;
    }

    float* data() {
        if (!slot_) return nullptr;
        auto* stage = reinterpret_cast<std::atomic<int32_t>*>(slot_ + 8);
        int s = stage->load(std::memory_order_acquire);
        float* base = reinterpret_cast<float*>(slot_ + slot_header_);
        return base + static_cast<size_t>(s) * (static_cast<size_t>(frames_) * channels_);
    }

private:
    uint8_t* slot_       = nullptr;
    uint32_t frames_     = 0;
    uint32_t channels_   = 0;
    uint32_t slot_header_ = 0;
    int      last_stage_ = -1;
};

// A view onto one of the OSC/debug transport rings, for passive observation
// (the GUI tails these with its own cursor; see the SuperSonic panel).
struct ring_view {
    uint8_t* base = nullptr;   // ring data base
    uint32_t size = 0;         // ring capacity in bytes
    std::atomic<int32_t>* head = nullptr;  // producer cursor (bytes)
    std::atomic<int32_t>* tail = nullptr;  // consumer cursor (bytes)
};

// A view onto the node-tree mirror, for visualisation. `header` points at the
// NodeTreeHeader (node_count, version, dropped_count — u32 each); `entries`
// at the NodeEntry[] array. Field layouts mirror shared_memory.h; the reader
// interprets them. `version` lets the GUI redraw only on change.
struct node_tree_view {
    uint8_t* header      = nullptr;
    uint8_t* entries     = nullptr;
    uint32_t max_nodes   = 0;
    uint32_t entry_bytes = 0;
};

// Native-only live engine stats (0 on a web-origin segment / when unmapped).
struct native_stats {
    uint32_t synthdefs    = 0;
    uint32_t buffers      = 0;
    uint32_t buffer_bytes = 0;
};

// ──── Client (GUI / reader side) ────────────────────────────────────────

class server_shared_memory_client {
public:
    server_shared_memory_client(const server_shared_memory_client&) = delete;
    server_shared_memory_client& operator=(const server_shared_memory_client&) = delete;

    server_shared_memory_client(unsigned int port_number):
        shmem_name(make_shmem_name(port_number)),
        handle(shm_open_existing(shmem_name))
    {
        auto* header = static_cast<shm_segment_header*>(handle.ptr);
        if (header->magic != shm_segment_header::MAGIC)
            throw std::runtime_error(
                "Invalid shared memory magic — is the audio engine running?");

        // Acquire pairs with the engine's release before the MAGIC store, so
        // observing MAGIC implies a fully-published header.
        std::atomic_thread_fence(std::memory_order_acquire);

        hdr_  = *header;  // snapshot the self-describing offsets/geometry
        blob_ = reinterpret_cast<uint8_t*>(handle.ptr) + hdr_.blob_offset;
    }

    ~server_shared_memory_client() {
        shm_close(handle);
    }

    // Flat PerformanceMetrics view (METRICS_FIELD_COUNT contiguous u32 fields).
    const std::atomic<uint32_t>* get_metrics() {
        return reinterpret_cast<const std::atomic<uint32_t>*>(blob_ + hdr_.metrics_offset);
    }
    uint32_t metrics_field_count() const { return hdr_.metrics_field_count; }

    shm_scope_buffer_reader get_scope_buffer_reader(unsigned int index) {
        if (index >= hdr_.scope_max)
            return shm_scope_buffer_reader();
        uint8_t* slot = blob_ + hdr_.scope_offset + hdr_.scope_header_bytes
                      + static_cast<size_t>(index) * hdr_.scope_slot_bytes;
        return shm_scope_buffer_reader(slot, hdr_.scope_frames, hdr_.scope_channels,
                                       hdr_.scope_slot_header);
    }

    // Slot pointer for the session recorder's own reader. Slot 0 is the master
    // output mix; 1..N are AudioOut2 UGens. nullptr if out of range.
    shm_audio_buffer* get_audio_buffer(unsigned int index) {
        if (index >= hdr_.audio_slot_count)
            return nullptr;
        return reinterpret_cast<shm_audio_buffer*>(
            blob_ + hdr_.audio_offset + static_cast<size_t>(index) * hdr_.audio_slot_bytes);
    }
    shm_audio_buffer_reader get_audio_buffer_reader(unsigned int index) {
        return shm_audio_buffer_reader(get_audio_buffer(index));
    }

    // ── OSC / debug transport rings (passive observation) ──────────────
    // The ControlPointers struct is a run of int32 atomics at control_offset;
    // its field order (in/out/debug head,tail,sequence,…) matches
    // shared_memory.h. Head/tail are byte cursors into the matching ring.
    enum ControlIndex {
        CI_IN_HEAD = 0, CI_IN_TAIL, CI_OUT_HEAD, CI_OUT_TAIL,
        CI_DEBUG_HEAD, CI_DEBUG_TAIL
    };
    std::atomic<int32_t>* control_word(ControlIndex i) {
        return reinterpret_cast<std::atomic<int32_t>*>(blob_ + hdr_.control_offset)
             + static_cast<int>(i);
    }
    ring_view get_in_ring() {
        return { blob_ + hdr_.in_ring_offset, hdr_.in_ring_size,
                 control_word(CI_IN_HEAD), control_word(CI_IN_TAIL) };
    }
    ring_view get_out_ring() {
        return { blob_ + hdr_.out_ring_offset, hdr_.out_ring_size,
                 control_word(CI_OUT_HEAD), control_word(CI_OUT_TAIL) };
    }
    ring_view get_debug_ring() {
        return { blob_ + hdr_.debug_ring_offset, hdr_.debug_ring_size,
                 control_word(CI_DEBUG_HEAD), control_word(CI_DEBUG_TAIL) };
    }

    // ── Node-tree mirror (for visualisation) ───────────────────────────
    uint8_t* node_tree_header()  { return blob_ + hdr_.node_tree_offset; }
    uint8_t* node_tree_entries() { return blob_ + hdr_.node_tree_offset + hdr_.node_tree_header_bytes; }
    uint32_t node_tree_max_nodes()   const { return hdr_.node_tree_max_nodes; }
    uint32_t node_tree_entry_bytes() const { return hdr_.node_tree_entry_bytes; }

    node_tree_view get_node_tree() {
        return { node_tree_header(), node_tree_entries(),
                 hdr_.node_tree_max_nodes, hdr_.node_tree_entry_bytes };
    }

    // ── Native-only live engine stats (synthdef count, allocated buffers) ──
    // Three contiguous u32 (synthdefs, buffers, buffer_bytes) the native engine
    // publishes; absent (offset 0) on a web-origin segment.
    uint32_t native_synthdefs() {
        if (!hdr_.native_stats_offset) return 0;
        return reinterpret_cast<const std::atomic<uint32_t>*>(blob_ + hdr_.native_stats_offset + 0)
            ->load(std::memory_order_relaxed);
    }
    uint32_t native_buffers() {
        if (!hdr_.native_stats_offset) return 0;
        return reinterpret_cast<const std::atomic<uint32_t>*>(blob_ + hdr_.native_stats_offset + 4)
            ->load(std::memory_order_relaxed);
    }
    uint32_t native_buffer_bytes() {
        if (!hdr_.native_stats_offset) return 0;
        return reinterpret_cast<const std::atomic<uint32_t>*>(blob_ + hdr_.native_stats_offset + 8)
            ->load(std::memory_order_relaxed);
    }
    native_stats get_native_stats() {
        return { native_synthdefs(), native_buffers(), native_buffer_bytes() };
    }

private:
    string              shmem_name;
    shm_handle          handle;
    shm_segment_header  hdr_{};
    uint8_t*            blob_ = nullptr;
};

} /* namespace detail_server_shm */

using detail_server_shm::shm_scope_buffer_reader;
using detail_server_shm::server_shared_memory_client;
using detail_server_shm::ring_view;
using detail_server_shm::node_tree_view;
using detail_server_shm::native_stats;
using detail_server_shm::METRICS_FIELD_COUNT;
// shm_audio_buffer + AUDIO_* names are exported by shm_audio_buffer.hpp.
