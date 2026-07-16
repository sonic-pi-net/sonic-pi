//  Shared memory interface to the SuperSonic engine (reader / consumer side).
//
//  Thin shim over supersonic's own server_shm.hpp (app/external/supersonic):
//  the engine that writes the segment and the client that reads it compile
//  from the same header, so the layout cannot drift and readers carry the
//  engine's hardening (mapping keep-alive, size checks, stage clamping).

#pragma once

#include "synth/common/server_shm.hpp"

using detail_server_shm::server_shared_memory_client;
using detail_server_shm::ring_view;
using detail_server_shm::node_tree_view;
using detail_server_shm::native_stats;
using detail_server_shm::sample_clock_view;
