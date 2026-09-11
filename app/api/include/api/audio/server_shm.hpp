//  Shared memory interface to the SuperSonic engine (reader / consumer side).
//
//  Thin shim over the engine's own shm_segment.hpp (external/supersonic,
//  clockwork/src): the engine that writes the segment and the client that reads
//  it compile from the same header, so the layout cannot drift and readers
//  carry the engine's hardening (mapping keep-alive, size checks, a layout
//  guard against the engine's self-described geometry).
//
//  The segment is named after the product (CLOCKWORK_PRODUCT_NAME, from
//  cmake/supersonic.cmake) and the engine's port.

#pragma once

// Angle brackets on purpose: the engine's headers are found on the include
// path, and the quoted form would look in this directory first, where a
// same-named shim lives.
#include <shm_segment.hpp>
#include <node_tree.h>

// shm_segment_client, ring_view, native_stats and sample_clock_view are
// exported at global scope by shm_segment.hpp itself.

// The node tree is SuperSonic's, not clockwork's: it is published in the
// window clockwork reserves for its guest, whose contents clockwork does not
// read. The client hands the window over as bytes and SuperSonic's
// node_tree.h says what they are — typed here, so no reader counts offsets.
struct node_tree_view {
    const NodeTreeHeader* header    = nullptr;
    const NodeEntry*      entries   = nullptr;
    uint32_t              max_nodes = 0;
};

inline node_tree_view node_tree_of(shm_segment_client& client)
{
    uint8_t* window = client.get_window();
    if (!window) return {};
    return { reinterpret_cast<const NodeTreeHeader*>(window),
             reinterpret_cast<const NodeEntry*>(window + NODE_TREE_HEADER_SIZE),
             NODE_TREE_MIRROR_MAX_NODES };
}
