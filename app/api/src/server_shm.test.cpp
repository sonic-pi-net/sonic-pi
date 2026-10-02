//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

// Regression guarded against: SuperSonic 0.89.0 stopped fixing the node-tree
// mirror's capacity at compile time — it is whatever the window the host hands
// over holds — and removed NODE_TREE_MIRROR_MAX_NODES. The reader here still
// used the constant, so the API stopped compiling. The capacity is now read
// from the window, the same sum the engine does when it binds
// (supersonic_node_tree_bind) and SuperSonic's JS reader does
// (js/lib/node_tree_parser.js).

#include <cstdint>
#include <vector>

#include <catch2/catch_test_macros.hpp>

#include <api/audio/server_shm.hpp>

TEST_CASE("the node tree holds what its window holds", "[shm][node_tree]")
{
    std::vector<uint8_t> window(NODE_TREE_HEADER_SIZE + 1024 * NODE_TREE_ENTRY_SIZE);
    const node_tree_view nt = node_tree_in(window.data(), window.size());
    CHECK(nt.header == reinterpret_cast<const NodeTreeHeader*>(window.data()));
    CHECK(nt.entries == reinterpret_cast<const NodeEntry*>(window.data() + NODE_TREE_HEADER_SIZE));
    CHECK(nt.max_nodes == 1024);
}

TEST_CASE("a part entry at the end of the window is not a node", "[shm][node_tree]")
{
    std::vector<uint8_t> window(NODE_TREE_HEADER_SIZE + 3 * NODE_TREE_ENTRY_SIZE + NODE_TREE_ENTRY_SIZE - 1);
    CHECK(node_tree_in(window.data(), window.size()).max_nodes == 3);
}

TEST_CASE("a window too small for one node has no tree", "[shm][node_tree]")
{
    // The engine leaves such a window alone (it writes no header), so there
    // is nothing to read — the same empty view as no window at all.
    std::vector<uint8_t> window(NODE_TREE_HEADER_SIZE + NODE_TREE_ENTRY_SIZE - 1);
    const node_tree_view small = node_tree_in(window.data(), window.size());
    CHECK(small.header == nullptr);
    CHECK(small.entries == nullptr);
    CHECK(small.max_nodes == 0);

    const node_tree_view none = node_tree_in(nullptr, 0);
    CHECK(none.header == nullptr);
    CHECK(none.max_nodes == 0);
}
