//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2024 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef SONIC_PI_NODE_TREE_ORDER_HPP
#define SONIC_PI_NODE_TREE_ORDER_HPP

#include <cstdint>
#include <unordered_map>
#include <unordered_set>
#include <vector>

// Reconstruct scsynth child ordering from the SuperSonic node-tree mirror.
//
// The mirror is a flat array of NodeEntry slots. A node's position in that array
// is *allocation* order (slots are freed and reused LIFO across runs), which is
// NOT its sibling order. The true execution order — the order the engine's
// depth-first audio traversal visits children, and the order a faithful node
// graph must draw them — is encoded by the per-group sibling chain: a group's
// `head` points at its first child, and each child's `next` points at the
// following sibling.
//
// order_children() walks that chain so the resulting order is independent of how
// the nodes happen to be laid out in the array. It is the single source of truth
// shared by the Qt node-tree graph and its tests.
namespace sonic_pi {
namespace node_tree {

// Minimal flat view of one mirror node — only the fields needed to rebuild order.
struct OrderNode
{
    int32_t id = -1;
    int32_t parent = -1;   // parent group id (-1 if root / not present)
    int32_t head = -1;     // first child (groups only; -1 otherwise)
    int32_t next = -1;     // next sibling in the parent's child list (-1 if last)
    bool is_group = false; // only groups own a head chain
};

// children[parentId] = child ids in execution order; roots = top-level node ids.
struct OrderedTree
{
    std::unordered_map<int32_t, std::vector<int32_t>> children;
    std::vector<int32_t> roots;
};

// Build the ordered child adjacency for `nodes`.
//
// Robustness: the chain walk is bounded by the node count so a torn read taken
// mid-update (the mirror is written by the audio thread, polled by the GUI)
// cannot loop forever; any node the chain doesn't reach is still placed via its
// parent id so it never silently disappears. roots keeps `nodes` order, matching
// the array-order fallback used before sibling chaining existed.
inline OrderedTree order_children(const std::vector<OrderNode>& nodes)
{
    OrderedTree out;

    std::unordered_map<int32_t, size_t> index; // id -> position in `nodes`
    index.reserve(nodes.size() * 2);
    for (size_t i = 0; i < nodes.size(); ++i)
        index[nodes[i].id] = i;

    std::unordered_set<int32_t> placed;
    placed.reserve(nodes.size() * 2);

    // Primary: order each group's children by walking head -> next -> next ...
    for (const auto& n : nodes)
    {
        if (!n.is_group)
            continue;
        int32_t childId = n.head;
        for (size_t guard = 0;
             childId >= 0 && index.count(childId) && guard <= nodes.size();
             ++guard)
        {
            out.children[n.id].push_back(childId);
            placed.insert(childId);
            childId = nodes[index[childId]].next;
        }
    }

    // Fallback: place anything the chain missed by parent id, and collect roots.
    for (const auto& n : nodes)
    {
        if (n.parent >= 0 && index.count(n.parent))
        {
            if (!placed.count(n.id))
                out.children[n.parent].push_back(n.id);
        }
        else
        {
            out.roots.push_back(n.id);
        }
    }

    return out;
}

} // namespace node_tree
} // namespace sonic_pi

#endif // SONIC_PI_NODE_TREE_ORDER_HPP
