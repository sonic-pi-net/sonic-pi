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

// Tests for the SuperSonic node-tree mirror -> child ordering reconstruction
// used by the Qt node-tree graph. The bug these guard against: the mirror's
// array/slot order is allocation order (slots are freed and reused LIFO across
// runs), NOT sibling order, so identical sub-trees rendered in different orders
// on each run. order_children() must derive order purely from the head/next
// sibling chain, independent of how the nodes are laid out in the array.

#include <algorithm>
#include <vector>

#include <catch2/catch_test_macros.hpp>

#include <api/audio/node_tree_order.hpp>

using sonic_pi::node_tree::OrderNode;
using sonic_pi::node_tree::OrderedTree;
using sonic_pi::node_tree::order_children;

namespace
{
// Convenience builders mirroring how the mirror records nodes.
OrderNode grp(int32_t id, int32_t parent, int32_t head, int32_t next)
{
    return OrderNode{ id, parent, head, next, /*is_group=*/true };
}
OrderNode syn(int32_t id, int32_t parent, int32_t next)
{
    return OrderNode{ id, parent, /*head=*/-1, next, /*is_group=*/false };
}

std::vector<int32_t> kids(const OrderedTree& t, int32_t parent)
{
    auto it = t.children.find(parent);
    return it == t.children.end() ? std::vector<int32_t>{} : it->second;
}
} // namespace

TEST_CASE("children follow the head/next chain in order", "[node_tree_order]")
{
    // group 10 has two children: synth 100 then group 200; group 200 has synth 300.
    std::vector<OrderNode> nodes = {
        grp(0, -1, /*head=*/10, /*next=*/-1),
        grp(10, 0, /*head=*/100, /*next=*/-1),
        syn(100, 10, /*next=*/200),
        grp(200, 10, /*head=*/300, /*next=*/-1),
        syn(300, 200, /*next=*/-1),
    };

    const OrderedTree t = order_children(nodes);

    CHECK(kids(t, 0) == std::vector<int32_t>{ 10 });
    CHECK(kids(t, 10) == std::vector<int32_t>{ 100, 200 });
    CHECK(kids(t, 200) == std::vector<int32_t>{ 300 });
    CHECK(t.roots == std::vector<int32_t>{ 0 });
}

TEST_CASE("child order is independent of array/slot order", "[node_tree_order]")
{
    // The same logical tree as above. The ONLY difference between the two inputs
    // is the order of the vector — exactly what changes run-to-run as the mirror
    // reuses freed slots. The reconstructed order must be identical.
    const std::vector<OrderNode> creationOrder = {
        grp(0, -1, 10, -1),
        grp(10, 0, 100, -1),
        syn(100, 10, 200),
        grp(200, 10, 300, -1),
        syn(300, 200, -1),
    };

    std::vector<OrderNode> shuffled = creationOrder;
    std::reverse(shuffled.begin(), shuffled.end());
    // A second, arbitrary permutation for good measure.
    std::vector<OrderNode> rotated = { creationOrder[2], creationOrder[4], creationOrder[0],
                                       creationOrder[3], creationOrder[1] };

    const OrderedTree a = order_children(creationOrder);
    const OrderedTree b = order_children(shuffled);
    const OrderedTree c = order_children(rotated);

    CHECK(kids(a, 10) == std::vector<int32_t>{ 100, 200 });
    CHECK(kids(b, 10) == kids(a, 10));
    CHECK(kids(c, 10) == kids(a, 10));
    CHECK(kids(b, 0) == kids(a, 0));
    CHECK(kids(c, 200) == kids(a, 200));
}

TEST_CASE("add-to-head reversal is honoured, not array order", "[node_tree_order]")
{
    // Two synths added to the HEAD of group 10: the second (200) becomes head,
    // so the sibling chain is 200 -> 100. The array lists them in allocation
    // order (100 then 200); the result must still be {200, 100}.
    std::vector<OrderNode> nodes = {
        grp(0, -1, 10, -1),
        grp(10, 0, /*head=*/200, -1),
        syn(100, 10, /*next=*/-1), // added first, now the tail
        syn(200, 10, /*next=*/100), // added second to head, now points at 100
    };

    const OrderedTree t = order_children(nodes);
    CHECK(kids(t, 10) == std::vector<int32_t>{ 200, 100 });
}

TEST_CASE("with_fx :reverb sub-trees render identically across runs", "[node_tree_order]")
{
    // Models two runs of `with_fx :reverb do play 70 end`. Each run builds an FX
    // container group (tail of the run group) holding an fx synth (head) and a
    // synth sub-group (tail). The two runs get different node IDs AND land in a
    // different array order, but each container's children must be {fx, subgroup}.
    auto subtree = [](int32_t base) {
        const int32_t container = base;       // fx container group
        const int32_t fx = base + 1;          // reverb fx synth (head)
        const int32_t subgrp = base + 2;      // synth sub-group (tail)
        const int32_t player = base + 3;      // the `play 70` synth
        return std::vector<OrderNode>{
            grp(container, 2, /*head=*/fx, /*next=*/-1),
            syn(fx, container, /*next=*/subgrp),
            grp(subgrp, container, /*head=*/player, /*next=*/-1),
            syn(player, subgrp, /*next=*/-1),
        };
    };

    // Run group (id 2) under root (id 0); two containers chained at its tail.
    std::vector<OrderNode> run1;
    run1.push_back(grp(0, -1, /*head=*/2, -1));
    run1.push_back(grp(2, 0, /*head=*/100, -1));
    {
        auto a = subtree(100);
        auto b = subtree(200);
        // container 100 -> container 200 (tail chain)
        a[0].next = 200;
        run1.insert(run1.end(), a.begin(), a.end());
        run1.insert(run1.end(), b.begin(), b.end());
    }

    // Same tree, but the array is reversed (slot reuse on the second run).
    std::vector<OrderNode> run2 = run1;
    std::reverse(run2.begin(), run2.end());

    const OrderedTree t1 = order_children(run1);
    const OrderedTree t2 = order_children(run2);

    // Each container's children: fx synth first, then the synth sub-group.
    CHECK(kids(t1, 100) == std::vector<int32_t>{ 101, 102 });
    CHECK(kids(t1, 200) == std::vector<int32_t>{ 201, 202 });
    // Run group keeps both containers in creation order.
    CHECK(kids(t1, 2) == std::vector<int32_t>{ 100, 200 });

    // Critically: a reshuffled array yields the IDENTICAL ordering.
    CHECK(kids(t2, 100) == kids(t1, 100));
    CHECK(kids(t2, 200) == kids(t1, 200));
    CHECK(kids(t2, 2) == kids(t1, 2));
}

TEST_CASE("nodes the chain misses are still placed via parent id", "[node_tree_order]")
{
    // Torn snapshot: group 10's head chain only reaches 100 (200's link is not
    // yet visible), but 200 still has a valid parent. It must not vanish — it is
    // appended after the chain-ordered children.
    std::vector<OrderNode> nodes = {
        grp(0, -1, 10, -1),
        grp(10, 0, /*head=*/100, -1),
        syn(100, 10, /*next=*/-1), // chain ends here
        syn(200, 10, /*next=*/-1), // orphaned by the chain, parent still 10
    };

    const OrderedTree t = order_children(nodes);
    const auto k = kids(t, 10);
    REQUIRE(k.size() == 2);
    CHECK(k[0] == 100);           // chain-ordered first
    CHECK(k[1] == 200);           // fallback-placed after
}

TEST_CASE("roots collects nodes with no parent in the snapshot", "[node_tree_order]")
{
    std::vector<OrderNode> nodes = {
        grp(0, -1, 10, -1),
        grp(10, 0, -1, -1),
        grp(99, 42, -1, -1), // parent 42 absent -> treated as a root
    };
    const OrderedTree t = order_children(nodes);
    CHECK(std::find(t.roots.begin(), t.roots.end(), 0) != t.roots.end());
    CHECK(std::find(t.roots.begin(), t.roots.end(), 99) != t.roots.end());
    CHECK(std::find(t.roots.begin(), t.roots.end(), 10) == t.roots.end());
}

TEST_CASE("a cyclic chain terminates and does not duplicate endlessly", "[node_tree_order]")
{
    // Corrupt chain: 100 -> 200 -> 100 (cycle). The bounded walk must terminate;
    // we only require it returns without hanging and produces a bounded result.
    std::vector<OrderNode> nodes = {
        grp(0, -1, 10, -1),
        grp(10, 0, /*head=*/100, -1),
        syn(100, 10, /*next=*/200),
        syn(200, 10, /*next=*/100), // points back -> cycle
    };

    const OrderedTree t = order_children(nodes);
    // Walk is bounded by node count, so the child list can't grow without bound.
    CHECK(kids(t, 10).size() <= nodes.size() + 1);
}
