//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

// The node tree's motion: what NodeTreeGraph keeps once a node has ended, how
// that fades, how big each kind is drawn and when the labels show. The rules
// and numbers are the web's Threads view's (app/web/app/src/process-tree.js),
// so the two trees end and size things alike. Kept free of Qt so they are
// tested on their own (gui-tests/nodetreemotion.test.cpp).
//
// SuperSonic's mirror holds only what is live: a freed node's slot is empty at
// the next read. So the tree keeps what has ended, where it was among its
// siblings, and lets it fade — a sound over a second, a group or fx over 2.5 s,
// shrinking as it goes — and keeps a node while anything under it is still
// drawn.

#include <api/audio/node_tree_order.hpp>

#include <algorithm>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace SonicPi {
namespace NodeTreeMotion {

enum class Kind { Group = 0, Synth, Fx, Sample };

inline bool isSound(Kind k) { return k == Kind::Synth || k == Kind::Sample; }

// By Sonic Pi's names: its fx are sonic-pi-fx_*, and samples play through the
// *_player synths.
inline Kind kindOf(bool isGroup, const std::string& defName)
{
    if (isGroup) return Kind::Group;
    if (defName.find("-fx_") != std::string::npos || defName.find("-fx-") != std::string::npos) return Kind::Fx;
    if (defName.find("stereo_player") != std::string::npos || defName.find("mono_player") != std::string::npos)
        return Kind::Sample;
    return Kind::Synth;
}

// A live node, as the mirror has it.
struct LiveNode
{
    int32_t id = -1;
    int32_t parent = -1;
    int32_t head = -1;   // first child (groups) — scsynth's sibling chain
    int32_t next = -1;   // next sibling in the parent's list
    Kind kind = Kind::Synth;
    std::string defName;
};

// A drawn node: live, or ended and fading.
struct Node
{
    int32_t id = -1;
    int32_t parent = -1;
    Kind kind = Kind::Synth;
    std::string defName;
    double ended = -1;   // when it was first missing from the mirror, in seconds; -1 while live
};

constexpr double kSoundLinger  = 1.0;    // a sound fades out over this once it ends
constexpr double kLinger       = 2.5;    // a group or fx, over this...
constexpr double kFaintest     = 0.15;   // ...never fainter than this
constexpr double kParentOfLive = 0.5;    // an ended node something live still hangs from
constexpr int    kMaxAnimated  = 200;    // above this: positions snap, edges go straight, nodes draw smaller
// Labels while few enough to read them, counting only what carries one: off
// past the most, back only at fewer, so a count at the edge does not flash
// them.
constexpr int    kLabelsOff    = 60;
constexpr int    kLabelsOn     = 48;

// A sibling list read afresh (`live`), with the ended nodes of the list it
// replaces (`before`) kept where they were: each goes back in just after what
// preceded it there. A node in `before` that is neither in `live` nor ended
// has moved to another parent, and is in that one's list.
template <typename Ended>
std::vector<int32_t> keepEnded(const std::vector<int32_t>& before, const std::vector<int32_t>& live, Ended ended)
{
    std::vector<int32_t> out = live;
    size_t at = 0;   // where the next ended node goes
    for (int32_t id : before)
    {
        auto it = std::find(out.begin(), out.end(), id);
        if (it != out.end()) { at = static_cast<size_t>(it - out.begin()) + 1; continue; }
        if (!ended(id)) continue;
        out.insert(out.begin() + static_cast<std::ptrdiff_t>(at), id);
        ++at;
    }
    return out;
}

inline double fade(const Node& n, double now, bool parentOfLive)
{
    if (n.ended < 0) return 1.0;
    const double since = std::max(0.0, now - n.ended);
    if (isSound(n.kind)) return std::clamp(1.0 - since / kSoundLinger, 0.0, 1.0);
    if (parentOfLive) return kParentOfLive;
    return std::max(kFaintest, 1.0 - since / kLinger);
}

inline double radius(Kind k, bool dense)
{
    if (dense) return k == Kind::Group ? 4.0 : isSound(k) ? 2.0 : 3.0;
    return k == Kind::Group ? 6.0 : k == Kind::Fx ? 5.0 : isSound(k) ? 2.5 : 4.5;
}

// An ended group or fx shrinks as it fades; a sound only fades.
inline double drawnRadius(const Node& n, bool dense, double alpha, bool parentOfLive)
{
    const double r = radius(n.kind, dense);
    if (n.ended >= 0 && !isSound(n.kind) && !parentOfLive) return r * (0.6 + 0.4 * alpha);
    return r;
}

// Only the fx are labelled: they are the nodes with names. A group is Sonic
// Pi's plumbing — two for every with_fx — with only a number, which the
// tooltip gives; labelling them hid every label behind the crowding rule
// (40-odd groups for two live loops, one with an fx). A sound comes and goes
// on every beat.
inline bool labelled(Kind k) { return k == Kind::Fx; }

// What is written under a node: an fx's name as a program says it.
inline std::string label(const Node& n)
{
    if (!labelled(n.kind)) return {};
    std::string name = n.defName;
    for (const char* prefix : { "sonic-pi-fx_", "sonic-pi-" })
    {
        const std::string p(prefix);
        if (name.compare(0, p.size(), p) == 0) { name.erase(0, p.size()); break; }
    }
    return ":" + name;
}

// Whether there are too many labelled nodes to label, given whether there
// were last time.
inline bool crowded(int labelled, bool wasCrowded)
{
    return labelled > (wasCrowded ? kLabelsOn : kLabelsOff);
}

class Tree
{
public:
    struct Target { float tx = 0.5f, ty = 0.f; };

    // A fresh read of the mirror at `now` (seconds). True when the layout
    // needs working out again: a node is new, or has moved. A node that ends
    // keeps its place, so ending alone is not a change.
    bool observe(const std::vector<LiveNode>& live, double now)
    {
        std::unordered_set<int32_t> present;
        present.reserve(live.size() * 2);
        for (const LiveNode& l : live) present.insert(l.id);

        for (const LiveNode& l : live)
        {
            Node& n = m_nodes[l.id];
            // A node read again after going missing is live again, in place:
            // a read taken mid-change can miss one for a frame.
            n = { l.id, l.parent, l.kind, l.defName, -1 };
        }
        for (auto& [id, n] : m_nodes)
            if (n.ended < 0 && !present.count(id)) n.ended = now;

        std::vector<sonic_pi::node_tree::OrderNode> flat;
        flat.reserve(live.size());
        for (const LiveNode& l : live) flat.push_back({ l.id, l.parent, l.head, l.next, l.kind == Kind::Group });
        sonic_pi::node_tree::OrderedTree ordered = sonic_pi::node_tree::order_children(flat);

        // A live node whose parent was missed is drawn under that parent
        // still, not as a root of its own.
        std::vector<int32_t> liveRoots;
        for (int32_t r : ordered.roots)
        {
            const Node& n = m_nodes.at(r);
            if (n.parent >= 0 && n.parent != r && m_nodes.count(n.parent)) ordered.children[n.parent].push_back(r);
            else liveRoots.push_back(r);
        }

        auto ended = [this](int32_t id) {
            auto it = m_nodes.find(id);
            return it != m_nodes.end() && it->second.ended >= 0;
        };
        std::unordered_map<int32_t, std::vector<int32_t>> children;
        for (auto& [parent, list] : ordered.children)
        {
            auto before = m_children.find(parent);
            children[parent] = before == m_children.end() ? list : keepEnded(before->second, list, ended);
        }
        for (auto& [parent, list] : m_children)
        {
            if (children.count(parent)) continue;
            std::vector<int32_t> kept = keepEnded(list, {}, ended);
            if (!kept.empty()) children[parent] = std::move(kept);
        }
        std::vector<int32_t> roots = keepEnded(m_roots, liveRoots, ended);

        const bool changed = children != m_children || roots != m_roots;
        m_children = std::move(children);
        m_roots = std::move(roots);
        return changed;
    }

    // Let go of the ended nodes that have faded, and of their places. A node
    // goes only once nothing under it is still drawn. True when any went.
    bool expire(double now)
    {
        bool any = false;
        for (bool again = true; again;)
        {
            again = false;
            for (auto it = m_nodes.begin(); it != m_nodes.end();)
            {
                const Node& n = it->second;
                const double linger = isSound(n.kind) ? kSoundLinger : kLinger;
                if (n.ended < 0 || now - n.ended < linger || hasChildren(n.id)) { ++it; continue; }
                forget(n.id, n.parent);
                it = m_nodes.erase(it);
                any = again = true;
            }
        }
        return any;
    }

    // Whether anything ended is still drawn, fading.
    bool fading() const
    {
        for (const auto& [id, n] : m_nodes)
            if (n.ended >= 0) return true;
        return false;
    }

    const std::unordered_map<int32_t, Node>& nodes() const { return m_nodes; }
    const std::vector<int32_t>& roots() const { return m_roots; }
    const std::vector<int32_t>& children(int32_t id) const
    {
        static const std::vector<int32_t> none;
        auto it = m_children.find(id);
        return it == m_children.end() ? none : it->second;
    }

    // The ended nodes something live still hangs from.
    std::unordered_set<int32_t> parentsOfLive() const
    {
        std::unordered_set<int32_t> out;
        for (const auto& [id, n] : m_nodes)
        {
            if (n.ended >= 0) continue;
            for (auto p = m_nodes.find(n.parent); p != m_nodes.end() && !out.count(p->first); p = m_nodes.find(p->second.parent))
            {
                if (p->first == p->second.parent) break;
                out.insert(p->first);
            }
        }
        return out;
    }

    // Where each node goes, both in 0..1: leaves take slots across in sibling
    // order, parents centre over their children, depth goes down.
    std::unordered_map<int32_t, Target> targets() const
    {
        std::unordered_map<int32_t, int> depth;
        std::unordered_map<int32_t, double> xpos;
        double leaf = 0.0;
        int maxDepth = 0;
        // Iterative depth-first walk: a deep chain cannot overflow the stack,
        // and the guard stops a corrupt read from looping.
        struct Frame { int32_t id; int depth; size_t next; };
        std::vector<Frame> stack;
        std::unordered_set<int32_t> seen;
        for (int32_t root : m_roots)
        {
            if (!seen.insert(root).second) continue;
            stack.push_back({ root, 0, 0 });
            while (!stack.empty())
            {
                Frame& f = stack.back();
                const std::vector<int32_t>& ch = children(f.id);
                if (f.next == 0)
                {
                    depth[f.id] = f.depth;
                    maxDepth = std::max(maxDepth, f.depth);
                }
                if (f.next < ch.size())
                {
                    const int32_t c = ch[f.next++];
                    if (seen.insert(c).second) stack.push_back({ c, f.depth + 1, 0 });
                    continue;
                }
                double sum = 0;
                int placed = 0;
                for (int32_t c : ch)
                    if (auto x = xpos.find(c); x != xpos.end()) { sum += x->second; ++placed; }
                if (placed == 0) xpos[f.id] = leaf++;
                else xpos[f.id] = sum / placed;
                stack.pop_back();
            }
        }
        const double maxX = std::max(1.0, leaf - 1.0);
        const int md = std::max(1, maxDepth);
        std::unordered_map<int32_t, Target> out;
        out.reserve(xpos.size() * 2);
        for (const auto& [id, x] : xpos)
            out[id] = { static_cast<float>(leaf <= 1.0 ? 0.5 : x / maxX),
                        static_cast<float>(static_cast<double>(depth[id]) / md) };
        return out;
    }

private:
    bool hasChildren(int32_t id) const
    {
        auto it = m_children.find(id);
        return it != m_children.end() && !it->second.empty();
    }

    void forget(int32_t id, int32_t parent)
    {
        auto drop = [id](std::vector<int32_t>& list) { list.erase(std::remove(list.begin(), list.end(), id), list.end()); };
        drop(m_roots);
        if (auto it = m_children.find(parent); it != m_children.end())
        {
            drop(it->second);
            if (it->second.empty()) m_children.erase(it);
        }
        m_children.erase(id);
    }

    std::unordered_map<int32_t, Node> m_nodes;
    std::unordered_map<int32_t, std::vector<int32_t>> m_children;   // parent → children in sibling order, ended ones kept
    std::vector<int32_t> m_roots;
};

} // namespace NodeTreeMotion
} // namespace SonicPi
