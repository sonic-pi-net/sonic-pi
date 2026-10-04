// Tests for the node tree's motion: what it keeps once a node has ended, how
// that fades, how big each kind is drawn, and when the labels show. The
// numbers are the web's Threads view's (app/web/app/src/process-tree.js), so
// the two trees end things, size things and label things alike.

#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

#include "utils/nodetreemotion.h"

using namespace SonicPi::NodeTreeMotion;
using Catch::Approx;

namespace
{
LiveNode group(int32_t id, int32_t parent, int32_t head = -1, int32_t next = -1)
{
    return { id, parent, head, next, Kind::Group, "" };
}

LiveNode synth(int32_t id, int32_t parent, int32_t next = -1, const char* def = "sonic-pi-beep")
{
    return { id, parent, -1, next, kindOf(false, def), def };
}

const Node& at(const Tree& t, int32_t id) { return t.nodes().at(id); }
} // namespace

// ── What a node is ──────────────────────────────────────────────────────────

TEST_CASE("kinds: Sonic Pi's names say fx and samples", "[nodetree]")
{
    CHECK(kindOf(true, "") == Kind::Group);
    CHECK(kindOf(false, "sonic-pi-fx_reverb") == Kind::Fx);
    CHECK(kindOf(false, "sonic-pi-stereo_player") == Kind::Sample);
    CHECK(kindOf(false, "sonic-pi-basic_mono_player") == Kind::Sample);
    CHECK(kindOf(false, "sonic-pi-beep") == Kind::Synth);
    CHECK(isSound(Kind::Synth));
    CHECK(isSound(Kind::Sample));
    CHECK_FALSE(isSound(Kind::Fx));
    CHECK_FALSE(isSound(Kind::Group));
}

// ── Keeping what has ended ──────────────────────────────────────────────────

TEST_CASE("keepEnded: an ended node keeps its place among its siblings", "[nodetree]")
{
    auto endedIs = [](int32_t which) { return [which](int32_t id) { return id == which; }; };
    // in the middle
    CHECK(keepEnded({ 1, 2, 3 }, { 1, 3 }, endedIs(2)) == std::vector<int32_t>{ 1, 2, 3 });
    // at the head
    CHECK(keepEnded({ 2, 1, 3 }, { 1, 3 }, endedIs(2)) == std::vector<int32_t>{ 2, 1, 3 });
    // at the tail
    CHECK(keepEnded({ 1, 3, 2 }, { 1, 3 }, endedIs(2)) == std::vector<int32_t>{ 1, 3, 2 });
    // a new node at the head moves nothing that was there
    CHECK(keepEnded({ 1, 2, 3 }, { 4, 1, 3 }, endedIs(2)) == std::vector<int32_t>{ 4, 1, 2, 3 });
    // all of them ended
    CHECK(keepEnded({ 1, 2 }, {}, [](int32_t) { return true; }) == std::vector<int32_t>{ 1, 2 });
}

TEST_CASE("keepEnded: a node that moved to another group is not kept here", "[nodetree]")
{
    CHECK(keepEnded({ 1, 2, 3 }, { 1, 3 }, [](int32_t) { return false; }) == std::vector<int32_t>{ 1, 3 });
}

TEST_CASE("tree: a freed synth stays, ended when it was first missed, where it was", "[nodetree]")
{
    Tree t;
    // 0 ─┬─ 10 (beep) ─ 11 (beep) ─ 12 (beep)
    REQUIRE(t.observe({ group(0, -1, 10), synth(10, 0, 11), synth(11, 0, 12), synth(12, 0) }, 1.0));
    const auto before = t.targets();

    // 11 is freed: the mirror now chains 10 → 12
    t.observe({ group(0, -1, 10), synth(10, 0, 12), synth(12, 0) }, 2.0);
    REQUIRE(t.nodes().count(11) == 1);
    CHECK(at(t, 11).ended == Approx(2.0));
    CHECK(at(t, 10).ended < 0);
    CHECK(t.children(0) == std::vector<int32_t>{ 10, 11, 12 });

    // its place in the layout is the one it had: nothing moves as it fades
    const auto after = t.targets();
    for (int32_t id : { 0, 10, 11, 12 })
    {
        CHECK(after.at(id).tx == Approx(before.at(id).tx));
        CHECK(after.at(id).ty == Approx(before.at(id).ty));
    }

    // read again later, still missing: it ended when it was first missed
    t.observe({ group(0, -1, 10), synth(10, 0, 12), synth(12, 0) }, 2.5);
    CHECK(at(t, 11).ended == Approx(2.0));
}

TEST_CASE("tree: a node read again after it went missing is live again, in place", "[nodetree]")
{
    Tree t;
    t.observe({ group(0, -1, 10), synth(10, 0, 11), synth(11, 0) }, 1.0);
    t.observe({ group(0, -1, 10), synth(10, 0) }, 1.1);
    REQUIRE(at(t, 11).ended == Approx(1.1));
    t.observe({ group(0, -1, 10), synth(10, 0, 11), synth(11, 0) }, 1.2);
    CHECK(at(t, 11).ended < 0);
    CHECK(t.children(0) == std::vector<int32_t>{ 10, 11 });
}

TEST_CASE("tree: a group freed with what is in it keeps them all, in order", "[nodetree]")
{
    Tree t;
    // 0 ── 5 ─┬─ 20 (fx) ─ 21 (beep)
    t.observe({ group(0, -1, 5), group(5, 0, 20), synth(20, 5, 21, "sonic-pi-fx_echo"), synth(21, 5) }, 1.0);
    t.observe({ group(0, -1) }, 2.0);
    CHECK(at(t, 5).ended == Approx(2.0));
    CHECK(at(t, 20).ended == Approx(2.0));
    CHECK(at(t, 21).ended == Approx(2.0));
    CHECK(t.children(0) == std::vector<int32_t>{ 5 });
    CHECK(t.children(5) == std::vector<int32_t>{ 20, 21 });
}

TEST_CASE("tree: observe says whether the layout needs working out again", "[nodetree]")
{
    Tree t;
    CHECK(t.observe({ group(0, -1, 10), synth(10, 0) }, 1.0));         // new nodes
    CHECK_FALSE(t.observe({ group(0, -1, 10), synth(10, 0) }, 1.1));   // the same
    CHECK_FALSE(t.observe({ group(0, -1) }, 1.2));                     // 10 ended: kept in place
    CHECK(t.observe({ group(0, -1, 11), synth(11, 0) }, 1.3));         // 11 is new
}

// ── Fading and going ────────────────────────────────────────────────────────

TEST_CASE("fade: a sound fades out over a second", "[nodetree]")
{
    Node n{ 10, 0, Kind::Synth, "sonic-pi-beep", -1 };
    CHECK(fade(n, 5.0, false) == Approx(1.0));
    n.ended = 5.0;
    CHECK(fade(n, 5.0, false) == Approx(1.0));
    CHECK(fade(n, 5.5, false) == Approx(0.5));
    CHECK(fade(n, 6.0, false) == Approx(0.0));
    CHECK(fade(n, 9.0, false) == Approx(0.0));
}

TEST_CASE("fade: a group or fx fades over 2.5 s, never below 15%", "[nodetree]")
{
    for (Kind k : { Kind::Group, Kind::Fx })
    {
        Node n{ 5, 0, k, "", 10.0 };
        CHECK(fade(n, 10.0, false) == Approx(1.0));
        CHECK(fade(n, 11.25, false) == Approx(0.5));
        CHECK(fade(n, 12.4, false) == Approx(0.15));
    }
}

TEST_CASE("fade: an ended node with something live under it stays at half", "[nodetree]")
{
    Node n{ 5, 0, Kind::Group, "", 10.0 };
    CHECK(fade(n, 12.0, true) == Approx(0.5));
}

TEST_CASE("parentsOfLive: the ended nodes something live hangs from", "[nodetree]")
{
    Tree t;
    t.observe({ group(0, -1, 5), group(5, 0, 10), synth(10, 5) }, 1.0);
    // a torn read: 5 is missing but 10 is still under it
    t.observe({ group(0, -1), synth(10, 5) }, 2.0);
    const auto under = t.parentsOfLive();
    CHECK(under.count(5) == 1);
    CHECK(under.count(0) == 1);
    CHECK(under.count(10) == 0);
    // and it is drawn where it hangs, not as a root of its own
    CHECK(t.children(5) == std::vector<int32_t>{ 10 });
    CHECK(t.roots() == std::vector<int32_t>{ 0 });
}

TEST_CASE("expire: a sound goes after a second, a group or fx after 2.5 s", "[nodetree]")
{
    Tree t;
    t.observe({ group(0, -1, 5), group(5, 0, 10), synth(10, 5, 11), synth(11, 5, -1, "sonic-pi-fx_reverb") }, 1.0);
    // 10 ends first
    t.observe({ group(0, -1, 5), group(5, 0, 11), synth(11, 5, -1, "sonic-pi-fx_reverb") }, 2.0);
    CHECK_FALSE(t.expire(2.9));
    CHECK(t.expire(3.0));
    CHECK(t.nodes().count(10) == 0);
    CHECK(t.children(5) == std::vector<int32_t>{ 11 });
    CHECK(t.fading() == false);
    // then the group and its fx
    t.observe({ group(0, -1) }, 4.0);
    CHECK(t.fading());
    CHECK_FALSE(t.expire(6.4));
    CHECK(t.expire(6.5));
    CHECK(t.nodes().count(5) == 0);
    CHECK(t.nodes().count(11) == 0);
    CHECK(t.children(0).empty());
    CHECK_FALSE(t.fading());
}

TEST_CASE("expire: a group is kept while anything under it is still drawn", "[nodetree]")
{
    // scsynth frees a group with everything in it, so this order only comes
    // from a read taken mid-change: 5 missing while 6 is still read under it
    Tree t;
    t.observe({ group(0, -1, 5), group(5, 0, 6), group(6, 5) }, 1.0);
    t.observe({ group(0, -1), group(6, 5) }, 2.0);
    t.observe({ group(0, -1) }, 3.0);                   // now 6 too
    CHECK_FALSE(t.expire(4.6));                          // 5's time has passed, 6's has not
    CHECK(t.nodes().count(5) == 1);
    CHECK(t.expire(5.5));
    CHECK(t.nodes().count(5) == 0);
    CHECK(t.nodes().count(6) == 0);
}

// ── Layout ──────────────────────────────────────────────────────────────────

TEST_CASE("targets: leaves take slots across, parents centre over their children", "[nodetree]")
{
    Tree t;
    // 0 ─┬─ 5 ─┬─ 10
    //    │     └─ 11
    //    └─ 12
    t.observe({ group(0, -1, 5), group(5, 0, 10, 12), synth(10, 5, 11), synth(11, 5), synth(12, 0) }, 1.0);
    const auto x = t.targets();
    CHECK(x.at(10).tx == Approx(0.0));
    CHECK(x.at(11).tx == Approx(0.5));
    CHECK(x.at(12).tx == Approx(1.0));
    CHECK(x.at(5).tx == Approx(0.25));
    CHECK(x.at(0).tx == Approx(0.625));
    CHECK(x.at(0).ty == Approx(0.0));
    CHECK(x.at(5).ty == Approx(0.5));
    CHECK(x.at(10).ty == Approx(1.0));
    CHECK(x.at(12).ty == Approx(0.5));
}

TEST_CASE("targets: a lone node sits in the middle", "[nodetree]")
{
    Tree t;
    t.observe({ group(0, -1) }, 1.0);
    CHECK(t.targets().at(0).tx == Approx(0.5));
    CHECK(t.targets().at(0).ty == Approx(0.0));
}

// ── Drawing ─────────────────────────────────────────────────────────────────

TEST_CASE("radius: by kind, and smaller when the tree is dense", "[nodetree]")
{
    CHECK(radius(Kind::Group, false) == Approx(6.0));
    CHECK(radius(Kind::Fx, false) == Approx(5.0));
    CHECK(radius(Kind::Synth, false) == Approx(2.5));
    CHECK(radius(Kind::Sample, false) == Approx(2.5));
    CHECK(radius(Kind::Group, true) == Approx(4.0));
    CHECK(radius(Kind::Fx, true) == Approx(3.0));
    CHECK(radius(Kind::Synth, true) == Approx(2.0));
}

TEST_CASE("drawnRadius: an ended group or fx shrinks as it fades, a sound does not", "[nodetree]")
{
    Node fx{ 20, 5, Kind::Fx, "sonic-pi-fx_echo", -1 };
    CHECK(drawnRadius(fx, false, 1.0, false) == Approx(5.0));
    fx.ended = 1.0;
    CHECK(drawnRadius(fx, false, 0.5, false) == Approx(5.0 * 0.8));
    CHECK(drawnRadius(fx, false, 0.5, true) == Approx(5.0));   // something live under it
    Node beep{ 21, 5, Kind::Synth, "sonic-pi-beep", 1.0 };
    CHECK(drawnRadius(beep, false, 0.5, false) == Approx(2.5));
}

TEST_CASE("labels: fx by name; groups and sounds have none", "[nodetree]")
{
    // A group is Sonic Pi's plumbing (two for every with_fx) and has only a
    // number, which the tooltip gives.
    CHECK(label({ 0, -1, Kind::Group, "", -1 }).empty());
    CHECK(label({ 1004, 0, Kind::Group, "", -1 }).empty());
    CHECK(label({ 20, 5, Kind::Fx, "sonic-pi-fx_reverb", -1 }) == ":reverb");
    CHECK(label({ 21, 5, Kind::Synth, "sonic-pi-beep", -1 }).empty());
    CHECK(label({ 22, 5, Kind::Sample, "sonic-pi-stereo_player", -1 }).empty());
}

TEST_CASE("labels: only what carries one counts towards too many", "[nodetree]")
{
    CHECK(labelled(Kind::Fx));
    CHECK_FALSE(labelled(Kind::Group));
    CHECK_FALSE(labelled(Kind::Synth));
    CHECK_FALSE(labelled(Kind::Sample));
}

TEST_CASE("labels: off past 60 labelled nodes, back only at 48", "[nodetree]")
{
    CHECK_FALSE(crowded(60, false));
    CHECK(crowded(61, false));
    CHECK(crowded(50, true));
    CHECK(crowded(49, true));
    CHECK_FALSE(crowded(48, true));
}
