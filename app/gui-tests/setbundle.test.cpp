// Tests for SetBundle — the single-file .sonicpi format for a "set" (all ten
// editor buffers saved together). Plain text with #-- marker lines, so the
// file is human-readable, diffable, and almost valid Ruby. Pure QtCore.

#include <catch2/catch_test_macros.hpp>

#include <QFile>
#include <QTemporaryDir>

#include "utils/setbundle.h"

using SonicPi::SetBundle;

static QVector<QString> emptyBuffers()
{
    return QVector<QString>(SetBundle::MaxBuffers);
}

static QVector<int> defaultZooms()
{
    return QVector<int>(SetBundle::MaxBuffers, SetBundle::DefaultZoom);
}

static void writeRaw(const QString& path, const QByteArray& bytes)
{
    QFile f(path);
    REQUIRE(f.open(QFile::WriteOnly));
    f.write(bytes);
}

TEST_CASE("set round-trips buffers, zooms and current index", "[setbundle]")
{
    auto buffers = emptyBuffers();
    buffers[0] = "play 60";
    buffers[3] = "live_loop :drums do\n  sample :bd_haus\n  sleep 0.5\nend";
    buffers[9] = "# ambient pads éè❤";
    auto zooms = defaultZooms();
    zooms[3] = 7;
    zooms[9] = -4;

    const QString text = SetBundle::serialise(buffers, 3, zooms);
    const auto load = SetBundle::deserialise(text);

    REQUIRE(load.ok);
    CHECK(load.currentBuffer == 3);
    REQUIRE(load.buffers.size() == SetBundle::MaxBuffers);
    CHECK(load.buffers[0] == buffers[0]);
    CHECK(load.buffers[3] == buffers[3]);
    CHECK(load.buffers[9] == buffers[9]);
    CHECK(load.buffers[1].isEmpty());
    CHECK(load.zooms[3] == 7);
    CHECK(load.zooms[9] == -4);
    CHECK(load.zooms[0] == SetBundle::DefaultZoom);
}

TEST_CASE("trailing newlines survive round-trips exactly", "[setbundle]")
{
    auto buffers = emptyBuffers();
    buffers[0] = "play 60";        // no trailing newline
    buffers[1] = "play 61\n";      // one trailing newline
    buffers[2] = "play 62\n\n\n";  // several

    const auto load = SetBundle::deserialise(SetBundle::serialise(buffers, 0, defaultZooms()));
    REQUIRE(load.ok);
    CHECK(load.buffers[0] == buffers[0]);
    CHECK(load.buffers[1] == buffers[1]);
    CHECK(load.buffers[2] == buffers[2]);

    // and a second cycle changes nothing
    const auto again = SetBundle::deserialise(SetBundle::serialise(load.buffers, 0, load.zooms));
    REQUIRE(again.ok);
    CHECK(again.buffers == load.buffers);
}

TEST_CASE("marker-like lines inside code are escaped and restored", "[setbundle]")
{
    auto buffers = emptyBuffers();
    buffers[0] = "play 60\n#-- buffer 5\nplay 62";
    buffers[5] = "#-- Sonic Pi Set v1";
    buffers[6] = "#-- ~already escaped-looking\n#-- meta {}";

    const auto load = SetBundle::deserialise(SetBundle::serialise(buffers, 0, defaultZooms()));
    REQUIRE(load.ok);
    CHECK(load.buffers[0] == buffers[0]);
    CHECK(load.buffers[5] == buffers[5]);
    CHECK(load.buffers[6] == buffers[6]);
    // the collision must not have leaked an extra buffer
    CHECK(load.buffers[1].isEmpty());
}

TEST_CASE("empty and whitespace-only buffers are omitted from the file", "[setbundle]")
{
    auto buffers = emptyBuffers();
    buffers[2] = "play 60";
    buffers[4] = "   \n\t\n";

    const QString text = SetBundle::serialise(buffers, 0, defaultZooms());
    CHECK(text.count("#-- buffer") == 1);

    const auto load = SetBundle::deserialise(text);
    REQUIRE(load.ok);
    CHECK(load.buffers[2] == "play 60");
    CHECK(load.buffers[4].isEmpty());
}

TEST_CASE("the serialised file is readable plain text", "[setbundle]")
{
    auto buffers = emptyBuffers();
    buffers[0] = "play 60";

    const QString text = SetBundle::serialise(buffers, 0, defaultZooms());
    CHECK(text.startsWith("#-- Sonic Pi Set v1\n"));
    CHECK(text.contains("#-- meta {"));
    CHECK(text.contains("#-- buffer 0\nplay 60\n"));
}

TEST_CASE("file write/read round-trip", "[setbundle]")
{
    QTemporaryDir tmp;
    const QString path = tmp.path() + "/my-track.sonicpi";

    auto buffers = emptyBuffers();
    buffers[1] = "play 60";
    REQUIRE(SetBundle::write(path, buffers, 1, defaultZooms()).isEmpty());

    const auto load = SetBundle::read(path);
    REQUIRE(load.ok);
    CHECK(load.buffers[1] == "play 60");
    CHECK(load.currentBuffer == 1);
}

TEST_CASE("reading a missing file fails with an error", "[setbundle]")
{
    QTemporaryDir tmp;
    const auto load = SetBundle::read(tmp.path() + "/nowhere.sonicpi");
    CHECK_FALSE(load.ok);
    CHECK_FALSE(load.error.isEmpty());
}

TEST_CASE("a file with no buffers is rejected", "[setbundle]")
{
    CHECK_FALSE(SetBundle::deserialise("").ok);
    CHECK_FALSE(SetBundle::deserialise("play 60\nsleep 1").ok);
    CHECK_FALSE(SetBundle::deserialise("#-- Sonic Pi Set v1\n#-- meta {}\n").ok);
}

TEST_CASE("missing or corrupt meta falls back to defaults", "[setbundle]")
{
    const auto noMeta = SetBundle::deserialise("#-- Sonic Pi Set v1\n#-- buffer 0\nplay 60\n");
    REQUIRE(noMeta.ok);
    CHECK(noMeta.currentBuffer == 0);
    CHECK(noMeta.zooms == defaultZooms());

    const auto badMeta = SetBundle::deserialise(
        "#-- Sonic Pi Set v1\n#-- meta { not json !!!\n#-- buffer 2\nplay 60\n");
    REQUIRE(badMeta.ok);
    CHECK(badMeta.buffers[2] == "play 60");
    CHECK(badMeta.currentBuffer == 0);
}

TEST_CASE("hostile meta values are clamped or ignored", "[setbundle]")
{
    const auto load = SetBundle::deserialise(
        "#-- Sonic Pi Set v1\n"
        "#-- meta {\"current\":42,\"zooms\":[999,-999,\"cheese\"]}\n"
        "#-- buffer 0\nplay 60\n"
        "#-- buffer 99\nignored\n");
    REQUIRE(load.ok);
    CHECK(load.currentBuffer == 0);
    CHECK(load.zooms[0] == SetBundle::MaxZoom);
    CHECK(load.zooms[1] == SetBundle::MinZoom);
    CHECK(load.zooms[2] == SetBundle::DefaultZoom);
    CHECK(load.buffers[0] == "play 60");
}

TEST_CASE("a headerless file with buffer markers still loads", "[setbundle]")
{
    const auto load = SetBundle::deserialise("#-- buffer 4\nplay 60\n");
    REQUIRE(load.ok);
    CHECK(load.buffers[4] == "play 60");
}

TEST_CASE("CRLF files are normalised on read", "[setbundle]")
{
    QTemporaryDir tmp;
    const QString path = tmp.path() + "/crlf.sonicpi";
    writeRaw(path, "#-- Sonic Pi Set v1\r\n#-- buffer 0\r\nplay 60\r\nsleep 1\r\n");

    const auto load = SetBundle::read(path);
    REQUIRE(load.ok);
    CHECK(load.buffers[0] == "play 60\nsleep 1");
}

TEST_CASE("a hand-edited file without a final newline still parses", "[setbundle]")
{
    const auto load = SetBundle::deserialise("#-- buffer 0\nplay 60");
    REQUIRE(load.ok);
    CHECK(load.buffers[0] == "play 60");
}
