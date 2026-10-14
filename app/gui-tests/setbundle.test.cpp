// Tests for SetBundle — the single-file .sonicpi format for a "set" (all ten
// editor buffers saved together). Plain text with #-- marker lines, so the
// file is human-readable, diffable, and almost valid Ruby. Pure QtCore.

#include <catch2/catch_test_macros.hpp>

#include <QFile>
#include <QJsonArray>
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

TEST_CASE("a set written to a file is named after it, as the web names the sets it opens", "[setbundle]")
{
    QTemporaryDir tmp;
    const QString path = tmp.path() + "/Tuesday Gig.sonicpi";

    auto buffers = emptyBuffers();
    buffers[0] = "play 60";
    // a name it had before, from the web, is the file's now
    QJsonObject meta;
    meta["name"] = "Groove";
    meta["size"] = 8;
    REQUIRE(SetBundle::write(path, buffers, 0, defaultZooms(), meta).isEmpty());

    const auto load = SetBundle::read(path);
    REQUIRE(load.ok);
    CHECK(load.meta.value("name").toString() == "Tuesday Gig");
    CHECK(load.meta.value("size").toInt() == 8);
}

TEST_CASE("a set not yet saved keeps the name it came with; a saved one is its file's", "[setbundle]")
{
    QJsonObject meta;
    meta["name"] = "Groove";
    CHECK(SetBundle::namedAfter(QString(), meta).value("name").toString() == "Groove");
    CHECK(SetBundle::namedAfter("/Users/me/Music/Tuesday.sonicpi", meta).value("name").toString() == "Tuesday");
    CHECK_FALSE(SetBundle::namedAfter(QString(), QJsonObject()).contains("name"));
}

TEST_CASE("a set carries its buffers' names, read as the web reads them", "[setbundle]")
{
    auto buffers = emptyBuffers();
    buffers[0] = "play 60";
    buffers[3] = "sample :bd_haus";
    QJsonObject meta;
    meta["names"] = QJsonArray({ "drums", "", "", "  bass  ", 42, QString(100, 'x') });
    const auto load = SetBundle::deserialise(SetBundle::serialise(buffers, 0, defaultZooms(), meta));
    REQUIRE(load.ok);
    REQUIRE(load.names.size() == SetBundle::MaxBuffers);
    CHECK(load.names[0] == "drums");
    CHECK(load.names[1].isEmpty());
    CHECK(load.names[3] == "bass");                       // trimmed
    CHECK(load.names[4].isEmpty());                       // not a string: no name
    CHECK(load.names[5] == QString(80, 'x'));             // at most 80 characters
    CHECK(load.names[9].isEmpty());

    // and none, when the set says none
    CHECK(SetBundle::deserialise("#-- buffer 0\nplay 60\n").names == QVector<QString>(SetBundle::MaxBuffers));
}

TEST_CASE("reading a missing file fails with an error", "[setbundle]")
{
    QTemporaryDir tmp;
    const auto load = SetBundle::read(tmp.path() + "/nowhere.sonicpi");
    CHECK_FALSE(load.ok);
    CHECK_FALSE(load.error.isEmpty());
}

TEST_CASE("a file with neither a header nor a buffer marker is not a set", "[setbundle]")
{
    CHECK_FALSE(SetBundle::deserialise("").ok);
    CHECK_FALSE(SetBundle::deserialise("play 60\nsleep 1").ok);
    CHECK(SetBundle::deserialise("play 60\nsleep 1").error == "not a Sonic Pi set file");
}

TEST_CASE("a set with every buffer empty saves and opens again", "[setbundle]")
{
    const QString text = SetBundle::serialise(emptyBuffers(), 4, defaultZooms());
    const auto load = SetBundle::deserialise(text);
    REQUIRE(load.ok);
    CHECK(load.currentBuffer == 4);
    CHECK(load.buffers == emptyBuffers());

    // the header alone says it is a set, as the first line: code that ends
    // with it is still not one
    CHECK(SetBundle::deserialise("#-- Sonic Pi Set v1\n").ok);
    CHECK_FALSE(SetBundle::deserialise("play 60\n#-- Sonic Pi Set v1\n").ok);
}

TEST_CASE("a byte order mark before the header is read past", "[setbundle]")
{
    const auto load = SetBundle::deserialise(QString(QChar(0xFEFF)) + "#-- Sonic Pi Set v1\n#-- meta {\"current\":2}\n#-- buffer 2\nplay 60\n");
    REQUIRE(load.ok);
    CHECK(load.currentBuffer == 2);
    CHECK(load.buffers[2] == "play 60");
    CHECK_FALSE(SetBundle::deserialise(QString(QChar(0xFEFF)) + "#-- Sonic Pi Set v2\n#-- buffer 0\nplay 60\n").ok);
}

TEST_CASE("a set is told from a buffer's code by its header or its name", "[setbundle]")
{
    CHECK(SetBundle::isSet("#-- Sonic Pi Set v1\n#-- buffer 0\nplay 60\n"));
    CHECK(SetBundle::isSet(QString(QChar(0xFEFF)) + "#-- Sonic Pi Set v1\n"));
    CHECK(SetBundle::isSet("#-- Sonic Pi Set v1\n", "drums.txt"));
    CHECK(SetBundle::isSet("#-- Sonic Pi Set v1"));
    CHECK(SetBundle::isSet("#-- Sonic Pi Set v1\r\n#-- buffer 0\r\nplay 60\r\n"));
    // a newer version's is a set too, so it is refused as one rather than read as code
    CHECK(SetBundle::isSet("#-- Sonic Pi Set v12\n#-- buffer 0\nplay 60\n", "drums.txt"));

    // a .sonicpi file is a set whatever it holds, so a hand-made one without the header opens as one
    CHECK(SetBundle::isSet("#-- buffer 0\nplay 60\n", "drums.sonicpi"));
    CHECK(SetBundle::isSet("", "/Users/me/Music/DRUMS.SONICPI"));

    CHECK_FALSE(SetBundle::isSet("play 60\n"));
    CHECK_FALSE(SetBundle::isSet("play 60\n", "drums.rb"));
    CHECK_FALSE(SetBundle::isSet("play 60\n#-- Sonic Pi Set v1\n", "drums.txt"));
    CHECK_FALSE(SetBundle::isSet("#-- buffer 0\nplay 60\n", "drums.txt"));
    CHECK_FALSE(SetBundle::isSet("play 60\n", "drums.sonicpi.txt"));
    CHECK_FALSE(SetBundle::isSet("#-- Sonic Pi Set v1 and more\n"));
    CHECK_FALSE(SetBundle::isSet(" #-- Sonic Pi Set v1\n"));
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

    // a buffer number too big to count is no buffer, not buffer 0
    CHECK_FALSE(SetBundle::deserialise("#-- buffer 4294967296\nplay 60\n").ok);
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

TEST_CASE("the header names the format's version", "[setbundle]")
{
    auto buffers = emptyBuffers();
    buffers[0] = "play 60";
    CHECK(SetBundle::serialise(buffers, 0, defaultZooms()).startsWith(QString("#-- Sonic Pi Set v%1\n").arg(SetBundle::Version)));
}

TEST_CASE("a set from a newer version is refused, not misread", "[setbundle]")
{
    const auto load = SetBundle::deserialise(QString("#-- Sonic Pi Set v%1\n#-- buffer 0\nplay 60\n").arg(SetBundle::Version + 1));
    CHECK_FALSE(load.ok);
    CHECK(load.error.contains("newer version of Sonic Pi"));

    // a version too big to count is newer still
    CHECK_FALSE(SetBundle::deserialise("#-- Sonic Pi Set v99999999999\n#-- buffer 0\nplay 60\n").ok);

    // this version, and the files of versions before it, still open
    CHECK(SetBundle::deserialise(QString("#-- Sonic Pi Set v%1\n#-- buffer 0\nplay 60\n").arg(SetBundle::Version)).ok);
    CHECK(SetBundle::deserialise("#-- Sonic Pi Set v0\n#-- buffer 0\nplay 60\n").ok);
}

TEST_CASE("meta keys this version does not know are kept through a load and a save", "[setbundle]")
{
    const auto load = SetBundle::deserialise(
        "#-- Sonic Pi Set v1\n"
        "#-- meta {\"current\":1,\"names\":[\"drums\",\"bass\"],\"web\":{\"fontSize\":20},\"zooms\":[2,3]}\n"
        "#-- buffer 1\nplay 60\n");
    REQUIRE(load.ok);
    CHECK(load.meta.value("web").toObject().value("fontSize").toInt() == 20);

    // written back beside the current buffer and zooms, which the save sets afresh
    const auto again = SetBundle::deserialise(SetBundle::serialise(load.buffers, 4, load.zooms, load.meta));
    REQUIRE(again.ok);
    CHECK(again.currentBuffer == 4);
    CHECK(again.zooms[1] == 3);
    CHECK(again.meta.value("names").toArray().size() == 2);
    CHECK(again.meta.value("names").toArray().at(1).toString() == "bass");
    CHECK(again.meta.value("web").toObject().value("fontSize").toInt() == 20);
}
