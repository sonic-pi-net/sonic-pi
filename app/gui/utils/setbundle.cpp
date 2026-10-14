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

#include "setbundle.h"

#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QRegularExpression>

namespace SonicPi
{

static const QString headerLine = QStringLiteral("#-- Sonic Pi Set v%1").arg(SetBundle::Version);
static const QRegularExpression headerMarker(QStringLiteral("^#-- Sonic Pi Set v(\\d+)$"));
static const QString metaPrefix = QStringLiteral("#-- meta ");
// Content lines starting "#--" are written behind this prefix so they can't
// be mistaken for markers; the reader strips exactly one.
static const QString escapePrefix = QStringLiteral("#-- ~");
static const QRegularExpression bufferMarker(QStringLiteral("^#-- buffer (\\d+)$"));
static const QChar byteOrderMark(0xFEFF);

static int clampZoom(int zoom)
{
    if (zoom < SetBundle::MinZoom) return SetBundle::MinZoom;
    if (zoom > SetBundle::MaxZoom) return SetBundle::MaxZoom;
    return zoom;
}

QString SetBundle::serialise(const QVector<QString>& buffers, int currentBuffer,
                             const QVector<int>& zooms, const QJsonObject& keep)
{
    QJsonArray metaZooms;
    for (int i = 0; i < MaxBuffers; i++)
    {
        metaZooms.append(clampZoom(i < zooms.size() ? zooms[i] : DefaultZoom));
    }
    QJsonObject meta = keep;
    meta[QStringLiteral("current")] = currentBuffer;
    meta[QStringLiteral("zooms")] = metaZooms;

    QString out = headerLine + QLatin1Char('\n')
        + metaPrefix + QString::fromUtf8(QJsonDocument(meta).toJson(QJsonDocument::Compact))
        + QLatin1Char('\n');

    for (int i = 0; i < MaxBuffers && i < buffers.size(); i++)
    {
        if (buffers[i].trimmed().isEmpty())
        {
            continue;
        }
        out += QStringLiteral("#-- buffer %1\n").arg(i);
        const QStringList lines = buffers[i].split(QLatin1Char('\n'));
        for (const QString& line : lines)
        {
            if (line.startsWith(QStringLiteral("#--")))
            {
                out += escapePrefix;
            }
            out += line;
            out += QLatin1Char('\n');
        }
    }
    return out;
}

SetBundle::Load SetBundle::deserialise(const QString& text)
{
    Load load;
    load.buffers = QVector<QString>(MaxBuffers);
    load.zooms = QVector<int>(MaxBuffers, DefaultZoom);
    load.names = QVector<QString>(MaxBuffers);

    QString body = text;
    if (body.startsWith(byteOrderMark))
    {
        body.remove(0, 1);
    }
    body.replace(QStringLiteral("\r\n"), QStringLiteral("\n"));
    // serialise ends every section with exactly one newline; drop it so the
    // final buffer doesn't grow a phantom trailing line.
    if (body.endsWith(QLatin1Char('\n')))
    {
        body.chop(1);
    }

    int currentBuffer = -1;
    QVector<QStringList> sections(MaxBuffers);
    // a buffer marker, or the header as the first line: a set whose buffers
    // are all empty has only its header and meta
    bool found = false;

    const QStringList lines = body.split(QLatin1Char('\n'));
    for (qsizetype n = 0; n < lines.size(); n++)
    {
        const QString& line = lines[n];
        if (line.startsWith(escapePrefix) && currentBuffer >= 0)
        {
            sections[currentBuffer].append(line.mid(escapePrefix.size()));
            continue;
        }
        const auto marker = bufferMarker.match(line);
        if (marker.hasMatch())
        {
            bool inRange = false;
            const int i = marker.captured(1).toInt(&inRange);
            currentBuffer = (inRange && i >= 0 && i < MaxBuffers) ? i : -1;
            if (currentBuffer >= 0)
            {
                found = true;
            }
            continue;
        }
        if (currentBuffer < 0)
        {
            const auto header = headerMarker.match(line);
            if (header.hasMatch())
            {
                bool inRange = false;
                const int version = header.captured(1).toInt(&inRange);
                if (!inRange || version > Version)   // too big for an int is newer still
                {
                    load.error = QStringLiteral("it was made by a newer version of Sonic Pi (set format v%1, where this one reads up to v%2): update Sonic Pi to open it")
                        .arg(header.captured(1)).arg(Version);
                    return load;
                }
                found = found || n == 0;
            }
            if (line.startsWith(metaPrefix))
            {
                const QJsonDocument doc = QJsonDocument::fromJson(line.mid(metaPrefix.size()).toUtf8());
                if (doc.isObject())
                {
                    const QJsonObject meta = doc.object();
                    load.meta = meta;
                    const int current = meta[QStringLiteral("current")].toInt(0);
                    load.currentBuffer = (current >= 0 && current < MaxBuffers) ? current : 0;
                    const QJsonArray zooms = meta[QStringLiteral("zooms")].toArray();
                    for (int i = 0; i < MaxBuffers && i < zooms.size(); i++)
                    {
                        load.zooms[i] = clampZoom(zooms[i].toInt(DefaultZoom));
                    }
                    const QJsonArray names = meta[QStringLiteral("names")].toArray();
                    for (int i = 0; i < MaxBuffers && i < names.size(); i++)
                    {
                        load.names[i] = names[i].toString().trimmed().left(NameMax);
                    }
                }
            }
            continue;
        }
        sections[currentBuffer].append(line);
    }

    if (!found)
    {
        load.error = QStringLiteral("not a Sonic Pi set file");
        return load;
    }

    for (int i = 0; i < MaxBuffers; i++)
    {
        if (!sections[i].isEmpty())
        {
            load.buffers[i] = sections[i].join(QLatin1Char('\n'));
        }
    }

    load.ok = true;
    return load;
}

bool SetBundle::isSet(const QString& text, const QString& fileName)
{
    if (fileName.endsWith(QStringLiteral(".sonicpi"), Qt::CaseInsensitive))
    {
        return true;
    }
    // the header of any version: a newer set is refused as a set, not read as code
    QStringView first = text.startsWith(byteOrderMark) ? QStringView(text).mid(1) : QStringView(text);
    first = first.left(first.indexOf(QLatin1Char('\n')));
    if (first.endsWith(QLatin1Char('\r')))
    {
        first.chop(1);
    }
    return headerMarker.match(first.toString()).hasMatch();   // not matchView: the i686 build is on Qt 6.4
}

QString SetBundle::write(const QString& path, const QVector<QString>& buffers, int currentBuffer,
                         const QVector<int>& zooms, const QJsonObject& keep)
{
    QFile f(path);
    if (!f.open(QFile::WriteOnly))
    {
        return QStringLiteral("could not write %1: %2").arg(path, f.errorString());
    }
    f.write(serialise(buffers, currentBuffer, zooms, namedAfter(path, keep)).toUtf8());
    return QString();
}

QJsonObject SetBundle::namedAfter(const QString& path, const QJsonObject& keep)
{
    QJsonObject meta = keep;
    if (!path.isEmpty())
    {
        meta[QStringLiteral("name")] = QFileInfo(path).completeBaseName();
    }
    return meta;
}

SetBundle::Load SetBundle::read(const QString& path)
{
    QFile f(path);
    if (!f.open(QFile::ReadOnly))
    {
        Load load;
        load.buffers = QVector<QString>(MaxBuffers);
        load.zooms = QVector<int>(MaxBuffers, DefaultZoom);
        load.names = QVector<QString>(MaxBuffers);
        load.error = QStringLiteral("could not read %1: %2").arg(path, f.errorString());
        return load;
    }
    return deserialise(QString::fromUtf8(f.readAll()));
}

} // namespace SonicPi
