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
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QRegularExpression>

namespace SonicPi
{

static const QString headerLine = QStringLiteral("#-- Sonic Pi Set v1");
static const QString metaPrefix = QStringLiteral("#-- meta ");
// Content lines starting "#--" are written behind this prefix so they can't
// be mistaken for markers; the reader strips exactly one.
static const QString escapePrefix = QStringLiteral("#-- ~");
static const QRegularExpression bufferMarker(QStringLiteral("^#-- buffer (\\d+)$"));

static int clampZoom(int zoom)
{
    if (zoom < SetBundle::MinZoom) return SetBundle::MinZoom;
    if (zoom > SetBundle::MaxZoom) return SetBundle::MaxZoom;
    return zoom;
}

QString SetBundle::serialise(const QVector<QString>& buffers, int currentBuffer,
                             const QVector<int>& zooms)
{
    QJsonArray metaZooms;
    for (int i = 0; i < MaxBuffers; i++)
    {
        metaZooms.append(clampZoom(i < zooms.size() ? zooms[i] : DefaultZoom));
    }
    QJsonObject meta;
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

    QString body = text;
    body.replace(QStringLiteral("\r\n"), QStringLiteral("\n"));
    // serialise ends every section with exactly one newline; drop it so the
    // final buffer doesn't grow a phantom trailing line.
    if (body.endsWith(QLatin1Char('\n')))
    {
        body.chop(1);
    }

    int currentBuffer = -1;
    QVector<QStringList> sections(MaxBuffers);
    bool anyBuffer = false;

    for (const QString& line : body.split(QLatin1Char('\n')))
    {
        if (line.startsWith(escapePrefix) && currentBuffer >= 0)
        {
            sections[currentBuffer].append(line.mid(escapePrefix.size()));
            continue;
        }
        const auto marker = bufferMarker.match(line);
        if (marker.hasMatch())
        {
            const int i = marker.captured(1).toInt();
            currentBuffer = (i >= 0 && i < MaxBuffers) ? i : -1;
            if (currentBuffer >= 0)
            {
                anyBuffer = true;
            }
            continue;
        }
        if (currentBuffer < 0)
        {
            if (line.startsWith(metaPrefix))
            {
                const QJsonDocument doc = QJsonDocument::fromJson(line.mid(metaPrefix.size()).toUtf8());
                if (doc.isObject())
                {
                    const QJsonObject meta = doc.object();
                    const int current = meta[QStringLiteral("current")].toInt(0);
                    load.currentBuffer = (current >= 0 && current < MaxBuffers) ? current : 0;
                    const QJsonArray zooms = meta[QStringLiteral("zooms")].toArray();
                    for (int i = 0; i < MaxBuffers && i < zooms.size(); i++)
                    {
                        load.zooms[i] = clampZoom(zooms[i].toInt(DefaultZoom));
                    }
                }
            }
            continue;
        }
        sections[currentBuffer].append(line);
    }

    if (!anyBuffer)
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

QString SetBundle::write(const QString& path, const QVector<QString>& buffers, int currentBuffer,
                         const QVector<int>& zooms)
{
    QFile f(path);
    if (!f.open(QFile::WriteOnly))
    {
        return QStringLiteral("could not write %1: %2").arg(path, f.errorString());
    }
    f.write(serialise(buffers, currentBuffer, zooms).toUtf8());
    return QString();
}

SetBundle::Load SetBundle::read(const QString& path)
{
    QFile f(path);
    if (!f.open(QFile::ReadOnly))
    {
        Load load;
        load.buffers = QVector<QString>(MaxBuffers);
        load.zooms = QVector<int>(MaxBuffers, DefaultZoom);
        load.error = QStringLiteral("could not read %1: %2").arg(path, f.errorString());
        return load;
    }
    return deserialise(QString::fromUtf8(f.readAll()));
}

} // namespace SonicPi
