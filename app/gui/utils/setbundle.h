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

#include <QJsonObject>
#include <QString>
#include <QVector>

namespace SonicPi
{

// A "set" is all ten editor buffers saved together as one .sonicpi file:
// plain text with #-- marker lines (header, meta JSON, one per non-empty
// buffer). Markers are Ruby comments, keeping the file readable and diffable.
// The web app reads and writes the same files (app/web/app/src/set-bundle.js).
//
// Room to grow: the header names the format's version, and a file from a newer
// version is refused rather than misread. Anything new goes in the meta, whose
// keys this version does not know are kept, read and written back as they
// came, so a set passed through an older app or the web loses none of them.
struct SetBundle
{
    static constexpr int MaxBuffers = 10;
    static constexpr int Version = 1;

    // Editor zoom bounds; DefaultZoom must stay in step with
    // SonicPiScintilla::kDefaultZoom.
    static constexpr int DefaultZoom = 2;
    static constexpr int MinZoom = -5;
    static constexpr int MaxZoom = 20;

    struct Load
    {
        bool ok = false;
        QString error;
        QVector<QString> buffers;
        QVector<int> zooms;
        int currentBuffer = 0;
        // the meta as read, keys this version does not know included: hand it
        // back to serialise/write to keep them
        QJsonObject meta;
    };

    // meta: keys to keep beside current and zooms (which these arguments set)
    static QString serialise(const QVector<QString>& buffers, int currentBuffer,
                             const QVector<int>& zooms, const QJsonObject& meta = QJsonObject());
    static Load deserialise(const QString& text);

    // File wrappers around serialise/deserialise. write returns an empty
    // string on success, else a message describing the failure.
    static QString write(const QString& path, const QVector<QString>& buffers, int currentBuffer,
                         const QVector<int>& zooms, const QJsonObject& meta = QJsonObject());
    static Load read(const QString& path);
};

} // namespace SonicPi
