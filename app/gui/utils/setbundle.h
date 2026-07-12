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

#include <QString>
#include <QVector>

namespace SonicPi
{

// A "set" is all ten editor buffers saved together as one .sonicpi file:
// plain text with #-- marker lines (header, meta JSON, one per non-empty
// buffer). Markers are Ruby comments, keeping the file readable and diffable.
struct SetBundle
{
    static constexpr int MaxBuffers = 10;

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
    };

    static QString serialise(const QVector<QString>& buffers, int currentBuffer,
                             const QVector<int>& zooms);
    static Load deserialise(const QString& text);

    // File wrappers around serialise/deserialise. write returns an empty
    // string on success, else a message describing the failure.
    static QString write(const QString& path, const QVector<QString>& buffers, int currentBuffer,
                         const QVector<int>& zooms);
    static Load read(const QString& path);
};

} // namespace SonicPi
