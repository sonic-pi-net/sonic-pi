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

#include "sampleheaderinfo.h"

#include <QFile>
#include <cmath>

namespace SonicPi
{

QString sampleHeaderInfo(const QString& path) {
  QFile f(path);
  if (!f.open(QIODevice::ReadOnly)) return QString();
  const QByteArray magic = f.read(4);

  double seconds = 0;
  int channels = 0, rate = 0;

  auto be16 = [](const uchar* p) { return (p[0] << 8) | p[1]; };
  auto be32 = [](const uchar* p) {
    return (quint32(p[0]) << 24) | (p[1] << 16) | (p[2] << 8) | p[3];
  };

  if (magic == "fLaC") {
    // First metadata block is always STREAMINFO: 4-byte block header, then
    // min/max blocksize (4) + min/max framesize (6), then 8 bytes packing
    // rate:20 | channels-1:3 | bits-1:5 | totalSamples:36.
    f.seek(4 + 4 + 10);
    const QByteArray b = f.read(8);
    if (b.size() == 8) {
      const uchar* p = reinterpret_cast<const uchar*>(b.constData());
      rate = (p[0] << 12) | (p[1] << 4) | (p[2] >> 4);
      channels = ((p[2] >> 1) & 0x7) + 1;
      const quint64 total = (quint64(p[3] & 0x0F) << 32) | (quint64(p[4]) << 24)
                            | (p[5] << 16) | (p[6] << 8) | p[7];
      if (rate > 0) seconds = double(total) / rate;
    }
  } else if (magic == "RIFF") {
    f.seek(8);  // past RIFF size + "WAVE"
    if (f.read(4) == "WAVE") {
      quint32 byteRate = 0;
      while (!f.atEnd()) {
        const QByteArray hdr = f.read(8);
        if (hdr.size() < 8) break;
        const quint32 size = quint32(uchar(hdr[4])) | (uchar(hdr[5]) << 8)
                             | (uchar(hdr[6]) << 16) | (quint32(uchar(hdr[7])) << 24);
        if (hdr.startsWith("fmt ")) {
          const QByteArray fmt = f.read(qMin<quint32>(size, 16));
          if (fmt.size() >= 16) {
            const uchar* p = reinterpret_cast<const uchar*>(fmt.constData());
            channels = p[2] | (p[3] << 8);
            rate = p[4] | (p[5] << 8) | (p[6] << 16) | (p[7] << 24);
            byteRate = p[8] | (p[9] << 8) | (p[10] << 16) | (quint32(p[11]) << 24);
          }
          if (size > 16) f.seek(f.pos() + size - 16);
        } else if (hdr.startsWith("data")) {
          if (byteRate > 0) seconds = double(size) / byteRate;
          break;
        } else {
          f.seek(f.pos() + size + (size & 1));
        }
      }
    }
  } else if (magic == "FORM") {
    f.seek(8);
    const QByteArray form = f.read(4);
    if (form == "AIFF" || form == "AIFC") {
      while (!f.atEnd()) {
        const QByteArray hdr = f.read(8);
        if (hdr.size() < 8) break;
        const uchar* hp = reinterpret_cast<const uchar*>(hdr.constData());
        const quint32 size = be32(hp + 4);
        if (hdr.startsWith("COMM")) {
          const QByteArray c = f.read(qMin<quint32>(size, 18));
          if (c.size() >= 18) {
            const uchar* p = reinterpret_cast<const uchar*>(c.constData());
            channels = be16(p);
            const quint32 frames = be32(p + 2);
            // Sample rate is an 80-bit extended float: 1|15 exponent, 64 mantissa.
            const int exp = be16(p + 8) & 0x7FFF;
            quint64 mant = 0;
            for (int i = 0; i < 8; ++i) mant = (mant << 8) | p[10 + i];
            const double r = double(mant) * std::pow(2.0, exp - 16383 - 63);
            rate = qRound(r);
            if (rate > 0) seconds = double(frames) / rate;
          }
          break;
        }
        f.seek(f.pos() + size + (size & 1));
      }
    }
  }

  if (seconds <= 0 || channels <= 0 || rate <= 0) return QString();
  const QString len = seconds < 10 ? QString::number(seconds, 'f', 2)
                                   : QString::number(seconds, 'f', 1);
  const QString ch = channels == 1 ? QStringLiteral("mono")
                   : channels == 2 ? QStringLiteral("stereo")
                                   : QStringLiteral("%1ch").arg(channels);
  return QStringLiteral("%1s · %2 · %3 kHz")
      .arg(len, ch, QString::number(rate / 1000.0, 'g', 3));
}

} // namespace SonicPi
