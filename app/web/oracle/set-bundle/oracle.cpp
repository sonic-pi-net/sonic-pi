// SPDX-License-Identifier: AGPL-3.0-or-later
// The oracle for sets: native Sonic Pi's own SetBundle (app/gui/utils/setbundle.cpp), fed cases as JSON on stdin,
// what it makes of each as JSON on stdout. scripts/gen-set-fixtures.mjs builds and runs it, and writes
// app/test/fixtures/set-bundle.json, which app/test/set-bundle.test.mjs holds the web's set-bundle.js to.
//
//   a case {buffers, current, zooms, meta} → {text}: the set native writes
//   a case {text}                          → {ok, error, buffers, zooms, current, meta}: what native reads
#include "setbundle.h"
#include <QCoreApplication>
#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QTextStream>
#include <cstdio>
using namespace SonicPi;
int main(int argc, char** argv) {
  QCoreApplication app(argc, argv);
  QFile in;
  if (!in.open(stdin, QFile::ReadOnly)) return 1;
  QJsonArray cases = QJsonDocument::fromJson(in.readAll()).array(), out;
  for (const auto& c : cases) {
    QJsonObject o = c.toObject(), r;
    if (o.contains("text")) {
      auto l = SetBundle::deserialise(o["text"].toString());
      QJsonArray b, z; for (auto& s : l.buffers) b.append(s); for (int v : l.zooms) z.append(v);
      r["ok"] = l.ok; r["error"] = l.error; r["buffers"] = b; r["zooms"] = z; r["current"] = l.currentBuffer; r["meta"] = l.meta;
    } else {
      QVector<QString> b; QVector<int> z;
      for (auto v : o["buffers"].toArray()) b.append(v.toString());
      for (auto v : o["zooms"].toArray()) z.append(v.toInt());
      r["text"] = SetBundle::serialise(b, o["current"].toInt(), z, o["meta"].toObject());
    }
    out.append(r);
  }
  fwrite(QJsonDocument(out).toJson(QJsonDocument::Compact).constData(), 1, QJsonDocument(out).toJson(QJsonDocument::Compact).size(), stdout);
}
