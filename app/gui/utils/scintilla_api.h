//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

#include <Qsci/qsciabstractapis.h>
#include <QHash>
#include <QList>
#include <functional>

// A single completion candidate for the custom popup: the text inserted/shown,
// a kind tag ("synth", "fx", "fn", "opt", "sample", "cue", …) used for the icon,
// and an optional one-line summary shown dimmed beside the name.
struct CompletionItem
{
    QString text;
    QString kind;
    QString summary;          // one-line summary (header)
    QString doc;              // full docstring (markdown), for the detail pane
    int note = -1;            // MIDI note number for kind=="note", else -1
    // For kind=="range": a slider value-picker over [rmin, rmax] (e.g. pan: -1..1).
    bool slider = false;
    double rmin = 0, rmax = 0, rdefault = 0;
};

class ScintillaAPI : public QsciAbstractAPIs
{
 public:
  enum { Func, FX, Synth, Sample, Chord, Scale, MCBlock, PlayParam, SampleParam, Tuning, Examples, MidiParam, MidiOuts, CuePath, RandomSource, LinkAudioPeer, LinkAudioChannel, NContext};

  ScintillaAPI(QsciLexer *lexer);

  void addSymbol(int context, QString sym);
  void addKeyword(int context, QString keyword);
  void addFXArgs(QString fx, QStringList args);
  void addSynthArgs(QString fx, QStringList args);
  void addCuePath(QString path);
  void loadSamples(QString sample_path);
  void updateMidiOuts(QString port_info);
  void updateLinkAudioStreams(const QStringList& peers, const QStringList& channels);
  void setPlayArgs(const QStringList& args);
  void setSampleArgs(const QStringList& args);
  // Resolver returning the synth in effect at the cursor (e.g. "dsaw" set by
  // use_synth), so `play` completes only that synth's opts. Empty = unknown.
  void setSynthResolver(std::function<QString()> resolver);

  // Register a one-line summary for a completion entry (name as it appears in
  // the list, e.g. ":reverb" / "play"). Used by the custom completion popup.
  void setSummary(const QString& name, const QString& summary);
  // Register the full docstring (markdown) for a completion entry.
  void setDoc(const QString& name, const QString& doc);
  // Register a numeric range for a bounded opt (e.g. "pan:", -1, 1, 0) so its
  // value position offers a slider instead of a list.
  void setOptRange(const QString& name, double lo, double hi, double def);

  // Richer query for the custom popup: same context logic as
  // updateAutoCompletionList, but each candidate carries its kind + summary.
  QList<CompletionItem> completionsFor(const QStringList& context);


  //! \reimp
  virtual void updateAutoCompletionList(const QStringList &context,
					QStringList &list);

  virtual QStringList callTips(const QStringList &context, int commas,
			       QsciScintilla::CallTipsStyle style,
			       QList<int> &shifts);


 private:
  // Maps the final completion context (after the switch in
  // updateAutoCompletionList) to a kind tag for the popup.
  static QString kindForContext(int ctx);

  QStringList keywords[NContext];
  QHash<QString, QStringList> fxArgs;
  QHash<QString, QStringList> synthArgs;
  QHash<QString, QString> summaries;
  QHash<QString, QString> docs;
  struct OptRange { double lo, hi, def; };
  QHash<QString, OptRange> optRanges;
  std::function<QString()> synthResolver;
  QString lastKind; // kind resolved by the most recent updateAutoCompletionList
};
