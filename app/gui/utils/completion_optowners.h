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

#ifndef SONICPI_COMPLETION_OPTOWNERS_H
#define SONICPI_COMPLETION_OPTOWNERS_H

#include <QHash>
#include <QString>
#include <QStringList>

namespace SonicPi {

// Opt names are reused across synths and FX and don't mean the same thing on
// each: room: is a 0..1 mix on :reverb but metres on :gverb, wave: reaches 4 on
// :tremolo but only 2 on :tb303, depth: is flange depth on :flanger and carrier
// depth on :fm. The completion tables key everything by opt name, so they can
// only hold one owner's version of each fact — whichever synth/FX qt-doc.rb
// reached first.
//
// This is the second tier: the facts belonging to a specific (owner, opt) pair
// rather than to the name. qt-doc.rb emits an entry for every owner that differs
// from the global one (completion_optowners.gen.h) and the popup prefers it
// whenever the completion context names the owner.

struct OptOwnerRange
{
    double lo = 0, hi = 0, def = 0;
    // Engine validations that exclude the edge itself (res: must be < 1). The
    // consumer pulls the edge inside its display grid; the table stores what
    // the validation says.
    bool loExcl = false, hiExcl = false;
};

class OptOwnerTable
{
public:
    void setDoc(const QString& owner, const QString& opt, const QString& doc)
    {
        m_docs.insert(key(owner, opt), doc);
    }

    void setOptions(const QString& owner, const QString& opt, const QStringList& values)
    {
        m_options.insert(key(owner, opt), values);
    }

    void setRange(const QString& owner, const QString& opt, double lo, double hi, double def,
                  bool loExcl = false, bool hiExcl = false)
    {
        m_ranges.insert(key(owner, opt), OptOwnerRange{ lo, hi, def, loExcl, hiExcl });
    }

    // This owner's own version of the fact, or `fallback` (the global one) when
    // the owner agrees with it and so has no entry.
    QString doc(const QString& owner, const QString& opt, const QString& fallback) const
    {
        return m_docs.value(key(owner, opt), fallback);
    }

    QStringList options(const QString& owner, const QString& opt, const QStringList& fallback) const
    {
        return m_options.value(key(owner, opt), fallback);
    }

    bool hasRange(const QString& owner, const QString& opt) const
    {
        return m_ranges.contains(key(owner, opt));
    }

    OptOwnerRange range(const QString& owner, const QString& opt) const
    {
        return m_ranges.value(key(owner, opt));
    }

private:
    // Owners are always ":name" and opts always "name:", so the two can't run
    // together ambiguously.
    static QString key(const QString& owner, const QString& opt)
    {
        return owner + QLatin1Char(' ') + opt;
    }

    QHash<QString, QString> m_docs;
    QHash<QString, QStringList> m_options;
    QHash<QString, OptOwnerRange> m_ranges;
};

} // namespace SonicPi

#endif
