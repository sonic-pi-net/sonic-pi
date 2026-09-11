// SPDX-License-Identifier: MIT
//
// A plugin parameter as the editor's completion knows it, and the rule that
// turns the plugin's name for it into the opt key the language takes:
// "Filter 1 Cutoff" -> filter_1_cutoff:. The same rule, word for word, is
// track_param_key in server/ruby/lib/sonicpi/studio.rb; they must agree,
// because the editor offers what the language resolves.

#ifndef SONICPI_TRACKPARAM_H
#define SONICPI_TRACKPARAM_H

#include <QList>
#include <QString>

namespace SonicPi {

struct TrackParam
{
    QString name;      // as the plugin names it
    QString plugin;    // which plugin on the track, when the track has more than one
    QString group;     // the plugin's own grouping, if it has one
    double  min = 0.0;
    double  max = 1.0;
    double  value = 0.0;
};

// Lower case, every run of anything but a letter or digit made one
// underscore, none at the ends; a leading digit gets an underscore in
// front so it can be a Ruby symbol.
inline QString trackParamKey(const QString& name)
{
    QString k;
    bool sep = false;
    for (const QChar c : name.toLower())
    {
        if (c.isLetterOrNumber() && c.unicode() < 128)
        {
            if (sep && !k.isEmpty()) k += QLatin1Char('_');
            sep = false;
            k += c;
        }
        else sep = true;
    }
    if (!k.isEmpty() && k[0].isDigit()) k.prepend(QLatin1Char('_'));
    return k;
}

} // namespace SonicPi

#endif
