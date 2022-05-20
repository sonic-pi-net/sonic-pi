# gettext
*gettext for Ruby*

[![Gem Version](https://badge.fury.io/rb/gettext.svg)](https://badge.fury.io/rb/gettext)
[![Build Status](https://travis-ci.org/ruby-gettext/gettext.svg?branch=master)](https://travis-ci.org/ruby-gettext/gettext)

> **NOTE:**
> Gettext gem 3.0.0 removed many deprecated APIs and improves internal APIs.
> We want to keep backward compatibility as much as possible but some existing codes may be broken by gettext gem API change.
> If your code breaks by gettext gem 3.0.0, please report your problem.
> We will fix the problem and release a new version.

> https://github.com/ruby-gettext/gettext is the official gettext gem repository.
> It is moved from https://github.com/mutoh/gettext .
> Mutoh agreed with the move.

Gettext gem is a pure Ruby Localization(L10n) library and tool which is modeled after the GNU gettext package.

This library was called as "Ruby-GetText-Package".
Since 2.3.0, this library is called just "gettext".
You can call this library as "gettext gem" or "Ruby gettext" to distinguish from GNU gettext.

This library translates original messages to localized messages using client-side locale information(environment variable or CGI variable).

The tools for developers support creating, useing, and modifying localized message files(message catalogs).

Rails support has been removed.

## Table of Contents

* [Features](#features)
* [Requirements](#requirements)
* [Installation](#installation)
* [Usage](#usage)
  * [Translation](#translation)
    * [`_()`](#_-basic-translation-method)
    * [`n_()`](#n_-pluralized)
    * [`p_()`](#p_-context-aware)
    * [`s_()`](#s_-without-context)
    * [Combinations](#combinations)
    * [`N_()`, `Nn_()`](#n_-nn_-makes-dynamic-translation-messages-readable-for-the-gettext-parser)
  * [Bind textdomains to the classes](#bind-textdomains-to-the-classes)
  * [Locale](#locale)
* [License](#license)
* [Translators](#translators)
* [Maintainer](#maintainer)
* [Links](#links)

## Features

* Translate singular/plural messages with simple APIs (similar to GNU gettext)
* Thread safety. Message resources are shared from all threads, but returns translated messages of the current thread's locale.
* Tools to find message IDs
  * Extract message IDs to `po`-files using `rxgettext` from
    * ruby scripts
    * glade-2 XML file (`.glade`)
    * ERB file (`.rhtml`, `.erb`)
    * Anything (with your own parsers)
    * The `po`-files are compatible with GNU gettext.
  * `rmsgfmt` creates a `mo`-file from a `po`-file.
    The `mo`-file is compatible with GNU gettext (`msgfmt`).
  * Using `rxgettext`/`rmsgfmt` as Rake tasks
* textdomain's scope is adapt to ruby class/module mechanism.
  * A class/module can have plural textdomains.
  * a message is looked up in its class/module and ancestors.
* CGI support (gettext/cgi)
  * Locale is retrieved from client informations using Ruby-Locale.
    (`HTTP_ACCEPT_LANGUAGE`, `HTTP_ACCEPT_CHARSET`, `QUERY_STRING` (lang), Cookies (lang)).

## Requirements

* [Ruby](http://www.ruby-lang.org) 1.9.3 or later
* [RubyGems](http://www.rubygems.org/)
* [locale](http://ruby-gettext.github.io/) gem
  ```shell
  gem install locale
  ```

For development:

  * [Racc](http://rubygems.org/gems/racc) 1.4.3 or later (for compiling `src/rmsgfmt.ry` only)

## Installation

Uninstall old gettext if exists. (You need to do this when updating 1.93.0 -> 2.0.1)

```shell
# sudo/su on POSIX system
gem uninstall gettext
```

### RubyGems

```shell
# sudo/su on POSIX system
gem install gettext
```

### Download tar-ball

```shell
# De-Compress archive and enter its top directory.
# sudo/su on POSIX system
ruby setup.rb
```

You can also install files in your favorite directory by supplying setup.rb some options.
Try `ruby setup.rb --help`.

## Usage

### Translation

#### `_()` or `gettext()`: basic translation method

Translates the message, using the `msgid` if a translation is not found.

```ruby
_("Hello") => "Bonjour"  # Found
```

This translation will appear in the po or pot file as:

```
msgid: "Hello"
msgstr: "Bonjour"
```

When a translation is not found it, it will return the `msgid`. This is a core
benefit of gettext and applies to all its translation methods.

```ruby
_("Hello") => "Hello"  # Not Found
```

Additional gettext methods come in 3 combinable flavors:

#### `n_()` or `ngettext()`: pluralized

Returns singular or plural form, depending on how many you have.

```ruby
n_("Apple", "%{num} Apples", n) => "3 Pommes"  # When n = 3
n_("Apple", "%{num} Apple", n)  => "Pomme"     # When n = 1
n_(["Apple", "%{num} Apple"], n)  => "Pomme"   # 2 arg variation
```

This translation will appear in the po or pot file as:

```
msgid "Apple"
msgid_plural "%{num} Apples"
msgstr[0] "Pomme"
msgstr[1] "#{num} Pommes"
```

#### `p_()` or `pgettext()`: context aware

A context is a prefix to your translation, useful when one word has different meanings, depending on its context.  

```ruby
p_("Printer","Open") => "Öffnen" #translation found
p_("Printer","Open") => "Open"   #translation not found
```

This translation will appear in the po or pot file as:

```
msgctxt "Printer"
msgid "Open"
msgstr "Öffnen"
```

Note that the parser when sorting by `msgid` will strictly sort by the `msgid` ignoring
the `msgctxt`. If you prefer to sort with the `msgctxt` you should consider the
`s_()` method.

#### `s_()` or `sgettext()`: without context

The `s_()` method is very similar to the `p_()` method except that the context is
inside the msgid.

```ruby
s_("Printer|Open") => "Öffnen" #translation found
s_("Printer|Open") => "Open"   #translation not found
```

```
msgid "Printer|Open"
msgstr "Öffnen"
```

Note the the parser when sorting by `msgid` will take the context into consideration
as it is part of the `msgid` unlike the `p_()` method.

Your preference of using `s_()` or `p_()` will depend on your translation workflow and process.

#### Combinations

You can combine `n_()` with either `p_()` or `s_()`.

#### `np_()` or `npgettext()`: context aware pluralized

```ruby
np_("Fruit", "Apple", "%{num} Apples", 3)
np_(["Fruit","Apple","%{num} Apples"], 3) # 2 arg variation
```

```
msgctxt "Fruit"
msgid "Apple"
msgid_plural "%{num} Apples"
msgstr[0] ""
msgstr[1] ""
```

#### `sp_()` or `spgettext()`: context aware pluralized


```ruby
ns_("Fruit|Apple","%{num} Apples", 3)
ns_(["Fruit|Apple","%{num} Apples"], 3) # 2 arg variation
```

```
msgid "Fruit|Apple"
msgid_plural "%{num} Apples"
msgstr[0] ""
msgstr[1] ""
```

#### `N_()` and `Nn_()`: makes dynamic translation messages readable for the gettext parser

`_(fruit)` cannot be understood by the gettext parser.
To help the parser find all your translations, you can add `fruit = N_("Apple")` which does not translate, but tells the parser: "Apple" needs translation.

```ruby
fruit = N_("Apple")   # same as fruit = "Apple"
_(fruit)              # does a normal translation

fruits = Nn_("Apple", "%{num} Apples")
n_(fruits, 3)
```

### Interpolating translations

This is not a feature of gettext but worth noting. You can interpolate translated strings without the ruby String `%` operator.

```ruby
N_("active"); N_("inactive"); N_("paused") # possible value of status for parser to find.
_("Your account is #{account_state}.") % { account_state: _(status) }
```


### Bind textdomains to the classes

A textdomain has a translation file in each language.
A module/class can have multiple textdomains.
This means the libraries/applications can have their own textdomains.

```ruby
class Foo
  include GetText
  bindtextdomain "your_app_domain_name"
end

class Book
  include GetText
  bindtextdomain "general"
  bindtextdomain "book"
end
```

### Locale

If you need to set the locale by yourself, then use:

```ruby
GetText.locale = "en_US" # translate into english from now on
GetText.locale # => en_US
```

Or

```ruby
include GetText
set_locale "en_US"
```

For more details and options, have a look at the samples folder.

## License

This program is licenced under the same licence as Ruby (See `doc/text/ruby-license.txt`) or
LGPL (Lesser General Public License: `doc/text/lgpl-3.0.txt` or http://www.gnu.org/licenses/lgpl-3.0.txt).  

`mofile.rb`
```
Copyright (C) 2001-2009 Masao Mutoh <mutoh at highwhay.ne.jp>
Copyright (C) 2001,2002 Masahiro Sakai <s01397ms at sfc.keio.ac.jp>
```

`gettext.rb`
```
Copyright (C) 2001-2009 Masao Mutoh <mutoh at highwhay.ne.jp>
Copyright (C) 2001,2002 Masahiro Sakai <s01397ms at sfc.keio.ac.jp>
```

`rxgettext`
```
Copyright (C) 2001-2009 Masao Mutoh <mutoh at highwhay.ne.jp>
Copyright (C) 2001,2002 Yasushi Shoji <yashi at atmark-techno.com>
```

Others
```
Copyright (C) 2001-2009 Masao Mutoh <mutoh at highwhay.ne.jp>
```

## Translators

| Language | Translator | Status |
| ---      | ---        | --- |
| Bosnian (bs)   | Sanjin Sehic `<saserr at gmail.com>`       | 1.90.0 (old) |
| Bulgarian (bg) | Sava Chankov `<sava.chankov at gmail.com>` | 2.0.1 |
| Catalan (ca)   | Ramon Salvadó `<rsalvado at gnuine.com>`   | 2.0.1 |
| Chinese (Simplified)(zh_CN) | Yang Bob `<bob.yang.dev at gmail.com>` *(current)* <br> Yingfeng `<blogyingfeng at gmail.com>` | 2.0.1 |
| Chinese (Traditional)(zh_TW) | Yang Bob `<bob.yang.dev at gmail.com>` *(current)* <br> Lin Chung-Yi `<xmarsh at gmail.com>` | 2.0.1 |
| Croatian (hr)  | Sanjin Sehic `<saserr at gmail.com>`       | 1.90.0 (old) |
| Czech (cs)     | Karel Miarka `<kajism at yahoo.com>`       | 1.9.0 (old)  |
| Dutch (nl)     | Menno Jonkers `<ruby-gettext at jonkers.com>` | 1.90.0 (old) |
| English (default) | | 2.1.0 |
| Esperanto (eo) | Malte Milatz `<malte at gmx-topmail.de>`   | 2.0.1 |
| Estonian (et)  | Erkki Eilonen `<erkki at itech.ee>`        | 2.0.1 |
| French (fr)    | Vincent Isambart `<vincent.isambart at gmail.com>` *(current)* <br> David Sulc `<davidsulc at gmail.com>` <br> Laurent Sansonetti `<laurent.sansonetti at gmail.com>` | 2.0.1 |
| German (de)    | Patrick Lenz `<patrick at limited-overload.de>` *(current)* <br> Detlef Reichl `<detlef.reichl at gmx.org>` <br> Sven Herzberg `<herzi at abi02.de>` <br> Sascha Ebach `<se at digitale-wertschoepfung.de>` | 2.0.1 |
| Greek (el)     | Vassilis Rizopoulos `<damphyr at gmx.net>` | 2.0.1 |
| Hungarian (hu) | Tamás Tompa `<tompata at gmail.com>`       | 2.0.1 |
| Italian (it)   | Marco Lazzeri `<marco.lazzeri at gmail.com>` <br> Gabriele Renzi `<surrender_it at yahoo.it>` | 1.6.0 (old) |
| Japanese (ja)  | Masao Mutoh `<mutomasa at gmail.com>`      | 2.1.0 |
| Korean (ko)    | Gyoung-Yoon Noh `<nohmad at gmail.com>`    | 1.9.0 (old)|
| Latvian (lv)   | Aivars Akots `<aivars.akots at gmail.com>` | 2.0.1 |
| Norwegian (nb) | Runar Ingebrigtsen `<runar at mopo.no>`    | 2.0.1 |
| Portuguese (Brazil)(pt_BR) | Antonio S. de A. Terceiro `<terceiro at softwarelivre.org>` *(current)* <br> Joao Pedrosa `<joaopedrosa at gmail.com>` | 2.0.1 |
| Russian (ru)   | Yuri Kozlov `<kozlov.y at gmail.com>`      | 2.0.1 |
| Serbian (sr)   | Slobodan Paunović `<slobodan.paunovic at gmail.com>` | 2.0.1 |
| Spanish (es)   | David Espada `<davinci at escomposlinux.org>` *(current)* <br> David Moreno Garza `<damog at damog.net>` | 2.0.1 |
| Swedish (sv)   | Nikolai Weibull `<mailing-lists.ruby-talk at rawuncut.elitemail.org>` | 0.8.0 (very old) |
| Ukrainian (uk) | Alex Rootoff `<rootoff at pisem.net>`      | 2.0.1 |
| Vietnamese (vi) | Ngoc Dao Thanh `<ngocdaothanh at gmail.com>` | 2.0.1 |

## Maintainer

* Kouhei Sutou `<kou@clear-code.com>`

Old maintainer

* Masao Mutoh `<mutomasa at gmail.com>`

## Links

* [Homepage](https://ruby-gettext.github.io/)
* [GitHub](https://github.com/ruby-gettext/gettext)
