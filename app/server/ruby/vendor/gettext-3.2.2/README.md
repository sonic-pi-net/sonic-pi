# gettext - gettext for Ruby

HELP US: Please fix markup of this file. This file's markup is
migrated from RDoc format. But some RDoc markups still exist.

NOTE: Gettext gem 3.0.0 removed many deprecated APIs and improves
internal APIs. We want to keep backward compatibility as much as
possible but some existing codes may be broken by gettext gem API
change. If your code breaks by gettext gem 3.0.0, please report your
problem. We will fix the problem and release a new version.

https://github.com/ruby-gettext/gettext is the official gettext gem
repository. It is moved from https://github.com/mutoh/gettext . Mutoh
agreed with the move.

Gettext gem is a pure Ruby Localization(L10n) library and tool
which is modeled after the GNU gettext package.

This library was called as "Ruby-GetText-Package". Since 2.3.0, this
library is called just "gettext". You can call this library as
"gettext gem" or "Ruby gettext" to distinguish from GNU gettext.

This library translates original messages to localized
messages using client-side locale information(environment
variable or CGI variable).

The tools for developers support creating, useing, and modifying
localized message files(message catalogs).

((**Rails**))
Rails support has been removed.

## Website

* [Homepage](http://ruby-gettext.github.io/)
* on [GitHub](http://github.com/ruby-gettext/gettext)

## Features

* Translate singular/plural messages with simple APIs(similar to GNU gettext)

* Thread safety. Message resources are shared from all threads, but
  returns translated messages of the current thread's locale.

* Tools to find message IDs
  * Extract message IDs to po-files using rxgettext from
    * ruby scripts
    * glade-2 XML file(.glade)
    * ERB file(.rhtml, .erb)
    * Anything (with your own parsers)
    * The po-files are compatible to GNU gettext.

  * rmsgfmt creates a mo-file from a po-file.
    The mo-file is compatible to GNU gettext(msgfmt).

  * Using rxgettext/rmsgfmt as Rake tasks

* textdomain's scope is adapt to ruby class/module mechanism.
  * A class/module can have plural textdomains.
  * a message is looked up in its class/module and ancestors.

* CGI support (gettext/cgi)
  * Locale is retrieved from client informations using Ruby-Locale.
    (HTTP_ACCEPT_LANGUAGE, HTTP_ACCEPT_CHARSET, QUERY_STRING(lang), Cookies(lang)).

## Requirements

* [Ruby 1.9.3 or later](http://www.ruby-lang.org)
* [RubyGems](http://www.rubygems.org/)
* [locale gem](http://ruby-gettext.github.io/)
  * $ gem install locale
* (for development only)
  * [Racc-1.4.3 or later](http://rubygems.org/gems/racc)
    * (for compiling src/rmsgfmt.ry only)

## Install

* Uninstall old gettext if exists. (You need to do this when updating 1.93.0 -> 2.0.1)

        (sudo/su on POSIX system)
        gem uninstall gettext

* gem

        #from rubyforge
        (sudo/su on POSIX system)
        gem install gettext

* download tar-ball

        # De-Compress archive and enter its top directory.
        (sudo/su on POSIX system)
        ruby setup.rb

You can also install files in your favorite directory by
supplying setup.rb some options. Try `ruby setup.rb --help`.

## Usage

### Translation

- _: Basic translation method

    Translates the message.

        _("Hello")

The gettext methods comes in 3 combinable flavors

- n: Pluralized

    Returns singular or plural form, depending on how many you have.

        n_("Apple", "%{num} Apples", 3)
        n_(["Apple", "%{num} Apples"], 3)

- p: context aware

    A context is a prefix to your translation, usefull when one word has different meanings, depending on its context.

        p_("Printer","Open") <=> p_("File","Open")
        is the same as s_("Printer|Open")  <=> s_("File|Open")

- s: without context

    If a translation could not be found, return the msgid without context.

        s_("Printer|Open") => "Öffnen" #translation found
        s_("Printer|Open") => "Open"   #translation not found

- combinations

        np_("Fruit", "Apple", "%{num} Apples", 3)
        ns_("Fruit|Apple","%{num} Apples", 3)

        np_(["Fruit","Apple","%{num} Apples"], 3)
        ns_(["Fruit|Apple","%{num} Apples"], 3)

- N_, Nn_: Makes dynamic translation messages readable for the gettext parser.

    `_(fruit)` cannot be understood by the gettext parser. To help the parser find all your translations,
    you can add `fruit = N_("Apple")` which does not translate, but tells the parser: "Apple" needs translation.

        fruit = N_("Apple")   # same as fruit = "Apple"
        _(fruit)              # does a normal translation

        fruits = Nn_("Apple", "%{num} Apples")
        n_(fruits, 3)

### Bind textdomains to the classes.

A textdomain has a translation file in each language.
A module/class can have multi textdomains. This means the
libraries/applications can have their own textdomains.

    class Foo
      include GetText
      bindtextdomain "your_app_domain_name"
    end

    class Book
      include GetText
      bindtextdomain "general"
      bindtextdomain "book"
    end

### Locale

If you need to set the locale by yourself, then use:

    GetText.locale = "en_US" # translate into english from now on
    GetText.locale # => en_US

Or

    include GetText
    set_locale "en_US"

For more details and options, have a look at the samples folder or
consult the [tutorial](http://www.yotabanana.com/hiki/ruby-gettext-howto.html).

## License

This program is licenced under the same licence as Ruby(See doc/text/ruby-license.txt) or 
LGPL(Lesser General Public License: doc/text/lgpl-3.0.txt or http://www.gnu.org/licenses/lgpl-3.0.txt).  

* mofile.rb
  * Copyright (C) 2001-2009 Masao Mutoh `<mutoh at highwhay.ne.jp>`
  * Copyright (C) 2001,2002 Masahiro Sakai `<s01397ms at sfc.keio.ac.jp>`

* gettext.rb
  * Copyright (C) 2001-2009 Masao Mutoh `<mutoh at highwhay.ne.jp>`
  * Copyright (C) 2001,2002 Masahiro Sakai `<s01397ms at sfc.keio.ac.jp>`

* rxgettext
  * Copyright (C) 2001-2009 Masao Mutoh `<mutoh at highwhay.ne.jp>`
  * Copyright (C) 2001,2002 Yasushi Shoji `<yashi at atmark-techno.com>`

* Others
  * Copyright (C) 2001-2009 Masao Mutoh `<mutoh at highwhay.ne.jp>`


## Translators

* Bosnian(bs)                - Sanjin Sehic `<saserr at gmail.com>`
* Bulgarian(bg)              - Sava Chankov `<sava.chankov at gmail.com>`
* Catalan(ca)                - Ramon Salvadó `<rsalvado at gnuine.com>`
* Chinese(Simplified)(zh_CN)
  * Yang Bob `<bob.yang.dev at gmail.com>` (current)
  * Yingfeng `<blogyingfeng at gmail.com>`
* Chinese(Traditional)(zh_TW)
  * Yang Bob `<bob.yang.dev at gmail.com>` (current)
  * LIN CHUNG-YI `<xmarsh at gmail.com>`
* Croatian(hr)               - Sanjin Sehic `<saserr at gmail.com>`
* Czech(cs)                  - Karel Miarka `<kajism at yahoo.com>`
* Dutch(nl)                  - Menno Jonkers `<ruby-gettext at jonkers.com>`
* Esperanto(eo)              - Malte Milatz `<malte at gmx-topmail.de>`
* Estonian(et)               - Erkki Eilonen `<erkki at itech.ee>`
* French(fr)
  * Vincent Isambart `<vincent.isambart at gmail.com>` (current)
  * David Sulc `<davidsulc at gmail.com>`
  * Laurent Sansonetti `<laurent.sansonetti at gmail.com>`
* German(de)
  * Patrick Lenz `<patrick at limited-overload.de>` (current)
  * Detlef Reichl `<detlef.reichl at gmx.org>`
  * Sven Herzberg `<herzi at abi02.de>`
  * Sascha Ebach `<se at digitale-wertschoepfung.de>`
* Greek(el)                  - Vassilis Rizopoulos `<damphyr at gmx.net>`
* Hungarian(hu)              - Tamás Tompa `<tompata at gmail.com>`
* Italian(it)
  * Marco Lazzeri `<marco.lazzeri at gmail.com>`
  * Gabriele Renzi `<surrender_it at yahoo.it>`
* Japanese(ja)               - Masao Mutoh `<mutomasa at gmail.com>`
* Korean(ko)                 - Gyoung-Yoon Noh `<nohmad at gmail.com>`
* Latvian(lv)                - Aivars Akots `<aivars.akots at gmail.com>`
* Norwegian(nb)              - Runar Ingebrigtsen `<runar at mopo.no>`
* Portuguese(Brazil)(pt_BR)
  * Antonio S. de A. Terceiro `<terceiro at softwarelivre.org>` (current)
  * Joao Pedrosa `<joaopedrosa at gmail.com>`
* Russian(ru)                - Yuri Kozlov `<kozlov.y at gmail.com>`
* Serbian(sr)                - Slobodan Paunović" `<slobodan.paunovic at gmail.com>`
* Spanish(es)
  * David Espada `<davinci at escomposlinux.org>` (current)
  * David Moreno Garza `<damog at damog.net>`
* Swedish(sv)                - Nikolai Weibull `<mailing-lists.ruby-talk at rawuncut.elitemail.org>`
* Ukrainian(uk)              - Alex Rootoff `<rootoff at pisem.net>`
* Vietnamese(vi)             - Ngoc Dao Thanh `<ngocdaothanh at gmail.com>`

## Status of translations

* Bosnian(bs)               - 1.90.0 (old)
* Bulgarian(bg)             - 2.0.1
* Catalan(ca)               - 2.0.1
* Croatian(hr)              - 1.90.0 (old)
* Chinese(zh_CN)            - 2.0.1
* Chinese(zh_TW)            - 2.0.1
* Czech(cs)                 - 1.9.0 (old)
* Dutch(nl)                 - 1.90.0 (old)
* English(default)          - 2.1.0
* Esperanto(eo)             - 2.0.1
* Estonian(et)              - 2.0.1
* French(fr)                - 2.0.1
* German(de)                - 2.0.1
* Greek(el)                 - 2.0.1
* Hungarian(hu)             - 2.0.1
* Italian(it)               - 1.6.0 (old)
* Japanese(ja)              - 2.1.0
* Korean(ko)                - 1.9.0 (old)
* Latvian(lv)               - 2.0.1
* Norwegian(nb)             - 2.0.1
* Portuguese(Brazil)(pt_BR) - 2.0.1
* Russian(ru)               - 2.0.1
* Serbian(sr)               - 2.0.1
* Spanish(es)               - 2.0.1
* Swedish(sv)               - 0.8.0 (too much old)
* Ukrainian(uk)             - 2.0.1
* Vietnamese(vi)            - 2.0.1

## Maintainer

* Kouhei Sutou `<kou@clear-code.com>`

### Old maintainer

* Masao Mutoh `<mutomasa at gmail.com>`
