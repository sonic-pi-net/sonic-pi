# News

## 3.4.0: 2021-08-26 {#version-3-4-0}

### Improvements

  * test: Added missing Red Datasets availability check.
    [GitHub#87][Reported by Mamoru TASAKA]

### Thanks

  * Mamoru TASAKA

## 3.3.9: 2021-08-26 {#version-3-3-9}

### Improvements

  * msginit: Made Red Datasets dependency optional.
    [GitHub:red-datatools/red-datasets#105][Suggested by Mamoru TASAKA]

### Thanks

  * Mamoru TASAKA

## 3.3.8: 2021-06-09 {#version-3-3-8}

### Improvements

  * msginit: Added support for generating plural forms with
    Unicode's CLDR plural rules data.
    [GitHub#85][Suggested by Michaël Hoste]

  * rxgettext ui: Added support for GtkBuilder UI definitions format
    with `.glade` extension.
    [GitHub#74][Reported by dorle-o]

### Fixes

  * rxgettext ruby: Fixed a bug that `Nn_` isn't extracted.
    [GitHub#86][Reported by Kai Ramuenke]

### Thanks

  * Kai Ramuenke

  * Michaël Hoste

  * dorle-o

## 3.3.7: 2021-01-18 {#version-3-3-7}

### Improvements

  * msgmerge: Added `--no-report-warning` option.
    [GitHub#81][Reported by Akim Demaille]

### Thanks

  * Akim Demaille

## 3.3.6: 2020-08-04 {#version-3-3-6}

### Improvements

  * erb: Added support for `-%>`.
    [GitHub#77][Reported by lremes]

  * Removed ChangeLog.
    [GitHub#76][Reported by Will Stephenson]

  * Don't assume `RbConfig::CONFIG["datadir"]` is available. For
    example, TruffleRuby doesn't provide it.

### Thanks

  * lremes

  * Will Stephenson

## 3.3.5: 2020-02-15 {#version-3-3-5}

### Improvements

  * Added support for `|` method.
    [GitHub#73][Reported by dorle-o]

### Thanks

  * dorle-o

## 3.3.4: 2020-02-12 {#version-3-3-4}

### Improvements

  * Added support for `%W`.
    [GitHub#72][Reported by Michaël Hoste]

### Thanks

  * Michaël Hoste

## 3.3.3: 2020-02-05 {#version-3-3-3}

### Improvements

  * Added support for `%I`.
    [GitHub#71][Reported by Kai Ramuenke]

### Thanks

  * Kai Ramuenke

## 3.3.2: 2020-01-13 {#version-3-3-2}

### Fixes

  * Fixed a bug that `rake gettext:po:add]` raises an error.
    [GitHub#70][Patch by KITAITI Makoto]

### Thanks

  * KITAITI Makoto

## 3.3.1: 2020-01-12 {#version-3-3-1}

### Improvements

  * Stopped detecting string interpolation literal.
    [GitHub#21][Reported by Remo]

  * `rxgettext`: Added support for adding a new parser by `--require`.

  * Added support for GtkBuilder UI definitions file.
    [GitHub#63][Reported by Alex]

  * Improved percent literal parsing.
    [GitHub#67][Patch by KITAITI Makoto]

### Thanks

  * Remo

  * Alex

  * KITAITI Makoto

## 3.3.0: 2020-01-08 {#version-3-3-0}

### Improvements

  * Fixed README markup.
    [GitHub#57][Patch by Alexander Paukste]

  * Suppressed warnings.
    [GitHub#58][Patch by 284km]

  * Improved README.
    [GitHub#62][Patch by Robert Graff]

  * Added support for finding `racc` of Ruby 2.7.
    [GitHub#65][Patch by KITAITI Makoto]

  * Added support for Ruby 2.7.
    [GitHub#64][Reported by Anatol Pomozov]

  * Dropped support for Ruby 2.4.

### Fixes

  * Fixed a bug that `n_` may return nil.
    [GitHub#60][Patch by Michaël Hoste]

  * Fixed a sort by msgid bug.
    [GitHub#61][Patch by Robert Graff]

### Thanks

  * Alexander Paukste

  * 284km

  * Michaël Hoste

  * Robert Graff

  * KITAITI Makoto

  * Anatol Pomozov

## 3.2.9: 2018-03-05 {#version-3-2-9}

### Fixes

  * `rmsgcat`: Removed a debug print.

## 3.2.8: 2018-03-05 {#version-3-2-8}

### Fixes

  * `rmsgcat`: Fixed a bug that extra newline is added by
    `--update-po-revision-date` option.

## 3.2.7: 2018-03-05 {#version-3-2-7}

### Improvements

  * `rmsgcat`: Added `--update-po-revision-date` option.

## 3.2.6: 2017-12-17 {#version-3-2-6}

### Fixes

  * Fixed a regression bug that `\'` and `\\` aren't processed in
    `'...'`.
    [GitHub#56][Reported by Michaël Hoste]

### Thanks

  * Michaël Hoste

## 3.2.5: 2017-12-14 {#version-3-2-5}

### Improvements

  * Stop to use `eval`.
    [GitHub#56][Reported by Michaël Hoste]

### Thanks

  * Michaël Hoste

## 3.2.4: 2017-08-13 {#version-3-2-4}

### Fixes

  * Fixed a bug that block parameter is handled as method name.
    [GitHub#53][Reported by Renaud Chaput]

### Thanks

  * Renaud Chaput

## 3.2.3: 2017-06-24 {#version-3-2-3}

### Improvements

  * Disabled unmaintainable Ruby from CI.
    [GitHub#48][Reported by JP Hastings-Spital]

  * Supported `--enable-frozen-string-literal` `ruby` option.
    [GitHub#52][Reported by Pat Allan]

### Thanks

  * JP Hastings-Spital

  * Pat Allan

## 3.2.2: 2016-04-17 {#version-3-2-2}

### Improvements

  * Supported non POSIX locale format such as "zh-Hant" for
    .mo search path.
    [GitHub#45][Patch by Michaël Hoste]

### Thanks

  * Michaël Hoste

## 3.2.1: 2016-01-23 {#version-3-2-1}

### Improvements

  * Supported customizing msgmerge options on merging edit.po to .po.
    [GitHub#44][Patch by Dominic Cleal]

### Thanks

  * Dominic Cleal

## 3.2.0: 2015-12-31 {#version-3-2-0}

### Improvements

  * Improved fuzzy detection for sub text.

## 3.1.9: 2015-12-30 {#version-3-1-9}

### Improvements

  * Improved fuzzy detection for small texts.
    [GitHub#43][Reported by Mamoru TASAKA]

### Thanks

  * Mamoru TASAKA

## 3.1.8: 2015-12-29 {#version-3-1-8}

### Improvements

  * Improved fuzzy detection.

## 3.1.7: 2015-09-22 {#version-3-1-7}

### Improvements

  * Supported 3 character language names.
    [GitHub#39][Patch by Vilius Paulauskas]
  * Suppressed duplicated range in regular expression warning.
    [GitHub#40][Patch by Vilius Paulauskas]
  * Replaced invalid character instead of raising an error on encoding
    conversion.
    [GitHub#41][Patch by Vilius Paulauskas]

### Fixes

  * Fixed a bug that undefined method is used.
    [GitHub#38][Patch by Hiroshi Hatake]

### Thanks

  * Hiroshi Hatake
  * Vilius Paulauskas

## 3.1.6: 2015-01-20 {#version-3-1-6}

### Fixes

  * Added a missing required text gem version.
    [GitHub#37][Reported by Hans de Graaff]

### Thanks

  * Hans de Graaff

## 3.1.5: 2015-01-17 {#version-3-1-5}

### Improvements

  * `rmsgmerge`: Improves fuzzy matching speed.
    [GitHub#36][Reported by Dominic Cleal]

### Fixes

  * Fixed a bug that msgid uses wrong encoding.
    [Patch by OBATA Akio]
  * Fixed a typo in document.
    [GitHub#35][Patch by Masafumi Yokoyama]
  * `rmsgmerge`: Fixed a bug that `fuzzy` flag may be added twice.

### Thanks

  * OBATA Akio
  * Masafumi Yokoyama
  * Dominic Cleal

## 3.1.4: 2014-08-26 {#version-3-1-4}

### Improvements

  * Improved document markups by Markdown.
    [GitHub#33] [Patch by Masafumi Yokoyama]

### Fixes

  * `GetText::Tools::Task`: Fixed a bug that `Errno::ENOENT: No such
    file or directory @ rb_file_s_mtime - doc/po/ja/rroonga.edit.po`
    like error is occurred.
  * `GetText::Tools::Task`: Fixed markup in document.

### Thanks

  * Masafumi Yokoyama

## 3.1.3: 2014-07-13 {#version-3-1-3}

### Improvements

  * Supported `Pathname` in `$LOAD_PATH`.
    [GitHub#32] [Patch by Ben Carlsson]
  * `GetText::Tools::Task`: Added `msgcat_options` to custom `rmsgcat`
    command line.
  * `GetText::Tools::Task`: Migrated to `.edit.po` style.
  * `GetText::Tools::Task`: Added `pot_creator` to custom POT creation.
  * `rmsgcat`: Added `--no-translator-comment` option.
  * `rmsgcat`: Added `--no-extracted-comment` option.
  * `rmsgcat`: Added `--no-flag-comment` option.
  * `rmsgcat`: Added `--no-previous-comment` option.

### Fixes

  * `rmsgmerge`: Fixed a bug that metadata in fuzzy message aren't merged.

### Thanks

  * Ben Carlsson

## 3.1.2: 2014-04-24 {#version-3-1-2}

### Improvements

  * Travis CI: Enabled Rubinius again.
    [GitHub#30] [Patch by Masafumi Yokoyama]
  * `GetText::Tools::Task`: Added `msginit_options` to customize `msginit`
    command line.
  * `rmsginit`: Supported `--translator` option.
  * `GetText::Tools::Task`: Changed to not set translator information
    by default. [GitHub#31] [Reported by David Silva]

### Thanks

  * Masafumi Yokoyama
  * David Silva

## 3.1.1: 2014-02-23 {#version-3-1-1}

### Improvements

  * `rmsgcat`: Added `--remove-header-field` option.

### Fixes

  * `rmsgcat`: Fixed a bug that fuzzy entries are remained when
    `--no-fuzzy` option is used with `--no-all-comments`.

## 3.1.0: 2014-02-09 {#version-3-1-0}

### Improvements

  * `rmsgcat`: Added `--no-obsolete-entries` option.

## 3.0.9: 2014-02-09 {#version-3-0-9}

### Improvements

  * `rmsgmerge`: Improves fuzzy matching speed.

## 3.0.8: 2014-02-09 {#version-3-0-8}

### Fixes

  * `rmsginit`: Fixed a typo.

## 3.0.7: 2014-02-09 {#version-3-0-7}

### Improvements

  * `rmsginit`: Added `--no-translator`.
  * `rmsginit`: Added `--translator-name`.
  * `rmsginit`: Added `--translator-email`.

## 3.0.6: 2014-02-02 {#version-3-0-6}

### Improvements

  * Added {GetText::POEntry#translated?}.
  * `rmsgcat` chooses translated entry if it exists.
  * `rmsgmerge`: Added `--no-obsolete-entries` option.

## 3.0.5: 2014-02-02 {#version-3-0-5}

### Fixes

  * Added missing `require` for {GetText::Tools::MsgCat}.

## 3.0.4: 2014-02-02 {#version-3-0-4}

### Improvements

  * Supported `Module#prepend`. [GitHub#29] [Reported by akira yamada]
  * Added {GetText::POEntry#fuzzy?}.
  * Added {GetText::Tools::MsgCat}.
  * Added `rmsgcat` command. [GitHub#23] [Requested by Andreas Loupasakis]
  * Changed `:references` {GetText::PO#order} value to `:reference`.
    `:references` is still usable but it is deprecated. It will be
    remove at 4.0.0. Don't use it for newly written code.
  * Removed `--no-sort-by-msgid` of `rmsgmerge` feature. It is not
    straightforward behavior.
  * Removed `--no-sort-by-file` of `rmsgmerge` feature. It is not
    straightforward behavior.
  * Added `--sort-by-location` to `rmsgmerge`.
  * Added `:include_translator_comment` option to
    {GetText::POEntry#to_s} options.
  * Added `:include_extracted_comment` option to
    {GetText::POEntry#to_s} options.
  * Added `:include_flag_comment` option to {GetText::POEntry#to_s}
    options.
  * Added `:include_previous_comment` option to
    {GetText::POEntry#to_s} options.
  * Added `:include_all_comments` option to {GetText::POEntry#to_s}
    options.
  * Added {GetText::POEntry#flags} and
    {GetText::POEntry#flags=}. {GetText::POEntry#flag} and
    {GetText::POEntry#flag=} are deprecated. Don't use them for newly
    written code.

### Fixes

  * Fixed `--sort-output` of `rmsgmerge` behavior. It used location for
    sort key but it was not GNU gettext compatible behavior. GNU
    gettext uses msgid for sort key. Now, `--sort-output` uses
    msgid like GNU gettext.

### Thanks

  * akira yamada
  * Andreas Loupasakis

## 3.0.3: 2013-12-15 {#version-3-0-3}

### Improvements

  * Documented {GetText::Tools::Task#namespace_prefix}.
  * Added `--copyright-year` option to {GetText::Tools::XGetText}.
    [GitHub#25] [Debian #726941] [Reported by Francesco Poli]
    [Reported by 375gnu]
  * {GetText::Tools::XGetText} respects new lines in translate target
    message.
  * Added {GetText::POEntry#header?}.
  * Added {GetText::POEntry#obsolete?}.
  * Added `--no-fuzzy-matching` option to {GetText::Tools::MsgMerge}.
    [GitHub#28] [Reported by Sam Lown]

### Fixes

  * Fixed cache key hash conflict on armv7hl. Memoization feature is
    removed for this fix. If you get performance issue. Please report
    it. We will solve the issue. See also locale gem's GitHub issue #3.
    [GitHub#22] [Reported by mtasaka]
  * Fixed a bug that obsolete comment misses the last new line.

### Thanks

  * Francesco Poli
  * 375gnu
  * Sam Lown
  * mtasaka

## 3.0.2: 2013-09-29 {#version-3-0-2}

### Improvements

  * Added {GetText::PO#empty?}.
  * Added `:encoding` option to {GetText::POEntry#to_s}.
  * xgettext: Added `--no-location` option.
  * xgettext: Added `--sort-output` option.
  * xgettext: Added `--sort-by-file` option.
  * xgettext: Added `--sort-by-msgid` option.
  * xgettext: Added `--width` option.
  * xgettext: Added `--no-wrap` option.

## 3.0.1: 2013-09-20 {#version-3-0-1}

### Improvements

  * Removed an unused file. [GitHub#19] [Reported by Ladislav Slezák]
  * msginit: Added full user name guessing by /etc/passwd.
  * incompatible: {GetText::Tools::Task} no longer require spec.
  * Added {GetText::Tools::Task.define}. It is the recommended API
    rather than {GetText::Tools::Task.new}.
  * Supported "utf8" as a valid charset.
    [GitHub#20][Reported by Antonio Terceiro]
  * Added {GetText::Tools::Task#enable_description=}.
  * Added {GetText::Tools::Task#enable_description?}.
  * Added {GetText::Tools::Task#enable_po=}.
  * Added {GetText::Tools::Task#enable_po?}.
  * Added {GetText::Tools::Task#msgmerge_options=}.
  * Added {GetText::Tools::Task#msgmerge_options}.
  * task: Added `gettext:po:add[LOCALE]` task.
  * msgmerge: add `--sort-output` option.
  * msgmerge: add `--sort-by-file` option.
  * msgmerge: add `--sort-by-msgid` option.
  * msgmerge: add `--no-location` option.
  * msgmerge: add `--width` option.
  * msgmerge: add `--no-wrap` option.
  * msgmerge: add `--update` option.

### Thanks

  * Ladislav Slezák
  * Antonio Terceiro

## 3.0.0: 2013-08-31 {#version-3-0-0}

This is a new major version up release!

This release removes many deprecated APIs and improves internal
APIs. We want to keep backward compatibility as much as possible but
some existing codes may be broken by gettext gem API change. If your
code breaks by gettext gem 3.0.0, please report your problem. We will
fix the problem and release a new version.

### Improvements

  * Removed deprecated APIs
    * `require "gettext/parser/erb"`.
       Use `require "gettext/tools/parser/erb"` instead.
    * `require "gettext/parser/glade"`.
      Use `require "gettext/tools/parser/glade"` instead.
    * `require "gettext/parser/ruby"`.
      Use `require "gettext/tools/parser/ruby"` instead.
    * `require "gettext/utils"`.
      Use `require "gettext/tools"` instead.
    * `GetText.msgmerge`. Use `GetText::Tools::MsgMerge.run` instead.
    * `GetText.create_mofiles`. Use `GetText::Tools::Task` instead.
    * `GetText::PoParser`. Use `GetText::POParser` instead.
    * `require "gettext/tools/poparser"`.
       Use `require "gettext/po_parser"` instead.
    * `require "gettext/runtime/mofile"`.
       Use `require "gettext/mo"` instead.
    * `GetText::MoFile`. Use `GetText::MO` instead.
    * `GetText::Task`. Use `GetText::Tools::Task` instead.
    * `GetText.set_locale_all`. Use `GetText.set_locale` instead.
    * `GetText.setlocale`. Use `GetText.set_locale` instead.
    * `GetText::Tools::MsgMerge::PoData`. Use `GetText::POEntry` instead.
  * Removed Ruby 1.8 support.
  * Supported Rake 10.1.0.
  * Stopped to remove `TRANSLATORS:` tag because GNU gettext doesn't
    remove it.
  * Stopped to use `TRANSLATORS:` as comment tag. It is GNU gettext
    compatible behavior.
  * rxgettext: Added `--add-comments[=TAG]` option that exists in
    xgettext. [GitHub #16] [Reported by Ladislav Slezák]
  * Supported escaping tab character as `\t`.

### Fixes

  * po: Added a missing new line for multiple extracted comments.
    [GitHub #17] [Patch by Ladislav Slezák]
  * Fixed a bug that encoding may not be set.
  * Fixed a bug that `\n` is escaped as `\\n`.
    [GitHub #18] [Debian #716916] [Reported by Ladislav Slezák]
    [Reported by Francesco Poli]

### Thanks

  * Ladislav Slezák
  * Francesco Poli

## 2.3.9: 2013-04-21 {#version-2-3-9}

This is a msgmerge updated release.

### Improvements

  * [tools] Used the more modern word "cannot" instead of "can
    not". [GitHub #15] [Patch by Benjamin Kerensa]
  * Cleared license descriptions. [Suggested by Jérémy Bobbio]

### Fixes

  * Avoided including native extentions in this gem for Windows users.

### Thanks

  * Benjamin Kerensa
  * Jérémy Bobbio

## 2.3.8: 2013-04-05 {#version-2-3-8}

This is a msgmerge improved release.

### Improvements

  * Added licence information to the gemspec.
    [GitHub #13] [Patch by jordimassaguerpla]
  * Supported Ruby 2.0.0.
    [GitHub #14] [Reported by mtasaka]

### Fixes

  * [rxgettext] Fixed a bug that the comment for the previous message
    also exists in the current message.
    [Debian #684184] [Reported by Francesco Poli] [Patch by Jérémy Bobbio]

### Thanks

  * jordimassaguerpla
  * mtasaka
  * Francesco Poli
  * Jérémy Bobbio

## 2.3.7: 2013-01-11 {#version-2-3-7}

This is a msgmerge improved release.

### Improvements

  * [msgmerge] Speeded up fuzzy matching.

### Fixes

  * [msgmerge] Fix the bug that msgmerge adds needless fuzzy flag from
    not fuzzy entries in merged PO.
  * [POEntry] Pretty formated all messages except msgstr.

## 2.3.6: 2012-12-19 {#version-2-3-6}

This is a bug fix release.

### Fixes

  * [POEntry] Fixed the bug that obsolete comment mark (#~) is added
    to already comment.
  * [msgmerge] Fixed the bug that no separator (blank line) didn't exist
    between each obsolete entry.
  * [msgmerge] Fixed tne bug that obsolete entries in old PO file are
    added to new PO file. Any obsolete entries in old PO file aren't
    treated for merging.

## 2.3.5: 2012-12-11 {#version-2-3-5}

This is a bug fix release.

### Fixes

  * [POParser] Fixed the class name for backward compatibility.

## 2.3.4: 2012-12-11 {#version-2-3-4}

This is a many changes and new implements release.

### Improvements

  * [Merger] Implemented "fuzzy-match" with Levenshtein distance.
  * Added the class "PO" for management PO entries. Please use PO
    instead of PoData. (see details in
    http://rubydoc.info/gems/gettext/GetText/PO.html)
  * [POEntry (renamed from PoMessages)] Supported to specify msgstr.
  * [POEntry]　Stored comments each type
    (translator\_comment, extracted\_comment, flag, previous).
    see
    http://www.gnu.org/software/gettext/manual/html_node/PO-Files.html
    for details of comment type.
  * [POEntry] Checked if specified type is valid in #type=.
  * [PoParser][MO] Concatenated msgctxt, msgid, msgid\_plural to
    "#{msgctxt}\004#{msgid}\000"{msgid\_plural}" by MO instead of
    PoParser. PoData and MO treat a concatenated string as msgid, but
    PO doesn't.
  * [PoParser] Parsed each type comment from whole comment.

### Changes

  * Rename some classes and methods.
    * PoMessage to PoEntry. This isn't "message" but "entry".
      (See http://www.gnu.org/software/gettext/manual/gettext.html#PO-Files)
    * PoMessages#== to POEntry#mergeable?.
    * PoMessages#to\_po\_str to POEntry#to\_s.
    * PoMessages#sources(sources=) to POEntry#references(references=)
    * MoFile to MO. For backword compatible, MoFile can be used now.
    * PoParser to POParser. For backword compatible, PoParser can be used now.
  * Raised no error when POEntry doesn't have references.
    It is useful for no references in .PO file.

## 2.3.3: 2012-10-18 {#version-2-3-3}

It's a package fix and msginit improvement release.

### Improvements

  * [msginit] Supported plural forms for Bosnian, Catalan, Norwegian
    Bokmal and Chinese.

### Fixes

  * Fixed the bug that messages (i.e. the help message for rmsgfmt)
    aren't localized in each environment. However, some
    messages aren't tranlated or resolved fuzzy. Please
    help us to translate or resolve them.
    [Github #12][Reported by mtasaka]
  * Used String#% to localize some messages.

### Thanks

  * mtasaka

## 2.3.2: 2012-09-20 {#version-2-3-2}

It's a bug fix release.

### Fixes

  * Fixed the bug that untranslated messages are included in a .mo file.
    [Github #11][Reported by Ramón Cahenzli]

### Thanks

  * Ramón Cahenzli

## 2.3.1: 2012-09-13 {#version-2-3-1}

It's a Bug and package fix release.
Then, it's also encoding support release, only if you use Ruby 1.9.

### Improvements

  * [xgettext] Added backword compatibility method
    (GetText::RGetText.run).
    [Suggested by Fotos Georgiadis]
  * [xgettext] Removed deprecated parse argument support.
  * [erb parer] Assumed the encoding in the magic comment of the
    input file as the encoding of it.
  * [ruby parser] Assumed the encoding in the magic comment of the
    input file as the encoding of it.
  * [xgettext] Added the "--output-encoding" option to set encoding of
    output pot file.
  * [xgettext] Used UTF-8 as the default encoding of output pot file.
  * [xgettext] Supported multiple encoding sources.

### Changes

  * [MoFile] Returned nil instead of "" as msgstr when its msgid isn't
    translated (when this msgstr is "").
  * [PoParser] Converted msgstr from "" to nil when parsing.

### Fixes

  * Added missing .yardopts file. [Reported by Takahiro Kambe]
  * [news] Fixed Eddie Lau name instead of github name.
  * [msginit] Added the "Plural-Forms:" entry to the header even if a
    pot file doesn't have it.
  * [msgmerge] Fixed the bug the new line between a header and
    contents doesn't exist.
  * [msginit] Fixed the bug that msgstr with msgid_plural aren't
    generated in output po file.
  * [xgettext] Supported class based xgettext parser add API.
    [GitHub #10] [Suggested by Michael Grosser]
  * [erb parer] Fixed erb parser bug with unicode msgid in Ruby 1.9
    ERB templates.
    [Github #9] [Patch by Fotos Georgiadis]
  * Added missing documents for GetText::Tools::XGetText.

### Thanks

  * Takahiro Kambe
  * Michael Grosser
  * Fotos Georgiadis

## 2.3.0: 2012-08-28 {#version-2-3-0}

Various improvements, changes and fixes release.

### Improvements

  * Improved TextDomain#translate\_singluar\_message performance.
    [Base idea is provided by @angelf]
  * Added msginit command.
  * [xgettext] Added command line options for package name, version,
    copyright holder and msgid bugs address.[Github#8]
    [Reported by Francesco Poli (wintermute) and 375gnu, and patch by 375gnu]
  * [xgettext] Supported s\_ and ns\_ with parameter.
  * [poparser] Reported warnings when fuzzy message is used.
    [Reported by Michael Grosser]
  * Used %{...} to check the availability of String#% with hash and
    raise Error if this syntax isn't supported.
  * Searched mo files under LC_MESSAGES/ directory.
  * Updated documents for tools.

### Changes

  * Renamed the package name from "Ruby-GetText-Package" to "gettext".
  * Renamed RGetText to XGetText, RMsgMerge to MsgMerge, RMsgFmt to MsgFmt.
  * Renamed rgettext to rxgettext.
  * Defined tools(XGetText, MsgMerge, MsgFmt) as Class under GetText::Tools
    module.
  * Removed shortcuts for tools in GetText module.
    Please use GetText::Tools:XXX.run instead of GetText.xxx.
  * Changed API of tools.
    e.g.) Before: GetText.rsmgfmt(targetfile, output\_path)
          Now: GetText::Tools::MsgFmt.run(targetfile, "-o", output\_path)
  * [xgettext] Used relative path for source path.
    This path appears in generated pot file.
  * [xgettext] Returned the pot header instead of "" as the translation of
    "" msgid.
  * [poparser] Treated not translated msgid when parsing po file.
    A translation of no translated msgid is msgid itself even now.
  * [xgettext] Removed descriptions of ruby in information by "-v" option.

### Fixes

  * Included msgctxt when generating .po file. [Patch by 3dd13]
  * Fixed a typo in msgmerge. [Patch by Yves-Eric Martin]
  * [msgmerge] Followed PoParser API change.
  * [ruby-parser] Reseted the last comment when po message is stored.[Github#6]
    [Reported by 375gnu and Francesco Poli (wintermute), and Patch by 375gnu]
  * [ruby-parser] Processed RubyToken::TkDSTRING too.[Github#6]
    [Reported by 375gnu and Francesco Poli (wintermute), and Patch by 375gnu]
  * [msgmerge] Fixed not to add fuzzy to header message.
  * [msgmerge] Escaped backslash and "\n".

### Thanks

  * @angelf
  * Francesco Poli (wintermute)
  * 375gnu
  * Michael Grosser
  * Eddie Lau
  * Yves-Eric Martin

## 2.2.0: 2012-03-11 {#version-2-2-0}

Ruby 1.9 support release.

### Improvements

  * Supported ruby-1.9. [Patch by hallelujah]
  * Supported $SAFE=1. [Patch by bon]
  * Improved argument check. [Suggested by Morus Walter]
  * Supported ruby-1.8.6 again. [Bug#27447] [Reported by Mamoru Tasaka]

### Fixes

  * Fixed Ukrainan translation path. [Bug#28277] [Reported by Gunnar Wolf]
  * Fixed a bug that only the last path in GETTEXT_PATH environment
    variable is used. [Bug#28345] [Reported by Ivan Pirlik]
  * Fixed a bug that Ruby-GetText-Package modifies $LOAD_PATH. [Bug#28094]
    [Reported by Tatsuki Sugiura]

### Thanks

  * hallelujah
  * bon
  * Morus Walter
  * Mamoru Tasaka
  * Gunnar Wolf
  * Ivan Pirlik
  * Tatsuki Sugiura

## 2.2.1: 2012-05-20

### Changes

  * Supported non ASCII string in msgid. [GitHub#1]
    [Patch by Urban Hafner]
  * Stopped overriding String#% on Ruby 1.9.
  * Fixed a bug that "\" is too escaped.
  * Removed GetText.bindtext dependency from GetText::PoParser.
  * Ranamed GetText::MOFile to GetText::MoFile but GetText::MOFile
    is still available.

### Thanks

  * Urban Hafner

## 2.2.0: 2012-03-11

### Changes

  * Supported ruby-1.9. [Patch by hallelujah]
  * Supported $SAFE=1. [Patch by bon]
  * Improved argument check. [Suggested by Morus Walter]
  * Supported ruby-1.8.6 again. [Bug#27447] [Reported by Mamoru Tasaka]
  * Fixed Ukrainan translation path. [Bug#28277] [Reported by Gunnar Wolf]
  * Fixed a bug that only the last path in GETTEXT_PATH environment
    variable is used. [Bug#28345] [Reported by Ivan Pirlik]
  * Fixed a bug that Ruby-GetText-Package modifies $LOAD_PATH. [Bug#28094]
    [Reported by Tatsuki Sugiura]

### Thanks

  * hallelujah
  * bon
  * Morus Walter
  * Mamoru Tasaka
  * Gunnar Wolf
  * Ivan Pirlik
  * Tatsuki Sugiura


## 2.1.0: 2009-11-14

### Changes

  * Implemented parsing of translator comments (GNU gettext feature)
    [by Vladimir Dobriakov]
  * Refactor the directory structure.
    * Move files for runtime to lib/gettext/runtime/*.
    * Move files for development(rgettext/rmsgfmt) to
      lib/gettext/tools/*.
  * Refactor parsers for po. po-message object is defined as GetText::PoMessage
    class. [by Vladimir Dobriakov, Masao Mutoh]
  * Speed up when lots of objects are created to share the textdomain in each
    objects. [Reported by Gaël Séchaud.]
  * Fix "%{foo.bar}" %{"foo.bar".to_sym => "a"} doesn't work.
    [Bug#26663, Reported by Danilo Castilho]
  * lib/gettext.rb: Fixed to work unless gem. [Reported by Vladimir Dobriakov]
  * Fixed a wrong String literal in a CGI sample.
    [Bug #26531, by Eugene Mikhailov]
  * Update license information(explicit to use ruby's or LGPL).
    [Pointed out by Masateru Yoshikawa]
  * Code cleanup.
  * Update minor version.

## 2.0.4: 2009-05-23

### Changes

  * Fix String#% return nil when the string have no place holders. [by okkez]
  * Update pofiles and remove old messages.
  * suppress some warnings on Ruby 1.9.x. [by Nobuhiro IMAI]
  * Fix not to run tests on Win32.

## 2.0.3: 2009-05-09

### Changes

  * Fixed the dependencies. [Reported by Hans de Graaff]

## 2.0.2: 2009-05-04

### Changes

  * Support ruby-1.9.1 style format string such as `%<foo>d`.
  * Apply new `Locale.set_app_language_tags` and Locale.candidates.
    [Suggested by Vladimir Dobriakov]
  * Enhance to support ruby-1.9.x [by OZAWA Sakuro]
    * poparser work with proper encoding.
    * string.rb: #bytesize alias to #size in older ruby version such as 1.8.5.
  * Fixed bugs
    * `n_()` to work when Plural-Forms line in po-file is not correct.
      [Reported by Sava Chankov (Bug#25570)]
    * GetText::Locale.default_rules_path : $LOAD_PATH is not well parsed.
      [by hallelujah]
    * locale_path.rb: Fixed warning message.

### Thanks

  * hallelujah
  * Sava Chankov
  * OZAWA Sakuro
  * Vladimir Dobriakov

## 2.0.1: 2009-04-17

### Changes

  * Fixed bugs
    * doesn't work with ruby-1.8.5. [Reported by Dan Coutu]
    * GetText.locale= can't keep the locale. [Reported by Adam Ilan]
    * Break backward compatibility of bindtextdomain
      [Reported by Mamoru Tasaka(#24947), Mathieu Blondel]
    * Other trivial fixes/improvement.
  * 1.8 times faster than 2.0.0.
  * GetText::LocalePath is separated from GetText::TextDomainManager.
    Improve to find the locale path.
  * Enhance to support ruby-1.9.x [by OZAWA Sakuro]

### Thanks

  * OZAWA Sakuro
  * Tietew
  * Adam Ilan
  * Mamoru Tasaka
  * Mathieu Blondel

## 2.0.0: 2009-03-21

### Changes

  * Separate this library to locale and rails-support to locale,
    locale\_rails, gettext\_activerecord, gettext\_rails.
    * Depends on locale(ruby-locale).
    * Removes to support rails.
  * A lot of referctoring, improvements.
  * Thread safe.
  * New APIs for gettext/tools instead of gettext/utils.
  * Move to github.com.

### Special thanks

  * Michael Grosser: A lot of improvement.

### Thanks

  * Tietew
  * Kazuhiro NISHIYAMA
  * Fabio M.A.
  * Tuptus
  * Morus Walter
  * Vladimir Dobriakov
  * Ramsey
