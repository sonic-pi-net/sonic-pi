## version 1.6.2: 2014-08-16

  * swift: updates for beta 5 (thanks @radex!)

## version 1.6.1: 2014-07-26

  * hotfix release for common lisp, php, objective c, and qml lexers

## version 1.6.0: 2014-07-26

  * haml: balance braces in interpolation
  * new lexer: slim (thanks @knutaldrin and @greggroth!)
  * javascript: inner tokens in regexes are now lexed, as well as improvments to
    the block / object distinction.

## version 1.5.1: 2014-07-13

  * ruby bugfixes for symbol edgecases and one-letter constants
  * utf-8 all of the things
  * update all builtins
  * rust: add `box` keyword and associated builtins

## version 1.5.0: 2014-07-11

  * new lexer: swift (thanks @totocaster!)
  * update elixir for new upstream features (thanks @splattael!)
  * ruby bugfixes:
    - add support for method calls with trailing dots
    - fix for `foo[bar] / baz` being highlighted as a regex
  * terminal256 formatter: re-style each line - some platforms reset on each line

## version 1.4.0: 2014-05-28

  * breaking: wrap code in `<pre ...><code>...</code></pre>` if `:wrap` is not overridden
    (thanks @Arcovion)
  * Allow passing a theme name as a string to `:inline_theme` (thanks @Arcovion)
  * Add `:start_line` option for html line numbers (thanks @sencer)
  * List available themes in `rougify help style`

## version 1.3.4: 2014-05-03

  * New lexers:
    - QML (thanks @seanchas116)
    - Applescript (thanks @joshhepworth)
    - Properties (thanks @pkuczynski)
  * Ruby bugfix for `{ key: /regex/ }` (#134)
  * JSON bugfix: properly highlight null (thanks @zsalzbank)
  * Implement a noop formatter for perf testing (thanks @splattael)

## version 1.3.3: 2014-03-02

  * prolog bugfix: was raising an error on some inputs (#126)
  * python bugfix: was inconsistently highlighting keywords/builtins mid-word (#127)
  * html formatter: always end output with a newline (#125)

## version 1.3.2: 2014-01-13

  * Now tested in Ruby 2.1
  * C family bugfix: allow exponential floats without decimals (`1e-2`)
  * cpp: allow single quotes as digit separators (`100'000'000`)
  * ruby: highlight `%=` as an operator in the right context

## version 1.3.1: 2013-12-23

  * fill in some lexer descriptions and add the behat alias for gherkin

## version 1.3.0: 2013-12-23

  * assorted CLI bugfixes: better error handling, CGI-style options, no loadpath munging
  * html: support multiline doctypes
  * ocaml: bugfix for OO code: allows `#` as an operator
  * inline some styles in tableized output instead of relying on the theme
  * redcarpet: add overrideable `#rouge_formatter` for custom formatting options

## version 1.2.0: 2013-11-26

  * New lexers:
    - MATLAB (thanks @adambard!)
    - Scala (thanks @sgrif!)
    - Standard ML (sml)
    - OCaml
  * Major performance overhaul, now ~2x faster (see [#114][]) (thanks @korny!)
  * Deprecate `RegexLexer#group` (internal).  Use `#groups` instead.
  * Updated PHP builtins
  * CLI now responds to `rougify --version`

[#114]: https://github.com/jayferd/rouge/pull/114

## version 1.1.0: 2013-11-04

  * For tableized line numbers, the table is no longer surrounded by a `<pre>`
    tag, which is invalid HTML.  This was previously causing issues with HTML
    post-processors such as loofah.  This may break some stylesheets, as it
    changes the generated markup, but stylesheets only referring to the scope
    passed to the formatter should be unaffected.
  * New lexer: moonscript (thanks @nilnor!)
  * New theme: monokai, for real this time! (thanks @3100!)
  * Fix intermittent loading errors for good with `Lexer.load_const`, which
    closes the long-standing #66

## version 1.0.0: 2013-09-28

  * lua: encoding bugfix, and a performance tweak for string literals
  * The Big 1.0!  From now on, strict semver will apply, and new lexers and
    features will be introduced in minor releases, reserving patch releases
    for bugfixes.

## version 0.5.4: 2013-09-21

  * Cleaned up stray invalid error tokens
  * Fix C++/objc loading bug in `rougify`
  * Guessing alg tweaks: don't give up if no filename or mimetype matches
  * Rebuilt the CLI without thor (removed the thor dependency)
  * objc: Bugfix for `:forward_classname` error tokens

## version 0.5.3: 2013-09-15

  * Critical bugfixes (#98 and #99) for Ruby and Markdown. Some inputs
    would throw errors. (thanks @hrysd!)

## version 0.5.2: 2013-09-15

  * Bugfixes for C/C++
  * Major bugfix: YAML was in a broken state :\ (thanks @hrysd!)
  * Implement lexer subclassing, with `append` and `prepend`
  * new lexer: objective c (!)

## version 0.5.1: 2013-09-15

  * Fix non-default themes (thanks @tiroc!)
  * Minor lexing bugfixes in ruby

## version 0.5.0: 2013-09-02

  * [Various performance optimizations][perf-0.5]
  * javascript:
    - quoted object keys were not being highlighted correctly
    - multiline comments were not being highlighted
  * common lisp: fix commented forms
  * golang: performance bump
  * ruby: fix edge case for `def-@`
  * c: fix a pathological performance case
  * fix line number alignment on non-newline-terminated code (#91)

### Breaking API Changes in v0.5.0

  * `Rouge::Lexers::Text` renamed to `Rouge::Lexers::PlainText`
  * Tokens are now constants, rather than strings.  This only affects
    you if you've written a custom lexer, formatter, or theme.

[perf-0.5]: https://github.com/jayferd/rouge/pull/41#issuecomment-23561787

## version 0.4.0: 2013-08-14

  * Add the `:inline_theme` option to `Formatters::HTML` for environments
    that don't support stylesheets (like super-old email clients)
  * Improve documentation of `Formatters::HTML` options
  * bugfix: don't include subsequent whitespace in an elixir keyword.
    In certain fonts/themes, this can cause inconsistent indentation if
    bold spaces are wider than non-bold spaces.  (thanks @splattael!)

## version 0.3.10: 2013-07-31

  * Add the `license` key in the gemspec
  * new lexer: R

## version 0.3.9: 2013-07-19

  * new lexers:
    - elixir (thanks @splattael!)
    - racket (thanks @greghendershott!)

## version 0.3.8: 2013-07-02

  * new lexers:
    - erlang! (thanks @potomak!)
    - http (with content-type based delegation)
  * bugfix: highlight true and false in JSON

## version 0.3.7: 2013-06-07

  * bugfix: Add the local lib dir to the path in ./bin/rougify
    so the internal `require` works properly.
  * php: Properly lex variables in double-quoted strings and provide the
    correct token for heredocs (thanks @hrysd!)
  * Add a `:wrap` option to the html formatter (default true) to provide
    the `<pre>` wrapper.  This allows skipping the wrapper entirely for
    postprocessing.  (thanks @cjohansen!)

## version 0.3.6: 2013-05-27

  * fixed bad release that included unfinished D and wdiff lexers :\

## version 0.3.5: 2013-05-24

  * Added a github theme (thanks @simonc!) (#75)
  * Correctly highlight ruby 1.9-style symbols and %i() syntax
    (thanks @simonc!) (#74)
  * Fixed a performance bug in the C++ lexer (#73)
    reported by @jeffgran

## version 0.3.4: 2013-05-02

  * New lexer: go (thanks @hashmal!)
  * Clojure bugfix: allow # in keywords and symbols

## version 0.3.3: 2013-04-09

  * Basic prompt support in the shell lexer
  * Add CSS3 attributes to CSS/Sass/SCSS lexers
  * Bugfix for a crash in the vim lexer

## version 0.3.2: 2013-03-11

  * Another hotfix release for the Sass/SCSS lexers, because I am being dumb

## version 0.3.1: 2013-03-11

  * Hotfix release: fix errors loading the SCSS lexer on some systems.

## version 0.3.0: 2013-03-06

  * Refactor source guessing to return fewer false positives, and
    to be better at disambiguating between filename matches (such as
    `nginx.conf` vs. `*.conf`, or `*.pl` for both prolog and perl)
  * Added `Lexer.guesses` which can return multiple or zero results for a
    guess.
  * Fix number literals in C#
  * New lexers:
    - Gherkin (cucumber)
    - Prolog (@coffeejunk)
    - LLVM (@coffeejunk)

## version 0.2.15: 2013-03-03

  * New lexer: lua (thanks, @nathany!)
  * Add extra filetypes that map to Ruby (`Capfile`, `Vagrantfile`,
    `*.ru` and `*.prawn`) (@nathany)
  * Bugfix: add demos for ini and toml
  * The `thankful_eyes` theme now colors `Literal.Date`
  * No more gigantic load list in `lib/rouge.rb`

## version 0.2.14: 2013-02-28

  * New lexers:
    - puppet
    - literate coffeescript
    - literate haskell
    - ini
    - toml (@coffeejunk)
  * clojure: `cljs` alias, and make it more visually balanced by using
    `Name` instead of `Name.Variable`.
  * Stop trying to read /etc/bash.bashrc in the specs (@coffeejunk)

## version 0.2.13: 2013-02-12

  * Highlight ClojureScipt files (`*.cljs`) as Clojure (@blom)
  * README and doc enhancements (plus an actual wiki!) (@robin850)
  * Don't open `Regexp`, especially if we're not adding anything to it.

## version 0.2.12: 2013-02-07

  * Python: bugfix for lone quotes in triple-quoted strings
  * Ruby: bugfix for `#` in `%`-delimited strings

## version 0.2.11: 2013-02-04

  * New lexer: C# (csharp)
  * rust: better macro handling
  * Python bugfix for "'" and '"' (@garybernhardt)

## version 0.2.10: 2013-01-14

  * New lexer: rust (rust-lang.org)
  * Include rouge.gemspec with the built gem
  * Update the PHP builtins

## version 0.2.9: 2012-11-28

  * New lexers: io, sed, conf, and nginx
  * fixed an error on numbers in the shell lexer
  * performance bumps for shell and ruby by prioritizing more
    common patterns
  * (@korny) Future-proofed the regexes in the Perl lexer
  * `rougify` now streams the formatted text to stdout as it's
    available instead of waiting for the lex to be done.

## version 0.2.8: 2012-10-30

  * Bugfix for tableized line numbers when the code doesn't end
    with a newline.

## version 0.2.7: 2012-10-22

  * Major performance improvements.  80% running time reduction for
    some files since v0.2.5 (thanks again @korny!)
  * Deprecated `postprocess` for performance reasons - it wasn't that
    useful in the first place.
  * The shell lexer should now recognize .bashrc, .profile and friends

## version 0.2.6: 2012-10-21
  * coffeescript: don't yield error tokens for keywords as attributes
  * add the `--scope=SELECTOR` option to `rougify style`
  * Add the `:line_numbers` option to the HTML formatter to get line
    numbers!  The styling for the line numbers is determined by
    the theme's styling for `'Generic.Lineno'`
  * Massive performance improvements by reducing calls to `option`
    and to `Regexp#source` (@korny)

## version 0.2.5: 2012-10-20
  * hotfix: ship the demos with the gem.

## version 0.2.4: 2012-10-20

  * Several improvements to the javasript and scheme lexers
  * Lexer.demo, with small demos for each lexer
  * Rouge.highlight takes a string for the formatter
  * Formatter.format delegates to the instance
  * sass: Support the @extend syntax, fix new-style attributes, and
    support 3.2 placeholder syntax

## version 0.2.3: 2012-10-16

  * Fixed several postprocessing-related bugs
  * New lexers: coffeescript, sass, smalltalk, handlebars/mustache

## version 0.2.2: 2012-10-13

  * In terminal256, stop highlighting backgrounds of text-like tokens
  * Fix a bug which was breaking guessing with filenames beginning with .
  * Fix the require path for redcarpet in the README (@JustinCampbell)
  * New lexers: clojure, groovy, sass, scss
  * YAML: detect files with the %YAML directive
  * Fail fast for non-UTF-8 strings
  * Formatter#render deprecated, renamed to Formatter#format.
    To be removed in v0.3.
  * Lexer#tag delegates to the class
  * Better keyword/builtin highlighting for CSS
  * Add the `:token` option to the text lexer

## version 0.2.1: 2012-10-11

  * Began the changelog
  * Removed several unused methods and features from Lexer and RegexLexer
  * Added a lexer for SQL
  * Added a lexer for VimL, along with `rake builtins:vim`
  * Added documentation for RegexLexer, TextAnalyzer, and the formatters
  * Refactored `rake phpbuiltins` - renamed to `rake builtins:php`
  * Fixed a major bug in the Ruby lexer that prevented highlighting the
    `module` keyword.
  * Changed the default formatter for the `rougify` executable to
    `terminal256`.
  * Implemented `rougify list`, and added short descriptions to all of
    the lexers.
  * Fixed a bug in the C lexer that was yielding error tokens in case
    statements.
