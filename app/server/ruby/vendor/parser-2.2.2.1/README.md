# Parser

[![Gem Version](https://badge.fury.io/rb/parser.png)](https://badge.fury.io/rb/parser)
[![Build Status](https://travis-ci.org/whitequark/parser.png?branch=master)](https://travis-ci.org/whitequark/parser)
[![Code Climate](https://codeclimate.com/github/whitequark/parser.png)](https://codeclimate.com/github/whitequark/parser)
[![Coverage Status](https://coveralls.io/repos/whitequark/parser/badge.png?branch=master)](https://coveralls.io/r/whitequark/parser)

_Parser_ is a production-ready Ruby parser written in pure Ruby. It performs on
par or better than Ripper, Melbourne, JRubyParser or ruby\_parser.

You can also use [unparser](https://github.com/mbj/unparser) to produce
equivalent source code from Parser's ASTs.

Sponsored by [Evil Martians](http://evilmartians.com).

## Installation

    $ gem install parser

## Usage

Parse a chunk of code:

    require 'parser/current'

    p Parser::CurrentRuby.parse("2 + 2")
    # (send
    #   (int 2) :+
    #   (int 2))

Access the AST's source map:

    p Parser::CurrentRuby.parse("2 + 2").loc
    # #<Parser::Source::Map::Send:0x007fe5a1ac2388
    #   @dot=nil,
    #   @begin=nil,
    #   @end=nil,
    #   @selector=#<Source::Range (string) 2...3>,
    #   @expression=#<Source::Range (string) 0...5>>

    p Parser::CurrentRuby.parse("2 + 2").loc.selector.source
    # "+"

Traverse the AST: see the documentation for [gem ast](https://whitequark.github.io/ast/).

Parse a chunk of code and display all diagnostics:

    parser = Parser::CurrentRuby.new
    parser.diagnostics.consumer = lambda do |diag|
      puts diag.render
    end

    buffer = Parser::Source::Buffer.new('(string)')
    buffer.source = "foo *bar"

    p parser.parse(buffer)
    # (string):1:5: warning: `*' interpreted as argument prefix
    # foo *bar
    #     ^
    # (send nil :foo
    #   (splat
    #     (send nil :bar)))

If you reuse the same parser object for multiple `#parse` runs, you need to
`#reset` it.

You can also use the `ruby-parse` utility (it's bundled with the gem) to play
with Parser:

    $ ruby-parse -L -e "2+2"
    (send
      (int 2) :+
      (int 2))
    2+2
     ~ selector
    ~~~ expression
    (int 2)
    2+2
    ~ expression
    (int 2)
    2+2

    $ ruby-parse -E -e "2+2"
    2+2
    ^ tINTEGER 2                                    expr_end     [0 <= cond] [0 <= cmdarg]
    2+2
     ^ tPLUS "+"                                    expr_beg     [0 <= cond] [0 <= cmdarg]
    2+2
      ^ tINTEGER 2                                  expr_end     [0 <= cond] [0 <= cmdarg]
    2+2
      ^ false "$eof"                                expr_end     [0 <= cond] [0 <= cmdarg]
    (send
      (int 2) :+
      (int 2))

## Features

* Precise source location reporting.
* [Documented](doc/AST_FORMAT.md) AST format which is convenient to work with.
* A simple interface and a powerful, tweakable one.
* Parses 1.8, 1.9, 2.0, 2.1 and 2.2 syntax with backwards-compatible
  AST formats.
* [Rewriting][rewriting] support.
* Parsing error recovery.
* Improved [clang-like][] diagnostic messages with location information.
* Written in pure Ruby, runs on MRI 1.8.7 or >=1.9.2, JRuby and Rubinius in 1.8
  and 1.9 mode.
* Only one runtime dependency: the [ast][] gem.
* [Insane][insane-lexer] Ruby lexer rewritten from scratch in Ragel.
* 100% test coverage for Bison grammars (except error recovery).
* Readable, commented source code.

[clang-like]: http://clang.llvm.org/diagnostics.html
[ast]: https://rubygems.org/gems/ast
[insane-lexer]: http://whitequark.org/blog/2013/04/01/ruby-hacking-guide-ch-11-finite-state-lexer/
[rewriting]: http://whitequark.org/blog/2013/04/26/lets-play-with-ruby-code/

## Documentation

Documentation for parser is available [online](https://whitequark.github.io/parser/).

### Node names

Several Parser nodes seem to be confusing enough to warrant a dedicated README section.

#### (block)

The `(block)` node passes a Ruby block, that is, a closure, to a method call represented by its first child, a `(send)`, `(super)` or `(zsuper)` node. To demonstrate:

```
$ ruby-parse -e 'foo { |x| x + 2 }'
(block
  (send nil :foo)
  (args
    (arg :x))
  (send
    (lvar :x) :+
    (int 2)))
```

#### (begin) and (kwbegin)

**TL;DR: Unless you perform rewriting, treat `(begin)` and `(kwbegin)` as the same node type.**

Both `(begin)` and `(kwbegin)` nodes represent compound statements, that is, several expressions which are executed sequentally and the value of the last one is the value of entire compound statement. They may take several forms in the source code:

  * `foo; bar`: without delimiters
  * `(foo; bar)`: parenthesized
  * `begin foo; bar; end`: grouped with `begin` keyword
  * `def x; foo; bar; end`: grouped inside a method definition

and so on.

```
$ ruby-parse -e '(foo; bar)'
(begin
  (send nil :foo)
  (send nil :bar))
$ ruby-parse -e 'def x; foo; bar end'
(def :x
  (args)
  (begin
    (send nil :foo)
    (send nil :bar)))
```

Note that, despite its name, `kwbegin` node only has tangential relation to the `begin` keyword. Normally, Parser AST is semantic, that is, if two constructs look differently but behave identically, they get parsed to the same node. However, there exists a peculiar construct called post-loop in Ruby:

```
begin
  body
end while condition
```

This specific syntactic construct, that is, keyword `begin..end` block followed by a postfix `while`, [behaves][postloop] very unlike other similar constructs, e.g. `(body) while condition`. While the body itself is wrapped into a `while-post` node, Parser also supports rewriting, and in that context it is important to not accidentally convert one kind of loop into another.

  [postloop]: http://rosettacode.org/wiki/Loops/Do-while#Ruby

```
$ ruby-parse -e 'begin foo end while cond'
(while-post
  (send nil :cond)
  (kwbegin
    (send nil :foo)))
$ ruby-parse -e 'foo while cond'
(while
  (send nil :cond)
  (send nil :foo))
$ ruby-parse -e '(foo) while cond'
(while
  (send nil :cond)
  (begin
    (send nil :foo)))
```

(Parser also needs the `(kwbegin)` node type internally, and it is highly problematic to map it back to `(begin)`.)

## Compatibility with Ruby MRI

Unfortunately, Ruby MRI often changes syntax in patchlevel versions. This has happened, at least, for every release since 1.9; for example, commits [c5013452](https://github.com/ruby/ruby/commit/c501345218dc5fb0fae90d56a0c6fd19d38df5bb) and [04bb9d6b](https://github.com/ruby/ruby/commit/04bb9d6b75a55d4000700769eead5a5cb942c25b) were backported all the way from HEAD to 1.9. Moreover, there is no simple way to track these changes.

This policy makes it all but impossible to make Parser precisely compatible with the Ruby MRI parser. Indeed, at September 2014, it would be necessary to maintain and update ten different parsers together with their lexer quirks in order to be able to emulate any given released Ruby MRI version.

As a result, Parser chooses a different path: the `parser/rubyXY` parsers recognize the syntax of the latest minor version of Ruby MRI X.Y at the time of the gem release.

## Known issues

Adding support for the following Ruby MRI features in Parser would needlessly complicate it, and as they all are very specific and rarely occuring corner cases, this is not done.

Parser has been extensively tested; in particular, it parses almost entire [Rubygems][rg] corpus. For every issue, a breakdown of affected gems is offered.

 [rg]: https://rubygems.org

### Void value expressions

Ruby MRI prohibits so-called "void value expressions". For a description
of what a void value expression is, see [this
gist](https://gist.github.com/JoshCheek/5625007) and [this Parser
issue](https://github.com/whitequark/parser/issues/72).

It is unknown whether any gems are affected by this issue.

### Invalid characters inside comments

Ruby MRI permits arbitrary non-7-bit characters to appear in comments regardless of source encoding.

As of 2013-07-25, there are about 180 affected gems.

### \u escape in 1.8 mode

Ruby MRI 1.8 permits to specify a bare `\u` escape sequence in a string; it treats it like `u`. Ruby MRI 1.9 and later treat `\u` as a prefix for Unicode escape sequence and do not allow it to appear bare. Parser follows 1.9+ behavior.

As of 2013-07-25, affected gems are: activerdf, activerdf_net7, fastreader, gkellog-reddy.

### Invalid Unicode escape sequences

Ruby MRI 1.9+ permits to specify invalid Unicode codepoints in Unicode escape sequences, such as `\u{d800}`.

As of 2013-07-25, affected gems are: aws_cloud_search.

### Dollar-dash

(This one is so obscure I couldn't even think of a saner name for this issue.) Pre-2.1 Ruby allows
to specify a global variable named `$-`. Ruby 2.1 and later treat it as a syntax error. Parser
follows 2.1 behavior.

No known code is affected by this issue.

## Contributors

* Peter Zotov ([whitequark][])
* Markus Schirp ([mbj][])
* Yorick Peterse ([yorickpeterse][])
* Magnus Holm ([judofyr][])
* Bozhidar Batsov ([bbatsov][])

[whitequark]:     https://github.com/whitequark
[mbj]:            https://github.com/mbj
[yorickpeterse]:  https://github.com/yorickpeterse
[judofyr]:        https://github.com/judofyr
[bbatsov]:        https://github.com/bbatsov

## Acknowledgements

The lexer testsuite is derived from
[ruby\_parser](https://github.com/seattlerb/ruby_parser).

The Bison parser rules are derived from [Ruby MRI](https://github.com/ruby/ruby)
parse.y.

## Contributing

1. Make sure you have [Ragel ~> 6.7](http://www.complang.org/ragel/) installed
2. Fork it
3. Create your feature branch (`git checkout -b my-new-feature`)
4. Commit your changes (`git commit -am 'Add some feature'`)
5. Push to the branch (`git push origin my-new-feature`)
6. Create new Pull Request
