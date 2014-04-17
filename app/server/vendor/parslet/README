INTRODUCTION

Parslet makes developing complex parsers easy. It does so by

* providing the best error reporting possible
* not generating reams of code for you to debug

Parslet takes the long way around to make your job easier. It allows for
incremental language construction. Often, you start out small, implementing
the atoms of your language first; _parslet_ takes pride in making this
possible.

Eager to try this out? Please see the associated web site:
http://kschiess.github.com/parslet

SYNOPSIS

  require 'parslet'
  include Parslet

  # parslet parses strings
  str('foo').
    parse('foo') # => "foo"@0

  # it matches character sets
  match['abc'].parse('a') # => "a"@0
  match['abc'].parse('b') # => "b"@0
  match['abc'].parse('c') # => "c"@0

  # and it annotates its output
  str('foo').as(:important_bit).
    parse('foo') # => {:important_bit=>"foo"@0}

  # you can construct parsers with just a few lines
  quote = str('"')
  simple_string = quote >> (quote.absent? >> any).repeat >> quote

  simple_string.
    parse('"Simple Simple Simple"') # => "\"Simple Simple Simple\""@0

  # or by making a fuss about it 
  class Smalltalk < Parslet::Parser
    root :smalltalk

    rule(:smalltalk) { statements }
    rule(:statements) { 
      # insert smalltalk parser here (outside of the scope of this readme)
    }
  end

  # and then
  Smalltalk.new.parse('smalltalk')

FEATURES

  * Tools for every part of the parser chain
  * Transformers generate Abstract Syntax Trees
  * Accelerators transform parsers, making them quite a bit faster
  * Pluggable error reporters
  * Graphviz export for your parser
  * Rspec testing support rig
  * Simply Ruby, composable and hackable

COMPATIBILITY

This library is intended to work with Ruby variants >= 1.9. I've tested it on 
MRI 1.9, rbx-head, jruby. Please report as a bug if you encounter issues.

STATUS 

Production worthy.

(c) 2010-2014 Kaspar Schiess
