# Ruby Beautify [![Build Status](https://travis-ci.org/erniebrodeur/ruby-beautify.png?branch=master)](https://travis-ci.org/erniebrodeur/ruby-beautify) [![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/erniebrodeur/ruby-beautify?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This gem provides a cli binary named 'ruby-beautify' that will pretty up ruby code.

Currenty, 'rbeautify' is included for backwards compatibility but will likely be phased out at some point.

## Installation

  `gem install ruby-beautify`

## Usage

To Pretty up a file:

  `ruby-beautify filename`

Without a filename it reads from STDIN, suitable for piping:

  `curl https://raw.githubusercontent.com/erniebrodeur/ruby-beautify/master/spec/monolithic_example.rb | ruby-beautify`

It has help:

  `ruby-beautify --help`

You can pick your indent character:

  `ruby-beautify --(t)abs`

  `ruby-beautify --(s)paces`

You can also pick the count of characters:

  `ruby-beautify --indent_(c)ount 1`

Examples:

  `ruby-beautify -c 2 -s filename`

  `ruby-beautify filename`

  `ruby-beautify -t -c 2 filename`

## Advanced Usage

You can over write files in place, this is useful for doing an entire directory of files at once.  This will not over write any files that fail syntax check.

  `ruby-beautify --overwrite **/*.rb`

## Configuration file

It can use a configuration file like some of the other ruby projects out there.  The config file consists of each argument on a new line.  Something like this:

```
--spaces
--indent_count=2
```

Note, you'll have to add the equal sign between value and argument (tricky bit, that).

Placing this into a `.ruby-beautify` anywhere in your tree (like git) will work.  This allows you to put it at the root of a project and have it change the defaults anywhere in the project.

## Bugs

Please feel free to open issues, I am actively working on this project again, thanks entirely to the ripper gem.

The gaps are getting smaller.  I think we have most of the basic ruby use cases in place.  I don't use rails/dsl's too often so I haven't tested those.  I suspect it should 'just work' since the way we do syntax matching is really agnostic to what a DSL can change.

## Todo

* Add vim style comment hinting.
* add specs/pipe testing (epic).
* remove the link to rbeautify (by 1.0).

Longer term I'd like to do some more to assignment, line wrapping, and spacing in/around keywords.

## Contributing

Please see the [Contribution Guide](CONTRIB.md) file for specifics on how to contribute to this project.

# History

The original analyzer is available at: http://www.arachnoid.com/ruby/rubyBeautifier.html.

My work is based off of this sublime-text2 plugin: https://github.com/CraigWilliams/BeautifyRuby but cleaned up and made suitable for use directly in a shell.

I've recently re-written this to use the stdlib `ripper` gem to do the lexical analyzing.  Consequently I've dropped all of the old legacy code that did this.
