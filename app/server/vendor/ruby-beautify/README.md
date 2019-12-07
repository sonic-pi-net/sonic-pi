# RBeautify

This gem provides a cli binary named 'rbeautify' that will pretty up ruby code.

## Installation

    $ gem install ruby-beautify

## Usage

To Pretty up a file:

		$ rbeautify filename

It can take mulitple filenames:

		$ rbeautify a b c

Without a filename it reads from STDIN, suitable for piping:

		$ curl 'http://example.org/ugly-file.rb' | rbeautify

It has help:

		$ rbeautify -h

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Added some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

# History

The original analyzer is available at: http://www.arachnoid.com/ruby/rubyBeautifier.html.

My work is based off of this sublime-text2 plugin: https://github.com/CraigWilliams/BeautifyRuby but cleaned up and made suitable for use directly in a shell.
