# Titleize

http://rubygems.org/gems/titleize

### Description

Adds `String#titleize` for creating properly capitalized titles. It can be called as `Titleize.titleize` or `"a string".titleize`. It is also aliased as `titlecase`.

The list of "small words" which are not capped comes from the New York Times Manual of Style, plus 'vs' and 'v'.

If loaded in a Rails environment, it modifies `Inflector#titleize`. By default ActiveSupport calls `Inflector#underscore` and `Inflector#humanize`. This however can be problematic with words like "iPod", "GPS", and "McArthur". To disable this behavior, use the options `:underscore => false` and `:humanize => false`.

Based on [TitleCase.pl](http://daringfireball.net/2008/05/title_case) by John Gruber.

### Synopsis

`"a lovely and talented title".titleize # => "A Lovely and Talented Title"`

### Install

`gem install titleize`

### License

(The MIT License)

Copyright (c) 2008 Grant Hollingworth

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the 'Software'), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
