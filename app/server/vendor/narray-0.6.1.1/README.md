# Ruby/NArray

* ver 0.6.1.0 (2014-06-02)
* [Home page](http://masa16.github.io/narray/)
* [GitHub Repository](https://github.com/masa16/narray)
* [RubyGems](https://rubygems.org/gems/narray)
* [NArray method list](https://github.com/masa16/narray/blob/master/SPEC.en.txt)

## NArray Features:

* Fast and easy calculation for large numerical array.
* Accepting Elements:
  8,16,32 bit integer, single/double float/complex, Ruby Object.
* Easy extraction/substitution of array subset,
  using assignment with number, range, array index.
* Operator: +, -, *, /, %, **, etc.
* NMath: Mathematics functions.
* FFTW version 2 or 3 is separately supported.
* Ruby/PGPLOT: Graphics library interface (separately distributed)
  X-Y Graph, Histogram, Contour map, Image map, etc.

* NArray is similar to:
  * Python/NumPy, Perl/PDL, Yorick, IDL

* NArray is far from completed!
  * Experimental!  Specification may be changed.
  * Far from completed.
  * Bugs may be included.
  * No document.

## Installation

    ruby extconf.rb
    make
    make install

## Tested Platform

* ruby 2.1.2p95 (2014-05-08 revision 45877) [x86_64-linux]
* gcc version 4.4.7 20120313 (Red Hat 4.4.7-4) (GCC)

## License

    This program is free software.
    You can distribute/modify this program
    under the same terms as Ruby itself.
    NO WARRANTY.

## Author

Masahiro TANAKA
