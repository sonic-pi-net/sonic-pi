Adds a `__metaclass__` method to all Ruby objects.

## Motivations

* Even though WhyTheLuckyStiff's [metaid gem](https://rubygems.org/gems/metaid) does something similar, apparently the metaclass method without underscores [doesn't play well with Rails v2.3](https://github.com/floehopper/mocha/commit/f0749d6d291164cc9280aa8ba16f33d652d45fe1#commitcomment-475799).
* I'm trying to extract code out of the [mocha gem](https://github.com/floehopper/mocha) and this is an obvious candidate.

## License

This library is released under the [MIT License](http://www.opensource.org/licenses/MIT). See [COPYING.txt](https://github.com/floehopper/metaclass/blob/master/COPYING.txt).