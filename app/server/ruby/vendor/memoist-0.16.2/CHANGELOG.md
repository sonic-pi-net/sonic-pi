# Changelog

## [v0.16.2](https://github.com/matthewrudy/memoist/tree/v0.16.2) (2019-12-04)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/v0.16.1...v0.16.2)

**Merged pull requests:**

- Fix regression introduced by frozen symbol fix [\#86](https://github.com/matthewrudy/memoist/pull/86) ([sebjacobs](https://github.com/sebjacobs))

## [v0.16.1](https://github.com/matthewrudy/memoist/tree/v0.16.1) (2019-11-08)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/v0.16.0...v0.16.1)

**Merged pull requests:**

- Remove ruby 1.9.2 from travis build matrix [\#84](https://github.com/matthewrudy/memoist/pull/84) ([unasuke](https://github.com/unasuke))
- Make Memoist.escape\_punctuation compatible with MRI 2.7 [\#82](https://github.com/matthewrudy/memoist/pull/82) ([casperisfine](https://github.com/casperisfine))
- add 2.5.1 to travis [\#77](https://github.com/matthewrudy/memoist/pull/77) ([matthewrudy](https://github.com/matthewrudy))
- Remove ghit.me [\#74](https://github.com/matthewrudy/memoist/pull/74) ([matthewrudy](https://github.com/matthewrudy))
- Place sample code for execution in README.md [\#73](https://github.com/matthewrudy/memoist/pull/73) ([3nan3](https://github.com/3nan3))
- Require Ruby \>=1.9.2 [\#69](https://github.com/matthewrudy/memoist/pull/69) ([matthewrudy](https://github.com/matthewrudy))

## [v0.16.0](https://github.com/matthewrudy/memoist/tree/v0.16.0) (2017-06-20)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/v0.15.0...v0.16.0)

**Merged pull requests:**

- Fix undefined `memoized\_methods` error raised when a parent class has not call `memoize` [\#68](https://github.com/matthewrudy/memoist/pull/68) ([PikachuEXE](https://github.com/PikachuEXE))
- Add support for class-level cache flushing. [\#67](https://github.com/matthewrudy/memoist/pull/67) ([JoeMcB](https://github.com/JoeMcB))
- Add ruby 2.4 to travis \(bump 2.2 and 2.3 versions\) [\#64](https://github.com/matthewrudy/memoist/pull/64) ([jrafanie](https://github.com/jrafanie))
- Fix tests for Ruby \< 1.9.3 [\#56](https://github.com/matthewrudy/memoist/pull/56) ([matthewrudy](https://github.com/matthewrudy))
- Add return in comments for `flush\_cache`. [\#55](https://github.com/matthewrudy/memoist/pull/55) ([joshuapinter](https://github.com/joshuapinter))
- Update readme [\#53](https://github.com/matthewrudy/memoist/pull/53) ([biow0lf](https://github.com/biow0lf))

## [v0.15.0](https://github.com/matthewrudy/memoist/tree/v0.15.0) (2016-08-23)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/v0.14.0...v0.15.0)

**Merged pull requests:**

- Remove test warnings [\#52](https://github.com/matthewrudy/memoist/pull/52) ([matthewrudy](https://github.com/matthewrudy))
- Use SVG badge over PNG [\#44](https://github.com/matthewrudy/memoist/pull/44) ([olivierlacan](https://github.com/olivierlacan))

## [v0.14.0](https://github.com/matthewrudy/memoist/tree/v0.14.0) (2015-12-15)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/v0.13.0...v0.14.0)

**Merged pull requests:**

- Faster2: Cache the method, ivar, and arity and the ancestry memoized methods [\#38](https://github.com/matthewrudy/memoist/pull/38) ([jrafanie](https://github.com/jrafanie))

## [v0.13.0](https://github.com/matthewrudy/memoist/tree/v0.13.0) (2015-11-26)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/v0.12.0...v0.13.0)

**Merged pull requests:**

- Faster memoist with less object allocations [\#36](https://github.com/matthewrudy/memoist/pull/36) ([jrafanie](https://github.com/jrafanie))
- Be optimistic about bundler version [\#35](https://github.com/matthewrudy/memoist/pull/35) ([lotyrin](https://github.com/lotyrin))
- Add syntax highlighting for code blocks. [\#34](https://github.com/matthewrudy/memoist/pull/34) ([joshuapinter](https://github.com/joshuapinter))

## [v0.12.0](https://github.com/matthewrudy/memoist/tree/v0.12.0) (2015-04-13)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/v0.11.0...v0.12.0)

**Merged pull requests:**

- Fix forking link [\#30](https://github.com/matthewrudy/memoist/pull/30) ([brandondrew](https://github.com/brandondrew))
- Update README with :identifier info [\#29](https://github.com/matthewrudy/memoist/pull/29) ([fervic](https://github.com/fervic))

## [v0.11.0](https://github.com/matthewrudy/memoist/tree/v0.11.0) (2014-10-10)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/v0.10.0...v0.11.0)

**Merged pull requests:**

- Call abs on arity when extracting reload [\#27](https://github.com/matthewrudy/memoist/pull/27) ([bradylove](https://github.com/bradylove))

## [v0.10.0](https://github.com/matthewrudy/memoist/tree/v0.10.0) (2014-08-13)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/v0.9.3...v0.10.0)

**Merged pull requests:**

- Make memoize return a :symbol [\#24](https://github.com/matthewrudy/memoist/pull/24) ([matthewrudy](https://github.com/matthewrudy))
- Use Minitest [\#19](https://github.com/matthewrudy/memoist/pull/19) ([matthewrudy](https://github.com/matthewrudy))

## [v0.9.3](https://github.com/matthewrudy/memoist/tree/v0.9.3) (2014-06-01)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/v0.9.2...v0.9.3)

**Merged pull requests:**

- Remove Array caching hack [\#17](https://github.com/matthewrudy/memoist/pull/17) ([matthewrudy](https://github.com/matthewrudy))

## [v0.9.2](https://github.com/matthewrudy/memoist/tree/v0.9.2) (2014-04-16)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/0.9.0...v0.9.2)

**Merged pull requests:**

- Give double-memoize errors their own error class [\#15](https://github.com/matthewrudy/memoist/pull/15) ([zachhale](https://github.com/zachhale))
- Add tax-themed example for class method memoization fixes \#9 [\#10](https://github.com/matthewrudy/memoist/pull/10) ([fny](https://github.com/fny))

## [0.9.0](https://github.com/matthewrudy/memoist/tree/0.9.0) (2013-03-20)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/0.2.0...0.9.0)

**Merged pull requests:**

- Update README.md to include memoization bypass description [\#6](https://github.com/matthewrudy/memoist/pull/6) ([andreychernih](https://github.com/andreychernih))
- Adds a note about the MIT License [\#4](https://github.com/matthewrudy/memoist/pull/4) ([matiaskorhonen](https://github.com/matiaskorhonen))

## [0.2.0](https://github.com/matthewrudy/memoist/tree/0.2.0) (2012-08-15)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/0.1.0...0.2.0)

**Merged pull requests:**

- Improved performance of flush\_cache and prime\_cache when parameters are passed to them. [\#2](https://github.com/matthewrudy/memoist/pull/2) ([jrafanie](https://github.com/jrafanie))

## [0.1.0](https://github.com/matthewrudy/memoist/tree/0.1.0) (2012-01-24)

[Full Changelog](https://github.com/matthewrudy/memoist/compare/7a5352d6b6c4219f37f329d2422985961c749748...0.1.0)



\* *This Changelog was automatically generated by [github_changelog_generator](https://github.com/github-changelog-generator/github-changelog-generator)*
