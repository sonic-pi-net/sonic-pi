## [v0.9.10](https://github.com/yuki24/did_you_mean/tree/v0.9.10)

_<sup>released on 2015-05-14 03:04:47 UTC</sup>_

#### Bug Fixes

- Fixed a bug where a duplicate "did you mean?" message was appended each time `#to_s` is called ( [@danfinnie](https://github.com/danfinnie), [#51](https://github.com/yuki24/did_you_mean/issues/51 "Duplicate output for constants in separate gem"))

## [v0.9.9](https://github.com/yuki24/did_you_mean/tree/v0.9.9)

_<sup>released on 2015-05-13 03:48:19 UTC</sup>_

#### Small/Internal Changes

- Order word suggestions based on Levenshtein distance ( [@tleish](https://github.com/tleish), [#31](https://github.com/yuki24/did_you_mean/pull/31 "Order word suggestions based on Levenshtein.distance."))
- Reduce memory allocation by about 40%
- Speed up Levenshtein distance calculation by about 40%
- The Java extension has been replaced with a pure JRuby implementation

## [v0.9.8](https://github.com/yuki24/did_you_mean/tree/v0.9.8)

_<sup>released on 2015-04-12 01:55:27 UTC</sup>_

#### Internal Changes

- Speed up Levenshtein by 50% and reduce 97% of memory usage

## [v0.9.7](https://github.com/yuki24/did_you_mean/tree/v0.9.7)

_<sup>released on 2015-04-02 04:20:26 UTC</sup>_

#### Bug Fixes

- Fixed an issue where _did\_you\_mean_ doesn't install on JRuby properly.

## [v0.9.6](https://github.com/yuki24/did_you_mean/tree/v0.9.6)

_<sup>released on 2015-01-24 23:19:27 UTC</sup>_

#### Bug Fixes

- Fixed a bug where did\_you\_mean incorrectly suggests protected methods when it just isn't callable ( [@glittershark](https://github.com/glittershark), [#34](https://github.com/yuki24/did_you_mean/issues/34 "Did\_you\_mean incorrectly called when attempting to call protected/private method"))

## [v0.9.5](https://github.com/yuki24/did_you_mean/tree/v0.9.5)

_<sup>released on 2015-01-07 12:41:23 UTC</sup>_

#### Bug Fixes

- Whitelist `#safe_constantize` method from `ActiveSupport::Inflector` to avoid significant performance slowdown ( [@tleish](https://github.com/tleish), [#19](https://github.com/yuki24/did_you_mean/issues/19 "Significant Slowdown when Using Debugger"), [#20](https://github.com/yuki24/did_you_mean/pull/20 "Whitelisting safe\_constantize (ActiveSupport::Inflector) method"))

## [v0.9.4](https://github.com/yuki24/did_you_mean/tree/v0.9.4)

_<sup>released on 2014-11-19 20:00:00 UTC</sup>_

#### Bug Fixes

- Fixed a bug where no suggestions will be made on JRuby

## [v0.9.3](https://github.com/yuki24/did_you_mean/tree/v0.9.3)

_<sup>released on 2014-11-18 03:50:11 UTC</sup>_

**This version has been yanked from rubygems.org as it doesn't work with jRuby at all. Please upgrade to 0.9.4 or higher as soon as possible.**

#### Internal Changes

- Replaced the crazy C extension with a so much better one (thanks to [@nobu](https://github.com/nobu)!)

## [v0.9.2](https://github.com/yuki24/did_you_mean/tree/v0.9.2)

_<sup>released on 2014-11-17 15:32:33 UTC</sup>_

#### Bug Fixes

- Fixed a bug where did\_you\_mean doesn't compile on Ruby 2.1.2/2.1.5 ( [#16](https://github.com/yuki24/did_you_mean/issues/16 "Gem building failed on Debian 6.0.10 x86\_64"))

## [v0.9.1](https://github.com/yuki24/did_you_mean/tree/v0.9.1)

_<sup>released on 2014-11-16 18:54:24 UTC</sup>_

**This version has been yanked from rubygems.org as it doesn't compile on Ruby 2.1.2 and 2.1.5. Please upgrade to 0.9.4 or higher as soon as possible.**

#### Internal Changes

- Shrink the gem size by removing unneeded ruby header files.
- Now it forces everyone to upgrade the gem when they upgrade Ruby to a new version. This avoids introducing a bug like [#14](https://github.com/yuki24/did_you_mean/issues/14 "Compatibility with `letter\_opener` gem").

## [v0.9.0](https://github.com/yuki24/did_you_mean/tree/v0.9.0)

_<sup>released on 2014-11-09 01:26:31 UTC</sup>_

#### New Features

- did\_you\_mean now suggests instance variable names if `@` is missing ( [#12](https://github.com/yuki24/did_you_mean/issues/12 "Suggest instance- and class-vars"), [<tt>39d1e2b</tt>](https://github.com/yuki24/did_you_mean/commit/39d1e2bd66d6ff8acbc4dd5da922fc7e5fcefb20))

```ruby
@full_name = "Yuki Nishijima"
first_name, last_name = full_name.split(" ")
# => NameError: undefined local variable or method `full_name' for main:Object
#
#     Did you mean? @full_name
#
```

#### Bug Fixes

- Fixed a bug where did\_you\_mean changes some behaviours of Ruby 2.1.3/2.1.4 installed on Max OS X ( [#14](https://github.com/yuki24/did_you_mean/issues/14 "Compatibility with `letter\_opener` gem"), [<tt>44c451f</tt>](https://github.com/yuki24/did_you_mean/commit/44c451f8c38b11763ba28ddf1ceb9696707ccea0), [<tt>9ebde21</tt>](https://github.com/yuki24/did_you_mean/commit/9ebde211e92eac8494e704f627c62fea7fdbee16))
- Fixed a bug where sometimes `NoMethodError` suggests duplicate method names ( [<tt>9865cc5</tt>](https://github.com/yuki24/did_you_mean/commit/9865cc5a9ce926dd9ad4c20d575b710e5f257a4b))

## [v0.8.0](https://github.com/yuki24/did_you_mean/tree/v0.8.0)

_<sup>released on 2014-10-27 02:03:13 UTC</sup>_

**This version has been yanked from rubygems.org as it has a serious bug with Ruby 2.1.3 and 2.1.4 installed on Max OS X. Please upgrade to 0.9.4 or higher as soon as possible.**

#### New Features

- JRuby support!

#### Bug Fixes

- Fixed a bug where did\_you\_mean unexpectedly disables [better\_errors](https://github.com/charliesome/better_errors)'s REPL
- Replaced [binding\_of\_caller](https://github.com/banister/binding_of_caller) dependency with [interception](https://github.com/ConradIrwin/interception)
- Fixed the wrong implementation of Levenshtein algorithm ( [#2](https://github.com/yuki24/did_you_mean/pull/2 "Fix bug of DidYouMean::Levenshtein#min3."), [@fortissimo1997](https://github.com/fortissimo1997))

## [v0.7.0](https://github.com/yuki24/did_you_mean/tree/v0.7.0)

_<sup>released on 2014-09-26 03:37:18 UTC</sup>_

**This version has been yanked from rubygems.org as it has a serious bug with Ruby 2.1.3 and 2.1.4 installed on Max OS X. Please upgrade to 0.9.4 or higher as soon as possible.**

#### New Features

- Added support for Ruby 2.1.3, 2.2.0-preview1 and ruby-head
- Added support for ActiveRecord 4.2.0.beta1
- Word searching is now about 40% faster than v0.6.0
- Removed `text` gem dependency
- Better output on pry and Rspec

#### Small/Internal Changes

- A lot of internal refactoring

## [v0.6.0](https://github.com/yuki24/did_you_mean/tree/v0.6.0)

_<sup>released on 2014-05-18 00:23:24 UTC</sup>_

**This version has been yanked from rubygems.org as it has a serious bug with Ruby 2.1.3 and 2.1.4 installed on Max OS X. Please upgrade to 0.9.0 as soon as possible.**

#### New Features

- Added basic support for constants. Now you'll see class name suggestions when you misspelled a class names/module names:

```ruby
> Ocject
# => NameError: uninitialized constant Ocject
#
#     Did you mean? Object
#
```

#### Bug Fixes

- Fixed a bug where did\_you\_mean segfaults on Ruby head(2.2.0dev)

## [v0.5.0](https://github.com/yuki24/did_you_mean/tree/v0.5.0)

_<sup>released on 2014-05-10 17:59:54 UTC</sup>_

#### New Features

- Added support for Ruby 2.1.2

## [v0.4.0](https://github.com/yuki24/did_you_mean/tree/v0.4.0)

_<sup>released on 2014-04-20 02:10:31 UTC</sup>_

#### New Features

- did\_you\_mean now suggests a similar attribute name when you misspelled it.

```ruby
User.new(flrst_name: "wrong flrst name")
# => ActiveRecord::UnknownAttributeError: unknown attribute: flrst_name
#
#     Did you mean? first_name: string
#
```

#### Bug Fixes

- Fixed a bug where did\_you\_mean doesn't work with `ActiveRecord::UnknownAttributeError`

## [v0.3.1](https://github.com/yuki24/did_you_mean/tree/v0.3.1)

_<sup>released on 2014-03-20 23:16:20 UTC</sup>_

#### Small/Internal Changes

- Changed output for readability.
- Better algorithm to find the correct method.

## [v0.3.0](https://github.com/yuki24/did_you_mean/tree/v0.3.0)

_<sup>released on 2014-03-20 23:13:13 UTC</sup>_

#### New Features

- Added support for Ruby 2.1.1 and 2.2.0(head).

## [v0.2.0](https://github.com/yuki24/did_you_mean/tree/v0.2.0)

_<sup>released on 2014-03-20 23:12:13 UTC</sup>_

#### Incompatible Changes

- dropped support for JRuby and Rubbinious.

#### New Features

- did\_you\_mean no longer makes Ruby slow.

## [v0.1.0: First Release](https://github.com/yuki24/did_you_mean/tree/v0.1.0)

_<sup>released on 2014-03-20 23:11:14 UTC</sup>_

- Now you will have "did you mean?" experience in Ruby!
- but still very experimental since this gem makes Ruby a lot slower.
