Entry points
------------

Parser should be kept as slim as possible. This includes not loading
any potentially large files when they are likely to be unused in practice.

Parser has five main (classes of) `require` entry points:

 * `require 'parser'`. Main entry point, requires all classes which
   are used across the entire library.
 * `require 'parser/rubyXX'`. Version-specific entry point. Can raise
   a NotImplementedError if current Ruby runtime is unable to parse the
   requested Ruby version.
 * `require 'parser/all'`. Requires all available parsers for released
   versions of Ruby. Can raise NotImplementedError.
 * `require 'parser/runner'`. Requires all the stuff which is useful for
   command-line tools but not otherwise.
 * `require 'parser/runner/X'`. Runner-specific entry point.

All non-main entry points internally `require 'parser'`. Additionally, all
runner-specific entry points internally `requre 'parser/runner'`.
