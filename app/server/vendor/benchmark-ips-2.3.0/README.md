[![Gem Version](https://badge.fury.io/rb/benchmark-ips.svg)](http://badge.fury.io/rb/benchmark-ips)
[![Build Status](https://secure.travis-ci.org/evanphx/benchmark-ips.svg)](http://travis-ci.org/evanphx/benchmark-ips)
[![Inline docs](http://inch-ci.org/github/evanphx/benchmark-ips.svg)](http://inch-ci.org/github/evanphx/benchmark-ips)

# benchmark-ips

* https://github.com/evanphx/benchmark-ips

* [documentation](http://rubydoc.info/gems/benchmark-ips)

## DESCRIPTION:

A iterations per second enhancement to Benchmark.

## FEATURES/PROBLEMS:

 * benchmark/ips - benchmarks a blocks iterations/second. For short snippits
   of code, ips automatically figures out how many times to run the code
   to get interesting data. No more guessing at random iteration counts!

## SYNOPSIS:

```ruby
require 'benchmark/ips'

Benchmark.ips do |x|
  # Configure the number of seconds used during
  # the warmup phase (default 2) and calculation phase (default 5)
  x.config(:time => 5, :warmup => 2)

  # These parameters can also be configured this way
  x.time = 5
  x.warmup = 2

  # Typical mode, runs the block as many times as it can
  x.report("addition") { 1 + 2 }

  # To reduce overhead, the number of iterations is passed in
  # and the block must run the code the specific number of times.
  # Used for when the workload is very small and any overhead
  # introduces incorrectable errors.
  x.report("addition2") do |times|
    i = 0
    while i < times
      1 + 2
      i += 1
    end
  end

  # To reduce overhead even more, grafts the code given into
  # the loop that performs the iterations internally to reduce
  # overhead. Typically not needed, use the |times| form instead.
  x.report("addition3", "1 + 2")

  # Really long labels should be formatted correctly
  x.report("addition-test-long-label") { 1 + 2 }

  # Compare the iterations per second of the various reports!
  x.compare!
end
```

This will generate the following report:

```
Calculating -------------------------------------
            addition    71.254k i/100ms
           addition2    68.658k i/100ms
           addition3    83.079k i/100ms
addition-test-long-label
                        70.129k i/100ms
-------------------------------------------------
            addition     4.955M (± 8.7%) i/s -     24.155M
           addition2    24.011M (± 9.5%) i/s -    114.246M
           addition3    23.958M (±10.1%) i/s -    115.064M
addition-test-long-label
                         5.014M (± 9.1%) i/s -     24.545M

Comparison:
           addition2: 24011974.8 i/s
           addition3: 23958619.8 i/s - 1.00x slower
addition-test-long-label:  5014756.0 i/s - 4.79x slower
            addition:  4955278.9 i/s - 4.85x slower
```

Benchmark/ips will report the number of iterations per second for a given block
of code. When analyzing the results, notice the percent of [standard
deviation](http://en.wikipedia.org/wiki/Standard\_deviation) which tells us how
spread out our measurements are from the average. A high standard deviation
could indicate the results having too much variability.

One benefit to using this method is benchmark-ips automatically determines the
data points for testing our code, so we can focus on the results instead of
guessing iteration counts as we do with the traditional Benchmark library.

### Custom Suite

Pass a custom suite to disable garbage collection during benchmark:

```ruby
require 'benchmark/ips'

# Enable and start GC before each job run. Disable GC afterwards.
#
# Inspired by https://www.omniref.com/ruby/2.2.1/symbols/Benchmark/bm?#annotation=4095926&line=182
class GCSuite
  def warming(*)
    run_gc
  end

  def running(*)
    run_gc
  end

  def warmup_stats(*)
  end

  def add_report(*)
  end

  private

  def run_gc
    GC.enable
    GC.start
    GC.disable
  end
end

suite = GCSuite.new

Benchmark.ips do |x|
  x.config(:suite => suite)
  x.report("job1") { ... }
  x.report("job2") { ... }
end
```

## REQUIREMENTS:

* None!

## INSTALL:

    $ gem install benchmark-ips

## DEVELOPERS:

After checking out the source, run:

    $ rake newb

This task will install any missing dependencies, run the tests/specs,
and generate the RDoc.

## LICENSE:

(The MIT License)

Copyright (c) 2015 Evan Phoenix

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
'Software'), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
