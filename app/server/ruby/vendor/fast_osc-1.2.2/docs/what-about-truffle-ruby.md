# What about Truffle Ruby?

> A high performance implementation of the Ruby programming language. Built on GraalVM by Oracle Labs.

Just for fun, I re-ran the benchmarks using TruffleRuby. First some install steps:

```
git clone this repo
bundle install
export PATH="/usr/local/opt/llvm@4/bin:$PATH"
rake clean && rake compile
export WITH_BENCHMARKS=1
```

then the test:

```
$ rake test
ENCODING TEST
Warming up --------------------------------------
            fast_osc     3.000  i/100ms
             samsosc    28.219k i/100ms
            osc-ruby   234.000  i/100ms
Calculating -------------------------------------
            fast_osc     33.294  (±45.1%) i/s -    129.000  in   5.295649s
             samsosc      1.369M (±33.5%) i/s -      3.556M in   5.010343s
            osc-ruby    274.170k (±20.2%) i/s -      1.064M in   4.971832s
DECODING TEST
Warming up --------------------------------------
            fast_osc     9.000  i/100ms
             samsosc     6.087k i/100ms
            osc-ruby   418.000  i/100ms
Calculating -------------------------------------
            fast_osc     71.034  (±45.0%) i/s -    261.000  in   5.015393s
             samsosc    114.443k (±70.2%) i/s -    261.741k in   5.283892s
            osc-ruby     84.236k (±34.7%) i/s -    237.424k in   5.317738s

# update - retested using rc5
ENCODING TEST
Warming up --------------------------------------
            fast_osc   106.000  i/100ms
             samsosc   999.000  i/100ms
            osc-ruby   159.000  i/100ms
Calculating -------------------------------------
            fast_osc     24.507k (±28.6%) i/s -     83.316k in   4.984544s
             samsosc      1.494M (±22.4%) i/s -      2.785M in   4.988822s
            osc-ruby    265.322k (±26.5%) i/s -    597.522k in   5.014127s
DECODING TEST
Warming up --------------------------------------
            fast_osc   173.000  i/100ms
             samsosc   454.000  i/100ms
            osc-ruby   208.000  i/100ms
Calculating -------------------------------------
            fast_osc     73.712k (±66.4%) i/s -    163.831k in   5.049410s
             samsosc    325.920k (±33.7%) i/s -    875.766k in   4.989279s
            osc-ruby     63.624k (±52.3%) i/s -    129.792k in   5.005088s
Run options: --seed 56032

# update - retested using 19.0.1
ENCODING TEST
Warming up --------------------------------------
            fast_osc   210.000  i/100ms
             samsosc    12.833k i/100ms
            osc-ruby   262.000  i/100ms
Calculating -------------------------------------
            fast_osc    150.306k (±38.0%) i/s -    411.600k in   4.904832s
             samsosc      1.393M (±22.7%) i/s -      5.300M in   4.995566s
            osc-ruby    273.289k (±19.4%) i/s -    714.474k in   4.988380s
DECODING TEST
Warming up --------------------------------------
            fast_osc   507.000  i/100ms
             samsosc   871.000  i/100ms
            osc-ruby   329.000  i/100ms
Calculating -------------------------------------
            fast_osc    577.429k (±34.2%) i/s -    539.448k in   5.028383s
             samsosc    779.126k (±14.0%) i/s -      2.360M in   4.989644s
            osc-ruby    173.848k (±39.8%) i/s -    334.593k in   4.994280s
Run options: --seed 47839
```

### The Good

The encoding benchmark - the optimised pure Ruby version is nearly as fast as
the C extension!

```
# C ext in MRI
fast_osc    797.673k (±15.0%) i/s -      3.900M in   5.043770s
# Pure Ruby in Truffle rc3
samsosc      1.369M (±33.5%) i/s -      3.556M in   5.010343s
# Pure Ruby in Truffle rc5
samsosc      1.494M (±22.4%) i/s -      2.785M in   4.988822s
```

### The Bad

Decoding was generally slower, although the (non optimised) osc gem seemed to
prefer TruffleRuby, running around 40% faster.

~Also the performance of the C extension in TruffleRuby was very, very poor but
this may be due to not warming up enough.~ update - this appears to be better
in rc5.
