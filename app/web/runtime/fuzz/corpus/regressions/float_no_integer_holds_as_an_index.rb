# The nightly fuzzer, 26 November 2026: a Float that no integer holds (NaN, Infinity, 1e300) used as an array index
# went through a bare C cast in mruby (array.c aget_index), which is undefined: UBSan stopped the run, and without it
# the answer was the processor's own (1 on arm64, nil on x86 for [1, 2, 3][NaN]). Random.rand's limit had the same
# cast. mruby fixed both on 8 October 2026 (e2148c1b1): they are a RangeError now. A ring is an array underneath.
nan = 0.0 / 0.0
inf = 1.0 / 0.0
[nan, inf, -inf, 1e300, -1e300].each do |f|
  begin
    puts [1, 2, 3][f].inspect
  rescue RangeError, FloatDomainError => e
    puts e.class
  end
  begin
    puts (ring 1, 2, 3)[f].inspect
  rescue RangeError, FloatDomainError => e
    puts e.class
  end
  begin
    puts Random.rand(f).inspect
  rescue RangeError, FloatDomainError, ArgumentError => e
    puts e.class
  end
end
puts [1, 2, 3][1.9]
