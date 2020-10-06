set(benchmarks
  abs_benchmarks.cpp
  clear_benchmark.cpp
  clip_benchmarks.cpp
  copy_benchmark.cpp
  inplace_benchmark.cpp
   round_benchmark.cpp
   simd_ampmod_benchmarks.cpp
   simd_mix_benchmark.cpp
   simd_pan2_benchmark.cpp
   simd_peakmeter_benchmarks.cpp
   simd_plus_benchmarks.cpp
   simd_pow_benchmarks.cpp
   simd_slope_benchmarks.cpp
   simd_softclip_benchmarks.cpp
   simd_tan_benchmarks.cpp
   simd_tanh_benchmarks.cpp
   simd_unroll_benchmarks2.cpp
   simd_unroll_benchmarks.cpp
 )

# build benchmarks
foreach(benchmark ${benchmarks})
  string(REPLACE .cpp "" benchmark_name ${benchmark} )
  add_executable(${benchmark_name} ${benchmark} ${headers})
endforeach(benchmark)
