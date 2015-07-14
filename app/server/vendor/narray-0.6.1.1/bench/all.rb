require "narray"
T = (RUBY_VERSION<"1.8.0") ? Time : Process

ruby_narray = system( "ruby -r narray dummy.rb" )
python_numeric = system( "python dummy.py numeric" )
python_numarray = system( "python dummy.py numarray" )
python_numpy = system( "python dummy.py numpy" )
octave = system( "octave -qf dummy.m" )

def array_size
  list = [
    100000, 200000, 500000,
    1000000, 2000000, 5000000,
    10000000, 20000000, 50000000
  ]
  mlist = [
    150, 200, 300, 500, 700, 1000, 1500, 2000, 3000
    #300, 400, 700, 1000, 1400, 2000, 3000, 4000, 7000
  ]

  r = 50
  m = nil
  j = nil
  list.each_with_index do |n,i|
    a = NArray.float(n)
    b = NArray.float(n)
    t = bench_time(r) { c = a+b }
    j = i
    m = n
    break if t>0.5
  end
  [m, mlist[j], r*2]
end

def bench_time(n)
  t1 = T.times.utime
   for i in 1..n
     yield
   end
  t = T.times.utime - t1
  puts " Time: %.2f sec\n" % [t]
  t
end

n,m,r = array_size
puts "array size = #{n}, repeat = #{r}\n\n"

system "ruby   bench.rb          float  mul #{n} #{r}" if ruby_narray
system "python bench.py numeric  float  mul #{n} #{r}" if python_numeric
system "python bench.py numarray float  mul #{n} #{r}" if python_numarray
system "python bench.py numpy    float  mul #{n} #{r}" if python_numpy
system "octave -qf bench.m       float  mul #{n} #{r}" if octave
puts

system "ruby   bench.rb          int  add #{n} #{r}" if ruby_narray
system "python bench.py numeric  int  add #{n} #{r}" if python_numeric
system "python bench.py numarray int  add #{n} #{r}" if python_numarray
system "python bench.py numpy    int  add #{n} #{r}" if python_numpy
system "octave -qf bench.m       int  add #{n} #{r}" if octave
puts

system "ruby   bench.rb          complex  mul #{n} #{r}" if ruby_narray
system "python bench.py numeric  complex  mul #{n} #{r}" if python_numeric
system "python bench.py numarray complex  mul #{n} #{r}" if python_numarray
system "python bench.py numpy    complex  mul #{n} #{r}" if python_numpy
system "octave -qf bench.m       complex  mul #{n} #{r}" if octave
puts

system "ruby   bench.rb          float_cross mul    #{m*2} #{r}" if ruby_narray
system "python bench.py numeric  float_cross mul    #{m*2} #{r}" if python_numeric
system "python bench.py numarray float_cross mul    #{m*2} #{r}" if python_numarray
system "python bench.py numpy    float_cross mul    #{m*2} #{r}" if python_numpy
system "octave -qf bench.m       float_cross matmul #{m*2} #{r}" if octave
puts

system "ruby   bench.rb          float_matrix mul    #{m} 4" if ruby_narray
system "python bench.py numeric  float_matrix matmul #{m} 4" if python_numeric
system "python bench.py numarray float_matrix matmul #{m} 4" if python_numarray
system "python bench.py numpy    float_matrix matmul #{m} 4" if python_numpy
system "octave -qf bench.m       float_matrix matmul #{m} 4" if octave
puts

system "ruby   bench.rb          float_solve solve #{m} 2" if ruby_narray
system "python bench.py numeric  float_solve solve #{m} 2" if python_numeric
system "python bench.py numarray float_solve solve #{m} 2" if python_numarray
system "python bench.py numpy    float_solve solve #{m} 2" if python_numpy
system "octave -qf bench.m       float_solve solve #{m} 2" if octave
puts

exit
