require 'narray'
T = (RUBY_VERSION<"1.8.0") ? Time : Process

TYPE   = ARGV[0]
OP     = ARGV[1]
ARRSZ  = Integer(ARGV[2])
REPEAT = Integer(ARGV[3])

def bench_array(type=Float)
  [ NArray.new(type,ARRSZ).indgen!,
    NArray.new(type,ARRSZ).indgen!  ]
end

def bench_time(n=REPEAT)
  t1 = T.times.utime
   for i in 1..n
     yield
   end
  t = T.times.utime - t1
  printf "Ruby NArray type=%s size=%d op=%s repeat=%d  Time: %.2f sec\n",
    TYPE,ARRSZ,OP,REPEAT,t
end

n = ARRSZ

case TYPE
when "float"
  a,b = bench_array(Float)
when "int"
  a,b = bench_array(Integer)
when "complex"
  a,b = bench_array(Complex)
when "float_cross"
  a = NArray.float(n,1).indgen!
  b = NArray.float(1,n).indgen!
when "float_matrix"
  a = NArray.float(n,n).indgen!
  a = a % (n+1) + 1
  a = NMatrix.ref(a)#.transpose
  b = NArray.float(n,n).indgen!
  b = b % (n-1) + 1
  b = NMatrix.ref(b)#.transpose
when "float_solve"
  a = NMatrix.float(n,n).indgen!(1).transpose
  b = NArray.float(n,n).indgen!
  b = b % (n+1) + 1
  b = NMatrix.ref(b).transpose
end

c = 0

case OP
when "add"
  bench_time{ c = a+b }
when "mul"
  bench_time{ c = a*b }
when "solve"
  bench_time{ c = a/b }
end
