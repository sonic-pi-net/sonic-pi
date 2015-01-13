import time
import sys

MODULE = sys.argv[1]
TYPE   = sys.argv[2]
OP     = sys.argv[3]
ARRSZ  = int(sys.argv[4])
REPEAT = int(sys.argv[5])

if MODULE=="numeric":
    from Numeric import *
    from LinearAlgebra import *
elif MODULE=="numarray":
    from numarray import *
    from LinearAlgebra import *
elif MODULE=="numpy":
    from numpy import *
    from numpy.linalg import solve

def bench_time(func,repeat=REPEAT):
  #start = time.clock()
  start = time.time()
  for i in range(repeat):
    c = func()
  #stop = time.clock()
  stop = time.time()
  print "Python %s type=%s size=%d op=%s repeat=%d  Time: %.6f sec" % \
      (MODULE,TYPE,ARRSZ,OP,REPEAT,stop-start)
  #print shape(c)

n = ARRSZ

if MODULE=="numpy":
    def bench_array(type=float):
        return arange(ARRSZ,dtype=type)

    if TYPE=="float":
        a = bench_array(float)
        b = bench_array(float)
    elif TYPE=="int":
        a = bench_array(int)
        b = bench_array(int)
    elif TYPE=="complex":
        a = bench_array(complex)
        b = bench_array(complex)
    elif TYPE=="float_cross":
        a = reshape(arange(ARRSZ,dtype=float),(ARRSZ,1))
        b = reshape(arange(ARRSZ,dtype=float),(1,ARRSZ))
    elif TYPE=="float_matrix":
        a = reshape(arange(ARRSZ**2,dtype=float),(ARRSZ,ARRSZ))
        b = reshape(arange(ARRSZ**2,dtype=float),(ARRSZ,ARRSZ))
    elif TYPE=="float_solve":
        a = reshape(arange(n**2,dtype=float)%(n+1)+1,(n,n))
        b = reshape(arange(n**2,dtype=float)+1,(n,n))
else:
    def bench_array(type=float):
        return arrayrange(ARRSZ).astype(type)
    if TYPE=="float":
        a = bench_array(Float64)
        b = bench_array(Float64)
    elif TYPE=="int":
        a = bench_array(Int32)
        b = bench_array(Int32)
    elif TYPE=="complex":
        a = bench_array(Complex64)
        b = bench_array(Complex64)
    elif TYPE=="float_cross":
        a = reshape(arrayrange(ARRSZ),(ARRSZ,1)).astype(Float64)
        b = reshape(arrayrange(ARRSZ),(1,ARRSZ)).astype(Float64)
    elif TYPE=="float_matrix":
        a = reshape(arrayrange(ARRSZ**2),(ARRSZ,ARRSZ)).astype(Float64)
        b = reshape(arrayrange(ARRSZ**2),(ARRSZ,ARRSZ)).astype(Float64)
    elif TYPE=="float_solve":
        a = reshape(arrayrange(n*n)%(n+1)+1,(n,n)).astype(Float64)
        b = reshape(arrayrange(n*n)+1,(n,n)).astype(Float64)
    dot = matrixmultiply
    solve = solve_linear_equations

def lambda_add(a=a,b=b):    c = a+b; return c;
def lambda_mul(a=a,b=b):    c = a*b; return c;
def lambda_matmul(a=a,b=b): c = dot(a,b); return c;
def lambda_solve(a=a,b=b):  c = solve(a,b); return c;

if OP=="add":
    bench_time(lambda_add)
elif OP=="mul":
    bench_time(lambda_mul)
elif OP=="matmul":
    bench_time(lambda_matmul)
elif OP=="solve":
    bench_time(lambda_solve)
