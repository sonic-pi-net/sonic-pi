import sys

MODULE = sys.argv[1]

if MODULE=="numeric":
    from Numeric import *
    from LinearAlgebra import *
elif MODULE=="numarray":
    from numarray import *
    from LinearAlgebra import *
elif MODULE=="numpy":
    from numpy import *
    from numpy.linalg import solve
