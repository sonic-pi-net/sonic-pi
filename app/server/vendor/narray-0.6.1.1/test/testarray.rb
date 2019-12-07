require 'narray'

a = NArray.float(3,3).indgen

p NArray[a,[100,101]]

b = NArray[ [ [ 0.0, 1.0, 2.0 ], 
              [ 3.0, 4.0, 5.0 ], 
              [ 6.0, 7.0, 8.0 ] ],
            [100,101] ]
p b

a = NArray.float(2,2).indgen

b = NArray[ a,[a] ]
p b

b = NArray[ [ 0.0, 1.0 ], 
            [ [ 0.0, 1.0 ] ] ]
p b
