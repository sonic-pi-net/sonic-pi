require 'narray'
require 'rational'

#class Rational
#  def inspect
#    @numerator.to_s+"/"+@denominator.to_s
#  end
#end

srand(1)
n=5

m = NMatrix.object(n,n).collect{Rational(rand(10))}

puts 'm  #=>'
p m

puts 'm/m  #=>'
p m/m
