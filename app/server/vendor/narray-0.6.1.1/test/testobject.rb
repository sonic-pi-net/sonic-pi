require 'narray'
require 'rational'

n = 4
a = NArray.object(4,4).fill!(Rational(1))
b = NArray.object(4,4).indgen!(1).collect{|i| Rational(i)}

print 'a #=> '
p a

print 'b #=> '
p b


class Rational
  def inspect
    self.to_s
  end
end

print 'a+b #=> '
p a+b


print 'a/b #=> '
p a/b

print 'a/b - b #=> '
p a/b - b
