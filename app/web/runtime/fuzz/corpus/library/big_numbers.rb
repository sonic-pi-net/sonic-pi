# integers past 64 bits, rationals and complex numbers
x = 2**200
y = 3**150 + 12345678901234567890
puts x.to_s.size, (x * y).to_s(16).size, (x / 7 % 1000), (y - x).abs > 0, x.gcd(y), (x << 10) >> 10 == x
puts (x ** 2).to_s[0, 10], Integer.sqrt(x), -x.divmod(13)[1], x.to_f, (x & y | 1) ^ 5, x.bit_length
r = Rational(3, 4) + Rational(5, 6) * 2 - 1r / 3
puts r, r.numerator, r.denominator, r.to_f.round(4), Rational(x, y).denominator > 1, 0.75.to_r
c = Complex(1, 2) * Complex(3, -4)
puts c.real, c.imaginary, c.abs.round(3), (c / Complex(0, 1)).to_s
play 60 + (x % 12)
