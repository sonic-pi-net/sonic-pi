# small and awkward fractions accumulate exactly
10.times do
  play 60
  sleep 0.1
end
play 62
sleep 1/3.0
play 64
sleep Rational(1, 3)
play 65
