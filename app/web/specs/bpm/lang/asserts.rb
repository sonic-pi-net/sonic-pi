# assert and friends: a failed assertion is an error carrying its message
use_bpm 120   # at twice the tempo: specs/lang/asserts.rb
assert true
assert_equal 1, 1
assert_not false
assert_not_equal 1, 2
assert_similar 1.000001, 1.0
assert_error ArgumentError do
  raise ArgumentError, "x"
end
puts :still_here
in_thread do
  assert_equal [1, 2], [1, 3], "lists"
end
in_thread do
  sleep 0.1
  assert 1 == 2
end
