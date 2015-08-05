RSpec::Matchers.define :be_less_than_or_equal_to do |expected|
  match do |actual|
    actual <= expected
  end
end