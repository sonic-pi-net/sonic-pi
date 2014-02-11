require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  describe "#subset?" do

    [
      [[], [], true],
      [["A"], [], false],
      [[], ["A"], true],
      [["A"], ["A"], true],
      [%w[A B C], ["B"], false],
      [["B"], %w[A B C], true],
      [%w[A B C], %w[A C], false],
      [%w[A C], %w[A B C], true],
      [%w[A B C], %w[A B C], true],
      [%w[A B C], %w[A B C D], true],
      [%w[A B C D], %w[A B C], false],
    ].each do |a, b, expected|

      describe "for #{a.inspect} and #{b.inspect}" do

        before do
          @result = Hamster.set(*a).subset?(Hamster.set(*b))
        end

        it "returns #{expected}"  do
          @result.should == expected
        end

      end

    end

  end

end
