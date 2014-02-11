require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:combinations, :combination].each do |method|

    describe "##{method}" do

      it "is lazy" do
        -> { Hamster.stream { fail }.combinations(2) }.should_not raise_error
      end

      [
        [%w[A B C D], 1, [["A"], ["B"], ["C"], ["D"]]],
        [%w[A B C D], 2, [%w[A B], %w[A C], %w[A D], %w[B C], %w[B D], %w[C D]]],
        [%w[A B C D], 3, [%w[A B C], %w[A B D], %w[A C D], %w[B C D]]],
        [%w[A B C D], 4, [%w[A B C D]]],
        [%w[A B C D], 0, [[]]],
        [%w[A B C D], 5, []],
        [[], 0, [[]]],
        [[], 1, []],
      ].each do |values, number, expected|

        expected = expected.map { |x| Hamster.list(*x) }

        describe "on #{values.inspect} in groups of #{number}" do

          before do
            @original = Hamster.list(*values)
            @result = @original.send(method, number)
          end

          it "preserves the original" do
            @original.should == Hamster.list(*values)
          end

          it "returns #{expected.inspect}" do
            @result.should == Hamster.list(*expected)
          end

        end

      end

    end

  end

end
