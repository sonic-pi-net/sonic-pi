require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#inits" do

    it "is lazy" do
      -> { Hamster.stream { fail }.inits }.should_not raise_error
    end

    [
      [[], [[]]],
      [["A"], [[], ["A"]]],
      [%w[A B C], [[], ["A"], %w[A B], %w[A B C]]],
    ].each do |values, expected|

      expected = expected.map { |x| Hamster.list(*x) }

      describe "on #{values.inspect}" do

        before do
          @original = Hamster.list(*values)
          @result = @original.inits
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
