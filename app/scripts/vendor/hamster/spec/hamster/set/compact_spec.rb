require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  describe "#compact" do

    [
      [[], []],
      [["A"], ["A"]],
      [%w[A B C], %w[A B C]],
      [[nil], []],
      [[nil, "B"], ["B"]],
      [["A", nil], ["A"]],
      [[nil, nil], []],
      [["A", nil, "C"], %w[A C]],
      [[nil, "B", nil], ["B"]],
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          @original = Hamster.set(*values)
          @result = @original.compact
        end

        it "preserves the original" do
          @original.should == Hamster.set(*values)
        end

        it "returns #{expected.inspect}" do
          @result.should == Hamster.set(*expected)
        end

      end

    end

  end

end
