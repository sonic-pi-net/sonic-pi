require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#compact" do

    it "is lazy" do
      -> { Hamster.stream { fail }.compact }.should_not raise_error
    end

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
          @original = Hamster.list(*values)
          @result = @original.compact
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
