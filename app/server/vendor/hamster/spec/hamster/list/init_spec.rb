require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#init" do

    it "is lazy" do
      -> { Hamster.stream { false }.init }.should_not raise_error
    end

    [
      [[], []],
      [["A"], []],
      [%w[A B C], %w[A B]],
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          @original = Hamster.list(*values)
          @result = @original.init
        end

        it "preserves the original" do
          @original.should == Hamster.list(*values)
        end

        it "returns the list without the last element: #{expected.inspect}" do
          @result.should == Hamster.list(*expected)
        end

      end

    end

  end

end
