require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#take" do

    it "is lazy" do
      -> { Hamster.stream { fail }.take(1) }.should_not raise_error
    end

    [
      [[], 10, []],
      [["A"], 10, ["A"]],
      [["A"], -1, []],
      [%w[A B C], 0, []],
      [%w[A B C], 2, %w[A B]],
    ].each do |values, number, expected|

      describe "#{number} from #{values.inspect}" do

        before do
          @original = Hamster.list(*values)
          @result = @original.take(number)
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
