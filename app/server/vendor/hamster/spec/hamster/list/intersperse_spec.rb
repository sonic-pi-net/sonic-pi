require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#intersperse" do

    it "is lazy" do
      -> { Hamster.stream { fail }.intersperse("") }.should_not raise_error
    end

    [
      [[], []],
      [["A"], ["A"]],
      [%w[A B C], ["A", "|", "B", "|", "C"]]
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          @original = Hamster.list(*values)
          @result = @original.intersperse("|")
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
