require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#chunk" do

    it "is lazy" do
      -> { Hamster.stream { fail }.chunk(2) }.should_not raise_error
    end

    [
      [[], []],
      [["A"], [Hamster.list("A")]],
      [%w[A B C], [Hamster.list("A", "B"), Hamster.list("C")]],
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          @original = Hamster.list(*values)
          @result = @original.chunk(2)
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
