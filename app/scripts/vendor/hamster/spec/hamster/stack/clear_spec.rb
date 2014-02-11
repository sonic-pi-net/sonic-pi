require "spec_helper"

require "hamster/stack"

describe Hamster::Stack do

  describe "#stack" do

    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|

      describe "on #{values}" do

        before do
          @original = Hamster.stack(*values)
          @result = @original.clear
        end

        it "preserves the original" do
          @original.should == Hamster.stack(*values)
        end

        it "returns an empty list" do
          @result.should equal(Hamster.stack)
        end

      end

    end

  end

end
