require "spec_helper"

require "hamster/tuple"

describe Hamster::Tuple do

  describe "#to_a" do

    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|

      describe "on #{values.inspect}" do

        before do
          @tuple = Hamster::Tuple.new(*values)
          @result = @tuple.to_a
        end

        it "returns #{values.inspect}" do
          @result.should == values
        end

        it "returns a mutable array" do
          expect(@result.last).to_not eq("The End")
          @result << "The End"
          @result.last.should == "The End"
        end

      end

    end

  end

end
