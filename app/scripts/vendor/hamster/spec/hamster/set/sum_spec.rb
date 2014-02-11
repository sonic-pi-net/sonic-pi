require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  describe "#sum" do

    [
      [[], 0],
      [[2], 2],
      [[1, 3, 5, 7, 11], 27],
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          original = Hamster.set(*values)
          @result = original.sum
        end

        it "returns #{expected.inspect}" do
          @result.should == expected
        end

      end

    end

  end

end
