require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  describe "#to_set" do

    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|

      describe "on #{values.inspect}" do

        before do
          @original = Hamster.set(*values)
          @result = @original.to_set
        end

        it "returns self" do
          @result.should equal(@original)
        end

      end

    end

  end

end
