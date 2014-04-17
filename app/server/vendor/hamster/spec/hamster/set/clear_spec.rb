require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  describe "#clear" do

    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|

      describe "on #{values}" do

        before do
          @original = Hamster.set(*values)
          @result = @original.clear
        end

        it "preserves the original" do
          @original.should == Hamster.set(*values)
        end

        it "returns an empty set" do
          @result.should equal(Hamster.set)
        end

      end

    end

  end

end
