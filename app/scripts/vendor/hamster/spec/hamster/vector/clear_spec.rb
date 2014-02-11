require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do

  describe "#clear" do

    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|

      describe "on #{values}" do

        before do
          @original = Hamster.vector(*values)
          @result = @original.clear
        end

        it "preserves the original" do
          @original.should == Hamster.vector(*values)
        end

        it "returns an empty vector" do
          @result.should equal(Hamster.vector)
        end

      end

    end

  end

end
