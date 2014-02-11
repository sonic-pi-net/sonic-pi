require "spec_helper"

require "hamster/hash"

describe Hamster::Hash do

  describe "#clear" do

    [
      [],
      ["A" => "aye"],
      ["A" => "aye", "B" => "bee", "C" => "see"],
    ].each do |values|

      describe "on #{values}" do

        before do
          @original = Hamster.hash(*values)
          @result = @original.clear
        end

        it "preserves the original" do
          @original.should == Hamster.hash(*values)
        end

        it "returns an empty hash" do
          @result.should equal(Hamster.hash)
        end

      end

    end

  end

end
