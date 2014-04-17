require "spec_helper"

require "hamster/hash"

describe Hamster::Hash do

  [:remove, :reject, :delete_if].each do |method|

    describe "##{method}" do

      before do
        @original = Hamster.hash("A" => "aye", "B" => "bee", "C" => "see")
      end

      describe "when nothing matches" do

        before do
          @result = @original.send(method) { |key, value| false }
        end

        it "returns self" do
          @result.should equal(@original)
        end

      end

      describe "when only some things match" do

        describe "with a block" do

          before do
            @result = @original.send(method) { |key, value| key == "A" && value == "aye" }
          end

          it "preserves the original" do
            @original.should == Hamster.hash("A" => "aye", "B" => "bee", "C" => "see")
          end

          it "returns a set with the matching values" do
            @result.should == Hamster.hash("B" => "bee", "C" => "see")
          end

        end

        describe "with no block" do

          before do
            @result = @original.send(method)
          end

          it "returns self" do
            @result.should equal(@original)
          end

        end

      end

    end

  end

end
