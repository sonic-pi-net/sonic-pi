require "spec_helper"

require "hamster/hash"

describe Hamster::Hash do

  [:map, :collect].each do |method|

    describe "##{method}" do

      describe "when empty" do

        before do
          @original = Hamster.hash
          @mapped = @original.send(method) {}
        end

        it "returns self" do
          @mapped.should equal(@original)
        end

      end

      describe "when not empty" do

        before do
          @original = Hamster.hash("A" => "aye", "B"  => "bee", "C" => "see")
        end

        describe "with a block" do

          before do
            @mapped = @original.send(method) { |key, value| [key.downcase, value.upcase] }
          end

          it "preserves the original values" do
            @original.should == Hamster.hash("A" => "aye", "B"  => "bee", "C" => "see")
          end

          it "returns a new hash with the mapped values" do
            @mapped.should == Hamster.hash("a" => "AYE", "b"  => "BEE", "c" => "SEE")
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
