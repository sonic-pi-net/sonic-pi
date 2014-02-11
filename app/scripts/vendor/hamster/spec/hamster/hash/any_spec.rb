require "spec_helper"

require "hamster/hash"

describe Hamster::Hash do

  [:any?, :exist?, :exists?].each do |method|

    describe "##{method}" do

      describe "when empty" do

        before do
          @hash = Hamster.hash
        end

        it "with a block returns false" do
          @hash.send(method) {}.should == false
        end

        it "with no block returns false" do
          @hash.send(method).should == false
        end

      end

      describe "when not empty" do

        before do
          @hash = Hamster.hash("A" => "aye", "B" => "bee", "C" => "see", nil => "NIL")
        end

        describe "with a block" do
          [
            %w[A aye],
            %w[B bee],
            %w[C see],
            [nil, "NIL"],
          ].each do |pair|

            it "returns true if the block ever returns true (#{pair.inspect})" do
              @hash.send(method) { |key, value| key == pair.first && value == pair.last }.should == true
            end

            it "returns false if the block always returns false" do
              @hash.send(method) { |key, value| key == "D" && value == "dee" }.should == false
            end

          end

        end

        describe "with no block" do

          it "returns true" do
            @hash.send(method).should == true
          end

        end

      end

    end

  end

end
