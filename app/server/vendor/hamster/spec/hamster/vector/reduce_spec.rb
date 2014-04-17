require "spec_helper"

require "hamster/vector"

describe Hamster::Vector do

  [:reduce, :inject, :fold, :foldr].each do |method|

    describe "##{method}" do

      [
        [[], 10, 10],
        [[1], 10, 9],
        [[1, 2, 3], 10, 4],
      ].each do |values, initial, expected|

        describe "on #{values.inspect}" do

          before do
            @vector = Hamster.vector(*values)
          end

          describe "with an initial value of #{initial}" do

            describe "and a block" do

              it "returns #{expected.inspect}" do
                @vector.send(method, initial) { |memo, item| memo - item }.should == expected
              end

            end

            describe "and no block" do

              it "returns the memo" do
                @vector.send(method, initial).should == initial
              end

            end

          end

        end

      end

      [
        [[], nil],
        [[1], 1],
        [[1, 2, 3], -4],
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @vector = Hamster.vector(*values)
          end

          describe "with no initial value" do

            describe "and a block" do

              it "returns #{expected.inspect}" do
                @vector.send(method) { |memo, item| memo - item }.should == expected
              end

            end

            describe "and no block" do

              it "returns nil" do
                @vector.send(method).should be_nil
              end

            end

          end

        end

      end

    end

  end

end
