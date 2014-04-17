require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  [:find, :detect].each do |method|

    describe "##{method}" do

      [
        [[], "A", nil],
        [[], nil, nil],
        [["A"], "A", "A"],
        [["A"], "B", nil],
        [["A"], nil, nil],
        [["A", "B", nil], "A", "A"],
        [["A", "B", nil], "B", "B"],
        [["A", "B", nil], nil, nil],
        [["A", "B", nil], "C", nil],
      ].each do |values, item, expected|

        describe "on #{values.inspect}" do

          before do
            @set = Hamster.set(*values)
          end

          describe "with a block" do

            it "returns #{expected.inspect}" do
              @set.send(method) { |x| x == item }.should == expected
            end

          end

          describe "without a block" do

            it "returns nil" do
              @set.send(method).should be_nil
            end

          end

        end

      end

    end

  end

end
