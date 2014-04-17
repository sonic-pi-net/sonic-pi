require "spec_helper"

require "hamster/tuple"
require "hamster/hash"

describe Hamster::Hash do

  [:find, :detect].each do |method|

    describe "##{method}" do

      [
        [[], "A", nil],
        [[], nil, nil],
        [["A" => "aye"], "A", Hamster::Tuple.new("A", "aye")],
        [["A" => "aye"], "B", nil],
        [["A" => "aye"], nil, nil],
        [["A" => "aye", "B" => "bee", nil => "NIL"], "A", Hamster::Tuple.new("A", "aye")],
        [["A" => "aye", "B" => "bee", nil => "NIL"], "B", Hamster::Tuple.new("B", "bee")],
        [["A" => "aye", "B" => "bee", nil => "NIL"], nil, Hamster::Tuple.new(nil, "NIL")],
        [["A" => "aye", "B" => "bee", nil => "NIL"], "C", nil],
      ].each do |values, key, expected|

        describe "on #{values.inspect}" do

          before do
            @hash = Hamster.hash(*values)
          end

          describe "with a block" do

            before do
              @result = @hash.send(method) { |k, v| k == key }
            end

            it "returns #{expected.inspect}" do
              @result.should == expected
            end

          end

          describe "without a block" do

            it "returns nil" do
              @hash.send(method).should be_nil
            end

          end

        end

      end

    end

  end

end
