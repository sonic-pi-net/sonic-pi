require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:find_indices, :indices].each do |method|

    describe "##{method}" do

      it "is lazy" do
        -> { Hamster.stream { fail }.send(method) { |item| true } }.should_not raise_error
      end

      [
        [[], "A", []],
        [["A"], "B", []],
        [%w[A B A], "B", [1]],
        [%w[A B A], "A", [0, 2]],
        [[2], 2, [0]],
        [[2], 2.0, [0]],
        [[2.0], 2.0, [0]],
        [[2.0], 2, [0]],
      ].each do |values, item, expected|

        describe "looking for #{item.inspect} in #{values.inspect}" do

          before do
            list = Hamster.list(*values)
            @result = list.send(method) { |x| x == item }
          end

          it "returns #{expected.inspect}" do
            @result.should == Hamster.list(*expected)
          end

        end

      end

    end

  end

end
