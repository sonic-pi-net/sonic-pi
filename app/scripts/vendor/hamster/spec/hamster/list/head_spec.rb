require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:head, :first].each do |method|

    describe "##{method}" do

      [
        [[], nil],
        [["A"], "A"],
        [%w[A B C], "A"],
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @list = Hamster.list(*values)
          end

          it "returns #{expected.inspect}" do
            @list.send(method).should == expected
          end

        end

      end

    end

  end

end
