require "spec_helper"

require "hamster/queue"

describe Hamster::Queue do

  [:head, :first, :peek, :front].each do |method|

    describe "##{method}" do

      [
        [[], nil],
        [["A"], "A"],
        [%w[A B C], "A"],
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @queue = Hamster.queue(*values)
          end

          it "returns #{expected.inspect}" do
            @queue.send(method).should == expected
          end

        end

      end

    end

  end

end
