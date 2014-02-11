require "spec_helper"

require "hamster/queue"

describe Hamster::Queue do

  [:size, :length].each do |method|

    describe "##{method}" do

      [
        [[], 0],
        [["A"], 1],
        [%w[A B C], 3],
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
