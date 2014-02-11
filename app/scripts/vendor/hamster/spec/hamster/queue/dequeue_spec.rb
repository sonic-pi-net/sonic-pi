require "spec_helper"

require "hamster/queue"

describe Hamster::Queue do

  [:dequeue, :tail].each do |method|

    describe "##{method}" do

      [
        [[], []],
        [["A"], []],
        [%w[A B C], %w[B C]],
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @original = Hamster.queue(*values)
            @result = @original.send(method)
          end

          it "preserves the original" do
            @original.should == Hamster.queue(*values)
          end

          it "returns #{expected.inspect}" do
            @result.should == Hamster.queue(*expected)
          end

        end

      end

    end

  end

end
