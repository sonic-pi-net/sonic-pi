require "spec_helper"

require "hamster/queue"

describe Hamster::Queue do

  [:to_a, :entries].each do |method|

    describe "##{method}" do

      [
        [],
        ["A"],
        %w[A B C],
      ].each do |values|

        describe "on #{values.inspect}" do

          before do
            @queue = Hamster.queue(*values)
            @result = @queue.send(method)
          end

          it "returns #{values.inspect}" do
            @result.should == values
          end

          it "returns a mutable array" do
            expect(@result.last).to_not eq("The End")
            @result << "The End"
            @result.last.should == "The End"
          end

        end

      end

    end

  end

end
