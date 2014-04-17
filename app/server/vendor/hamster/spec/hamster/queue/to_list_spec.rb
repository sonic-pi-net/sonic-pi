require "spec_helper"

require "hamster/queue"
require "hamster/list"

describe Hamster::Queue do

  describe "#to_list" do

    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|

      describe "on #{values.inspect}" do

        before do
          @list = Hamster.queue(*values).to_list
        end

        it "returns a list containing #{values.inspect}" do
          @list.should == Hamster.list(*values)
        end

      end

    end

    describe "after dequeueing an item from #{%w[A B C].inspect}" do

      before do
        @list = Hamster.queue("A", "B", "C").dequeue.to_list
      end

      it "returns a list containing #{%w[B C].inspect}" do
        @list.should == Hamster.list("B", "C")
      end

    end

  end

end
