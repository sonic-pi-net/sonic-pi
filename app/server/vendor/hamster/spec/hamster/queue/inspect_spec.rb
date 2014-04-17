require "spec_helper"

require "hamster/queue"

describe Hamster::Queue do

  describe "#inspect" do

    [
      [[], "[]"],
      [["A"], "[\"A\"]"],
      [%w[A B C], "[\"A\", \"B\", \"C\"]"]
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          @queue = Hamster.queue(*values)
        end

        it "returns #{expected.inspect}" do
          @queue.inspect.should == expected
        end

      end

    end

  end

end
