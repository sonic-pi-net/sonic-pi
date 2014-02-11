require "spec_helper"

require "hamster/queue"

describe Hamster::Queue do

  describe "#clear" do

    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|

      describe "on #{values}" do

        before do
          @original = Hamster.queue(*values)
          @result = @original.clear
        end

        it "preserves the original" do
          @original.should == Hamster.queue(*values)
        end

        it "returns an empty queue" do
          @result.should equal(Hamster.queue)
        end

      end

    end

  end

end
