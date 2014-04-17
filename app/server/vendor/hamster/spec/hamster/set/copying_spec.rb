require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  before do
    @set = Hamster.set("A", "B", "C")
  end

  [:dup, :clone].each do |method|

    describe "##{method}" do

      it "returns self" do
        @set.send(method).should equal(@set)
      end

    end

  end

end
