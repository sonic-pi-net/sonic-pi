require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  before do
    @set = Hamster.set("A", "B", "C")
  end

  [:uniq, :nub, :remove_duplicates].each do |method|

    describe "##{method}" do

      it "returns self" do
        @set.send(method).should equal(@set)
      end

    end

  end

end
