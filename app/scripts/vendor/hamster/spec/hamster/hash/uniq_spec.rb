require "spec_helper"

require "hamster/hash"

describe Hamster::Hash do

  before do
    @hash = Hamster.hash("A" => "aye", "B" => "bee", "C" => "see")
  end

  [:uniq, :nub, :remove_duplicates].each do |method|

    describe "##{method}" do

      it "returns self" do
        @hash.send(method).should equal(@hash)
      end

    end

  end

end
