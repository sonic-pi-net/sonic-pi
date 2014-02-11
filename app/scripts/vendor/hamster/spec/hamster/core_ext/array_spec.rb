require "spec_helper"

require "hamster/core_ext/enumerable"

describe Array do

  describe "#to_list" do

    before do
      array = %w[A B C]
      @list = array.to_list
    end

    it "returns an equivalent list" do
      @list.should == Hamster.list("A", "B", "C")
    end

  end

end
