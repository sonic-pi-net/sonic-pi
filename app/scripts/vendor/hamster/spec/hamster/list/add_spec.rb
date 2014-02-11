require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#add" do

    it "should add an item onto the end of a list" do
      list = Hamster.list("a", "b")
      list.add("c").should == Hamster.list("a", "b", "c")
    end

  end

end
