require "spec_helper"
require "hamster/list"

describe Hamster::List do
  describe "#add" do
    it "adds an item onto the end of a list" do
      list = Hamster.list("a", "b")
      list.add("c").should eql(Hamster.list("a", "b", "c"))
      list.should eql(Hamster.list("a", "b"))
    end

    context "on an empty list" do
      it "returns a list with one item" do
        list = Hamster.list
        list.add("c").should eql(Hamster.list("c"))
        list.should eql(Hamster.list)
      end
    end
  end
end