require "spec_helper"
require "hamster/list"

describe Hamster::List do
  context "without a comparator" do
    context "on an empty list" do
      it "returns an empty list" do
        Hamster.list.merge_by.should be_empty
      end
    end

    context "on a single list" do
      let(:list) { Hamster.list(1, 2, 3) }

      it "returns the list" do
        Hamster.list(list).merge_by.should eql(list)
      end
    end

    context "with multiple lists" do
      subject { Hamster.list(Hamster.list(3, 6, 7, 8), Hamster.list(1, 2, 4, 5, 9)) }

      it "merges the lists based on natural sort order" do
        subject.merge_by.should == Hamster.list(1, 2, 3, 4, 5, 6, 7, 8, 9)
      end
    end
  end

  context "with a comparator" do
    context "on an empty list" do
      it "returns an empty list" do
        Hamster.list.merge_by { |item| fail("should never be called") }.should be_empty
      end
    end

    context "on a single list" do
      let(:list) { Hamster.list(1, 2, 3) }

      it "returns the list" do
        Hamster.list(list).merge_by { |item| -item }.should == Hamster.list(1, 2, 3)
      end
    end

    context "with multiple lists" do
      subject { Hamster.list(Hamster.list(8, 7, 6, 3), Hamster.list(9, 5, 4, 2, 1)) }

      it "merges the lists based on the specified transformer" do
        subject.merge_by { |item| -item }.should == Hamster.list(9, 8, 7, 6, 5, 4, 3, 2, 1)
      end
    end
  end
end