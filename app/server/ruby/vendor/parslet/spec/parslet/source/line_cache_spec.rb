require 'spec_helper'

describe Parslet::Source::RangeSearch do
  describe "<- #lbound" do
    context "for a simple array" do
      let(:ary) { [10, 20, 30, 40, 50] }
      before(:each) { ary.extend Parslet::Source::RangeSearch }

      it "should return correct answers for numbers not in the array" do
        ary.lbound(5).should == 0
        ary.lbound(15).should == 1
        ary.lbound(25).should == 2
        ary.lbound(35).should == 3
        ary.lbound(45).should == 4
      end
      it "should return correct answers for numbers in the array" do
        ary.lbound(10).should == 1
        ary.lbound(20).should == 2
        ary.lbound(30).should == 3
        ary.lbound(40).should == 4
      end
      it "should cover right edge case" do
        ary.lbound(50).should be_nil
        ary.lbound(51).should be_nil
      end 
      it "should cover left edge case" do
        ary.lbound(0).should == 0
      end
    end
    context "for an empty array" do
      let(:ary) { [] }
      before(:each) { ary.extend Parslet::Source::RangeSearch }

      it "should return nil" do
        ary.lbound(1).should be_nil
      end 
    end
  end
end

describe Parslet::Source::LineCache do
  describe "<- scan_for_line_endings" do
    context "calculating the line_and_columns" do
      let(:str) { "foo\nbar\nbazd" }

      it "should return the first line if we have no line ends" do
        subject.scan_for_line_endings(0, nil)
        subject.line_and_column(3).should == [1, 4]

        subject.scan_for_line_endings(0, "")
        subject.line_and_column(5).should == [1, 6]
      end

      it "should find the right line starting from pos 0" do
        subject.scan_for_line_endings(0, str)
        subject.line_and_column(5).should == [2, 2]
        subject.line_and_column(9).should == [3, 2]
      end

      it "should find the right line starting from pos 5" do
        subject.scan_for_line_endings(5, str)
        subject.line_and_column(11).should == [2, 3]
      end

      it "should find the right line if scannning the string multiple times" do
        subject.scan_for_line_endings(0, str)
        subject.scan_for_line_endings(0, "#{str}\nthe quick\nbrown fox")
        subject.line_and_column(10).should == [3,3]
        subject.line_and_column(24).should == [5,2]
      end
    end
  end
end

