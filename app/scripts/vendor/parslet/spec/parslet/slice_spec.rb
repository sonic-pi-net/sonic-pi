require 'spec_helper'

describe Parslet::Slice do
  describe "construction" do
    it "should construct from an offset and a string" do
      described_class.new('foobar', 40)
    end
  end
  context "('foobar', 40)" do
    let(:slice) { described_class.new('foobar', 40) }
    describe "comparison" do
      it "should be equal to other slices with the same attributes" do
        other = described_class.new('foobar', 40)
        slice.should == other
        other.should == slice
      end 
      it "should be equal to other slices (offset is irrelevant for comparison)" do
        other = described_class.new('foobar', 41)
        slice.should == other
        other.should == slice
      end 
      it "should be equal to a string with the same content" do
        slice.should == 'foobar'
      end
      it "should be equal to a string (inversed operands)" do
        'foobar'.should == slice
      end 
      it "should not be equal to a string" do
        slice.should_not equal('foobar')
      end 
      it "should not be eql to a string" do
        slice.should_not eql('foobar')
      end 
      it "should not hash to the same number" do
        slice.hash.should_not == 'foobar'.hash
      end 
    end
    describe "offset" do
      it "should return the associated offset" do
        slice.offset.should == 40
      end
      it "should fail to return a line and column" do
        lambda {
          slice.line_and_column
        }.should raise_error(ArgumentError)
      end 
      
      context "when constructed with a source" do
        let(:slice) { described_class.new(
          'foobar', 40,  
          flexmock(:cache, :line_and_column => [13, 14])) }
        it "should return proper line and column" do
          slice.line_and_column.should == [13, 14]
        end
      end
    end
    describe "string methods" do
      describe "matching" do
        it "should match as a string would" do
          slice.should match(/bar/)
          slice.should match(/foo/)

          md = slice.match(/f(o)o/)
          md.captures.first.should == 'o'
        end
      end
      describe "<- #size" do
        subject { slice.size }
        it { should == 6 } 
      end
      describe "<- #+" do
        let(:other) { described_class.new('baz', 10) }
        subject { slice + other }
        
        it "should concat like string does" do
          subject.size.should == 9
          subject.should == 'foobarbaz'
          subject.offset.should == 40
        end 
      end
    end
    describe "conversion" do
      describe "<- #to_slice" do
        it "should return self" do
          slice.to_slice.should eq(slice)
        end 
      end
      describe "<- #to_sym" do
        it "should return :foobar" do
          slice.to_sym.should == :foobar
        end 
      end
      describe "cast to Float" do
        it "should return a float" do
          Float(described_class.new('1.345', 11)).should == 1.345
        end 
      end
      describe "cast to Integer" do
        it "should cast to integer as a string would" do
          s = described_class.new('1234', 40)
          Integer(s).should == 1234
          s.to_i.should == 1234
        end 
        it "should fail when Integer would fail on a string" do
          lambda { Integer(slice) }.should raise_error
        end 
        it "should turn into zero when a string would" do
          slice.to_i.should == 0
        end 
      end
    end
    describe "inspection and string conversion" do
      describe "#inspect" do
        subject { slice.inspect }
        it { should == '"foobar"@40' }
      end
      describe "#to_s" do
        subject { slice.to_s }
        it { should == 'foobar' }
      end
    end
    describe "serializability" do
      it "should serialize" do
        Marshal.dump(slice)
      end
      context "when storing a line cache" do
        let(:slice) { described_class.new('foobar', 40, Parslet::Source::LineCache.new()) }
        it "should serialize" do
          Marshal.dump(slice)
        end
      end
    end
  end
end