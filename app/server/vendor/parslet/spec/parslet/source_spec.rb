# Encoding: UTF-8

require 'spec_helper'

describe Parslet::Source do
  describe "using simple input" do
    let(:str)     { "a"*100 + "\n" + "a"*100 + "\n" }
    let(:source)  { described_class.new(str) }
  
    describe "<- #read(n)" do
      it "should not raise error when the return value is nil" do
        described_class.new('').consume(1)
      end 
      it "should return 100 'a's when reading 100 chars" do
        source.consume(100).should == 'a'*100
      end
    end
    describe "<- #chars_left" do
      subject { source.chars_left }
  
      it { should == 202 }
      context "after depleting the source" do
        before(:each) { source.consume(10000) }
  
        it { should == 0 }
      end
    end
    describe "<- #pos" do
      subject { source.pos }
  
      it { should == 0 }
      context "after reading a few bytes" do
        it "should still be correct" do
          pos = 0
          10.times do
            pos += (n = rand(10)+1)
            source.consume(n)
  
            source.pos.should == pos
          end
        end 
      end
    end
    describe "<- #pos=(n)" do
      subject { source.pos }
      10.times do
        pos = rand(200)
        context "setting position #{pos}" do
          before(:each) { source.pos = pos }
  
          it { should == pos }
        end
      end
    end
    describe '#chars_until' do
      it 'should return 100 chars before line end' do
        source.chars_until("\n").should == 100
      end
    end
    describe "<- #column & #line" do
      subject { source.line_and_column }
  
      it { should == [1,1] }
  
      context "on the first line" do
        it "should increase column with every read" do
          10.times do |i|
            source.line_and_column.last.should == 1+i
            source.consume(1)
          end
        end 
      end
      context "on the second line" do
        before(:each) { source.consume(101) }
        it { should == [2, 1]}
      end
      context "after reading everything" do
        before(:each) { source.consume(10000) }
  
        context "when seeking to 9" do
          before(:each) { source.pos = 9 }
          it { should == [1, 10] }
        end
        context "when seeking to 100" do
          before(:each) { source.pos = 100 }
          it { should == [1, 101] }
        end
        context "when seeking to 101" do
          before(:each) { source.pos = 101 }
          it { should == [2, 1] }
        end
        context "when seeking to 102" do
          before(:each) { source.pos = 102 }
          it { should == [2, 2] }
        end
        context "when seeking beyond eof" do
          it "should not throw an error" do
            source.pos = 1000
          end 
        end
      end
      context "reading char by char, storing the results" do
        attr_reader :results
        before(:each) { 
          @results = {}
          while source.chars_left>0
            pos = source.pos
            @results[pos] = source.line_and_column
            source.consume(1)
          end
  
          @results.should have(202).entries
          @results
        }
  
        context "when using pos argument" do
          it "should return the same results" do
            results.each do |pos, result|
              source.line_and_column(pos).should == result
            end
          end 
        end
        it "should give the same results when seeking" do
          results.each do |pos, result|
            source.pos = pos
            source.line_and_column.should == result
          end
        end
        it "should give the same results when reading" do
          cur = source.pos = 0
          while source.chars_left>0
            source.line_and_column.should == results[cur]
            cur += 1
            source.consume(1)
          end
        end 
      end
    end
    
  end
  
  describe "reading encoded input" do
    let(:source) { described_class.new("éö変わる") }

    def r str
      Regexp.new(Regexp.escape(str))
    end
    
    it "should read characters, not bytes" do
      source.should match(r("é"))
      source.consume(1)
      source.pos.should == 2
      
      source.should match(r("ö"))
      source.consume(1)
      source.pos.should == 4
      
      source.should match(r("変"))
      source.consume(1)
      
      source.consume(2)
      source.chars_left.should == 0
    end 
  end
end
