require 'spec_helper'

describe Parslet::ErrorReporter::Deepest do
  let(:reporter) { described_class.new }
  let(:fake_source) { flexmock('source') }
  
  describe '#err' do
    before(:each) { fake_source.should_receive(
      :pos => 13, 
      :line_and_column => [1,1]) }
    
    it "returns the deepest cause" do
      flexmock(reporter).
        should_receive(:deepest).and_return(:deepest)
      reporter.err('parslet', fake_source, 'message').
        should == :deepest
    end 
  end
  describe '#err_at' do
    before(:each) { fake_source.should_receive(
      :pos => 13, 
      :line_and_column => [1,1]) }

    it "returns the deepest cause" do
      flexmock(reporter).
        should_receive(:deepest).and_return(:deepest)
      reporter.err('parslet', fake_source, 'message', 13).
        should == :deepest
    end
  end
  describe '#deepest(cause)' do
    def fake_cause(pos=13, children=nil)
      flexmock('cause' + pos.to_s, :pos => pos, :children => children)
    end
    
    context "when there is no deepest cause yet" do
      let(:cause) { fake_cause }
      it "returns the given cause" do
        reporter.deepest(cause).should == cause
      end
    end
    context "when the previous cause is deeper (no relationship)" do
      let(:previous) { fake_cause }
      before(:each) { 
        reporter.deepest(previous) }
      
      it "returns the previous cause" do
        reporter.deepest(fake_cause(12)).
          should == previous
      end 
    end
    context "when the previous cause is deeper (child)" do
      let(:previous) { fake_cause }
      before(:each) { 
        reporter.deepest(previous) }
        
      it "returns the given cause" do
        given = fake_cause(12, [previous])
        reporter.deepest(given).should == given
      end 
    end
    context "when the previous cause is shallower" do
      before(:each) { 
        reporter.deepest(fake_cause) }
      
      it "stores the cause as deepest" do
        deeper = fake_cause(14)
        reporter.deepest(deeper)
        reporter.deepest_cause.should == deeper
      end
    end
  end
end