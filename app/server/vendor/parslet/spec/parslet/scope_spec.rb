require 'spec_helper'

describe Parslet::Scope do
  let(:scope) { described_class.new }
  
  describe 'simple store/retrieve' do
    before(:each) { scope[:foo] = :bar }
    it "allows storing objects" do
      scope[:obj] = 42
    end 
    it "raises on access of empty slots" do
      expect {
        scope[:empty]
      }.to raise_error(Parslet::Scope::NotFound)
    end 
    it "allows retrieval of stored values" do
      scope[:foo].should == :bar
    end 
  end
  
  describe 'scoping' do
    before(:each) { scope[:depth] = 1 }
    before(:each) { scope.push }
    
    let(:depth) { scope[:depth] }
    subject { depth }
    
    it { should == 1 }
    describe 'after a push' do
      before(:each) { scope.push }
      it { should == 1 }
      
      describe 'and reassign' do
        before(:each) { scope[:depth] = 2 }
        
        it { should == 2 }

        describe 'and a pop' do
          before(:each) { scope.pop }
          it { should == 1 }
        end
      end
    end
  end
end