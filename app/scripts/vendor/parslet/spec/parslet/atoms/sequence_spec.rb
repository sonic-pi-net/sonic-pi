require 'spec_helper'

describe Parslet::Atoms::Sequence do
  include Parslet
  
  let(:sequence) { described_class.new }
  
  describe '>> shortcut' do
    let(:sequence) { str('a') >> str('b') }
    
    context "when chained with different atoms" do
      before(:each) { 
        # Chain something else to the sequence parslet. If it modifies the
        # parslet atom in place, we'll notice: 
        
        sequence >> str('d')
      }
      let!(:chained) { sequence >> str('c') }
      
      
      it "is side-effect free" do
        chained.should parse('abc')
        chained.should_not parse('abdc')
      end 
    end
    
  end
end
