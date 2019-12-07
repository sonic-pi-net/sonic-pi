require 'spec_helper'

describe Parslet::Atoms::Alternative do
  include Parslet
  
  describe '| shortcut' do
    let(:alternative) { str('a') | str('b') }
    
    context "when chained with different atoms" do
      before(:each) { 
        # Chain something else to the alternative parslet. If it modifies the
        # parslet atom in place, we'll notice: 
        
        alternative | str('d')
      }
      let!(:chained) { alternative | str('c') }
      
      
      it "is side-effect free" do
        chained.should parse('c')
        chained.should parse('a')
        chained.should_not parse('d')
      end 
    end
  end
end