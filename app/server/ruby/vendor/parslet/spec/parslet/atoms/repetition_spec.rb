require 'spec_helper'

describe Parslet::Atoms::Repetition do
  include Parslet

  describe "repeat" do
    let(:parslet) { str('a') }
    
    describe "(min, max)" do
      subject { parslet.repeat(1,2) }
      
      it { should_not parse("") }
      it { should parse("a") }
      it { should parse("aa") }
    end
    describe "0 times" do
      it "raises an ArgumentError" do
        expect {
          parslet.repeat(0,0)
        }.to raise_error(ArgumentError)
      end
    end
  end
end