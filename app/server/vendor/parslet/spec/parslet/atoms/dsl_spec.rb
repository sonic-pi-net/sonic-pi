require 'spec_helper'

describe Parslet::Atoms::DSL do
  describe "deprecated methods" do
    let(:parslet) { Parslet.str('foo') }
    describe "<- #absnt?" do
      subject { parslet.absnt? }
      its(:bound_parslet) { should == parslet }
      its(:positive) { should == false }
    end
    describe "<- #prsnt?" do
      subject { parslet.prsnt? }
      its(:bound_parslet) { should == parslet }
      its(:positive) { should == true }
    end
  end
end