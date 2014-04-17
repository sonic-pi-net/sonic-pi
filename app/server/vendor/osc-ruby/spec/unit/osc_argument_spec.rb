require File.join( File.dirname(__FILE__) , '..', 'spec_helper' )

describe OSC::OSCArgument do
  it "should not blow up" do
    OSC::OSCArgument.new 1
  end
end