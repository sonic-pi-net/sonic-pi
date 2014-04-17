require File.join( File.dirname(__FILE__) , '..', 'spec_helper' )

describe OSC::OSCInt32 do
  it "should not blow up" do
    OSC::OSCInt32.new 1
  end
end

describe OSC::OSCFloat32 do
  it "should not blow up" do
    OSC::OSCFloat32.new 1.0
  end
end

describe OSC::OSCString do
  it "should not blow up" do
    OSC::OSCString.new "1"
  end
end

describe OSC::OSCBlob do
  it "should not blow up" do
    OSC::OSCBlob.new 1
  end
end