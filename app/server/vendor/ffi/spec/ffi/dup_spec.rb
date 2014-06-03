#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#


require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

describe "Pointer#dup" do 
  it "clone should be independent" do
    p1 = FFI::MemoryPointer.new(:char, 1024)
    p1.put_string(0, "test123");
    p2 = p1.dup
    p1.put_string(0, "deadbeef")
    
    p2.get_string(0).should == "test123"
  end
  
  it "sliced pointer can be cloned" do
    p1 = FFI::MemoryPointer.new(:char, 1024)
    p1.put_string(0, "test123");
    p2 = p1[1].dup
    
    # first char will be excised
    p2.get_string(0).should == "est123"
    p1.get_string(0).should == "test123"
  end
  
  it "sliced pointer when cloned is independent" do
    p1 = FFI::MemoryPointer.new(:char, 1024)
    p1.put_string(0, "test123");
    p2 = p1[1].dup
    
    p1.put_string(0, "deadbeef")
    # first char will be excised
    p2.get_string(0).should == "est123"
  end
end


describe "Struct#dup" do
  it "clone should be independent" do
    s = Class.new(FFI::Struct) do
      layout :i, :int
    end
    s1 = s.new
    s1[:i] = 0x12345
    s2 = s1.dup
    s1[:i] = 0x98765
    s2[:i].should == 0x12345
    s1[:i].should == 0x98765
  end
  
end
