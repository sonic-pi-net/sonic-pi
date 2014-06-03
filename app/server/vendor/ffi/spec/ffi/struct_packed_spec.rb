#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

describe FFI::Struct do
  it "packed :char followed by :int should have size of 5" do
    Class.new(FFI::Struct) do
      packed
      layout :c, :char, :i, :int
    end.size.should == 5
  end

  it "packed :char followed by :int should have alignment of 1" do
    Class.new(FFI::Struct) do
      packed
      layout :c, :char, :i, :int
    end.alignment.should == 1
  end

  it "packed(2) :char followed by :int should have size of 6" do
    Class.new(FFI::Struct) do
      packed 2
      layout :c, :char, :i, :int
    end.size.should == 6
  end

  it "packed(2)  :char followed by :int should have alignment of 2" do
    Class.new(FFI::Struct) do
      packed 2
      layout :c, :char, :i, :int
    end.alignment.should == 2
  end

  it "packed :short followed by int should have size of 6" do
    Class.new(FFI::Struct) do
      packed
      layout :s, :short, :i, :int
    end.size.should == 6
  end

  it "packed :short followed by int should have alignment of 1" do
    Class.new(FFI::Struct) do
      packed
      layout :s, :short, :i, :int
    end.alignment.should == 1
  end

end
