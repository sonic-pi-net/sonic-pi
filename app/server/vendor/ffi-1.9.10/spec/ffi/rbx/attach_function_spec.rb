#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

class Timeval < FFI::Struct
  layout :tv_sec, :ulong, 0, :tv_usec, :ulong, 4  
end

module LibC
  extend FFI::Library
  ffi_lib FFI::Library::LIBC

  attach_function :gettimeofday, [:pointer, :pointer], :int
end

describe FFI::Library, "#attach_function" do
  it "correctly returns a value for gettimeofday" do
    t = Timeval.new
    time = LibC.gettimeofday(t.pointer, nil)
    expect(time).to be_kind_of(Integer)
  end
  
  it "correctly populates a struct for gettimeofday" do
    t = Timeval.new
    LibC.gettimeofday(t.pointer, nil)
    expect(t[:tv_sec]).to be_kind_of(Numeric)
    expect(t[:tv_usec]).to be_kind_of(Numeric)
  end
end

