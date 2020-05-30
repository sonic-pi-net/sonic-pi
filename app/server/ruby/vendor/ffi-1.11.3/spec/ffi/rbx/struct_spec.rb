#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

class Timeval < FFI::Struct
  layout :tv_sec, :ulong, 0, :tv_usec, :ulong, 4  
end

describe FFI::Struct do
  it "allows setting fields" do
    t = Timeval.new
    t[:tv_sec] = 12
    expect(t[:tv_sec]).to eq(12)
  end
end
