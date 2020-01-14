#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

describe "FFI.errno" do
  module LibTest
    extend FFI::Library
    ffi_lib TestLibrary::PATH
    attach_function :setLastError, [ :int ], :void
    attach_function :setErrno, [ :int ], :void
  end

  it "FFI.errno contains errno from last function, FFI::LastError.winapi_error works differently per OS" do
    # setup
    LibTest.setErrno(0)
    LibTest.setLastError(0)
    LibTest.setLastError(0x12345678)
    # cases
    case FFI::Platform::OS
      when "cygwin"
        expect(FFI::LastError.winapi_error).to eq(0x12345678)
        LibTest.setErrno(0x2A)
        expect(FFI.errno).to eq(0x2A)
      when "windows"
        expect(FFI::LastError.winapi_error).to eq(0x12345678)
        expect(FFI.errno).to eq(0x12345678)
      else
        # Linux, and else
        expect {FFI::LastError.winapi_error}.to raise_exception(NoMethodError)
        expect {FFI::LastError.winapi_error = 0}.to raise_exception(NoMethodError)
        expect(FFI.errno).to eq(0x12345678)
    end
  end
end
