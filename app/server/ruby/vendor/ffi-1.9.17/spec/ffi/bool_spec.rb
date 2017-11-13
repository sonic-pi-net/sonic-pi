#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

describe "Function with primitive boolean arguments and return values" do
  module LibTest
    extend FFI::Library
    ffi_lib TestLibrary::PATH
    attach_function :bool_return_true, [ ], :bool
    attach_function :bool_return_false, [ ], :bool
    attach_function :bool_return_val, [ :bool ], :bool
    attach_function :bool_reverse_val, [ :bool ], :bool
  end

  it "bools" do
    expect(LibTest.bool_return_true).to be true
    expect(LibTest.bool_return_false).to be false

    expect(LibTest.bool_return_val(true)).to be true
    expect(LibTest.bool_return_val(false)).to be false

    expect(LibTest.bool_reverse_val(true)).to be false
    expect(LibTest.bool_reverse_val(false)).to be true
  end

  it "raise error on invalid types" do
    expect { LibTest.bool_return_val(nil) }.to raise_error(::TypeError)
  end
end
