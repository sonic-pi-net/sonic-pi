#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))
require 'bigdecimal'

describe ":long_double arguments and return values" do
  module LibTest
    extend FFI::Library
    ffi_lib TestLibrary::PATH
    attach_function :add_f128, [ :long_double, :long_double ], :long_double
    attach_function :ret_f128, [ :long_double ], :long_double
  end

  it "returns first parameter" do
    LibTest.ret_f128(0.1).should be_within(0.01).of(0.1)
  end

  it "returns first parameter with high precision" do
    ld =        BigDecimal.new("1.234567890123456789")
    tolerance = BigDecimal.new("0.0000000000000000001")
    LibTest.ret_f128(ld).should be_within(tolerance).of(ld)
  end

  it "add two long double numbers" do
    LibTest.add_f128(0.1, 0.2).should be_within(0.01).of(0.3)
  end
end
