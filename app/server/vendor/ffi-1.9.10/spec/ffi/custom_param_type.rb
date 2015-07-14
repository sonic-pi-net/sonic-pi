#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

describe "functions with custom parameter types" do
  before :each do

    Custom_enum = Class.new do
      extend FFI::DataConverter
      ToNativeMap= { :a => 1, :b => 2 }
      FromNativeMap = { 1 => :a, 2 => :b }

      def self.native_type
        @native_type_called = true
        FFI::Type::INT32
      end

      def self.to_native(val, ctx)
        @to_native_called = true
        ToNativeMap[val]
      end

      def self.from_native(val, ctx)
        @from_native_called = true
        FromNativeMap[val]
      end
      def self.native_type_called?; @native_type_called; end
      def self.from_native_called?; @from_native_called; end
      def self.to_native_called?; @to_native_called; end
    end

    # FIXME add tests
  end
end
