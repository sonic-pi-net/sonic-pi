#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

describe "functions with custom types" do
  class Custom_enum
    extend FFI::DataConverter
    ToNativeMap= { :a => 1, :b => 2, :c => 3 }
    FromNativeMap = { 1 => :a, 2 => :b, 3 => :c }

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

  it "can attach with custom return type" do
    expect do
      Module.new do
        extend FFI::Library
        ffi_lib TestLibrary::PATH
        attach_function :ret_s32, [ :int ], Custom_enum
      end
    end.not_to raise_error
  end

  it "should return object of correct type" do

    m = Module.new do

      extend FFI::Library
      ffi_lib TestLibrary::PATH
      attach_function :ret_s32, [ :int ], Custom_enum
    end

    expect(m.ret_s32(1).is_a?(Symbol)).to be true
  end

  it "from_native should be called for result" do
    m = Module.new do
      extend FFI::Library
      ffi_lib TestLibrary::PATH
      attach_function :ret_s32, [ :int ], Custom_enum
    end
    m.ret_s32(1)
    expect(Custom_enum.from_native_called?).to be true
  end

  it "to_native should be called for parameter" do
    m = Module.new do
      extend FFI::Library
      ffi_lib TestLibrary::PATH
      attach_function :ret_s32, [ Custom_enum ], :int
    end
    m.ret_s32(:a)
    expect(Custom_enum.to_native_called?).to be true
  end
end
