#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

describe FFI::Struct, ' by_ref' do
  before :all do
    @struct_class = struct_class = Class.new(FFI::Struct) do
      layout :a, :pointer
    end

    @api = Module.new do
      extend FFI::Library
      ffi_lib TestLibrary::PATH
      fn = FFI::Type::POINTER.size == FFI::Type::LONG.size ? :ret_ulong : :ret_u64
      attach_function :struct_test, fn, [ struct_class.by_ref ], :pointer
    end
  end

  it "should accept instances of exact struct class" do
    s = @struct_class.new
    expect(@api.struct_test(s)).to eq(s.pointer)
  end

  it "should accept nil" do
    expect(@api.struct_test(nil)).to be_null
  end

  it "should reject other types" do
    expect { expect(@api.struct_test('test')).to be_nil }.to raise_error(TypeError)
  end

  it "should reject instances of other struct classes" do
    other_class = Class.new(FFI::Struct) do
      layout :a, :pointer
    end

    expect { @api.struct_test(other_class.new) }.to raise_error(TypeError)
  end
end

