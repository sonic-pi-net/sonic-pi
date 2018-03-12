#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))
describe "String tests" do
  include FFI
  module StrLibTest
    extend FFI::Library
    ffi_lib TestLibrary::PATH
    attach_function :ptr_ret_pointer, [ :pointer, :int], :string
    attach_function :string_equals, [ :string, :string ], :int
    attach_function :string_dummy, [ :string ], :void
    attach_function :string_null, [ ], :string
  end

  it "MemoryPointer#get_string returns a tainted string" do
    mp = FFI::MemoryPointer.new 1024
    mp.put_string(0, "test\0")
    str = mp.get_string(0)
    expect(str.tainted?).to be true
  end

  it "String returned by a method is tainted" do
    mp = FFI::MemoryPointer.new :pointer
    sp = FFI::MemoryPointer.new 1024
    sp.put_string(0, "test")
    mp.put_pointer(0, sp)
    str = StrLibTest.ptr_ret_pointer(mp, 0)
    expect(str).to eq("test")
    expect(str).to be_tainted
  end

  it "Poison null byte raises error" do
    s = "123\0abc"
    expect { StrLibTest.string_equals(s, s) }.to raise_error
  end

  it "Tainted String parameter should throw a SecurityError" do
    $SAFE = 1
    str = "test"
    str.taint
    begin
      expect(LibTest.string_equals(str, str)).to be false
    rescue SecurityError
    end
  end if false
  it "casts nil as NULL pointer" do
    expect(StrLibTest.string_dummy(nil)).to be_nil
  end

  it "return nil for NULL char*" do
    expect(StrLibTest.string_null).to be_nil
  end

  it "reads an array of strings until encountering a NULL pointer" do
    strings = ["foo", "bar", "baz", "testing", "ffi"]
    ptrary = FFI::MemoryPointer.new(:pointer, 6)
    ary = strings.inject([]) do |a, str|
      f = FFI::MemoryPointer.new(1024)
      f.put_string(0, str)
      a << f
    end
    ary.insert(3, nil)
    ptrary.write_array_of_pointer(ary)
    expect(ptrary.get_array_of_string(0)).to eq(["foo", "bar", "baz"])
  end

  it "reads an array of strings of the size specified, substituting nil when a pointer is NULL" do
    strings = ["foo", "bar", "baz", "testing", "ffi"]
    ptrary = FFI::MemoryPointer.new(:pointer, 6)
    ary = strings.inject([]) do |a, str|
      f = FFI::MemoryPointer.new(1024)
      f.put_string(0, str)
      a << f
    end
    ary.insert(2, nil)
    ptrary.write_array_of_pointer(ary)
    expect(ptrary.get_array_of_string(0, 4)).to eq(["foo", "bar", nil, "baz"])
  end

  it "reads an array of strings, taking a memory offset parameter" do
    strings = ["foo", "bar", "baz", "testing", "ffi"]
    ptrary = FFI::MemoryPointer.new(:pointer, 5)
    ary = strings.inject([]) do |a, str|
      f = FFI::MemoryPointer.new(1024)
      f.put_string(0, str)
      a << f
    end
    ptrary.write_array_of_pointer(ary)
    expect(ptrary.get_array_of_string(2 * FFI.type_size(:pointer), 3)).to eq(["baz", "testing", "ffi"])
  end

  it "raises an IndexError when trying to read an array of strings out of bounds" do
    strings = ["foo", "bar", "baz", "testing", "ffi"]
    ptrary = FFI::MemoryPointer.new(:pointer, 5)
    ary = strings.inject([]) do |a, str|
      f = FFI::MemoryPointer.new(1024)
      f.put_string(0, str)
      a << f
    end
    ptrary.write_array_of_pointer(ary)
    expect { ptrary.get_array_of_string(0, 6) }.to raise_error
  end

  it "raises an IndexError when trying to read an array of strings using a negative offset" do
    strings = ["foo", "bar", "baz", "testing", "ffi"]
    ptrary = FFI::MemoryPointer.new(:pointer, 5)
    ary = strings.inject([]) do |a, str|
      f = FFI::MemoryPointer.new(1024)
      f.put_string(0, str)
      a << f
    end
    ptrary.write_array_of_pointer(ary)
    expect { ptrary.get_array_of_string(-1) }.to raise_error
  end
end
