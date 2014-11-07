# coding: utf-8
#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

module CTest
  extend FFI::Library
  ffi_lib FFI::Library::LIBC

  attach_function :strcat, [:pointer, :pointer], :pointer
end

describe "MemoryPointer" do
  it "makes a pointer from a string" do
    m = FFI::MemoryPointer.from_string("FFI is Awesome")
    expect(m.total).to eq(15)
    expect(m.type_size).to eq(1)
  end

  it "does not make a pointer from non-strings" do
    expect { FFI::MemoryPointer.from_string(nil) }.to raise_error(TypeError)
  end

  it "makes a pointer from a string with multibyte characters" do
    m = FFI::MemoryPointer.from_string("ぱんだ")
    expect(m.total).to eq(10)
    expect(m.type_size).to eq(1)
  end

  it "reads back a string" do
    m = FFI::MemoryPointer.from_string("FFI is Awesome")
    expect(m.read_string).to eq("FFI is Awesome")
  end
  
  it "makes a pointer for a certain number of bytes" do
    m = FFI::MemoryPointer.new(8)
    m.write_array_of_int([1,2])
    expect(m.read_array_of_int(2)).to eq([1,2])
  end

  it "allows access to an element of the pointer (as an array)" do
    m = FFI::MemoryPointer.new(:int, 2)
    m.write_array_of_int([1,2])
    expect(m[0].read_int).to eq(1)
    expect(m[1].read_int).to eq(2)
  end
  
  it "allows writing as an int" do
    m = FFI::MemoryPointer.new(:int)
    m.write_int(1)
    expect(m.read_int).to eq(1)
  end
  
  it "allows writing as a long" do
    m = FFI::MemoryPointer.new(:long)
    m.write_long(10)
    expect(m.read_long).to eq(10)
  end
  
  it "raises an error if you try putting a long into a pointer of size 1" do
    m = FFI::MemoryPointer.new(1)
    expect { m.write_long(10) }.to raise_error
  end
  
  it "raises an error if you try putting an int into a pointer of size 1" do
    m = FFI::MemoryPointer.new(1)
    expect { m.write_int(10) }.to raise_error
  end
#  it "does not raise IndexError for opaque pointers" do
#    m = FFI::MemoryPointer.new(8)
#    p2 = FFI::MemoryPointer.new(1024)
#    m.write_long(p2.address)
#    p = m.read_pointer
#    lambda { p.write_int(10) }.should_not raise_error
#  end
  
  it "makes a pointer for a certain type" do
    m = FFI::MemoryPointer.new(:int)
    m.write_int(10)
    expect(m.read_int).to eq(10)
  end
  
  it "makes a memory pointer for a number of a certain type" do
    m = FFI::MemoryPointer.new(:int, 2)
    m.write_array_of_int([1,2])
    expect(m.read_array_of_int(2)).to eq([1,2])
  end
  
  it "makes a pointer for an object responding to #size" do
    m = FFI::MemoryPointer.new(Struct.new(:size).new(8))
    m.write_array_of_int([1,2])
    expect(m.read_array_of_int(2)).to eq([1,2])
  end

  it "makes a pointer for a number of an object responding to #size" do
    m = FFI::MemoryPointer.new(Struct.new(:size).new(4), 2)
    m.write_array_of_int([1,2])
    expect(m.read_array_of_int(2)).to eq([1,2])
  end  

  it "MemoryPointer#address returns correct value" do
    m = FFI::MemoryPointer.new(:long_long)
    magic = 0x12345678
    m.write_long(magic)
    expect(m.read_pointer.address).to eq(magic)
  end

  it "MemoryPointer#null? returns true for zero value" do
    m = FFI::MemoryPointer.new(:long_long)
    m.write_long(0)    
    expect(m.read_pointer.null?).to be true
  end

  it "MemoryPointer#null? returns false for non-zero value" do
    m = FFI::MemoryPointer.new(:long_long)
    m.write_long(0x12345678)
    expect(m.read_pointer.null?).to be false
  end
  
  it "initialize with block should execute block" do
    block_executed = false
    FFI::MemoryPointer.new(:pointer) do |ptr|
      block_executed = true
    end
    expect(block_executed).to be true
  end
end
