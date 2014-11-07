#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

describe "Library" do
  describe ".enum_value" do
    m = Module.new do
      extend FFI::Library
      enum :something, [:one, :two]
    end

    it "should return a value for a valid key" do
      expect(m.enum_value(:one)).to eq(0)
      expect(m.enum_value(:two)).to eq(1)
    end

    it "should return nil for an invalid key" do
      expect(m.enum_value(:three)).to be nil
    end
  end

  describe "#ffi_convention" do
    it "defaults to :default" do
      m = Module.new do
        extend FFI::Library
      end
      expect(m.ffi_convention).to eq(:default)
    end

    it "should be settable" do
      m = Module.new do
        extend FFI::Library
      end

      expect(m.ffi_convention).to eq(:default)
      m.ffi_convention :stdcall
      expect(m.ffi_convention).to eq(:stdcall)
    end
  end

  if FFI::Platform::OS =~ /windows|cygwin/ && FFI::Platform::ARCH == 'i386'
    module LibTestStdcall
      extend FFI::Library
      ffi_lib TestLibrary::PATH
      ffi_convention :stdcall

      class StructUCDP < FFI::Struct
        layout :a1, :uchar,
          :a2, :double,
          :a3, :pointer
      end

      attach_function :testStdcallManyParams, [ :pointer, :int8, :int16, :int32, :int64,
              StructUCDP.by_value, StructUCDP.by_ref, :float, :double ], :void
    end

    it "adds stdcall decoration: testStdcallManyParams@64" do
      s = LibTestStdcall::StructUCDP.new
      po = FFI::MemoryPointer.new :long
      LibTestStdcall.testStdcallManyParams po, 1, 2, 3, 4, s, s, 1.0, 2.0
    end
  end

  describe "ffi_lib" do
    it "empty name list should raise error" do
      expect {
        Module.new do |m|
          m.extend FFI::Library
          ffi_lib
        end
      }.to raise_error(LoadError)
    end
    
  end

  unless RbConfig::CONFIG['target_os'] =~ /mswin|mingw/
    it "attach_function with no library specified" do
      expect {
        Module.new do |m|
          m.extend FFI::Library
          attach_function :getpid, [ ], :uint
        end
      }.to raise_error
    end

    it "attach_function :getpid from this process" do
      expect {
        expect(Module.new do |m|
          m.extend FFI::Library
          ffi_lib FFI::Library::CURRENT_PROCESS
          attach_function :getpid, [ ], :uint
        end.getpid).to eq(Process.pid)
      }.not_to raise_error
    end

    it "attach_function :getpid from [ 'c', 'libc.so.6'] " do
      expect {
        expect(Module.new do |m|
          m.extend FFI::Library
          ffi_lib [ 'c', 'libc.so.6' ]
          attach_function :getpid, [ ], :uint
        end.getpid).to eq(Process.pid)
      }.not_to raise_error
    end

    it "attach_function :getpid from [ 'libc.so.6', 'c' ] " do
      expect {
        expect(Module.new do |m|
          m.extend FFI::Library
          ffi_lib [ 'libc.so.6', 'c' ]
          attach_function :getpid, [ ], :uint
        end.getpid).to eq(Process.pid)
      }.not_to raise_error
    end

    it "attach_function :getpid from [ 'libfubar.so.0xdeadbeef', nil, 'c' ] " do
      expect {
        expect(Module.new do |m|
          m.extend FFI::Library
          ffi_lib [ 'libfubar.so.0xdeadbeef', nil, 'c' ]
          attach_function :getpid, [ ], :uint
        end.getpid).to eq(Process.pid)
      }.not_to raise_error
    end

    it "attach_function :getpid from [ 'libfubar.so.0xdeadbeef' ] " do
      expect {
        expect(Module.new do |m|
          m.extend FFI::Library
          ffi_lib 'libfubar.so.0xdeadbeef'
          attach_function :getpid, [ ], :uint
        end.getpid).to eq(Process.pid)
      }.to raise_error(LoadError)
    end

    it "attach_function :bool_return_true from [ File.expand_path(#{TestLibrary::PATH.inspect}) ]" do
      mod = Module.new do |m|
        m.extend FFI::Library
        ffi_lib File.expand_path(TestLibrary::PATH)
        attach_function :bool_return_true, [ ], :bool
      end
      expect(mod.bool_return_true).to be true
    end
  end

  def gvar_lib(name, type)
    Module.new do |m|
      m.extend FFI::Library
      ffi_lib TestLibrary::PATH
      attach_variable :gvar, "gvar_#{name}", type
      attach_function :get, "gvar_#{name}_get", [], type
      attach_function :set, "gvar_#{name}_set", [ type ], :void
    end
  end

  def gvar_test(name, type, val)
    lib = gvar_lib(name, type)
    lib.set(val)
    expect(lib.gvar).to eq(val)
    lib.set(0)
    lib.gvar = val
    expect(lib.get).to eq(val)
  end

  [ 0, 127, -128, -1 ].each do |i|
    it ":char variable" do
      gvar_test("s8", :char, i)
    end
  end

  [ 0, 0x7f, 0x80, 0xff ].each do |i|
    it ":uchar variable" do
      gvar_test("u8", :uchar, i)
    end
  end

  [ 0, 0x7fff, -0x8000, -1 ].each do |i|
    it ":short variable" do
      gvar_test("s16", :short, i)
    end
  end

  [ 0, 0x7fff, 0x8000, 0xffff ].each do |i|
    it ":ushort variable" do
      gvar_test("u16", :ushort, i)
    end
  end

  [ 0, 0x7fffffff, -0x80000000, -1 ].each do |i|
    it ":int variable" do
      gvar_test("s32", :int, i)
    end
  end

  [ 0, 0x7fffffff, 0x80000000, 0xffffffff ].each do |i|
    it ":uint variable" do
      gvar_test("u32", :uint, i)
    end
  end

  [ 0, 0x7fffffffffffffff, -0x8000000000000000, -1 ].each do |i|
    it ":long_long variable" do
      gvar_test("s64", :long_long, i)
    end
  end

  [ 0, 0x7fffffffffffffff, 0x8000000000000000, 0xffffffffffffffff ].each do |i|
    it ":ulong_long variable" do
      gvar_test("u64", :ulong_long, i)
    end
  end

  if FFI::Platform::LONG_SIZE == 32
    [ 0, 0x7fffffff, -0x80000000, -1 ].each do |i|
      it ":long variable" do
        gvar_test("long", :long, i)
      end
    end

    [ 0, 0x7fffffff, 0x80000000, 0xffffffff ].each do |i|
      it ":ulong variable" do
        gvar_test("ulong", :ulong, i)
      end
    end
  else
    [ 0, 0x7fffffffffffffff, -0x8000000000000000, -1 ].each do |i|
      it ":long variable" do
        gvar_test("long", :long, i)
      end
    end

    [ 0, 0x7fffffffffffffff, 0x8000000000000000, 0xffffffffffffffff ].each do |i|
      it ":ulong variable" do
        gvar_test("ulong", :ulong, i)
      end
    end
  end

  it "Pointer variable" do
    lib = gvar_lib("pointer", :pointer)
    val = FFI::MemoryPointer.new :long
    lib.set(val)
    expect(lib.gvar).to eq(val)
    lib.set(nil)
    lib.gvar = val
    expect(lib.get).to eq(val)
  end

  [ 0, 0x7fffffff, -0x80000000, -1 ].each do |i|
    it "structure" do
      class GlobalStruct < FFI::Struct
        layout :data, :long
      end

      lib = Module.new do |m|
        m.extend FFI::Library
        ffi_lib TestLibrary::PATH
        attach_variable :gvar, "gvar_gstruct", GlobalStruct
        attach_function :get, "gvar_gstruct_get", [], GlobalStruct
        attach_function :set, "gvar_gstruct_set", [ GlobalStruct ], :void
      end

      val = GlobalStruct.new
      val[:data] = i
      lib.set(val)
      expect(lib.gvar[:data]).to eq(i)
      val[:data] = 0
      lib.gvar[:data] = i
      val = GlobalStruct.new(lib.get)
      expect(val[:data]).to eq(i)
    end
  end
end
