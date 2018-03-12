module FFISpecs
  #
  # Callback fixtures
  #
  module LibTest
    callback :cbVrS8, [ ], :char
    callback :cbVrU8, [ ], :uchar
    callback :cbVrS16, [ ], :short
    callback :cbVrU16, [ ], :ushort
    callback :cbVrS32, [ ], :int
    callback :cbVrU32, [ ], :uint
    callback :cbVrL, [ ], :long
    callback :cbVrUL, [ ], :ulong
    callback :cbVrS64, [ ], :long_long
    callback :cbVrU64, [ ], :ulong_long
    callback :cbVrP, [], :pointer
    callback :cbCrV, [ :char ], :void
    callback :cbSrV, [ :short ], :void
    callback :cbIrV, [ :int ], :void
    callback :cbLrV, [ :long ], :void
    callback :cbULrV, [ :ulong ], :void
    callback :cbLrV, [ :long_long ], :void

    attach_function :testCallbackVrS8, :testClosureVrB, [ :cbVrS8 ], :char
    attach_function :testCallbackVrU8, :testClosureVrB, [ :cbVrU8 ], :uchar
    attach_function :testCallbackVrS16, :testClosureVrS, [ :cbVrS16 ], :short
    attach_function :testCallbackVrU16, :testClosureVrS, [ :cbVrU16 ], :ushort
    attach_function :testCallbackVrS32, :testClosureVrI, [ :cbVrS32 ], :int
    attach_function :testCallbackVrU32, :testClosureVrI, [ :cbVrU32 ], :uint
    attach_function :testCallbackVrL, :testClosureVrL, [ :cbVrL ], :long
    attach_function :testCallbackVrUL, :testClosureVrL, [ :cbVrUL ], :ulong
    attach_function :testCallbackVrS64, :testClosureVrLL, [ :cbVrS64 ], :long_long
    attach_function :testCallbackVrU64, :testClosureVrLL, [ :cbVrU64 ], :ulong_long
    attach_function :testCallbackVrP, :testClosureVrP, [ :cbVrP ], :pointer
    attach_function :testCallbackCrV, :testClosureBrV, [ :cbCrV, :char ], :void
    attach_variable :cbVrS8, :gvar_pointer, :cbVrS8
    attach_variable :pVrS8, :gvar_pointer, :pointer
    attach_function :testGVarCallbackVrS8, :testClosureVrB, [ :pointer ], :char
    attach_function :testOptionalCallbackCrV, :testOptionalClosureBrV, [ :cbCrV, :char ], :void

    attach_function :testCallbackVrS8, :testClosureVrB, [ callback([ ], :char) ], :char

    callback :cb_return_type, [ :int ], :int
    callback :cb_lookup, [ ], :cb_return_type
    attach_function :testReturnsCallback, :testReturnsClosure, [ :cb_lookup, :int ], :int

    callback :funcptr, [ :int ], :int
    attach_function :testReturnsFunctionPointer, [  ], :funcptr

    callback :cbS8rV, [ :char ], :void
    callback :cbU8rV, [ :uchar ], :void
    callback :cbS16rV, [ :short ], :void
    callback :cbU16rV, [ :ushort ], :void

    callback :cbS32rV, [ :int ], :void
    callback :cbU32rV, [ :uint ], :void

    callback :cbLrV, [ :long ], :void
    callback :cbULrV, [ :ulong ], :void

    callback :cbS64rV, [ :long_long ], :void
    attach_function :testCallbackCrV, :testClosureBrV, [ :cbS8rV, :char ], :void
    attach_function :testCallbackU8rV, :testClosureBrV, [ :cbU8rV, :uchar ], :void
    attach_function :testCallbackSrV, :testClosureSrV, [ :cbS16rV, :short ], :void
    attach_function :testCallbackU16rV, :testClosureSrV, [ :cbU16rV, :ushort ], :void
    attach_function :testCallbackIrV, :testClosureIrV, [ :cbS32rV, :int ], :void
    attach_function :testCallbackU32rV, :testClosureIrV, [ :cbU32rV, :uint ], :void

    attach_function :testCallbackLrV, :testClosureLrV, [ :cbLrV, :long ], :void
    attach_function :testCallbackULrV, :testClosureULrV, [ :cbULrV, :ulong ], :void

    attach_function :testCallbackLLrV, :testClosureLLrV, [ :cbS64rV, :long_long ], :void
  end

  #
  # Enum fixtures
  #
  module TestEnum0
    extend FFI::Library
  end

  module TestEnum1
    extend FFI::Library
    ffi_lib LIBRARY

    enum [:c1, :c2, :c3, :c4]
    enum [:c5, 42, :c6, :c7, :c8]
    enum [:c9, 42, :c10, :c11, 4242, :c12]
    enum [:c13, 42, :c14, 4242, :c15, 424242, :c16, 42424242]

    attach_function :test_untagged_enum, [:int], :int
  end

  module TestEnum3
    extend FFI::Library
    ffi_lib LIBRARY

    enum :enum_type1, [:c1, :c2, :c3, :c4]
    enum :enum_type2, [:c5, 42, :c6, :c7, :c8]
    enum :enum_type3, [:c9, 42, :c10, :c11, 4242, :c12]
    enum :enum_type4, [:c13, 42, :c14, 4242, :c15, 424242, :c16, 42424242]

    attach_function :test_tagged_typedef_enum1, [:enum_type1], :enum_type1
    attach_function :test_tagged_typedef_enum2, [:enum_type2], :enum_type2
    attach_function :test_tagged_typedef_enum3, [:enum_type3], :enum_type3
    attach_function :test_tagged_typedef_enum4, [:enum_type4], :enum_type4
  end

  #
  # Errno fixtures
  #
  module LibTest
    attach_function :setLastError, [ :int ], :void
  end

  #
  # ManagedStruct fixtures
  #
  module LibTest
    attach_function :ptr_from_address, [ FFI::Platform::ADDRESS_SIZE == 32 ? :uint : :ulong_long ], :pointer
  end

  class NoRelease < ManagedStruct
    layout :i, :int
  end

  class WhatClassAmI < ManagedStruct
    layout :i, :int
    def self.release; end
  end

  class PleaseReleaseMe < ManagedStruct
    layout :i, :int
    @@count = 0
    def self.release
      @@count += 1
    end
    def self.wait_gc(count)
      loop = 5
      while loop > 0 && @@count < count
        loop -= 1
        if RUBY_PLATFORM =~ /java/
          require 'java'
          java.lang.System.gc
        else
          GC.start
        end
        sleep 0.05 if @@count < count
      end
    end
  end

  #
  # Number fixtures
  #
  module LibTest
    attach_function :ret_s8, [ :char ], :char
    attach_function :ret_u8, [ :uchar ], :uchar
    attach_function :ret_s16, [ :short ], :short
    attach_function :ret_u16, [ :ushort ], :ushort
    attach_function :ret_s32, [ :int ], :int
    attach_function :ret_u32, [ :uint ], :uint
    attach_function :ret_s64, [ :long_long ], :long_long
    attach_function :ret_u64, [ :ulong_long ], :ulong_long
    attach_function :ret_long, [ :long ], :long
    attach_function :ret_ulong, [ :ulong ], :ulong
    attach_function :set_s8, [ :char ], :void
    attach_function :get_s8, [ ], :char
    attach_function :set_float, [ :float ], :void
    attach_function :get_float, [ ], :float
    attach_function :set_double, [ :double ], :void
    attach_function :get_double, [ ], :double
  end

  PACK_VALUES = {
    's8' => [ 0x12  ],
    'u8' => [ 0x34  ],
    's16' => [ 0x5678 ],
    'u16' => [ 0x9abc ],
    's32' => [ 0x7654321f ],
    'u32' => [ 0xfee1babe ],
    'sL' => [ 0x1f2e3d4c ],
    'uL' => [ 0xf7e8d9ca ],
    's64' => [ 0x1eafdeadbeefa1b2 ],
    #'f32' => [ 1.234567 ], # TODO: Why is this disabled?
    'f64' => [ 9.87654321 ]
  }

  TYPE_MAP = {
    's8' => :char, 'u8' => :uchar, 's16' => :short, 'u16' => :ushort,
    's32' => :int, 'u32' => :uint, 's64' => :long_long, 'u64' => :ulong_long,
    'sL' => :long, 'uL' => :ulong, 'f32' => :float, 'f64' => :double
  }
  TYPES = TYPE_MAP.keys

  module LibTest
    [ 's32', 'u32', 's64', 'u64' ].each do |rt|
      TYPES.each do |t1|
        TYPES.each do |t2|
          TYPES.each do |t3|
            begin
              attach_function "pack_#{t1}#{t2}#{t3}_#{rt}",
                [ TYPE_MAP[t1], TYPE_MAP[t2], TYPE_MAP[t3], :buffer_out ], :void
            rescue FFI::NotFoundError
            end
          end
        end
      end
    end
  end

  #
  # Pointer fixtures
  #
  module LibTest
    attach_function :ptr_ret_int32_t, [ :pointer, :int ], :int
    attach_function :ptr_from_address, [ FFI::Platform::ADDRESS_SIZE == 32 ? :uint : :ulong_long ], :pointer
    attach_function :ptr_set_pointer, [ :pointer, :int, :pointer ], :void
  end

  class ToPtrTest
    def initialize(ptr)
      @ptr = ptr
    end
    def to_ptr
      @ptr
    end
  end

  require 'delegate'
  class PointerDelegate < DelegateClass(FFI::Pointer)
    def initialize(ptr)
      super
      @ptr = ptr
    end
    def to_ptr
      @ptr
    end
  end

  class AutoPointerTestHelper
    @@count = 0
    def self.release
      @@count += 1 if @@count > 0
    end
    def self.reset
      @@count = 0
    end
    def self.gc_everything(count)
      loop = 5
      while @@count < count && loop > 0
        loop -= 1
        if RUBY_PLATFORM =~ /java/
          require "java"
          java.lang.System.gc
        else
          GC.start
        end
        sleep 0.05 unless @@count == count
      end
      @@count = 0
    end
    def self.finalizer
      self.method(:release).to_proc
    end
  end

  #
  # String fixtures
  #
  module LibTest
    attach_function :ptr_ret_pointer, [ :pointer, :int], :string
    attach_function :string_equals, [ :string, :string ], :int
    attach_function :string_dummy, [ :string ], :void
  end

  #
  # Struct initialize fixtures
  #
  class StructWithInitialize < FFI::Struct
    layout :string, :string
    attr_accessor :magic
    def initialize
      super
      self.magic = 42
    end
  end

  #
  # Struct fixtures
  #
  StructTypes = {
    's8' => :char,
    's16' => :short,
    's32' => :int,
    's64' => :long_long,
    'long' => :long,
    'f32' => :float,
    'f64' => :double
  }

  module LibTest
    attach_function :ptr_ret_pointer, [ :pointer, :int], :string
    attach_function :ptr_ret_int32_t, [ :pointer, :int ], :int
    attach_function :ptr_from_address, [ :ulong ], :pointer
    attach_function :string_equals, [ :string, :string ], :int
    [ 's8', 's16', 's32', 's64', 'f32', 'f64', 'long' ].each do |t|
      attach_function "struct_align_#{t}", [ :pointer ], StructTypes[t]
    end
  end

  class PointerMember < FFI::Struct
    layout :pointer, :pointer
  end

  class StringMember < FFI::Struct
    layout :string, :string
  end

  module CallbackMember
    extend FFI::Library
    ffi_lib LIBRARY
    callback :add, [ :int, :int ], :int
    callback :sub, [ :int, :int ], :int

    class TestStruct < FFI::Struct
      layout :add, :add,
        :sub, :sub
    end

    attach_function :struct_call_add_cb, [TestStruct, :int, :int], :int
    attach_function :struct_call_sub_cb, [TestStruct, :int, :int], :int
  end

  module LibTest
    class NestedStruct < FFI::Struct
      layout :i, :int
    end

    class ContainerStruct < FFI::Struct
      layout :first, :char, :ns, NestedStruct
    end

    attach_function :struct_align_nested_struct, [ :pointer ], :int
    attach_function :struct_make_container_struct, [ :int ], :pointer

    class StructWithArray < FFI::Struct
      layout :first, :char, :a, [:int, 5]
    end

    attach_function :struct_make_struct_with_array, [:int, :int, :int, :int, :int], :pointer
    attach_function :struct_field_array, [:pointer], :pointer

    class BuggedStruct < FFI::Struct
      layout :visible, :uchar,
              :x, :uint,
              :y, :uint,
              :rx, :short,
              :ry, :short,
              :order, :uchar,
              :size, :uchar
    end

    attach_function :bugged_struct_size, [], :uint
  end

  module StructCustomTypedef
    extend FFI::Library
    ffi_lib LIBRARY
    typedef :uint, :fubar3_t

    class S < FFI::Struct
      layout :a, :fubar3_t
    end
  end

  #
  # Union fixtures
  #
  module LibTest
    Types = {
      's8' => [:char, :c, 1],
      's16' => [:short, :s, 0xff0],
      's32' => [:int, :i, 0xff00],
      's64' => [:long_long, :j, 0xffff00],
      'long' => [:long, :l, 0xffff],
      'f32' => [:float, :f, 1.0001],
      'f64' => [:double, :d, 1.000000001]
    }

    class TestUnion < FFI::Union
      layout( :a, [:char, 10],
              :i, :int,
              :f, :float,
              :d, :double,
              :s, :short,
              :l, :long,
              :j, :long_long,
              :c, :char )
    end

    Types.keys.each do |k|
      attach_function "union_align_#{k}", [ :pointer ], Types[k][0]
      attach_function "union_make_union_with_#{k}", [ Types[k][0] ], :pointer
    end

    attach_function :union_size, [], :uint
  end

  #
  # Variadic fixtures
  #
  module LibTest
    attach_function :pack_varargs, [ :buffer_out, :string, :varargs ], :void
  end

  module Varargs
    PACK_VALUES = {
      'c' => [ 0x12  ],
      'C' => [ 0x34  ],
      's' => [ 0x5678 ],
      'S' => [ 0x9abc ],
      'i' => [ 0x7654321f ],
      'I' => [ 0xfee1babe ],
      'l' => [ 0x1f2e3d4c ],
      'L' => [ 0xf7e8d9ca ],
      'j' => [ 0x1eafdeadbeefa1b2 ],
      'f' => [ 1.23456789 ],
      'd' => [ 9.87654321 ]
    }

    TYPE_MAP = {
      'c' => :char, 'C' => :uchar, 's' => :short, 'S' => :ushort,
      'i' => :int, 'I' => :uint, 'j' => :long_long, 'J' => :ulong_long,
      'l' => :long, 'L' => :ulong, 'f' => :float, 'd' => :double
    }
  end
end
