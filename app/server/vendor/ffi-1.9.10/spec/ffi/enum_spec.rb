#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

module TestEnum0
  extend FFI::Library
end

module TestEnum1
  extend FFI::Library
  ffi_lib TestLibrary::PATH

  enum [:c1, :c2, :c3, :c4]
  enum [:c5, 42, :c6, :c7, :c8]
  enum [:c9, 42, :c10, :c11, 4242, :c12]
  enum [:c13, 42, :c14, 4242, :c15, 424242, :c16, 42424242]
  
  attach_function :test_untagged_enum, [:int], :int
end

module TestEnum3
  extend FFI::Library
  ffi_lib TestLibrary::PATH

  enum :enum_type1, [:c1, :c2, :c3, :c4]
  enum :enum_type2, [:c5, 42, :c6, :c7, :c8]
  enum :enum_type3, [:c9, 42, :c10, :c11, 4242, :c12]
  enum :enum_type4, [:c13, 42, :c14, 4242, :c15, 424242, :c16, 42424242]

  attach_function :test_tagged_typedef_enum1, [:enum_type1], :enum_type1
  attach_function :test_tagged_typedef_enum2, [:enum_type2], :enum_type2
  attach_function :test_tagged_typedef_enum3, [:enum_type3], :enum_type3
  attach_function :test_tagged_typedef_enum4, [:enum_type4], :enum_type4
end

module TestEnum4
  extend FFI::Library
  ffi_lib TestLibrary::PATH

  enum [:c1, :c2, :c3, :c4]
  enum :enum_type1, [:c5, 0x42, :c6, :c7, :c8]
  enum :enum_type2, [:c9, 0x42, :c10, :c11, 0x4242, :c12]
  enum :enum_type3, [:c13, 0x42, :c14, 0x4242, :c15, 0x42424242, :c16, 0x4242424242424242]
  enum FFI::Type::UINT16, :enum_type4, [:c17, 0x42, :c18, :c19, :c20]
  enum FFI::Type::UINT32, :enum_type5, [:c21, 0x42, :c22, :c23, 0x4242, :c24]
  enum FFI::Type::UINT64, :enum_type6, [:c25, 0x42, :c26, 0x4242, :c27, 0x42424242, :c28, 0x4242424242424242]
  enum FFI::Type::UINT64, [:c29, 0x4242424242424242, :c30, :c31, :c32]

  attach_function :test_untagged_nonint_enum, [:uint8],  :uint8
  attach_function :test_tagged_nonint_enum1,  [:uint16], :uint16
  attach_function :test_tagged_nonint_enum2,  [:uint32], :uint32
  attach_function :test_tagged_nonint_enum3,  [:uint64], :uint64
  attach_function :test_tagged_nonint_enum4, :test_tagged_nonint_enum1,  [:enum_type4], :enum_type4
  attach_function :test_tagged_nonint_enum5, :test_tagged_nonint_enum2,  [:enum_type5], :enum_type5
  attach_function :test_tagged_nonint_enum6, :test_tagged_nonint_enum3,  [:enum_type6], :enum_type6
end

describe "A library with no enum defined" do
  it "returns nil when asked for an enum" do
    expect(TestEnum0.enum_type(:foo)).to be_nil
  end
end

describe "An untagged enum" do
  it "constants can be used as function parameters and return value" do
    expect(TestEnum1.test_untagged_enum(:c1)).to eq(0)
    expect(TestEnum1.test_untagged_enum(:c2)).to eq(1)
    expect(TestEnum1.test_untagged_enum(:c3)).to eq(2)
    expect(TestEnum1.test_untagged_enum(:c4)).to eq(3)
    expect(TestEnum1.test_untagged_enum(:c5)).to eq(42)
    expect(TestEnum1.test_untagged_enum(:c6)).to eq(43)
    expect(TestEnum1.test_untagged_enum(:c7)).to eq(44)
    expect(TestEnum1.test_untagged_enum(:c8)).to eq(45)
    expect(TestEnum1.test_untagged_enum(:c9)).to eq(42)
    expect(TestEnum1.test_untagged_enum(:c10)).to eq(43)
    expect(TestEnum1.test_untagged_enum(:c11)).to eq(4242)
    expect(TestEnum1.test_untagged_enum(:c12)).to eq(4243)
    expect(TestEnum1.test_untagged_enum(:c13)).to eq(42)
    expect(TestEnum1.test_untagged_enum(:c14)).to eq(4242)
    expect(TestEnum1.test_untagged_enum(:c15)).to eq(424242)
    expect(TestEnum1.test_untagged_enum(:c16)).to eq(42424242)
    expect(TestEnum4.test_untagged_nonint_enum(:c1)).to eq(0)
    expect(TestEnum4.test_untagged_nonint_enum(:c2)).to eq(1)
    expect(TestEnum4.test_untagged_nonint_enum(:c3)).to eq(2)
    expect(TestEnum4.test_untagged_nonint_enum(:c4)).to eq(3)
    expect(TestEnum4.test_tagged_nonint_enum3(:c29)).to eq(0x4242424242424242)
    expect(TestEnum4.test_tagged_nonint_enum3(:c30)).to eq(0x4242424242424243)
    expect(TestEnum4.test_tagged_nonint_enum3(:c31)).to eq(0x4242424242424244)
    expect(TestEnum4.test_tagged_nonint_enum3(:c32)).to eq(0x4242424242424245)
  end
end

describe "A tagged typedef enum" do
  it "is accessible through its tag" do
    expect(TestEnum3.enum_type(:enum_type1)).not_to be_nil
    expect(TestEnum3.enum_type(:enum_type2)).not_to be_nil
    expect(TestEnum3.enum_type(:enum_type3)).not_to be_nil
    expect(TestEnum3.enum_type(:enum_type4)).not_to be_nil
    expect(TestEnum4.enum_type(:enum_type1)).not_to be_nil
    expect(TestEnum4.enum_type(:enum_type2)).not_to be_nil
    expect(TestEnum4.enum_type(:enum_type3)).not_to be_nil
    expect(TestEnum4.enum_type(:enum_type4)).not_to be_nil
    expect(TestEnum4.enum_type(:enum_type5)).not_to be_nil
    expect(TestEnum4.enum_type(:enum_type6)).not_to be_nil
  end

  it "contains enum constants" do
    expect(TestEnum3.enum_type(:enum_type1).symbols.length).to eq(4)
    expect(TestEnum3.enum_type(:enum_type2).symbols.length).to eq(4)
    expect(TestEnum3.enum_type(:enum_type3).symbols.length).to eq(4)
    expect(TestEnum3.enum_type(:enum_type4).symbols.length).to eq(4)
    expect(TestEnum4.enum_type(:enum_type1).symbols.length).to eq(4)
    expect(TestEnum4.enum_type(:enum_type2).symbols.length).to eq(4)
    expect(TestEnum4.enum_type(:enum_type3).symbols.length).to eq(4)
    expect(TestEnum4.enum_type(:enum_type4).symbols.length).to eq(4)
    expect(TestEnum4.enum_type(:enum_type5).symbols.length).to eq(4)
    expect(TestEnum4.enum_type(:enum_type6).symbols.length).to eq(4)
  end

  it "constants can be used as function parameters and return value" do
    expect(TestEnum3.test_tagged_typedef_enum1(:c1)).to be :c1
    expect(TestEnum3.test_tagged_typedef_enum1(:c2)).to be :c2
    expect(TestEnum3.test_tagged_typedef_enum1(:c3)).to be :c3
    expect(TestEnum3.test_tagged_typedef_enum1(:c4)).to be :c4
    expect(TestEnum3.test_tagged_typedef_enum2(:c5)).to be :c5
    expect(TestEnum3.test_tagged_typedef_enum2(:c6)).to be :c6
    expect(TestEnum3.test_tagged_typedef_enum2(:c7)).to be :c7
    expect(TestEnum3.test_tagged_typedef_enum2(:c8)).to be :c8
    expect(TestEnum3.test_tagged_typedef_enum3(:c9)).to be :c9
    expect(TestEnum3.test_tagged_typedef_enum3(:c10)).to be :c10
    expect(TestEnum3.test_tagged_typedef_enum3(:c11)).to be :c11
    expect(TestEnum3.test_tagged_typedef_enum3(:c12)).to be :c12
    expect(TestEnum3.test_tagged_typedef_enum4(:c13)).to be :c13
    expect(TestEnum3.test_tagged_typedef_enum4(:c14)).to be :c14
    expect(TestEnum3.test_tagged_typedef_enum4(:c15)).to be :c15
    expect(TestEnum3.test_tagged_typedef_enum4(:c16)).to be :c16
    expect(TestEnum4.test_tagged_nonint_enum1(:c5)).to eq(0x42)
    expect(TestEnum4.test_tagged_nonint_enum1(:c6)).to eq(0x43)
    expect(TestEnum4.test_tagged_nonint_enum1(:c7)).to eq(0x44)
    expect(TestEnum4.test_tagged_nonint_enum1(:c8)).to eq(0x45)
    expect(TestEnum4.test_tagged_nonint_enum2(:c9)).to eq(0x42)
    expect(TestEnum4.test_tagged_nonint_enum2(:c10)).to eq(0x43)
    expect(TestEnum4.test_tagged_nonint_enum2(:c11)).to eq(0x4242)
    expect(TestEnum4.test_tagged_nonint_enum2(:c12)).to eq(0x4243)
    expect(TestEnum4.test_tagged_nonint_enum3(:c13)).to eq(0x42)
    expect(TestEnum4.test_tagged_nonint_enum3(:c14)).to eq(0x4242)
    expect(TestEnum4.test_tagged_nonint_enum3(:c15)).to eq(0x42424242)
    expect(TestEnum4.test_tagged_nonint_enum3(:c16)).to eq(0x4242424242424242)
    expect(TestEnum4.test_tagged_nonint_enum4(:c17)).to eq(:c17)
    expect(TestEnum4.test_tagged_nonint_enum4(:c18)).to eq(:c18)
    expect(TestEnum4.test_tagged_nonint_enum4(:c19)).to eq(:c19)
    expect(TestEnum4.test_tagged_nonint_enum4(:c20)).to eq(:c20)
    expect(TestEnum4.test_tagged_nonint_enum5(:c21)).to eq(:c21)
    expect(TestEnum4.test_tagged_nonint_enum5(:c22)).to eq(:c22)
    expect(TestEnum4.test_tagged_nonint_enum5(:c23)).to eq(:c23)
    expect(TestEnum4.test_tagged_nonint_enum5(:c24)).to eq(:c24)
    expect(TestEnum4.test_tagged_nonint_enum6(:c25)).to eq(:c25)
    expect(TestEnum4.test_tagged_nonint_enum6(:c26)).to eq(:c26)
    expect(TestEnum4.test_tagged_nonint_enum6(:c27)).to eq(:c27)
    expect(TestEnum4.test_tagged_nonint_enum6(:c28)).to eq(:c28)
  end

  it "integers can be used instead of constants" do
    expect(TestEnum3.test_tagged_typedef_enum1(0)).to be :c1
    expect(TestEnum3.test_tagged_typedef_enum1(1)).to be :c2
    expect(TestEnum3.test_tagged_typedef_enum1(2)).to be :c3
    expect(TestEnum3.test_tagged_typedef_enum1(3)).to be :c4
    expect(TestEnum3.test_tagged_typedef_enum2(42)).to be :c5
    expect(TestEnum3.test_tagged_typedef_enum2(43)).to be :c6
    expect(TestEnum3.test_tagged_typedef_enum2(44)).to be :c7
    expect(TestEnum3.test_tagged_typedef_enum2(45)).to be :c8
    expect(TestEnum3.test_tagged_typedef_enum3(42)).to be :c9
    expect(TestEnum3.test_tagged_typedef_enum3(43)).to be :c10
    expect(TestEnum3.test_tagged_typedef_enum3(4242)).to be :c11
    expect(TestEnum3.test_tagged_typedef_enum3(4243)).to be :c12
    expect(TestEnum3.test_tagged_typedef_enum4(42)).to be :c13
    expect(TestEnum3.test_tagged_typedef_enum4(4242)).to be :c14
    expect(TestEnum3.test_tagged_typedef_enum4(424242)).to be :c15
    expect(TestEnum3.test_tagged_typedef_enum4(42424242)).to be :c16
    expect(TestEnum4.test_tagged_nonint_enum4(0x42)).to eq(:c17)
    expect(TestEnum4.test_tagged_nonint_enum4(0x43)).to eq(:c18)
    expect(TestEnum4.test_tagged_nonint_enum4(0x44)).to eq(:c19)
    expect(TestEnum4.test_tagged_nonint_enum4(0x45)).to eq(:c20)
    expect(TestEnum4.test_tagged_nonint_enum5(0x42)).to eq(:c21)
    expect(TestEnum4.test_tagged_nonint_enum5(0x43)).to eq(:c22)
    expect(TestEnum4.test_tagged_nonint_enum5(0x4242)).to eq(:c23)
    expect(TestEnum4.test_tagged_nonint_enum5(0x4243)).to eq(:c24)
    expect(TestEnum4.test_tagged_nonint_enum6(0x42)).to eq(:c25)
    expect(TestEnum4.test_tagged_nonint_enum6(0x4242)).to eq(:c26)
    expect(TestEnum4.test_tagged_nonint_enum6(0x42424242)).to eq(:c27)
    expect(TestEnum4.test_tagged_nonint_enum6(0x4242424242424242)).to eq(:c28)
  end
end

describe "All enums" do
  it "have autonumbered constants when defined with names only" do
    expect(TestEnum1.enum_value(:c1)).to eq(0)
    expect(TestEnum1.enum_value(:c2)).to eq(1)
    expect(TestEnum1.enum_value(:c3)).to eq(2)
    expect(TestEnum1.enum_value(:c4)).to eq(3)

    expect(TestEnum3.enum_value(:c1)).to eq(0)
    expect(TestEnum3.enum_value(:c2)).to eq(1)
    expect(TestEnum3.enum_value(:c3)).to eq(2)
    expect(TestEnum3.enum_value(:c4)).to eq(3)

    expect(TestEnum4.enum_value(:c1)).to eq(0)
    expect(TestEnum4.enum_value(:c2)).to eq(1)
    expect(TestEnum4.enum_value(:c3)).to eq(2)
    expect(TestEnum4.enum_value(:c4)).to eq(3)
  end

  it "can have an explicit first constant and autonumbered subsequent constants" do
    expect(TestEnum1.enum_value(:c5)).to eq(42)
    expect(TestEnum1.enum_value(:c6)).to eq(43)
    expect(TestEnum1.enum_value(:c7)).to eq(44)
    expect(TestEnum1.enum_value(:c8)).to eq(45)

    expect(TestEnum3.enum_value(:c5)).to eq(42)
    expect(TestEnum3.enum_value(:c6)).to eq(43)
    expect(TestEnum3.enum_value(:c7)).to eq(44)
    expect(TestEnum3.enum_value(:c8)).to eq(45)

    expect(TestEnum4.enum_value(:c5)).to eq(0x42)
    expect(TestEnum4.enum_value(:c6)).to eq(0x43)
    expect(TestEnum4.enum_value(:c7)).to eq(0x44)
    expect(TestEnum4.enum_value(:c8)).to eq(0x45)

    expect(TestEnum4.enum_value(:c29)).to eq(0x4242424242424242)
    expect(TestEnum4.enum_value(:c30)).to eq(0x4242424242424243)
    expect(TestEnum4.enum_value(:c31)).to eq(0x4242424242424244)
    expect(TestEnum4.enum_value(:c32)).to eq(0x4242424242424245)
  end

  it "can have a mix of explicit and autonumbered constants" do
    expect(TestEnum1.enum_value(:c9)).to eq(42)
    expect(TestEnum1.enum_value(:c10)).to eq(43)
    expect(TestEnum1.enum_value(:c11)).to eq(4242)
    expect(TestEnum1.enum_value(:c12)).to eq(4243)

    expect(TestEnum3.enum_value(:c9)).to eq(42)
    expect(TestEnum3.enum_value(:c10)).to eq(43)
    expect(TestEnum3.enum_value(:c11)).to eq(4242)
    expect(TestEnum3.enum_value(:c12)).to eq(4243)

    expect(TestEnum4.enum_value(:c9)).to eq(0x42)
    expect(TestEnum4.enum_value(:c10)).to eq(0x43)
    expect(TestEnum4.enum_value(:c11)).to eq(0x4242)
    expect(TestEnum4.enum_value(:c12)).to eq(0x4243)

    expect(TestEnum4.enum_value(:c21)).to eq(0x42)
    expect(TestEnum4.enum_value(:c22)).to eq(0x43)
    expect(TestEnum4.enum_value(:c23)).to eq(0x4242)
    expect(TestEnum4.enum_value(:c24)).to eq(0x4243)
  end

  it "can have all its constants explicitely valued" do
    expect(TestEnum1.enum_value(:c13)).to eq(42)
    expect(TestEnum1.enum_value(:c14)).to eq(4242)
    expect(TestEnum1.enum_value(:c15)).to eq(424242)
    expect(TestEnum1.enum_value(:c16)).to eq(42424242)
    
    expect(TestEnum3.enum_value(:c13)).to eq(42)
    expect(TestEnum3.enum_value(:c14)).to eq(4242)
    expect(TestEnum3.enum_value(:c15)).to eq(424242)
    expect(TestEnum3.enum_value(:c16)).to eq(42424242)

    expect(TestEnum4.enum_value(:c13)).to eq(0x42)
    expect(TestEnum4.enum_value(:c14)).to eq(0x4242)
    expect(TestEnum4.enum_value(:c15)).to eq(0x42424242)
    expect(TestEnum4.enum_value(:c16)).to eq(0x4242424242424242)

    expect(TestEnum4.enum_value(:c25)).to eq(0x42)
    expect(TestEnum4.enum_value(:c26)).to eq(0x4242)
    expect(TestEnum4.enum_value(:c27)).to eq(0x42424242)
    expect(TestEnum4.enum_value(:c28)).to eq(0x4242424242424242)
  end

  it "return the constant corresponding to a specific value" do
    enum = TestEnum3.enum_type(:enum_type1)
    expect(enum[0]).to be :c1
    expect(enum[1]).to be :c2
    expect(enum[2]).to be :c3
    expect(enum[3]).to be :c4

    enum = TestEnum3.enum_type(:enum_type2)
    expect(enum[42]).to be :c5
    expect(enum[43]).to be :c6
    expect(enum[44]).to be :c7
    expect(enum[45]).to be :c8

    enum = TestEnum3.enum_type(:enum_type3)
    expect(enum[42]).to be :c9
    expect(enum[43]).to be :c10
    expect(enum[4242]).to be :c11
    expect(enum[4243]).to be :c12

    enum = TestEnum3.enum_type(:enum_type4)
    expect(enum[42]).to be :c13
    expect(enum[4242]).to be :c14
    expect(enum[424242]).to be :c15
    expect(enum[42424242]).to be :c16

    enum = TestEnum4.enum_type(:enum_type1)
    expect(enum[0x42]).to eq(:c5)
    expect(enum[0x43]).to eq(:c6)
    expect(enum[0x44]).to eq(:c7)
    expect(enum[0x45]).to eq(:c8)

    enum = TestEnum4.enum_type(:enum_type2)
    expect(enum[0x42]).to eq(:c9)
    expect(enum[0x43]).to eq(:c10)
    expect(enum[0x4242]).to eq(:c11)
    expect(enum[0x4243]).to eq(:c12)

    enum = TestEnum4.enum_type(:enum_type3)
    expect(enum[0x42]).to eq(:c13)
    expect(enum[0x4242]).to eq(:c14)
    expect(enum[0x42424242]).to eq(:c15)
    expect(enum[0x4242424242424242]).to eq(:c16)

    enum = TestEnum4.enum_type(:enum_type4)
    expect(enum[0x42]).to eq(:c17)
    expect(enum[0x43]).to eq(:c18)
    expect(enum[0x44]).to eq(:c19)
    expect(enum[0x45]).to eq(:c20)

    enum = TestEnum4.enum_type(:enum_type5)
    expect(enum[0x42]).to eq(:c21)
    expect(enum[0x43]).to eq(:c22)
    expect(enum[0x4242]).to eq(:c23)
    expect(enum[0x4243]).to eq(:c24)

    enum = TestEnum4.enum_type(:enum_type6)
    expect(enum[0x42]).to eq(:c25)
    expect(enum[0x4242]).to eq(:c26)
    expect(enum[0x42424242]).to eq(:c27)
    expect(enum[0x4242424242424242]).to eq(:c28)
  end

  it "return nil for values that don't have a symbol" do
    enum = TestEnum3.enum_type(:enum_type1)
    expect(enum[-1]).to be_nil
    expect(enum[4]).to be_nil

    enum = TestEnum3.enum_type(:enum_type2)
    expect(enum[0]).to be_nil
    expect(enum[41]).to be_nil
    expect(enum[46]).to be_nil

    enum = TestEnum3.enum_type(:enum_type3)
    expect(enum[0]).to be_nil
    expect(enum[41]).to be_nil
    expect(enum[44]).to be_nil
    expect(enum[4241]).to be_nil
    expect(enum[4244]).to be_nil

    enum = TestEnum3.enum_type(:enum_type4)
    expect(enum[0]).to be_nil
    expect(enum[41]).to be_nil
    expect(enum[43]).to be_nil
    expect(enum[4241]).to be_nil
    expect(enum[4243]).to be_nil
    expect(enum[424241]).to be_nil
    expect(enum[424243]).to be_nil
    expect(enum[42424241]).to be_nil
    expect(enum[42424243]).to be_nil

    enum = TestEnum4.enum_type(:enum_type1)
    expect(enum[0x0]).to be_nil
    expect(enum[0x41]).to be_nil
    expect(enum[0x46]).to be_nil

    enum = TestEnum4.enum_type(:enum_type2)
    expect(enum[0x0]).to be_nil
    expect(enum[0x41]).to be_nil
    expect(enum[0x44]).to be_nil
    expect(enum[0x4241]).to be_nil
    expect(enum[0x4244]).to be_nil

    enum = TestEnum4.enum_type(:enum_type3)
    expect(enum[0x0]).to be_nil
    expect(enum[0x41]).to be_nil
    expect(enum[0x43]).to be_nil
    expect(enum[0x4241]).to be_nil
    expect(enum[0x4243]).to be_nil
    expect(enum[0x42424241]).to be_nil
    expect(enum[0x42424243]).to be_nil
    expect(enum[0x4242424242424241]).to be_nil
    expect(enum[0x4242424242424243]).to be_nil

    enum = TestEnum4.enum_type(:enum_type4)
    expect(enum[0x0]).to be_nil
    expect(enum[0x41]).to be_nil
    expect(enum[0x46]).to be_nil

    enum = TestEnum4.enum_type(:enum_type5)
    expect(enum[0x0]).to be_nil
    expect(enum[0x41]).to be_nil
    expect(enum[0x44]).to be_nil
    expect(enum[0x4241]).to be_nil
    expect(enum[0x4244]).to be_nil

    enum = TestEnum4.enum_type(:enum_type6)
    expect(enum[0x0]).to be_nil
    expect(enum[0x41]).to be_nil
    expect(enum[0x43]).to be_nil
    expect(enum[0x4241]).to be_nil
    expect(enum[0x4243]).to be_nil
    expect(enum[0x42424241]).to be_nil
    expect(enum[0x42424243]).to be_nil
    expect(enum[0x4242424242424241]).to be_nil
    expect(enum[0x4242424242424243]).to be_nil
  end

  it "duplicate enum keys rejected" do
    expect { enum [ :a, 0xfee1dead, :b, 0xdeadbeef, :a, 0 ] }.to raise_error
    expect { enum FFI::Type::UINT64, [ :a, 0xfee1dead, :b, 0xdeadbeef, :a, 0 ] }.to raise_error
  end
end
