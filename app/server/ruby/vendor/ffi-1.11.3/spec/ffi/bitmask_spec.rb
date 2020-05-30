#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

module TestBitmask0
  extend FFI::Library
end

module TestBitmask1
  extend FFI::Library
  ffi_lib TestLibrary::PATH

  bitmask [:c1, :c2, :c3, :c4]
  bitmask [:c5, 2, :c6, :c7, :c8]
  bitmask [:c9, 2, :c10, :c11, 5, :c12]
  bitmask [:c13, 2, :c14, 4, :c15, 6, :c16, 8]

  attach_function :test_untagged_bitmask, [:int], :int
end

module TestBitmask3
  extend FFI::Library
  ffi_lib TestLibrary::PATH

  bitmask :bitmask_type1, [:c1, :c2, :c3, :c4]
  bitmask :bitmask_type2, [:c5, 2, :c6, :c7, :c8]
  bitmask :bitmask_type3, [:c9, 2, :c10, :c11, 5, :c12]
  bitmask :bitmask_type4, [:c13, 2, :c14, 4, :c15, 6, :c16, 8]

  attach_function :test_tagged_typedef_bitmask1, [:bitmask_type1], :bitmask_type1
  attach_function :test_tagged_typedef_bitmask2, [:bitmask_type2], :bitmask_type2
  attach_function :test_tagged_typedef_bitmask3, [:bitmask_type3], :bitmask_type3
  attach_function :test_tagged_typedef_bitmask4, [:bitmask_type4], :bitmask_type4
end

module TestBitmask4
  extend FFI::Library
  ffi_lib TestLibrary::PATH

  bitmask [:c1, :c2, :c3, :c4]
  bitmask :bitmask_type1, [:c5, 6, :c6, :c7, :c8]
  bitmask :bitmask_type2, [:c9, 6, :c10, :c11, 14, :c12]
  bitmask :bitmask_type3, [:c13, 6, :c14, 14, :c15, 30, :c16, 42]
  bitmask FFI::Type::UINT16, :bitmask_type4, [:c17, 6, :c18, :c19, :c20]
  bitmask FFI::Type::UINT32, :bitmask_type5, [:c21, 6, :c22, :c23, 14, :c24]
  bitmask FFI::Type::UINT64, :bitmask_type6, [:c25, 6, :c26, 14, :c27, 30, :c28, 42]
  bitmask FFI::Type::UINT64, [:c29, 42, :c30, :c31, :c32]

  attach_function :test_untagged_nonint_bitmask, [:uint8],  :uint8
  attach_function :test_tagged_nonint_bitmask1,  [:uint16], :uint16
  attach_function :test_tagged_nonint_bitmask2,  [:uint32], :uint32
  attach_function :test_tagged_nonint_bitmask3,  [:uint64], :uint64
  attach_function :test_tagged_nonint_bitmask4, :test_tagged_nonint_bitmask1,  [:bitmask_type4], :bitmask_type4
  attach_function :test_tagged_nonint_bitmask5, :test_tagged_nonint_bitmask2,  [:bitmask_type5], :bitmask_type5
  attach_function :test_tagged_nonint_bitmask6, :test_tagged_nonint_bitmask3,  [:bitmask_type6], :bitmask_type6
end

describe "A library with no bitmask or enum defined" do
  it "returns nil when asked for an enum" do
    expect(TestBitmask0.enum_type(:foo)).to be_nil
  end
end

describe "An untagged bitmask" do
  it "constants can be used as function parameters and return value" do
    expect(TestBitmask1.test_untagged_bitmask(:c1)).to eq(1<<0)
    expect(TestBitmask1.test_untagged_bitmask(:c2)).to eq(1<<1)
    expect(TestBitmask1.test_untagged_bitmask(:c3)).to eq(1<<2)
    expect(TestBitmask1.test_untagged_bitmask(:c4)).to eq(1<<3)
    expect(TestBitmask1.test_untagged_bitmask(:c5)).to eq(1<<2)
    expect(TestBitmask1.test_untagged_bitmask(:c6)).to eq(1<<3)
    expect(TestBitmask1.test_untagged_bitmask(:c7)).to eq(1<<4)
    expect(TestBitmask1.test_untagged_bitmask(:c8)).to eq(1<<5)
    expect(TestBitmask1.test_untagged_bitmask(:c9)).to eq(1<<2)
    expect(TestBitmask1.test_untagged_bitmask(:c10)).to eq(1<<3)
    expect(TestBitmask1.test_untagged_bitmask(:c11)).to eq(1<<5)
    expect(TestBitmask1.test_untagged_bitmask(:c12)).to eq(1<<6)
    expect(TestBitmask1.test_untagged_bitmask(:c13)).to eq(1<<2)
    expect(TestBitmask1.test_untagged_bitmask(:c14)).to eq(1<<4)
    expect(TestBitmask1.test_untagged_bitmask(:c15)).to eq(1<<6)
    expect(TestBitmask1.test_untagged_bitmask(:c16)).to eq(1<<8)
    expect(TestBitmask4.test_untagged_nonint_bitmask(:c1)).to eq(1<<0)
    expect(TestBitmask4.test_untagged_nonint_bitmask(:c2)).to eq(1<<1)
    expect(TestBitmask4.test_untagged_nonint_bitmask(:c3)).to eq(1<<2)
    expect(TestBitmask4.test_untagged_nonint_bitmask(:c4)).to eq(1<<3)
    expect(TestBitmask4.test_tagged_nonint_bitmask3(:c29)).to eq(1<<42)
    expect(TestBitmask4.test_tagged_nonint_bitmask3(:c30)).to eq(1<<43)
    expect(TestBitmask4.test_tagged_nonint_bitmask3(:c31)).to eq(1<<44)
    expect(TestBitmask4.test_tagged_nonint_bitmask3(:c32)).to eq(1<<45)
  end
end

describe "A tagged typedef bitmask" do
  it "is accessible through its tag" do
    expect(TestBitmask3.enum_type(:bitmask_type1)).not_to be_nil
    expect(TestBitmask3.enum_type(:bitmask_type2)).not_to be_nil
    expect(TestBitmask3.enum_type(:bitmask_type3)).not_to be_nil
    expect(TestBitmask3.enum_type(:bitmask_type4)).not_to be_nil
    expect(TestBitmask4.enum_type(:bitmask_type1)).not_to be_nil
    expect(TestBitmask4.enum_type(:bitmask_type2)).not_to be_nil
    expect(TestBitmask4.enum_type(:bitmask_type3)).not_to be_nil
    expect(TestBitmask4.enum_type(:bitmask_type4)).not_to be_nil
    expect(TestBitmask4.enum_type(:bitmask_type5)).not_to be_nil
    expect(TestBitmask4.enum_type(:bitmask_type6)).not_to be_nil
  end

  it "contains bitmask constants" do
    expect(TestBitmask3.enum_type(:bitmask_type1).symbols.length).to eq(4)
    expect(TestBitmask3.enum_type(:bitmask_type2).symbols.length).to eq(4)
    expect(TestBitmask3.enum_type(:bitmask_type3).symbols.length).to eq(4)
    expect(TestBitmask3.enum_type(:bitmask_type4).symbols.length).to eq(4)
    expect(TestBitmask4.enum_type(:bitmask_type1).symbols.length).to eq(4)
    expect(TestBitmask4.enum_type(:bitmask_type2).symbols.length).to eq(4)
    expect(TestBitmask4.enum_type(:bitmask_type3).symbols.length).to eq(4)
    expect(TestBitmask4.enum_type(:bitmask_type4).symbols.length).to eq(4)
    expect(TestBitmask4.enum_type(:bitmask_type5).symbols.length).to eq(4)
    expect(TestBitmask4.enum_type(:bitmask_type6).symbols.length).to eq(4)
  end

  it "constants can be used as function parameters and return value" do
    expect(TestBitmask3.test_tagged_typedef_bitmask1(:c1)).to eq([:c1])
    expect(TestBitmask3.test_tagged_typedef_bitmask1(:c2)).to eq([:c2])
    expect(TestBitmask3.test_tagged_typedef_bitmask1(:c3)).to eq([:c3])
    expect(TestBitmask3.test_tagged_typedef_bitmask1(:c4)).to eq([:c4])
    expect(TestBitmask3.test_tagged_typedef_bitmask2(:c5)).to eq([:c5])
    expect(TestBitmask3.test_tagged_typedef_bitmask2(:c6)).to eq([:c6])
    expect(TestBitmask3.test_tagged_typedef_bitmask2(:c7)).to eq([:c7])
    expect(TestBitmask3.test_tagged_typedef_bitmask2(:c8)).to eq([:c8])
    expect(TestBitmask3.test_tagged_typedef_bitmask3(:c9)).to eq([:c9])
    expect(TestBitmask3.test_tagged_typedef_bitmask3(:c10)).to eq([:c10])
    expect(TestBitmask3.test_tagged_typedef_bitmask3(:c11)).to eq([:c11])
    expect(TestBitmask3.test_tagged_typedef_bitmask3(:c12)).to eq([:c12])
    expect(TestBitmask3.test_tagged_typedef_bitmask4(:c13)).to eq([:c13])
    expect(TestBitmask3.test_tagged_typedef_bitmask4(:c14)).to eq([:c14])
    expect(TestBitmask3.test_tagged_typedef_bitmask4(:c15)).to eq([:c15])
    expect(TestBitmask3.test_tagged_typedef_bitmask4(:c16)).to eq([:c16])
    expect(TestBitmask4.test_tagged_nonint_bitmask1(:c5)).to eq(1<<6)
    expect(TestBitmask4.test_tagged_nonint_bitmask1(:c6)).to eq(1<<7)
    expect(TestBitmask4.test_tagged_nonint_bitmask1(:c7)).to eq(1<<8)
    expect(TestBitmask4.test_tagged_nonint_bitmask1(:c8)).to eq(1<<9)
    expect(TestBitmask4.test_tagged_nonint_bitmask2(:c9)).to eq(1<<6)
    expect(TestBitmask4.test_tagged_nonint_bitmask2(:c10)).to eq(1<<7)
    expect(TestBitmask4.test_tagged_nonint_bitmask2(:c11)).to eq(1<<14)
    expect(TestBitmask4.test_tagged_nonint_bitmask2(:c12)).to eq(1<<15)
    expect(TestBitmask4.test_tagged_nonint_bitmask3(:c13)).to eq(1<<6)
    expect(TestBitmask4.test_tagged_nonint_bitmask3(:c14)).to eq(1<<14)
    expect(TestBitmask4.test_tagged_nonint_bitmask3(:c15)).to eq(1<<30)
    expect(TestBitmask4.test_tagged_nonint_bitmask3(:c16)).to eq(1<<42)
    expect(TestBitmask4.test_tagged_nonint_bitmask4(:c17)).to eq([:c17])
    expect(TestBitmask4.test_tagged_nonint_bitmask4(:c18)).to eq([:c18])
    expect(TestBitmask4.test_tagged_nonint_bitmask4(:c19)).to eq([:c19])
    expect(TestBitmask4.test_tagged_nonint_bitmask4(:c20)).to eq([:c20])
    expect(TestBitmask4.test_tagged_nonint_bitmask5(:c21)).to eq([:c21])
    expect(TestBitmask4.test_tagged_nonint_bitmask5(:c22)).to eq([:c22])
    expect(TestBitmask4.test_tagged_nonint_bitmask5(:c23)).to eq([:c23])
    expect(TestBitmask4.test_tagged_nonint_bitmask5(:c24)).to eq([:c24])
    expect(TestBitmask4.test_tagged_nonint_bitmask6(:c25)).to eq([:c25])
    expect(TestBitmask4.test_tagged_nonint_bitmask6(:c26)).to eq([:c26])
    expect(TestBitmask4.test_tagged_nonint_bitmask6(:c27)).to eq([:c27])
    expect(TestBitmask4.test_tagged_nonint_bitmask6(:c28)).to eq([:c28])
  end

  it "constants can be combined into list to be used as function parameters and return values" do
    expect(TestBitmask3.test_tagged_typedef_bitmask1([:c2,:c4])).to eq([:c2,:c4])
    expect(TestBitmask3.test_tagged_typedef_bitmask2([:c6,:c8])).to eq([:c6,:c8])
    expect(TestBitmask3.test_tagged_typedef_bitmask3([:c10,:c12])).to eq([:c10,:c12])
    expect(TestBitmask3.test_tagged_typedef_bitmask4([:c14,:c16])).to eq([:c14,:c16])
    expect(TestBitmask4.test_tagged_nonint_bitmask4([:c18,:c20])).to eq([:c18,:c20])
    expect(TestBitmask4.test_tagged_nonint_bitmask5([:c22,:c24])).to eq([:c22,:c24])
    expect(TestBitmask4.test_tagged_nonint_bitmask6([:c26,:c28])).to eq([:c26,:c28])
  end

  it "integers can be used instead of constants" do
    expect(TestBitmask3.test_tagged_typedef_bitmask1(1<<0)).to eq([:c1])
    expect(TestBitmask3.test_tagged_typedef_bitmask1(1<<1)).to eq([:c2])
    expect(TestBitmask3.test_tagged_typedef_bitmask1(1<<2)).to eq([:c3])
    expect(TestBitmask3.test_tagged_typedef_bitmask1(1<<3)).to eq([:c4])
    expect(TestBitmask3.test_tagged_typedef_bitmask2(1<<2)).to eq([:c5])
    expect(TestBitmask3.test_tagged_typedef_bitmask2(1<<3)).to eq([:c6])
    expect(TestBitmask3.test_tagged_typedef_bitmask2(1<<4)).to eq([:c7])
    expect(TestBitmask3.test_tagged_typedef_bitmask2(1<<5)).to eq([:c8])
    expect(TestBitmask3.test_tagged_typedef_bitmask3(1<<2)).to eq([:c9])
    expect(TestBitmask3.test_tagged_typedef_bitmask3(1<<3)).to eq([:c10])
    expect(TestBitmask3.test_tagged_typedef_bitmask3(1<<5)).to eq([:c11])
    expect(TestBitmask3.test_tagged_typedef_bitmask3(1<<6)).to eq([:c12])
    expect(TestBitmask3.test_tagged_typedef_bitmask4(1<<2)).to eq([:c13])
    expect(TestBitmask3.test_tagged_typedef_bitmask4(1<<4)).to eq([:c14])
    expect(TestBitmask3.test_tagged_typedef_bitmask4(1<<6)).to eq([:c15])
    expect(TestBitmask3.test_tagged_typedef_bitmask4(1<<8)).to eq([:c16])
    expect(TestBitmask4.test_tagged_nonint_bitmask4(1<<6)).to eq([:c17])
    expect(TestBitmask4.test_tagged_nonint_bitmask4(1<<7)).to eq([:c18])
    expect(TestBitmask4.test_tagged_nonint_bitmask4(1<<8)).to eq([:c19])
    expect(TestBitmask4.test_tagged_nonint_bitmask4(1<<9)).to eq([:c20])
    expect(TestBitmask4.test_tagged_nonint_bitmask5(1<<6)).to eq([:c21])
    expect(TestBitmask4.test_tagged_nonint_bitmask5(1<<7)).to eq([:c22])
    expect(TestBitmask4.test_tagged_nonint_bitmask5(1<<14)).to eq([:c23])
    expect(TestBitmask4.test_tagged_nonint_bitmask5(1<<15)).to eq([:c24])
    expect(TestBitmask4.test_tagged_nonint_bitmask6(1<<6)).to eq([:c25])
    expect(TestBitmask4.test_tagged_nonint_bitmask6(1<<14)).to eq([:c26])
    expect(TestBitmask4.test_tagged_nonint_bitmask6(1<<30)).to eq([:c27])
    expect(TestBitmask4.test_tagged_nonint_bitmask6(1<<42)).to eq([:c28])
  end

  it "combination or list of integers can be used instead of constants" do
    expect(TestBitmask3.test_tagged_typedef_bitmask1(1<<1|1<<3)).to eq([:c2,:c4])
    expect(TestBitmask3.test_tagged_typedef_bitmask1([1<<1,1<<3])).to eq([:c2,:c4])
    expect(TestBitmask3.test_tagged_typedef_bitmask2(1<<3|1<<5)).to eq([:c6,:c8])
    expect(TestBitmask3.test_tagged_typedef_bitmask2([1<<3,1<<5])).to eq([:c6,:c8])
    expect(TestBitmask3.test_tagged_typedef_bitmask3(1<<3|1<<6)).to eq([:c10,:c12])
    expect(TestBitmask3.test_tagged_typedef_bitmask3([1<<3,1<<6])).to eq([:c10,:c12])
    expect(TestBitmask3.test_tagged_typedef_bitmask4(1<<4|1<<8)).to eq([:c14,:c16])
    expect(TestBitmask3.test_tagged_typedef_bitmask4([1<<4,1<<8])).to eq([:c14,:c16])
    expect(TestBitmask4.test_tagged_nonint_bitmask4(1<<7|1<<9)).to eq([:c18,:c20])
    expect(TestBitmask4.test_tagged_nonint_bitmask4([1<<7,1<<9])).to eq([:c18,:c20])
    expect(TestBitmask4.test_tagged_nonint_bitmask5(1<<7|1<<15)).to eq([:c22,:c24])
    expect(TestBitmask4.test_tagged_nonint_bitmask5([1<<7,1<<15])).to eq([:c22,:c24])
    expect(TestBitmask4.test_tagged_nonint_bitmask6(1<<14|1<<42)).to eq([:c26,:c28])
    expect(TestBitmask4.test_tagged_nonint_bitmask6([1<<14,1<<42])).to eq([:c26,:c28])
  end

  it "mixed list of integers and constants can be used instead of constants" do
    expect(TestBitmask3.test_tagged_typedef_bitmask1([:c2,1<<3])).to eq([:c2,:c4])
    expect(TestBitmask3.test_tagged_typedef_bitmask2([:c6,1<<5])).to eq([:c6,:c8])
    expect(TestBitmask3.test_tagged_typedef_bitmask3([:c10,1<<6])).to eq([:c10,:c12])
    expect(TestBitmask3.test_tagged_typedef_bitmask4([:c14,1<<8])).to eq([:c14,:c16])
    expect(TestBitmask4.test_tagged_nonint_bitmask4([:c18,1<<9])).to eq([:c18,:c20])
    expect(TestBitmask4.test_tagged_nonint_bitmask5([:c22,1<<15])).to eq([:c22,:c24])
    expect(TestBitmask4.test_tagged_nonint_bitmask6([:c26,1<<42])).to eq([:c26,:c28])
  end

  it "remainder is given if some undefined mask are returned" do
    expect(TestBitmask3.test_tagged_typedef_bitmask1(1<<1|1<<3|1<<4)).to eq([:c2,:c4,1<<4])
    expect(TestBitmask3.test_tagged_typedef_bitmask1([1<<1,1<<3,1<<4])).to eq([:c2,:c4,1<<4])
    expect(TestBitmask3.test_tagged_typedef_bitmask2(1<<3|1<<5|1<<6)).to eq([:c6,:c8,1<<6])
    expect(TestBitmask3.test_tagged_typedef_bitmask2([1<<3,1<<5,1<<6])).to eq([:c6,:c8,1<<6])
    expect(TestBitmask3.test_tagged_typedef_bitmask3(1<<3|1<<6|1<<7)).to eq([:c10,:c12,1<<7])
    expect(TestBitmask3.test_tagged_typedef_bitmask3([1<<3,1<<6,1<<7])).to eq([:c10,:c12,1<<7])
    expect(TestBitmask3.test_tagged_typedef_bitmask4(1<<4|1<<8|1<<9)).to eq([:c14,:c16,1<<9])
    expect(TestBitmask3.test_tagged_typedef_bitmask4([1<<4,1<<8,1<<9])).to eq([:c14,:c16,1<<9])
    expect(TestBitmask4.test_tagged_nonint_bitmask4(1<<7|1<<9|1<<10)).to eq([:c18,:c20,1<<10])
    expect(TestBitmask4.test_tagged_nonint_bitmask4([1<<7,1<<9,1<<10])).to eq([:c18,:c20,1<<10])
    expect(TestBitmask4.test_tagged_nonint_bitmask5(1<<7|1<<15|1<<16)).to eq([:c22,:c24,1<<16])
    expect(TestBitmask4.test_tagged_nonint_bitmask5([1<<7,1<<15,1<<16])).to eq([:c22,:c24,1<<16])
    expect(TestBitmask4.test_tagged_nonint_bitmask6(1<<14|1<<42|1<<43)).to eq([:c26,:c28,1<<43])
    expect(TestBitmask4.test_tagged_nonint_bitmask6([1<<14,1<<42,1<<43])).to eq([:c26,:c28,1<<43])
  end

  it "wrong constants rejected" do
    expect { TestBitmask3.test_tagged_typedef_bitmask1([:c2,:c4,:c5]) }.to raise_error(ArgumentError)
    expect { TestBitmask3.test_tagged_typedef_bitmask2([:c6,:c8,:c9]) }.to raise_error(ArgumentError)
    expect { TestBitmask3.test_tagged_typedef_bitmask3([:c10,:c12,:c13]) }.to raise_error(ArgumentError)
    expect { TestBitmask3.test_tagged_typedef_bitmask4([:c14,:c16,:c17]) }.to raise_error(ArgumentError)
    expect { TestBitmask4.test_tagged_nonint_bitmask4([:c18,:c20,:c21]) }.to raise_error(ArgumentError)
    expect { TestBitmask4.test_tagged_nonint_bitmask5([:c22,:c24,:c25]) }.to raise_error(ArgumentError)
    expect { TestBitmask4.test_tagged_nonint_bitmask6([:c26,:c28,:c29]) }.to raise_error(ArgumentError)
  end

end

describe "All bitmasks" do
  it "have autonumbered constants when defined with names only" do
    expect(TestBitmask1.enum_value(:c1)).to eq(1<<0)
    expect(TestBitmask1.enum_value(:c2)).to eq(1<<1)
    expect(TestBitmask1.enum_value(:c3)).to eq(1<<2)
    expect(TestBitmask1.enum_value(:c4)).to eq(1<<3)

    expect(TestBitmask3.enum_value(:c1)).to eq(1<<0)
    expect(TestBitmask3.enum_value(:c2)).to eq(1<<1)
    expect(TestBitmask3.enum_value(:c3)).to eq(1<<2)
    expect(TestBitmask3.enum_value(:c4)).to eq(1<<3)

    expect(TestBitmask4.enum_value(:c1)).to eq(1<<0)
    expect(TestBitmask4.enum_value(:c2)).to eq(1<<1)
    expect(TestBitmask4.enum_value(:c3)).to eq(1<<2)
    expect(TestBitmask4.enum_value(:c4)).to eq(1<<3)
  end

  it "can have an explicit first constant and autonumbered subsequent constants" do
    expect(TestBitmask1.enum_value(:c5)).to eq(1<<2)
    expect(TestBitmask1.enum_value(:c6)).to eq(1<<3)
    expect(TestBitmask1.enum_value(:c7)).to eq(1<<4)
    expect(TestBitmask1.enum_value(:c8)).to eq(1<<5)

    expect(TestBitmask3.enum_value(:c5)).to eq(1<<2)
    expect(TestBitmask3.enum_value(:c6)).to eq(1<<3)
    expect(TestBitmask3.enum_value(:c7)).to eq(1<<4)
    expect(TestBitmask3.enum_value(:c8)).to eq(1<<5)

    expect(TestBitmask4.enum_value(:c5)).to eq(1<<6)
    expect(TestBitmask4.enum_value(:c6)).to eq(1<<7)
    expect(TestBitmask4.enum_value(:c7)).to eq(1<<8)
    expect(TestBitmask4.enum_value(:c8)).to eq(1<<9)

    expect(TestBitmask4.enum_value(:c29)).to eq(1<<42)
    expect(TestBitmask4.enum_value(:c30)).to eq(1<<43)
    expect(TestBitmask4.enum_value(:c31)).to eq(1<<44)
    expect(TestBitmask4.enum_value(:c32)).to eq(1<<45)
  end

  it "can have a mix of explicit and autonumbered constants" do
    expect(TestBitmask1.enum_value(:c9)).to eq(1<<2)
    expect(TestBitmask1.enum_value(:c10)).to eq(1<<3)
    expect(TestBitmask1.enum_value(:c11)).to eq(1<<5)
    expect(TestBitmask1.enum_value(:c12)).to eq(1<<6)

    expect(TestBitmask3.enum_value(:c9)).to eq(1<<2)
    expect(TestBitmask3.enum_value(:c10)).to eq(1<<3)
    expect(TestBitmask3.enum_value(:c11)).to eq(1<<5)
    expect(TestBitmask3.enum_value(:c12)).to eq(1<<6)

    expect(TestBitmask4.enum_value(:c9)).to eq(1<<6)
    expect(TestBitmask4.enum_value(:c10)).to eq(1<<7)
    expect(TestBitmask4.enum_value(:c11)).to eq(1<<14)
    expect(TestBitmask4.enum_value(:c12)).to eq(1<<15)

    expect(TestBitmask4.enum_value(:c21)).to eq(1<<6)
    expect(TestBitmask4.enum_value(:c22)).to eq(1<<7)
    expect(TestBitmask4.enum_value(:c23)).to eq(1<<14)
    expect(TestBitmask4.enum_value(:c24)).to eq(1<<15)
  end

  it "can have all its constants explicitely valued" do
    expect(TestBitmask1.enum_value(:c13)).to eq(1<<2)
    expect(TestBitmask1.enum_value(:c14)).to eq(1<<4)
    expect(TestBitmask1.enum_value(:c15)).to eq(1<<6)
    expect(TestBitmask1.enum_value(:c16)).to eq(1<<8)

    expect(TestBitmask3.enum_value(:c13)).to eq(1<<2)
    expect(TestBitmask3.enum_value(:c14)).to eq(1<<4)
    expect(TestBitmask3.enum_value(:c15)).to eq(1<<6)
    expect(TestBitmask3.enum_value(:c16)).to eq(1<<8)

    expect(TestBitmask4.enum_value(:c13)).to eq(1<<6)
    expect(TestBitmask4.enum_value(:c14)).to eq(1<<14)
    expect(TestBitmask4.enum_value(:c15)).to eq(1<<30)
    expect(TestBitmask4.enum_value(:c16)).to eq(1<<42)

    expect(TestBitmask4.enum_value(:c25)).to eq(1<<6)
    expect(TestBitmask4.enum_value(:c26)).to eq(1<<14)
    expect(TestBitmask4.enum_value(:c27)).to eq(1<<30)
    expect(TestBitmask4.enum_value(:c28)).to eq(1<<42)
  end

  it "return a list containing a constant corresponding to a specific value" do
    bitmask = TestBitmask3.enum_type(:bitmask_type1)
    expect(bitmask[1<<0]).to eq([:c1])
    expect(bitmask[1<<1]).to eq([:c2])
    expect(bitmask[1<<2]).to eq([:c3])
    expect(bitmask[1<<3]).to eq([:c4])

    bitmask = TestBitmask3.enum_type(:bitmask_type2)
    expect(bitmask[1<<2]).to eq([:c5])
    expect(bitmask[1<<3]).to eq([:c6])
    expect(bitmask[1<<4]).to eq([:c7])
    expect(bitmask[1<<5]).to eq([:c8])

    bitmask = TestBitmask3.enum_type(:bitmask_type3)
    expect(bitmask[1<<2]).to eq([:c9])
    expect(bitmask[1<<3]).to eq([:c10])
    expect(bitmask[1<<5]).to eq([:c11])
    expect(bitmask[1<<6]).to eq([:c12])

    bitmask = TestBitmask3.enum_type(:bitmask_type4)
    expect(bitmask[1<<2]).to eq([:c13])
    expect(bitmask[1<<4]).to eq([:c14])
    expect(bitmask[1<<6]).to eq([:c15])
    expect(bitmask[1<<8]).to eq([:c16])

    bitmask = TestBitmask4.enum_type(:bitmask_type1)
    expect(bitmask[1<<6]).to eq([:c5])
    expect(bitmask[1<<7]).to eq([:c6])
    expect(bitmask[1<<8]).to eq([:c7])
    expect(bitmask[1<<9]).to eq([:c8])

    bitmask = TestBitmask4.enum_type(:bitmask_type2)
    expect(bitmask[1<<6]).to eq([:c9])
    expect(bitmask[1<<7]).to eq([:c10])
    expect(bitmask[1<<14]).to eq([:c11])
    expect(bitmask[1<<15]).to eq([:c12])

    bitmask = TestBitmask4.enum_type(:bitmask_type3)
    expect(bitmask[1<<6]).to eq([:c13])
    expect(bitmask[1<<14]).to eq([:c14])
    expect(bitmask[1<<30]).to eq([:c15])
    expect(bitmask[1<<42]).to eq([:c16])

    bitmask = TestBitmask4.enum_type(:bitmask_type4)
    expect(bitmask[1<<6]).to eq([:c17])
    expect(bitmask[1<<7]).to eq([:c18])
    expect(bitmask[1<<8]).to eq([:c19])
    expect(bitmask[1<<9]).to eq([:c20])

    bitmask = TestBitmask4.enum_type(:bitmask_type5)
    expect(bitmask[1<<6]).to eq([:c21])
    expect(bitmask[1<<7]).to eq([:c22])
    expect(bitmask[1<<14]).to eq([:c23])
    expect(bitmask[1<<15]).to eq([:c24])

    bitmask = TestBitmask4.enum_type(:bitmask_type6)
    expect(bitmask[1<<6]).to eq([:c25])
    expect(bitmask[1<<14]).to eq([:c26])
    expect(bitmask[1<<30]).to eq([:c27])
    expect(bitmask[1<<42]).to eq([:c28])
  end

  it "return a list containing constants corresponding to a specific value combination of values" do
    bitmask = TestBitmask3.enum_type(:bitmask_type1)
    expect(bitmask[1<<0|1<<1|1<<2|1<<3]).to eq([:c1,:c2,:c3,:c4])
    expect(bitmask[1<<0,1<<1,1<<2,1<<3]).to eq([:c1,:c2,:c3,:c4])
    expect(bitmask[-1]).to eq([:c1,:c2,:c3,:c4])
    expect(bitmask[1<<1|1<<3]).to eq([:c2,:c4])
    expect(bitmask[1<<1,1<<3]).to eq([:c2,:c4])
    expect(bitmask[1<<3|1<<5]).to eq([:c4])
    expect(bitmask[1<<3,1<<5]).to eq([:c4])

    bitmask = TestBitmask3.enum_type(:bitmask_type2)
    expect(bitmask[1<<2|1<<3|1<<4|1<<5]).to eq([:c5,:c6,:c7,:c8])
    expect(bitmask[1<<2,1<<3,1<<4,1<<5]).to eq([:c5,:c6,:c7,:c8])
    expect(bitmask[-1]).to eq([:c5,:c6,:c7,:c8])
    expect(bitmask[1<<3|1<<5]).to eq([:c6,:c8])
    expect(bitmask[1<<3,1<<5]).to eq([:c6,:c8])
    expect(bitmask[1<<5|1<<6]).to eq([:c8])
    expect(bitmask[1<<5,1<<6]).to eq([:c8])

    bitmask = TestBitmask3.enum_type(:bitmask_type3)
    expect(bitmask[1<<2|1<<3|1<<5|1<<6]).to eq([:c9,:c10,:c11,:c12])
    expect(bitmask[1<<2,1<<3,1<<5,1<<6]).to eq([:c9,:c10,:c11,:c12])
    expect(bitmask[-1]).to eq([:c9,:c10,:c11,:c12])
    expect(bitmask[1<<3|1<<6]).to eq([:c10,:c12])
    expect(bitmask[1<<3,1<<6]).to eq([:c10,:c12])
    expect(bitmask[1<<6|1<<7]).to eq([:c12])
    expect(bitmask[1<<6,1<<7]).to eq([:c12])

    bitmask = TestBitmask3.enum_type(:bitmask_type4)
    expect(bitmask[1<<2|1<<4|1<<6|1<<8]).to eq([:c13,:c14,:c15,:c16])
    expect(bitmask[1<<2,1<<4,1<<6,1<<8]).to eq([:c13,:c14,:c15,:c16])
    expect(bitmask[-1]).to eq([:c13,:c14,:c15,:c16])
    expect(bitmask[1<<4|1<<8]).to eq([:c14,:c16])
    expect(bitmask[1<<4,1<<8]).to eq([:c14,:c16])
    expect(bitmask[1<<8|1<<9]).to eq([:c16])
    expect(bitmask[1<<8,1<<9]).to eq([:c16])

    bitmask = TestBitmask4.enum_type(:bitmask_type1)
    expect(bitmask[1<<6|1<<7|1<<8|1<<9]).to eq([:c5,:c6,:c7,:c8])
    expect(bitmask[1<<6,1<<7,1<<8,1<<9]).to eq([:c5,:c6,:c7,:c8])
    expect(bitmask[-1]).to eq([:c5,:c6,:c7,:c8])
    expect(bitmask[1<<7|1<<9]).to eq([:c6,:c8])
    expect(bitmask[1<<7,1<<9]).to eq([:c6,:c8])
    expect(bitmask[1<<9|1<<10]).to eq([:c8])
    expect(bitmask[1<<9,1<<10]).to eq([:c8])

    bitmask = TestBitmask4.enum_type(:bitmask_type2)
    expect(bitmask[1<<6|1<<7|1<<14|1<<15]).to eq([:c9,:c10,:c11,:c12])
    expect(bitmask[1<<6,1<<7,1<<14,1<<15]).to eq([:c9,:c10,:c11,:c12])
    expect(bitmask[-1]).to eq([:c9,:c10,:c11,:c12])
    expect(bitmask[1<<7|1<<15]).to eq([:c10,:c12])
    expect(bitmask[1<<7,1<<15]).to eq([:c10,:c12])
    expect(bitmask[1<<15|1<<16]).to eq([:c12])
    expect(bitmask[1<<15,1<<16]).to eq([:c12])

    bitmask = TestBitmask4.enum_type(:bitmask_type3)
    expect(bitmask[1<<6|1<<14|1<<30|1<<42]).to eq([:c13,:c14,:c15,:c16])
    expect(bitmask[1<<6,1<<14,1<<30,1<<42]).to eq([:c13,:c14,:c15,:c16])
    expect(bitmask[-1]).to eq([:c13,:c14,:c15,:c16])
    expect(bitmask[1<<14|1<<42]).to eq([:c14,:c16])
    expect(bitmask[1<<14,1<<42]).to eq([:c14,:c16])
    expect(bitmask[1<<42|1<<43]).to eq([:c16])
    expect(bitmask[1<<42,1<<43]).to eq([:c16])

    bitmask = TestBitmask4.enum_type(:bitmask_type4)
    expect(bitmask[1<<6|1<<7|1<<8|1<<9]).to eq([:c17,:c18,:c19,:c20])
    expect(bitmask[1<<6,1<<7,1<<8,1<<9]).to eq([:c17,:c18,:c19,:c20])
    expect(bitmask[-1]).to eq([:c17,:c18,:c19,:c20])
    expect(bitmask[1<<7|1<<9]).to eq([:c18,:c20])
    expect(bitmask[1<<7,1<<9]).to eq([:c18,:c20])
    expect(bitmask[1<<9|1<<10]).to eq([:c20])
    expect(bitmask[1<<9,1<<10]).to eq([:c20])

    bitmask = TestBitmask4.enum_type(:bitmask_type5)
    expect(bitmask[1<<6|1<<7|1<<14|1<<15]).to eq([:c21,:c22,:c23,:c24])
    expect(bitmask[1<<6,1<<7,1<<14,1<<15]).to eq([:c21,:c22,:c23,:c24])
    expect(bitmask[-1]).to eq([:c21,:c22,:c23,:c24])
    expect(bitmask[1<<7|1<<15]).to eq([:c22,:c24])
    expect(bitmask[1<<7,1<<15]).to eq([:c22,:c24])
    expect(bitmask[1<<15|1<<16]).to eq([:c24])
    expect(bitmask[1<<15,1<<16]).to eq([:c24])

    bitmask = TestBitmask4.enum_type(:bitmask_type6)
    expect(bitmask[1<<6|1<<14|1<<30|1<<42]).to eq([:c25,:c26,:c27,:c28])
    expect(bitmask[1<<6,1<<14,1<<30,1<<42]).to eq([:c25,:c26,:c27,:c28])
    expect(bitmask[-1]).to eq([:c25,:c26,:c27,:c28])
    expect(bitmask[1<<14|1<<42]).to eq([:c26,:c28])
    expect(bitmask[1<<14,1<<42]).to eq([:c26,:c28])
    expect(bitmask[1<<42|1<<43]).to eq([:c28])
    expect(bitmask[1<<42,1<<43]).to eq([:c28])
  end

  it "return [] for values that don't have a symbol" do
    bitmask = TestBitmask3.enum_type(:bitmask_type1)
    expect(bitmask[1<<4]).to eq([])

    bitmask = TestBitmask3.enum_type(:bitmask_type2)
    expect(bitmask[1<<0]).to eq([])
    expect(bitmask[1<<1]).to eq([])
    expect(bitmask[1<<6]).to eq([])

    bitmask = TestBitmask3.enum_type(:bitmask_type3)
    expect(bitmask[1<<0]).to eq([])
    expect(bitmask[1<<1]).to eq([])
    expect(bitmask[1<<4]).to eq([])
    expect(bitmask[1<<7]).to eq([])

    bitmask = TestBitmask3.enum_type(:bitmask_type4)
    expect(bitmask[1<<0]).to eq([])
    expect(bitmask[1<<1]).to eq([])
    expect(bitmask[1<<3]).to eq([])
    expect(bitmask[1<<5]).to eq([])
    expect(bitmask[1<<7]).to eq([])
    expect(bitmask[1<<9]).to eq([])

    bitmask = TestBitmask4.enum_type(:bitmask_type1)
    expect(bitmask[1<<0]).to eq([])
    expect(bitmask[1<<5]).to eq([])
    expect(bitmask[1<<10]).to eq([])

    bitmask = TestBitmask4.enum_type(:bitmask_type2)
    expect(bitmask[1<<0]).to eq([])
    expect(bitmask[1<<5]).to eq([])
    expect(bitmask[1<<8]).to eq([])
    expect(bitmask[1<<13]).to eq([])
    expect(bitmask[1<<16]).to eq([])

    bitmask = TestBitmask4.enum_type(:bitmask_type3)
    expect(bitmask[1<<0]).to eq([])
    expect(bitmask[1<<5]).to eq([])
    expect(bitmask[1<<7]).to eq([])
    expect(bitmask[1<<13]).to eq([])
    expect(bitmask[1<<15]).to eq([])
    expect(bitmask[1<<29]).to eq([])
    expect(bitmask[1<<31]).to eq([])
    expect(bitmask[1<<41]).to eq([])
    expect(bitmask[1<<43]).to eq([])

    bitmask = TestBitmask4.enum_type(:bitmask_type4)
    expect(bitmask[1<<0]).to eq([])
    expect(bitmask[1<<5]).to eq([])
    expect(bitmask[1<<10]).to eq([])

    bitmask = TestBitmask4.enum_type(:bitmask_type5)
    expect(bitmask[1<<0]).to eq([])
    expect(bitmask[1<<5]).to eq([])
    expect(bitmask[1<<8]).to eq([])
    expect(bitmask[1<<13]).to eq([])
    expect(bitmask[1<<16]).to eq([])

    bitmask = TestBitmask4.enum_type(:bitmask_type6)
    expect(bitmask[1<<0]).to eq([])
    expect(bitmask[1<<5]).to eq([])
    expect(bitmask[1<<7]).to eq([])
    expect(bitmask[1<<13]).to eq([])
    expect(bitmask[1<<15]).to eq([])
    expect(bitmask[1<<29]).to eq([])
    expect(bitmask[1<<31]).to eq([])
    expect(bitmask[1<<41]).to eq([])
    expect(bitmask[1<<43]).to eq([])
  end

  it "duplicate bitmask keys rejected" do
    expect do
      Module.new do
        extend FFI::Library
        bitmask [ :a, 2, :b, 5, :a, 0 ]
      end
    end.to raise_error(ArgumentError, /duplicate/)
    expect do
      Module.new do
        extend FFI::Library
        bitmask FFI::Type::UINT64, [ :a, 2, :b, 5, :a, 0 ]
      end
    end.to raise_error(ArgumentError, /duplicate/)
  end
end
