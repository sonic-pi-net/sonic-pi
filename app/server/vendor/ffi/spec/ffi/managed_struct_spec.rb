#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

describe "Managed Struct" do
  include FFI
  module ManagedStructTestLib
    extend FFI::Library
    ffi_lib TestLibrary::PATH
    attach_function :ptr_from_address, [ FFI::Platform::ADDRESS_SIZE == 32 ? :uint : :ulong_long ], :pointer
  end

  it "should raise an error if release() is not defined" do
    class NoRelease < FFI::ManagedStruct ; layout :i, :int; end
    expect { NoRelease.new(ManagedStructTestLib.ptr_from_address(0x12345678)) }.to raise_error(NoMethodError)
  end

  it "should be the right class" do
    class WhatClassAmI < FFI::ManagedStruct
      layout :i, :int
      def self.release
      end
    end    

    expect(WhatClassAmI.new(ManagedStructTestLib.ptr_from_address(0x12345678)).class).to eq(WhatClassAmI)
  end

  it "should build with self reference" do
    class ClassWithSelfRef < FFI::ManagedStruct
      layout :data, self.ptr
      def self.release
      end
    end

    expect(ClassWithSelfRef.new(ManagedStructTestLib.ptr_from_address(0x12345678)).class).to eq(ClassWithSelfRef)
  end

  it "should release memory properly" do
    class PleaseReleaseMe < FFI::ManagedStruct
      layout :i, :int
      @@count = 0
      def self.release
        @@count += 1
      end
      def self.wait_gc(count)
        loop = 5
        while loop > 0 && @@count < count
          loop -= 1
          TestLibrary.force_gc
          sleep 0.05 if @@count < count
        end
      end
    end

    loop_count = 30
    wiggle_room = 5

    expect(PleaseReleaseMe).to receive(:release).at_least(loop_count-wiggle_room).times
    loop_count.times do
      PleaseReleaseMe.new(ManagedStructTestLib.ptr_from_address(0x12345678))
    end
    PleaseReleaseMe.wait_gc loop_count
  end
end
