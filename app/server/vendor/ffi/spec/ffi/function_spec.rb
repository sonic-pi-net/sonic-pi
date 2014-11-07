#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

describe FFI::Function do
  module LibTest
    extend FFI::Library
    ffi_lib TestLibrary::PATH
    attach_function :testFunctionAdd, [:int, :int, :pointer], :int
  end
  before do
    @libtest = FFI::DynamicLibrary.open(TestLibrary::PATH, 
                                        FFI::DynamicLibrary::RTLD_LAZY | FFI::DynamicLibrary::RTLD_GLOBAL)
  end

  it 'is initialized with a signature and a block' do
    fn = FFI::Function.new(:int, []) { 5 }
    expect(fn.call).to eql 5
  end

  it 'raises an error when passing a wrong signature' do
    expect { FFI::Function.new([], :int).new { } }.to raise_error TypeError 
  end

  it 'returns a native pointer' do
    expect(FFI::Function.new(:int, []) { }).to be_a_kind_of FFI::Pointer
  end

  it 'can be used as callback from C passing to it a block' do
    function_add = FFI::Function.new(:int, [:int, :int]) { |a, b| a + b }
    expect(LibTest.testFunctionAdd(10, 10, function_add)).to eq(20)
  end

  it 'can be used as callback from C passing to it a Proc object' do
    function_add = FFI::Function.new(:int, [:int, :int], Proc.new { |a, b| a + b })
    expect(LibTest.testFunctionAdd(10, 10, function_add)).to eq(20)
  end

  it 'can be used to wrap an existing function pointer' do
    expect(FFI::Function.new(:int, [:int, :int], @libtest.find_function('testAdd')).call(10, 10)).to eq(20)
  end

  it 'can be attached to a module' do
    module Foo; end
    fp = FFI::Function.new(:int, [:int, :int], @libtest.find_function('testAdd'))
    fp.attach(Foo, 'add')
    expect(Foo.add(10, 10)).to eq(20)
  end

  it 'can be used to extend an object' do
    fp = FFI::Function.new(:int, [:int, :int], @libtest.find_function('testAdd'))
    foo = Object.new
    class << foo
      def singleton_class
        class << self; self; end
      end
    end
    fp.attach(foo.singleton_class, 'add')
    expect(foo.add(10, 10)).to eq(20)
  end

  it 'can wrap a blocking function' do
    fp = FFI::Function.new(:void, [ :int ], @libtest.find_function('testBlocking'), :blocking => true)
    time = Time.now
    threads = []
    threads << Thread.new { fp.call(2) }
    threads << Thread.new(time) { expect(Time.now - time).to be < 1 }
    threads.each { |t| t.join }
  end

  it 'autorelease flag is set to true by default' do
    fp = FFI::Function.new(:int, [:int, :int], @libtest.find_function('testAdd'))
    expect(fp.autorelease?).to be true
  end

  it 'can explicity free itself' do
    fp = FFI::Function.new(:int, []) { }
    fp.free
    expect { fp.free }.to raise_error RuntimeError
  end

  it 'can\'t explicity free itself if not previously allocated' do
    fp = FFI::Function.new(:int, [:int, :int], @libtest.find_function('testAdd'))
    expect { fp.free }.to raise_error RuntimeError
  end
end
