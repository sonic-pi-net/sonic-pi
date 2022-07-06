#######################################################################
# sys_proctable_all_spec.rb
#
# Test suite for methods common to all platforms. Generally speaking
# you should run these specs using the 'rake test' task.
#######################################################################
require 'rspec'
require 'sys/proctable'
require_relative 'sys_top_spec'

describe Sys::ProcTable do
  let(:windows) { File::ALT_SEPARATOR }

  before(:all) do
    @pid = Process.pid
  end

  it "has a VERSION constant set to the expected value" do
    expect(Sys::ProcTable::VERSION).to eql('1.2.6')
    expect(Sys::ProcTable::VERSION).to be_frozen
  end

  it "defines a custom error class" do
    expect{ Sys::ProcTable::Error }.to_not raise_error
    expect(Sys::ProcTable::Error.new).to be_kind_of(StandardError)
  end

  context "fields" do
    it "has a fields singleton method" do
      expect(described_class).to respond_to(:fields)
    end

    it "returns the expected data type for the fields singleton method" do
      expect(described_class.fields).to be_kind_of(Array)
      expect(described_class.fields.first).to be_kind_of(String)
    end
  end

  context "ps" do
    it "defines a ps singleton method" do
      expect(described_class).to respond_to(:ps)
    end

    it "allows a pid option as an argument" do
      expect{ described_class.ps(pid: 0) }.to_not raise_error
    end

    it "allows the pid to be nil" do
      expect{ described_class.ps(pid: nil) }.to_not raise_error
      expect(described_class.ps(pid: nil)).to be_kind_of(Array)
    end

    it "returns expected results with no arguments" do
      expect(described_class.ps).to be_kind_of(Array)
    end

    it "returns expected results with a pid argument" do
      expect(described_class.ps(pid: @pid)).to be_kind_of(Struct::ProcTableStruct)
    end

    it "returns nil if the process does not exist" do
      expect(described_class.ps(pid: 999999999)).to be_nil
    end

    it "returns nil in block form whether or not a pid was provided" do
      expect(described_class.ps{}).to be_nil
      expect(described_class.ps(pid: 999999999){}).to be_nil
    end

    it "returns frozen structs" do
      expect(described_class.ps.first.frozen?).to eql(true)
    end

    it "expects a numeric pid argument if present" do
      expect{ described_class.ps(pid: 'vim') }.to raise_error(TypeError)
    end

    it "accepts keyword arguments only" do
      expect{ described_class.ps(0, 'localhost') }.to raise_error(ArgumentError)
    end

    it "disables the traditional constructor" do
      expect{ described_class.new }.to raise_error(NoMethodError)
    end

    it "works within a thread" do
      expect{ Thread.new{ described_class.ps }.value }.to_not raise_error
    end
  end
end
