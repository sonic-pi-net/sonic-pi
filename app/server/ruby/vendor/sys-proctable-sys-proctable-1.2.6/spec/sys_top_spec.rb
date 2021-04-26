##############################################################################
# sys_top_spec.rb
#
# Test suite for the sys-top library that is included with this distribution.
##############################################################################
require 'rspec'
require 'sys/top'

describe Sys::Top do
  context "constants" do
    it "sets the version to the expected value" do
      expect(Sys::Top::VERSION).to eql('1.0.5')
    end
  end

  context "top" do
    it "defines a top method" do
      expect(described_class).to respond_to(:top) 
    end

    it "returns an array" do
      expect(described_class.top).to be_kind_of(Array)
    end

    it "works with no arguments" do
      expect{ described_class.top }.to_not raise_error
    end

    it "accepts a maximum of two arguments" do
      expect{ described_class.top(1, 'foo', 2) }.to raise_error(ArgumentError)
    end

    it "accepts optional arguments" do
      expect{ described_class.top(5) }.to_not raise_error
      expect{ described_class.top(5, 'cmdline') }.to_not raise_error
    end

    it "returns the expected results with no arguments" do
      expect(described_class.top.size).to eql(10)
      expect(described_class.top.first).to be_kind_of(Struct::ProcTableStruct)
    end

    it "returns the expected result with a size argument" do
      expect(described_class.top(5).size).to eql(5)
    end

    it "returns the expected result with a size and sort_by argument" do
      expect(described_class.top(5, :cmdline).size).to eql(5)
    end
  end
end
