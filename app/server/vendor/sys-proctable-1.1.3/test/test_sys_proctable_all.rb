#######################################################################
# test_sys_proctable_all.rb
#
# Test suite for methods common to all platforms. Generally speaking
# you should run this test case using the 'rake test' task.
#######################################################################
require 'test-unit'
require 'sys/proctable'
require 'test/test_sys_top'
include Sys

class TC_ProcTable_All < Test::Unit::TestCase
  def self.startup
    @@windows = File::ALT_SEPARATOR
  end

  def setup
    @pid = Process.pid
  end

  test "version is set to expected value" do
    assert_equal('1.1.3', ProcTable::VERSION)
  end

  test "fields basic functionality" do
    assert_respond_to(ProcTable, :fields)
    assert_nothing_raised{ ProcTable.fields }
  end

  test "fields returns expected type" do
    assert_kind_of(Array, ProcTable.fields)
    assert_kind_of(String, ProcTable.fields.first)
  end

  test "ps basic functionality" do
    assert_respond_to(ProcTable, :ps)
    assert_nothing_raised{ ProcTable.ps }
    assert_nothing_raised{ ProcTable.ps{} }
  end

  test "ps accepts an optional pid" do
    assert_nothing_raised{ ProcTable.ps(0) }
  end

  test "ps with explicit nil works as expected" do
    assert_nothing_raised{ ProcTable.ps(nil) }
    assert_kind_of(Array, ProcTable.ps(nil))
  end

  test "ps returns expected results" do
    assert_kind_of(Array, ProcTable.ps)
    assert_kind_of(Struct::ProcTableStruct, ProcTable.ps(@pid))
  end

  test "ps returns nil if process does not exist" do
    assert_nil(ProcTable.ps(999999999))
    assert_nil(ProcTable.ps(999999999){})
    assert_nil(ProcTable.ps{})
  end

  test "structs returned by ps are frozen" do
    assert_true(ProcTable.ps.first.frozen?)
  end

  test "ps accepts numeric arguments only" do
    assert_raises(TypeError){ ProcTable.ps('vim') }
  end

  test "ps accepts a maximum of one argument on Unix platforms" do
    omit_if(@@windows, 'ArgumentError check skipped on MS Windows')
    assert_raises(ArgumentError){ ProcTable.ps(0, 'localhost') }
  end

  test "traditional constructor is disabled" do
    assert_raise(NoMethodError){ Sys::ProcTable.new }
  end

  test "custom error class is defined" do
    assert_not_nil(Sys::ProcTable::Error)
    assert_kind_of(StandardError, Sys::ProcTable::Error.new)
  end

  test "ps works within a thread" do
    assert_nothing_raised{
      Thread.new do
        Sys::ProcTable.ps
      end.value
    }
  end

  def teardown
    @pid  = nil
  end

  def self.teardown
    @@windows = nil
  end
end
