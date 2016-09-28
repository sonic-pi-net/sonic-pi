########################################################################
# test_sys_proctable_darwin.rb
#
# Test suite for the Darwin version of the sys-proctable library. You
# should run these tests via the 'rake test' task.
########################################################################
require 'test-unit'
require 'sys/proctable'
require 'test/test_sys_proctable_all'

class TC_ProcTable_Darwin < Test::Unit::TestCase
  def self.startup
    @@fields = %w[
      flags status xstatus pid ppid uid gid ruid rgid svuid svgid rfu1 comm
      name nfiles pgid pjobc tdev tpgid nice start_tvsec start_tvusec
      virtual_size resident_size total_user total_system threads_user
      threads_system policy faults pageins cow_faults messages_sent
      messages_received syscalls_mach syscalls_unix csw threadnum numrunning
      priority cmdline exe environ threadinfo
    ]

    @@pid = fork do
      exec('env', '-i', 'A=B', 'Z=', 'sleep', '60')
    end

    sleep 1 # wait to make sure env is replaced by sleep
  end

  def setup
    @ptable = Sys::ProcTable.ps(@@pid)
  end

  test "fields basic functionality" do
    assert_respond_to(Sys::ProcTable, :fields)
    assert_kind_of(Array, Sys::ProcTable.fields)
    assert_equal(@@fields, Sys::ProcTable.fields)
  end

  test "pid struct member is defined and returns expected value" do
    assert_respond_to(@ptable, :pid)
    assert_kind_of(Fixnum, @ptable.pid)
    assert_equal(@ptable.pid, @@pid)
  end

  test "ppid struct member is defined and returns expected value" do
    assert_respond_to(@ptable, :ppid)
    assert_kind_of(Fixnum, @ptable.ppid)
    assert_equal(Process.pid, @ptable.ppid)
  end

  test "pgid struct member is defined and returns expected value" do
    assert_respond_to(@ptable, :pgid)
    assert_kind_of(Fixnum, @ptable.pgid)
    assert_equal(Process.getpgrp, @ptable.pgid)
  end

  test "ruid struct member is defined and returns expected value" do
    assert_respond_to(@ptable, :ruid)
    assert_kind_of(Fixnum, @ptable.ruid)
    assert_equal(Process.uid, @ptable.ruid)
  end

  test "rgid struct member is defined and returns expected value" do
    assert_respond_to(@ptable, :rgid)
    assert_kind_of(Fixnum, @ptable.rgid)
    assert_equal(Process.gid, @ptable.rgid)
  end

  test "svuid struct member is defined and returns expected value" do
    assert_respond_to(@ptable, :svuid)
    assert_kind_of(Fixnum, @ptable.svuid)
    assert_equal(Process.uid, @ptable.svuid) # not valid for all processes
  end

  test "svgid struct member is defined and returns expected value" do
    assert_respond_to(@ptable, :svgid)
    assert_kind_of(Fixnum, @ptable.svgid)
    assert_equal(Process.gid, @ptable.svgid) # not valid for all processes
  end

  test "comm struct member is defined and returns expected value" do
    assert_respond_to(@ptable, :comm)
    assert_kind_of(String, @ptable.comm)
    assert_equal('sleep', @ptable.comm)
  end

  test "cmdline struct member is defined and returns expected value" do
    assert_respond_to(@ptable, :cmdline)
    assert_kind_of(String, @ptable.cmdline)
    assert_equal('sleep 60', @ptable.cmdline)
  end

  test "exe struct member is defined and returns expected value" do
    assert_respond_to(@ptable, :exe)
    assert_kind_of(String, @ptable.exe)
    assert_equal(`which sleep`.chomp, @ptable.exe)
  end

  test "environ struct member is defined and returns expected value" do
    assert_respond_to(@ptable, :environ)
    assert_kind_of(Hash, @ptable.environ)
    assert_equal({'A' => 'B', 'Z' => nil}, @ptable.environ)
  end

  def teardown
    @ptable = nil
  end

  def self.shutdown
    @@fields = nil
    Process.kill('TERM', @@pid)
  end
end
