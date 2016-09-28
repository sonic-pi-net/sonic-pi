#######################################################################
# test_sys_proctable_aix.rb
#
# Test suite for the AIX version of the sys-proctable library. You
# should run these tests via the 'rake test' task.
#######################################################################
require 'test-unit'
require 'sys/proctable'
require 'test/test_sys_proctable_all'
include Sys

class TC_ProcTable_AIX < Test::Unit::TestCase
  def self.startup
    File.open('aix-child.rb', 'w') do |out|
      out.puts 'trap("HUP") { exit }'
      out.puts 'trap("TERM") { exit }'
      out.puts 'sleep(60)'
    end

    @@myenv = ENV.to_hash
    @@p1args = %w/aix-child.rb testing how well this works 1/
    @@p2args = %w/aix-child.rb testing how well this works 2/

    @@pid1 = fork do
      exec('ruby', *@@p1args)
    end

    @@pid2 = fork do
      exec('ruby', *@@p2args)
    end

    sleep(2)  # wait to make sure the above execs have completed in children

    @@fields = %w/
        addr argc argv bindpro cid clname cmd_args cmdline cwd egid environ
        envp euid fd flag flag2 fname gid lwpid map nice nlwp onpro pgid pid
        policy ppid pri psargs ptid rssize sid size sname start state time
        ttydev uid wchan wtype s_ttydev
    /

    @@map_fields = %w/
        size vaddr mapname off mflags pathoff alias gp s_mflags path
    /

    @@p1info = ProcTable.ps(@@pid1)
    @@p2info = ProcTable.ps.select { |e| e.pid == @@pid2 }

    if @@p2info.size == 1
      @@p2info = @@p2info[0] 
    else
      $stderr.puts "expected a single process, have #{@@p2info.size}"
      exit 1
    end
  end

  def test_expected_fields
    assert_respond_to(ProcTable, :fields)
    assert_kind_of(Array, ProcTable.fields)
    assert_equal(@@fields.sort, ProcTable.fields.sort)
  end

  def test_flag
    assert_respond_to(@@p1info, :flag)
    assert_kind_of(Fixnum, @@p1info.flag)
  end

  def test_flag2
    assert_respond_to(@@p1info, :flag2)
    assert_kind_of(Fixnum, @@p1info.flag2)
  end

  def test_nlwp
    assert_respond_to(@@p1info, :nlwp)
    assert_kind_of(Fixnum, @@p1info.nlwp)
  end

  def test_uid
    assert_respond_to(@@p1info, :uid)
    assert_kind_of(Integer, @@p1info.uid)
    assert_equal(Process.uid, @@p1info.uid)
  end

  def test_euid
    assert_respond_to(@@p1info, :euid)
    assert_kind_of(Integer, @@p1info.euid)
    assert_equal(Process.euid, @@p1info.euid)
  end

  def test_gid
    assert_respond_to(@@p1info, :gid)
    assert_kind_of(Integer, @@p1info.gid)
    assert_equal(Process.gid, @@p1info.gid)
  end

  def test_egid
    assert_respond_to(@@p1info, :egid)
    assert_kind_of(Integer, @@p1info.egid)
    assert_equal(Process.egid, @@p1info.egid)
  end

  def test_pid
    assert_respond_to(@@p1info, :pid)
    assert_kind_of(Integer, @@p1info.pid)
    assert_equal(@@pid1, @@p1info.pid)
  end

  def test_ppid
    assert_respond_to(@@p1info, :ppid)
    assert_kind_of(Integer, @@p1info.ppid)
    assert_equal(Process.pid, @@p1info.ppid)
  end

  def test_pgid
    assert_respond_to(@@p1info, :pgid)
    assert_kind_of(Integer, @@p1info.pgid)
    assert_equal(Process.getpgrp, @@p1info.pgid)
  end

  def test_sid
    assert_respond_to(@@p1info, :sid)
    assert_kind_of(Integer, @@p1info.sid)
  end

  def test_ttydev
    assert_respond_to(@@p1info, :ttydev)
    assert_kind_of(Integer, @@p1info.ttydev)
  end

  def test_addr
    assert_respond_to(@@p1info, :addr)
    assert_kind_of(Integer, @@p1info.addr)
  end

  def test_size
    assert_respond_to(@@p1info, :size)
    assert_kind_of(Integer, @@p1info.size)
  end

  def test_rssize
    assert_respond_to(@@p1info, :rssize)
    assert_kind_of(Integer, @@p1info.rssize)
  end

  def test_start
    assert_respond_to(@@p1info, :start)
    assert_kind_of(Time, @@p1info.start)
  end

  def test_time
    assert_respond_to(@@p1info, :time)
    assert_kind_of(Integer, @@p1info.time)
  end

  def test_cid
    assert_respond_to(@@p1info, :cid)
    assert_kind_of(Fixnum, @@p1info.cid)
  end

  def test_argc
    assert_respond_to(@@p1info, :argc)
    assert_kind_of(Fixnum, @@p1info.argc)
    assert_equal(@@p1args.size + 1, @@p1info.argc)
  end

  def test_argv
    assert_respond_to(@@p1info, :argv)
    assert_kind_of(Integer, @@p1info.argv)
  end

  def test_envp
    assert_respond_to(@@p1info, :envp)
    assert_kind_of(Integer, @@p1info.envp)
  end

  def test_fname
    assert_respond_to(@@p1info, :fname)
    assert_kind_of(String, @@p1info.fname)
  end

  def test_psargs
    assert_respond_to(@@p1info, :psargs)
    assert_kind_of(String, @@p1info.psargs)
  end

  def test_lwpid
    assert_respond_to(@@p1info, :lwpid)
    assert_kind_of(Integer, @@p1info.lwpid)
  end

  def test_wchan
    assert_respond_to(@@p1info, :wchan)
    assert_kind_of(Integer, @@p1info.wchan)
  end

  def test_wtype
    assert_respond_to(@@p1info, :wtype)
    assert_kind_of(Fixnum, @@p1info.wtype)
  end

  def test_state
    assert_respond_to(@@p1info, :state)
    assert_kind_of(Fixnum, @@p1info.state)
  end

  def test_sname
    assert_respond_to(@@p1info, :sname)
    assert_kind_of(String, @@p1info.sname)
  end

  def test_nice
    assert_respond_to(@@p1info, :nice)
    assert_kind_of(Fixnum, @@p1info.nice)
  end

  def test_pri
    assert_respond_to(@@p1info, :pri)
    assert_kind_of(Fixnum, @@p1info.pri)
  end

  def test_policy
    assert_respond_to(@@p1info, :policy)
    assert_kind_of(Fixnum, @@p1info.policy)
  end

  def test_clname
    assert_respond_to(@@p1info, :clname)
    assert_kind_of(String, @@p1info.clname)
  end

  def test_onpro
    assert_respond_to(@@p1info, :onpro)
    assert_kind_of(Fixnum, @@p1info.onpro)
  end

  def test_bindpro
    assert_respond_to(@@p1info, :bindpro)
    assert_kind_of(Fixnum, @@p1info.bindpro)
  end

  def test_ptid
    assert_respond_to(@@p1info, :ptid)
    assert_kind_of(Fixnum, @@p1info.ptid)
  end

  def test_comm
    assert_respond_to(@@p1info, :comm)
    assert_kind_of(String, @@p1info.comm)
  end

  def test_fd
    assert_respond_to(@@p1info, :fd)
    assert_kind_of(Array, @@p1info.fd)
  end

  def test_cmd_args
    assert_respond_to(@@p1info, :cmd_args)
    assert_kind_of(Array, @@p1info.cmd_args)
    assert_equal([ 'ruby', @@p1args ].flatten, @@p1info.cmd_args)

    assert_respond_to(@@p2info, :cmd_args)
    assert_kind_of(Array, @@p2info.cmd_args)
    assert_equal([ 'ruby', @@p2args ].flatten, @@p2info.cmd_args)
  end

  def test_environ
    assert_respond_to(@@p1info, :environ)
    assert_kind_of(Hash, @@p1info.environ)
    assert_equal(@@myenv, @@p1info.environ)

    assert_respond_to(@@p2info, :environ)
    assert_kind_of(Hash, @@p2info.environ)
    assert_equal(@@myenv, @@p2info.environ)
  end

  def test_cmdline
    assert_respond_to(@@p1info, :cmdline)
    assert_kind_of(String, @@p1info.cmdline)
  end

  def test_cwd
    assert_respond_to(@@p1info, :cwd)
    assert_kind_of([String, NilClass], @@p1info.cwd)
  end

  def test_map
    assert_respond_to(@@p1info, :map)
    assert_kind_of([Array, NilClass], @@p1info.map)
  end

  def test_map_struct
    assert_not_nil(@@p1info.map)
    assert_equal(@@map_fields.sort, @@p1info.map[0].members.sort)
  end

  def test_map_print
    assert_not_nil(@@p1info.map)

    @@p1info.map.each do |m|
      assert_kind_of(Struct::ProcTableMapStruct, m)
      assert_kind_of(Integer, m.size)
      assert_kind_of(Integer, m.vaddr)
      assert_kind_of(String, m.mapname)
      assert_kind_of(Integer, m.size)
      assert_kind_of(Integer, m.off)
      assert_kind_of(Integer, m.mflags)
      assert_kind_of(String, m.s_mflags)
      assert_kind_of(Integer, m.pathoff)
      assert_kind_of(Integer, m.alias)
      assert_kind_of(Integer, m.gp)
      assert_kind_of(String, m.path)
    end
  end

  def self.shutdown
    Process.kill('TERM', @@pid1)
    Process.kill('TERM', @@pid2)
    File.unlink('aix-child.rb') rescue nil
    @@myenv = nil
    @@pid1 = nil
    @@pid2 = nil
    @@p1info = nil
    @@p2info = nil
  end
end
