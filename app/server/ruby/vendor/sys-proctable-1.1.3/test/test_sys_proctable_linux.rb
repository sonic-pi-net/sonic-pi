#######################################################################
# test_sys_proctable_linux.rb
#
# Test suite for the Linux version of the sys-proctable library. You
# should run these tests via the 'rake test' task.
#######################################################################
require 'test-unit'
require 'sys/proctable'
require 'test/test_sys_proctable_all'
include Sys

class TC_ProcTable_Linux < Test::Unit::TestCase
  def self.startup
    @@fields = %w/
      cmdline cwd exe pid name uid euid gid egid comm state ppid pgrp
      session tty_num tpgid flags minflt cminflt majflt cmajflt utime
      stime cutime cstime priority nice itrealvalue starttime vsize
      rss rlim startcode endcode startstack kstkesp kstkeip signal blocked
      sigignore sigcatch wchan nswap cnswap exit_signal processor environ
      pctcpu pctmem nlwp cgroup smaps
      /
  end

  def setup
    @ptable = ProcTable.ps.last
  end

  def test_cmdline
    assert_respond_to(@ptable, :cmdline)
    assert_kind_of(String, @ptable.cmdline)
  end

  def test_cwd
    assert_respond_to(@ptable, :cwd)
    assert_true([NilClass, String].include?(@ptable.cwd.class))
  end

  def test_environ
    assert_respond_to(@ptable, :environ)
    assert_kind_of(Hash, @ptable.environ)
  end

  def test_exe
    assert_respond_to(@ptable, :exe)
    assert_kind_of([NilClass, String], @ptable.exe)
  end

  def test_fd
    assert_respond_to(@ptable, :fd)
    assert_kind_of(Hash, @ptable.fd)
  end

  def test_root
    assert_respond_to(@ptable, :root)
    assert_kind_of([NilClass, String], @ptable.root)
  end

  def test_pid
    assert_respond_to(@ptable, :pid)
    assert_kind_of(Fixnum, @ptable.pid)
  end

  def test_comm
    assert_respond_to(@ptable, :comm)
    assert_kind_of(String, @ptable.comm)
  end

  def test_state
    assert_respond_to(@ptable, :state)
    assert_kind_of(String, @ptable.state)
  end

  def test_ppid
    assert_respond_to(@ptable, :ppid)
    assert_kind_of(Fixnum, @ptable.ppid)
  end

  def test_pgrp
    assert_respond_to(@ptable, :pgrp)
    assert_kind_of(Fixnum, @ptable.pgrp)
  end

  def test_session
    assert_respond_to(@ptable, :session)
    assert_kind_of(Fixnum, @ptable.session)
  end

  def test_tty_nr
    assert_respond_to(@ptable, :tty_nr)
    assert_kind_of(Fixnum, @ptable.tty_nr)
  end

  def test_tpgid
    assert_respond_to(@ptable, :tpgid)
    assert_kind_of(Fixnum, @ptable.tpgid)
  end

  def test_flags
    assert_respond_to(@ptable, :flags)
    assert_kind_of(Numeric, @ptable.flags)
  end

  def test_minflt
    assert_respond_to(@ptable, :minflt)
    assert_kind_of(Fixnum, @ptable.minflt)
  end

  def test_cminflt
    assert_respond_to(@ptable, :cminflt)
    assert_kind_of(Fixnum, @ptable.cminflt)
  end

  def test_majflt
    assert_respond_to(@ptable, :majflt)
    assert_kind_of(Fixnum, @ptable.majflt)
  end

  def test_cmajflt
    assert_respond_to(@ptable, :cmajflt)
    assert_kind_of(Fixnum, @ptable.cmajflt)
  end

  def test_utime
    assert_respond_to(@ptable, :utime)
    assert_kind_of(Fixnum, @ptable.utime)
  end

  def test_stime
    assert_respond_to(@ptable, :stime)
    assert_kind_of(Fixnum, @ptable.stime)
  end

  def test_cutime
    assert_respond_to(@ptable, :cutime)
    assert_kind_of(Fixnum, @ptable.cutime)
  end

  def test_cstime
    assert_respond_to(@ptable, :cstime)
    assert_kind_of(Fixnum, @ptable.cstime)
  end

  def test_priority
    assert_respond_to(@ptable, :priority)
    assert_kind_of(Fixnum, @ptable.priority)
  end

  def test_nice
    assert_respond_to(@ptable, :nice)
    assert_kind_of(Fixnum, @ptable.nice)
  end

  def test_itrealvalue
    assert_respond_to(@ptable, :itrealvalue)
    assert_kind_of(Fixnum, @ptable.itrealvalue)
  end

  def test_starttime
    assert_respond_to(@ptable, :starttime)
    assert_kind_of(Fixnum, @ptable.starttime)
  end

  def test_vsize
    assert_respond_to(@ptable, :vsize)
    assert_kind_of(Fixnum, @ptable.vsize)
  end

  def test_rss
    assert_respond_to(@ptable, :rss)
    assert_kind_of(Fixnum, @ptable.rss)
  end

  def test_rlim
    assert_respond_to(@ptable, :rlim)
    assert_kind_of(Integer, @ptable.rlim)
  end

  def test_startcode
    assert_respond_to(@ptable, :startcode)
    assert_kind_of(Fixnum, @ptable.startcode)
  end

  def test_endcode
    assert_respond_to(@ptable, :endcode)
    assert_kind_of(Fixnum, @ptable.endcode)
  end

  def test_startstack
    assert_respond_to(@ptable, :startstack)
    assert_kind_of(Integer, @ptable.startstack)
  end

  def test_kstkesp
    assert_respond_to(@ptable, :kstkesp)
    assert_kind_of(Integer, @ptable.kstkesp)
  end

  def test_kstkeip
    assert_respond_to(@ptable, :kstkeip)
    assert_kind_of(Integer, @ptable.kstkeip)
  end

  def test_signal
    assert_respond_to(@ptable, :signal)
    assert_kind_of(Integer, @ptable.signal)
  end

  def test_blocked
    assert_respond_to(@ptable, :blocked)
    assert_kind_of(Fixnum, @ptable.blocked)
  end

  def test_sigignore
    assert_respond_to(@ptable, :sigignore)
    assert_kind_of(Numeric, @ptable.sigignore)
  end

  def test_sigcatch
    assert_respond_to(@ptable, :sigcatch)
    assert_kind_of(Numeric, @ptable.sigcatch)
  end

  def test_wchan
    assert_respond_to(@ptable, :wchan)
    assert_kind_of(Numeric, @ptable.wchan)
  end

  def test_nswap
    assert_respond_to(@ptable, :nswap)
    assert_kind_of(Numeric, @ptable.nswap)
  end

  def test_cnswap
    assert_respond_to(@ptable, :cnswap)
    assert_kind_of(Numeric, @ptable.cnswap)
  end

  def test_exit_signal
    assert_respond_to(@ptable, :exit_signal)
    assert_kind_of(Numeric, @ptable.exit_signal)
  end

  def test_processor
    assert_respond_to(@ptable, :processor)
    assert_kind_of(Fixnum, @ptable.processor)
  end

  def test_rt_priority
    assert_respond_to(@ptable, :rt_priority)
    assert_kind_of(Fixnum, @ptable.rt_priority)
  end

  def test_policy
    assert_respond_to(@ptable, :policy)
    assert_kind_of(Fixnum, @ptable.policy)
  end

  def test_name
    assert_respond_to(@ptable, :name)
    assert_kind_of(String, @ptable.name)
  end

  def test_uid
    assert_respond_to(@ptable, :uid)
    assert_kind_of(Fixnum, @ptable.uid)
  end

  def test_euid
    assert_respond_to(@ptable, :euid)
    assert_kind_of(Fixnum, @ptable.euid)
  end

  def test_gid
    assert_respond_to(@ptable, :gid)
    assert_kind_of(Fixnum, @ptable.gid)
  end

  def test_egid
    assert_respond_to(@ptable, :egid)
    assert_kind_of(Fixnum, @ptable.egid)
  end

  def test_pctmem
    assert_respond_to(@ptable, :pctmem)
    assert_kind_of(Float, @ptable.pctmem)
  end

  def test_pctcpu
    assert_respond_to(@ptable, :pctcpu)
    assert_kind_of(Float, @ptable.pctcpu)
  end

  def test_nlwp
    assert_respond_to(@ptable, :nlwp)
    assert_kind_of(Fixnum, @ptable.nlwp)
  end

  def test_cgroup
    assert_respond_to(@ptable, :cgroup)
    assert_kind_of(Array, @ptable.cgroup)
    assert_kind_of(ProcTable::CgroupEntry, @ptable.cgroup.first)
  end

  def test_smaps
    assert_respond_to(@ptable, :smaps)
    assert_kind_of(ProcTable::Smaps, @ptable.smaps)
  end

  def teardown
    @ptable = nil
  end
end
