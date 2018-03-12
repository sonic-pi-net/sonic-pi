################################################################
# test_sys_proctable_freebsd.rb
#
# Test suite for FreeBSD for the sys-proctable library.
# You should run these tests via 'rake test'.
################################################################
require 'test-unit'
require 'sys/proctable'
require 'test/test_sys_proctable_all'
include Sys

class TC_Sys_ProcTable_FreeBSD < Test::Unit::TestCase
  def self.startup
    @@fields = %w[
      pid ppid pgid tpgid sid tsid jobc uid ruid rgid
      ngroups groups size rssize swrss tsize dsize ssize
      xstat acflag pctcpu estcpu slptime swtime runtime start
      flag state nice lock rqindex oncpu lastcpu wmesg login
      lockname comm ttynum ttydev jid priority usrpri cmdline
      utime stime maxrss ixrss idrss isrss minflt majflt nswap
      inblock oublock msgsnd msgrcv nsignals nvcsw nivcsw
    ]
  end

  def setup
    @ptable = ProcTable.ps(Process.pid)
  end

  def test_fields
    assert_respond_to(ProcTable, :fields)
    assert_kind_of(Array, ProcTable.fields)
    assert_equal(@@fields, ProcTable.fields)
  end

  def test_pid
    assert_respond_to(@ptable, :pid)
    assert_kind_of(Fixnum, @ptable.pid)
  end

  def test_ppid
    assert_respond_to(@ptable, :ppid)
    assert_kind_of(Fixnum, @ptable.ppid)
  end

  def test_pgid
    assert_respond_to(@ptable, :pgid)
    assert_kind_of(Fixnum, @ptable.pgid)
  end

  def test_ruid
    assert_respond_to(@ptable, :ruid)
    assert_kind_of(Fixnum, @ptable.ruid)
  end

  def test_rgid
    assert_respond_to(@ptable, :rgid)
    assert_kind_of(Fixnum, @ptable.rgid)
  end

  def test_comm
    assert_respond_to(@ptable, :comm)
    assert_kind_of(String, @ptable.comm)
  end

  def test_state
    assert_respond_to(@ptable, :state)
    assert_kind_of(String, @ptable.state)
  end

  def test_pctcpu
    assert_respond_to(@ptable, :pctcpu)
    assert_kind_of(Float, @ptable.pctcpu)
  end

  def test_oncpu
    assert_respond_to(@ptable, :oncpu)
    assert_kind_of(Fixnum, @ptable.oncpu)
  end

  def test_ttynum
    assert_respond_to(@ptable, :ttynum)
    assert_kind_of(Fixnum, @ptable.ttynum)
  end

  def test_ttydev
    assert_respond_to(@ptable, :ttydev)
    assert_kind_of(String, @ptable.ttydev)
  end

  def test_wmesg
    assert_respond_to(@ptable, :wmesg)
    assert_kind_of(String, @ptable.wmesg)
  end

  def test_runtime
    assert_respond_to(@ptable, :runtime)
    assert_kind_of(Fixnum, @ptable.runtime)
  end

  def test_priority
    assert_respond_to(@ptable, :priority)
    assert_kind_of(Fixnum, @ptable.priority)
  end

  def test_usrpri
    assert_respond_to(@ptable, :usrpri)
    assert_kind_of(Fixnum, @ptable.usrpri)
  end

  def test_nice
    assert_respond_to(@ptable, :nice)
    assert_kind_of(Fixnum, @ptable.nice)
  end

  def test_cmdline
    assert_respond_to(@ptable, :cmdline)
    assert_kind_of(String, @ptable.cmdline)
  end

  def test_start
    assert_respond_to(@ptable, :start)
    assert_kind_of(Time, @ptable.start)
  end

  def test_maxrss
    assert_respond_to(@ptable, :maxrss)
    assert_kind_of(Fixnum, @ptable.maxrss)
  end

  def test_ixrss
    assert_respond_to(@ptable, :ixrss)
    assert_kind_of(Fixnum, @ptable.ixrss)
  end

  def test_idrss
    assert_respond_to(@ptable, :idrss)
    assert_kind_of(Fixnum, @ptable.idrss)
  end

  def test_isrss
    assert_respond_to(@ptable, :isrss)
    assert_kind_of(Fixnum, @ptable.isrss)
  end

  def test_minflt
    assert_respond_to(@ptable, :minflt)
    assert_kind_of(Fixnum, @ptable.minflt)
  end

  def test_majflt
    assert_respond_to(@ptable, :majflt)
    assert_kind_of(Fixnum, @ptable.majflt)
  end

  def test_nswap
    assert_respond_to(@ptable, :nswap)
    assert_kind_of(Fixnum, @ptable.nswap)
  end

  def test_inblock
    assert_respond_to(@ptable, :inblock)
    assert_kind_of(Fixnum, @ptable.inblock)
  end

  def test_oublock
    assert_respond_to(@ptable, :oublock)
    assert_kind_of(Fixnum, @ptable.oublock)
  end

  def test_msgsnd
    assert_respond_to(@ptable, :msgsnd)
    assert_kind_of(Fixnum, @ptable.msgsnd)
  end

  def test_msgrcv
    assert_respond_to(@ptable, :msgrcv)
    assert_kind_of(Fixnum, @ptable.msgrcv)
  end

  def test_nsignals
    assert_respond_to(@ptable, :nsignals)
    assert_kind_of(Fixnum, @ptable.nsignals)
  end

  def test_nvcsw
    assert_respond_to(@ptable, :nvcsw)
    assert_kind_of(Fixnum, @ptable.nvcsw)
  end

  def test_nivcsw
    assert_respond_to(@ptable, :nivcsw)
    assert_kind_of(Fixnum, @ptable.nivcsw)
  end

  def test_utime
    assert_respond_to(@ptable, :utime)
    assert_kind_of(Fixnum, @ptable.utime)
  end

  def test_stime
    assert_respond_to(@ptable, :stime)
    assert_kind_of(Fixnum, @ptable.stime)
  end

  def teardown
    @ptable = nil
  end

  def self.shutdown
    @@fields = nil
  end
end
