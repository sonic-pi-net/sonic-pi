#######################################################################
# test_sys_proctable_sunos.rb
#
# Test suite for sys-proctable for SunOS/Solaris. This should be run
# run via the 'rake test' task.
#######################################################################
require 'test-unit'
require 'sys/proctable'
require 'test/test_sys_proctable_all'
include Sys

class TC_ProcTable_SunOS < Test::Unit::TestCase
  def self.startup
    @@fields = %w//
  end

  def setup
    @ptable = ProcTable.ps.first
  end

  # PSINFO

  def test_flag
    assert_respond_to(@ptable, :flag)
    assert_kind_of(Integer, @ptable.flag)
  end

  def test_nlwp
    assert_respond_to(@ptable, :nlwp)
    assert_kind_of(Integer, @ptable.nlwp)
    assert_true(@ptable.nlwp >= 0)
  end

  def test_pid
    assert_respond_to(@ptable, :pid)
    assert_kind_of(Integer, @ptable.pid)
    assert_true(@ptable.pid >= 0)
  end

  def test_ppid
    assert_respond_to(@ptable, :ppid)
    assert_kind_of(Integer, @ptable.ppid)
    assert_true(@ptable.ppid >= 0)
  end

  def test_pgid
    assert_respond_to(@ptable, :pgid)
    assert_kind_of(Integer, @ptable.pgid)
    assert_true(@ptable.pgid >= 0)
  end

  def test_sid
    assert_respond_to(@ptable, :sid)
    assert_kind_of(Integer, @ptable.sid)
    assert_true(@ptable.sid >= 0)
  end

  def test_uid
    assert_respond_to(@ptable, :uid)
    assert_kind_of(Integer, @ptable.uid)
    assert_true(@ptable.uid >= 0)
  end

  def test_euid
    assert_respond_to(@ptable, :euid)
    assert_kind_of(Integer, @ptable.euid)
    assert_true(@ptable.euid >= 0)
  end

  def test_gid
    assert_respond_to(@ptable, :gid)
    assert_kind_of(Integer, @ptable.gid)
    assert_true(@ptable.gid >= 0)
  end

  def test_egid
    assert_respond_to(@ptable, :egid)
    assert_kind_of(Integer, @ptable.egid)
    assert_true(@ptable.egid >= 0)
  end

  def test_addr
    assert_respond_to(@ptable, :addr)
    assert_kind_of(Integer, @ptable.addr)
    assert_true(@ptable.addr >= 0)
  end

  def test_size
    assert_respond_to(@ptable, :size)
    assert_kind_of(Integer, @ptable.size)
    assert_true(@ptable.size >= 0)
  end

  def test_rssize
    assert_respond_to(@ptable, :rssize)
    assert_kind_of(Integer, @ptable.rssize)
    assert_true(@ptable.rssize >= 0)
  end

  def test_ttydev
    assert_respond_to(@ptable, :ttydev)
    assert_kind_of(Integer, @ptable.ttydev)
    assert_true(@ptable.ttydev >= 0 || @ptable.ttydev == -1)
  end

  def test_pctcpu
    assert_respond_to(@ptable, :pctcpu)
    assert_kind_of(Float, @ptable.pctcpu)
    assert_true(@ptable.pctcpu >= 0)
  end

  def test_pctmem
    assert_respond_to(@ptable, :pctmem)
    assert_kind_of(Float, @ptable.pctmem)
    assert_true(@ptable.pctmem >= 0)
  end

  def test_start
    assert_respond_to(@ptable, :start)
    assert_kind_of(Time, @ptable.start)
  end

  def test_time
    assert_respond_to(@ptable, :time)
    assert_kind_of(Integer, @ptable.time)
    assert_true(@ptable.time >= 0)
  end

  def test_ctime
    assert_respond_to(@ptable, :ctime)
    assert_kind_of(Integer, @ptable.ctime)
    assert_true(@ptable.ctime >= 0)
  end

  def test_fname
    assert_respond_to(@ptable, :fname)
    assert_kind_of(String, @ptable.fname)
    assert_true(@ptable.fname.size > 0)
  end

  def test_comm_alias
    assert_respond_to(@ptable, :comm)
    assert_kind_of(String, @ptable.comm)
    assert_true(@ptable.comm.size > 0)
  end

  def test_psargs
    assert_respond_to(@ptable, :psargs)
    assert_kind_of(String, @ptable.psargs)
    assert_true(@ptable.psargs.size > 0)
  end

  def test_wstat
    assert_respond_to(@ptable, :wstat)
    assert_kind_of(Integer, @ptable.wstat)
    assert_true(@ptable.wstat >= 0)
  end

  def test_argc
    assert_respond_to(@ptable, :argc)
    assert_kind_of(Integer, @ptable.argc)
    assert_true(@ptable.argc >= 0)
  end

  def test_argv
    assert_respond_to(@ptable, :argv)
    assert_kind_of(Integer, @ptable.argv)
    assert_true(@ptable.argv >= 0)
  end

  def test_envp
    assert_respond_to(@ptable, :envp)
    assert_kind_of(Integer, @ptable.envp)
    assert_true(@ptable.envp >= 0)
  end

  def test_dmodel
    assert_respond_to(@ptable, :dmodel)
    assert_kind_of(Integer, @ptable.dmodel)
    assert_true(@ptable.dmodel >= 0)
  end

  def test_taskid
    assert_respond_to(@ptable, :taskid)
    assert_kind_of(Integer, @ptable.taskid)
    assert_true(@ptable.taskid >= 0)
  end

  def test_projid
    assert_respond_to(@ptable, :projid)
    assert_kind_of(Integer, @ptable.projid)
    assert_true(@ptable.projid >= 0)
  end

  def test_nzomb
    assert_respond_to(@ptable, :nzomb)
    assert_kind_of(Integer, @ptable.nzomb)
    assert_true(@ptable.nzomb >= 0)
  end

  def test_poolid
    assert_respond_to(@ptable, :poolid)
    assert_kind_of(Integer, @ptable.poolid)
    assert_true(@ptable.poolid >= 0)
  end

  def test_zoneid
    assert_respond_to(@ptable, :zoneid)
    assert_kind_of(Integer, @ptable.zoneid)
    assert_true(@ptable.zoneid >= 0)
  end

  def test_contract
    assert_respond_to(@ptable, :contract)
    assert_kind_of(Integer, @ptable.contract)
    assert_true(@ptable.contract >= 0 || @ptable.contract == -1)
  end

  # LWPSINFO

  def test_lwpid
    assert_respond_to(@ptable, :lwpid)
    assert_kind_of(Integer, @ptable.lwpid)
    assert_true(@ptable.lwpid >= 0)
  end

  def test_wchan
    assert_respond_to(@ptable, :wchan)
    assert_kind_of(Integer, @ptable.wchan)
    assert_true(@ptable.wchan >= 0)
  end

  def test_stype
    assert_respond_to(@ptable, :stype)
    assert_kind_of(Fixnum, @ptable.stype)
    assert_true(@ptable.stype >= 0)
  end

  def test_state
    assert_respond_to(@ptable, :state)
    assert_kind_of(Fixnum, @ptable.state)
    assert_true(@ptable.state >= 0)
  end

  def test_sname
    assert_respond_to(@ptable, :sname)
    assert_kind_of(String, @ptable.sname)
    assert_true(['S', 'R', 'Z', 'T', 'I', 'O'].include?(@ptable.sname))
  end

  def test_nice
    assert_respond_to(@ptable, :nice)
    assert_kind_of(Fixnum, @ptable.nice)
    assert_true(@ptable.nice >= 0)
  end

  def test_syscall
    assert_respond_to(@ptable, :syscall)
    assert_kind_of(Fixnum, @ptable.syscall)
    assert_true(@ptable.syscall >= 0)
  end

  def test_pri
    assert_respond_to(@ptable, :pri)
    assert_kind_of(Fixnum, @ptable.pri)
    assert_true(@ptable.pri >= 0)
  end

  def test_clname
    assert_respond_to(@ptable, :clname)
    assert_kind_of(String, @ptable.clname)
    assert_true(@ptable.clname.length >= 0 && @ptable.clname.length <= 8)
  end

  def test_name
    assert_respond_to(@ptable, :name)
    assert_kind_of(String, @ptable.name)
    assert_true(@ptable.name.length >= 0 && @ptable.name.length <= 16)
  end

  def test_onpro
    assert_respond_to(@ptable, :onpro)
    assert_kind_of(Fixnum, @ptable.onpro)
    assert_true(@ptable.onpro >= 0)
  end

  def test_bindpro
    assert_respond_to(@ptable, :bindpro)
    assert_kind_of(Fixnum, @ptable.bindpro)
    assert_true(@ptable.bindpro >= 0 || @ptable.bindpro == -1)
  end

  def test_bindpset
    assert_respond_to(@ptable, :bindpset)
    assert_kind_of(Fixnum, @ptable.bindpset)
    assert_true(@ptable.bindpset >= 0 || @ptable.bindpset == -1)
  end

  def teardown
    @ptable = nil
  end

  def self.shutdown
    @@fields = nil
  end
end
