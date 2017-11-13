#######################################################################
# test_sys_proctable_hpux.rb
#
# Test case for the HP-UX version of the sys-proctable library. You
# should run this test suite via the 'rake test' task.
#######################################################################
require 'rubygems'
gem 'test/unit'

require 'sys/proctable'
require 'test/test_sys_proctable_all'
include Sys

class TC_ProcTable_HPUX < Test::Unit::TestCase
   def self.startup
      @@fields = %w/
         comm uid pid ppid dsize tsize ssize nice ttydev pgrp pri addr
         cpu utime stime start flag stat wchan procnum cmd cmdline time
         cpticks cptickstotal fss pctcpu rssize suid shmsize mmsize usize
         iosize vtsize vdsize vssize vshmsize vmmsize vusize viosize
         minorfaults majorfaults nswap nsignals msgrcv msgsnd maxrss
         sid schedpolicy ticksleft euid egid gid sgid        
      /
   end

   def setup
      @ptable = ProcTable.ps.last
   end

   def test_expected_fields
      assert_respond_to(ProcTable, :fields)
      assert_kind_of(Array, ProcTable.fields)
      assert_equal(@@fields, ProcTable.fields)
   end

   def test_comm
      assert_respond_to(@ptable, :comm)
      assert_kind_of(String, @ptable.comm)
   end

   def test_uid
      assert_respond_to(@ptable, :uid)
      assert_kind_of(Fixnum, @ptable.uid)
   end

   def test_pid
      assert_respond_to(@ptable, :pid)
      assert_kind_of(Fixnum, @ptable.pid)
   end

   def test_ppid
      assert_respond_to(@ptable, :ppid)
      assert_kind_of(Fixnum, @ptable.ppid)
   end

   def test_dsize
      assert_respond_to(@ptable, :dsize)
      assert_kind_of(Fixnum, @ptable.dsize)
   end

   def test_tsize
      assert_respond_to(@ptable, :tsize)
      assert_kind_of(Fixnum, @ptable.tsize)
   end

   def test_ssize
      assert_respond_to(@ptable, :ssize)
      assert_kind_of(Fixnum, @ptable.ssize)
   end

   def test_nice
      assert_respond_to(@ptable, :nice)
      assert_kind_of(Fixnum, @ptable.nice)
   end

   def test_ttydev
      assert_respond_to(@ptable, :ttydev)
      assert_kind_of(String, @ptable.ttydev)
   end

   def test_pgrp
      assert_respond_to(@ptable, :pgrp)
      assert_kind_of(Fixnum, @ptable.pgrp)
   end

   def test_pri
      assert_respond_to(@ptable, :pri)
      assert_kind_of(Fixnum, @ptable.pri)
   end

   def test_addr
      assert_respond_to(@ptable, :addr)
      assert_kind_of(Fixnum, @ptable.addr)
   end

   def test_cpu
      assert_respond_to(@ptable, :cpu)
      assert_kind_of(Fixnum, @ptable.cpu)
   end

   def test_utime
      assert_respond_to(@ptable, :utime)
      assert_kind_of(Fixnum, @ptable.utime)
   end

   def test_stime
      assert_respond_to(@ptable, :stime)
      assert_kind_of(Fixnum, @ptable.stime)
   end

   def test_start
      assert_respond_to(@ptable, :start)
      assert_kind_of(Time, @ptable.start)
   end

   def test_flag
      assert_respond_to(@ptable, :flag)
      assert_kind_of(Fixnum, @ptable.flag)
   end

   def test_state
      assert_respond_to(@ptable, :state)
      assert_kind_of(String, @ptable.state)
   end

   def test_wchan
      assert_respond_to(@ptable, :wchan)
      assert_kind_of(Fixnum, @ptable.wchan)
   end

   def test_procnum
      assert_respond_to(@ptable, :procnum)
      assert_kind_of(Fixnum, @ptable.procnum)
   end

   def test_cmd
      assert_respond_to(@ptable, :cmd)
      assert_kind_of(String, @ptable.cmd)
   end

   def test_cmdline
      assert_respond_to(@ptable, :cmdline)
      assert_kind_of(String, @ptable.cmdline)
   end

   def test_time
      assert_respond_to(@ptable, :time)
      assert_kind_of(Fixnum, @ptable.time)
   end

   def test_cpticks
      assert_respond_to(@ptable, :cpticks)
      assert_kind_of(Fixnum, @ptable.cpticks)
   end

   def test_cptickstotal
      assert_respond_to(@ptable, :cptickstotal)
      assert_kind_of(Fixnum, @ptable.cptickstotal)
   end

   def test_fss
      assert_respond_to(@ptable, :fss)
      assert_kind_of(Fixnum, @ptable.fss)
   end

   def test_pctcpu
      assert_respond_to(@ptable, :pctcpu)
      assert_kind_of(Float, @ptable.pctcpu)
   end

   def test_rssize
      assert_respond_to(@ptable, :rssize)
      assert_kind_of(Fixnum, @ptable.rssize)
   end

   def test_suid
      assert_respond_to(@ptable, :suid)
      assert_kind_of(Fixnum, @ptable.suid)
   end

   def test_shmsize
      assert_respond_to(@ptable, :shmsize)
      assert_kind_of(Fixnum, @ptable.shmsize)
   end

   def test_mmsize
      assert_respond_to(@ptable, :mmsize)
      assert_kind_of(Fixnum, @ptable.mmsize)
   end

   def test_usize
      assert_respond_to(@ptable, :usize)
      assert_kind_of(Fixnum, @ptable.usize)
   end

   def test_iosize
      assert_respond_to(@ptable, :iosize)
      assert_kind_of(Fixnum, @ptable.iosize)
   end

   def test_vtsize
      assert_respond_to(@ptable, :vtsize)
      assert_kind_of(Fixnum, @ptable.vtsize)
   end

   def test_vdsize
      assert_respond_to(@ptable, :vdsize)
      assert_kind_of(Fixnum, @ptable.vdsize)
   end

   def test_vssize
      assert_respond_to(@ptable, :vssize)
      assert_kind_of(Fixnum, @ptable.vssize)
   end

   def test_vshmsize
      assert_respond_to(@ptable, :vshmsize)
      assert_kind_of(Fixnum, @ptable.vshmsize)
   end

   def test_vmmsize
      assert_respond_to(@ptable, :vmmsize)
      assert_kind_of(Fixnum, @ptable.vmmsize)
   end

   def test_vusize
      assert_respond_to(@ptable, :vusize)
      assert_kind_of(Fixnum, @ptable.vusize)
   end

   def test_viosize
      assert_respond_to(@ptable, :viosize)
      assert_kind_of(Fixnum, @ptable.viosize)
   end

   def test_minorfaults
      assert_respond_to(@ptable, :minorfaults)
      assert_kind_of(Integer, @ptable.minorfaults)
   end

   def test_majorfaults
      assert_respond_to(@ptable, :majorfaults)
      assert_kind_of(Integer, @ptable.majorfaults)
   end

   def test_nswap
      assert_respond_to(@ptable, :nswap)
      assert_kind_of(Integer, @ptable.nswap)
   end

   def test_nsignals
      assert_respond_to(@ptable, :nsignals)
      assert_kind_of(Integer, @ptable.nsignals)
   end

   def test_msgrcv
      assert_respond_to(@ptable, :msgrcv)
      assert_kind_of(Integer, @ptable.msgrcv)
   end

   def test_msgsnd
      assert_respond_to(@ptable, :msgsnd)
      assert_kind_of(Integer, @ptable.msgsnd)
   end

   def test_maxrss
      assert_respond_to(@ptable, :maxrss)
      assert_kind_of(Fixnum, @ptable.maxrss)
   end

   def test_sid
      assert_respond_to(@ptable, :sid)
      assert_kind_of(Fixnum, @ptable.sid)
   end

   def test_schedpolicy
      assert_respond_to(@ptable, :schedpolicy)
      assert_kind_of(Fixnum, @ptable.schedpolicy)
   end

   def test_ticksleft
      assert_respond_to(@ptable, :ticksleft)
      assert_kind_of(Fixnum, @ptable.ticksleft)
   end

   def test_euid
      assert_respond_to(@ptable, :euid)
      assert_kind_of(Fixnum, @ptable.euid)
   end

   def test_egid
      assert_respond_to(@ptable, :egid)
      assert_kind_of(Fixnum, @ptable.egid)
   end

   def test_gid
      assert_respond_to(@ptable, :gid)
      assert_kind_of(Fixnum, @ptable.gid)
   end

   def test_sgid
      assert_respond_to(@ptable, :sgid)
      assert_kind_of(Fixnum, @ptable.sgid)
   end

   def teardown
      @ptable = nil
   end

   def self.shutdown
      @@fields = nil
   end
end
