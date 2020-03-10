#######################################################################
# sys_proctable_sunos_spec.rb
#
# Test suite for sys-proctable for SunOS/Solaris. This should be run
# run via the 'rake test' task.
#######################################################################
require 'rspec'
require 'sys/proctable'
require_relative 'sys_proctable_all_spec'

describe Sys::ProcTable do
  let(:fields){
    %w[
        flag nlwp pid ppid pgid sid uid euid gid egid addr size
        rssize ttydev pctcpu pctmem start time ctime fname psargs
        wstat argc argv envp dmodel taskid projid nzomb poolid
        zoneid contract lwpid wchan stype state sname nice syscall
        pri clname name onpro bindpro bindpset count tstamp create
        term rtime utime stime ttime tftime dftime kftime ltime
        slptime wtime stoptime minf majf nswap inblk oublk msnd
        mrcv sigs vctx ictx sysc ioch path contracts fd cmd_args
        environ cmdline
      ]
  }

  context "fields singleton method" do
    it "responds to a fields method" do
      expect(described_class).to respond_to(:fields)
    end

    it "returns the expected results for the fields method" do
      expect(described_class.fields).to be_kind_of(Array)
      expect(described_class.fields).to eql(fields)
    end
  end

  context "ProcTable::Struct members" do
    subject { described_class.ps(pid: Process.pid) }

    it "contains a pid member and returns the expected value" do
      expect(subject).to respond_to(:pid)
      expect(subject.pid).to be_kind_of(Numeric)
      expect(subject.pid).to eql(Process.pid)
    end

    it "contains a ppid member and returns the expected value" do
      expect(subject).to respond_to(:ppid)
      expect(subject.ppid).to be_kind_of(Numeric)
      expect(subject.ppid).to eql(Process.ppid)
    end

    it "contains a pgid member and returns the expected value" do
      expect(subject).to respond_to(:pgid)
      expect(subject.pgid).to be_kind_of(Numeric)
      expect(subject.pgid).to eql(Process.getpgrp)
    end

    it "has a flag member that returns the expected value" do
      expect(subject).to respond_to(:flag)
      expect(subject.flag).to be_kind_of(Integer)
    end

    it "has an nlwp member that returns the expected value" do
      expect(subject).to respond_to(:nlwp)
      expect(subject.nlwp).to be_kind_of(Integer)
      expect(subject.nlwp).to be >= 0
    end

    it "has a sid member that returns the expected value" do
      expect(subject).to respond_to(:sid)
      expect(subject.sid).to be_kind_of(Integer)
      expect(subject.sid).to be >= 0
    end

    it "has a uid member that returns the expected value" do
      expect(subject).to respond_to(:uid)
      expect(subject.uid).to be_kind_of(Integer)
      expect(subject.uid).to eql(Process.uid)
    end

    it "has a euid member that returns the expected value" do
      expect(subject).to respond_to(:euid)
      expect(subject.euid).to be_kind_of(Integer)
      expect(subject.euid).to eql(Process.euid)
    end

    it "has a gid member that returns the expected value" do
      expect(subject).to respond_to(:gid)
      expect(subject.gid).to be_kind_of(Integer)
      expect(subject.gid).to eql(Process.gid)
    end

    it "has a egid member that returns the expected value" do
      expect(subject).to respond_to(:egid)
      expect(subject.egid).to be_kind_of(Integer)
      expect(subject.egid).to eql(Process.egid)
    end

    it "has an addr member that returns the expected value" do
      expect(subject).to respond_to(:addr)
      expect(subject.addr).to be_kind_of(Integer)
      expect(subject.addr).to be >= 0
    end

    it "has a size member that returns the expected value" do
      expect(subject).to respond_to(:size)
      expect(subject.size).to be_kind_of(Integer)
      expect(subject.size).to be >= 0
    end

    it "has a rssize member that returns the expected value" do
      expect(subject).to respond_to(:rssize)
      expect(subject.rssize).to be_kind_of(Integer)
      expect(subject.rssize).to be >= 0
    end

    it "has a ttydev member that returns the expected value" do
      expect(subject).to respond_to(:ttydev)
      expect(subject.ttydev).to be_kind_of(Integer)
      expect(subject.ttydev).to be >= -1
    end

    it "has a pctcpu member that returns the expected value" do
      expect(subject).to respond_to(:pctcpu)
      expect(subject.pctcpu).to be_kind_of(Float)
      expect(subject.pctcpu).to be >= 0.0
    end

    it "has a pctmem member that returns the expected value" do
      expect(subject).to respond_to(:pctmem)
      expect(subject.pctmem).to be_kind_of(Float)
      expect(subject.pctmem).to be >= 0.0
    end

    it "has a start member that returns the expected value" do
      expect(subject).to respond_to(:start)
      expect(subject.start).to be_kind_of(Time)
    end

    it "has a time member that returns the expected value" do
      expect(subject).to respond_to(:time)
      expect(subject.time).to be_kind_of(Integer)
      expect(subject.time).to be >= 0
    end

    it "has a ctime member that returns the expected value" do
      expect(subject).to respond_to(:ctime)
      expect(subject.ctime).to be_kind_of(Integer)
      expect(subject.ctime).to be >= 0
    end

    it "has a fname member that returns the expected value" do
      expect(subject).to respond_to(:fname)
      expect(subject.fname).to be_kind_of(String)
      expect(subject.fname.size).to be > 0
    end

    it "has a comm alias member" do
      expect(subject.method(:comm)).to eql(subject.method(:fname))
    end

    it "has a psargs member that returns the expected value" do
      expect(subject).to respond_to(:psargs)
      expect(subject.psargs).to be_kind_of(String)
      expect(subject.psargs.size).to be > 0
    end

    it "has a wstat member that returns the expected value" do
      expect(subject).to respond_to(:wstat)
      expect(subject.wstat).to be_kind_of(Integer)
      expect(subject.wstat).to be >= 0
    end

    it "has an args member that returns the expected value" do
      expect(subject).to respond_to(:argc)
      expect(subject.argc).to be_kind_of(Integer)
      expect(subject.argc).to be >= 0
    end

    it "has an argv member that returns the expected value" do
      expect(subject).to respond_to(:argv)
      expect(subject.argv).to be_kind_of(Integer)
      expect(subject.argv).to be >= 0
    end

    it "has a envp member that returns the expected value" do
      expect(subject).to respond_to(:envp)
      expect(subject.envp).to be_kind_of(Integer)
      expect(subject.envp).to be >= 0
    end

    it "has a dmodel member that returns the expected value" do
      expect(subject).to respond_to(:dmodel)
      expect(subject.dmodel).to be_kind_of(Integer)
      expect(subject.dmodel).to be >= 0
    end

    it "has a taskid member that returns the expected value" do
      expect(subject).to respond_to(:taskid)
      expect(subject.taskid).to be_kind_of(Integer)
      expect(subject.taskid).to be >= 0
    end

    it "has a projid member that returns the expected value" do
      expect(subject).to respond_to(:projid)
      expect(subject.projid).to be_kind_of(Integer)
      expect(subject.projid).to be >= 0
    end

    it "has a nzomb member that returns the expected value" do
      expect(subject).to respond_to(:nzomb)
      expect(subject.nzomb).to be_kind_of(Integer)
      expect(subject.nzomb).to be >= 0
    end

    it "has a poolid member that returns the expected value" do
      expect(subject).to respond_to(:poolid)
      expect(subject.poolid).to be_kind_of(Integer)
      expect(subject.poolid).to be >= 0
    end

    it "has a zoneid member that returns the expected value" do
      expect(subject).to respond_to(:zoneid)
      expect(subject.zoneid).to be_kind_of(Integer)
      expect(subject.zoneid).to be >= 0
    end

    it "has a contract member that returns the expected value" do
      expect(subject).to respond_to(:contract)
      expect(subject.contract).to be_kind_of(Integer)
      expect(subject.contract).to be >= 0
    end
  end

  context "lwpsinfo struct" do
    subject { described_class.ps(pid: Process.pid) }

    it "has a lwpid member that returns the expected value" do
      expect(subject).to respond_to(:lwpid)
      expect(subject.lwpid).to be_kind_of(Integer)
      expect(subject.lwpid).to be >= 0
    end

    it "has a wchan member that returns the expected value" do
      expect(subject).to respond_to(:wchan)
      expect(subject.wchan).to be_kind_of(Integer)
      expect(subject.wchan).to be >= 0
    end

    it "has a stype member that returns the expected value" do
      expect(subject).to respond_to(:stype)
      expect(subject.stype).to be_kind_of(Integer)
      expect(subject.stype).to be >= 0
    end

    it "has a state member that returns the expected value" do
      expect(subject).to respond_to(:state)
      expect(subject.state).to be_kind_of(Fixnum)
      expect(subject.state).to be >= 0
    end

    it "has a sname member that returns the expected value" do
      expect(subject).to respond_to(:sname)
      expect(subject.sname).to be_kind_of(String)
      expect(['S','R','Z','T','I','O']).to include(subject.sname)
    end

    it "has a nice member that returns the expected value" do
      expect(subject).to respond_to(:nice)
      expect(subject.nice).to be_kind_of(Fixnum)
      expect(subject.nice).to be >= 0
    end

    it "has a syscall member that returns the expected value" do
      expect(subject).to respond_to(:syscall)
      expect(subject.syscall).to be_kind_of(Fixnum)
      expect(subject.syscall).to be >= 0
    end

    it "has a pri member that returns the expected value" do
      expect(subject).to respond_to(:pri)
      expect(subject.pri).to be_kind_of(Fixnum)
      expect(subject.pri).to be >= 0
    end

    it "has a clname member that returns the expected value" do
      expect(subject).to respond_to(:clname)
      expect(subject.clname).to be_kind_of(String)
      expect(subject.clname.size).to be_between(0,8)
    end

    it "has a name member that returns the expected value" do
      expect(subject).to respond_to(:name)
      expect(subject.name).to be_kind_of(String)
      expect(subject.name.size).to be_between(0,16)
    end

    it "has an onpro member that returns the expected value" do
      expect(subject).to respond_to(:onpro)
      expect(subject.onpro).to be_kind_of(Fixnum)
      expect(subject.onpro).to be >= 0
    end

    it "has a bindpro member that returns the expected value" do
      expect(subject).to respond_to(:bindpro)
      expect(subject.bindpro).to be_kind_of(Fixnum)
      expect(subject.bindpro).to be >= -1
    end

    it "has a bindpset member that returns the expected value" do
      expect(subject).to respond_to(:bindpset)
      expect(subject.bindpset).to be_kind_of(Fixnum)
      expect(subject.bindpset).to be >= -1
    end
  end
end
