#######################################################################
# sys_proctable_linux_spec.rb
#
# Test suite for the Linux version of the sys-proctable library. You
# should run these tests via the 'rake test' task.
#######################################################################
require 'rspec'
require 'sys/proctable'
require_relative 'sys_proctable_all_spec'

describe Sys::ProcTable do
  let(:fields){ %w[
      cmdline cwd environ exe fd root pid name uid euid gid egid comm state ppid pgrp
      session tty_nr tpgid flags minflt cminflt majflt cmajflt utime
      stime cutime cstime priority nice num_threads itrealvalue starttime vsize
      rss rlim rsslim startcode endcode startstack kstkesp kstkeip signal blocked
      sigignore sigcatch wchan nswap cnswap exit_signal processor rt_priority
      policy delayacct_blkio_ticks guest_time cguest_time pctcpu pctmem nlwp cgroup smaps
    ]
  }

  context "struct members" do
    subject{ described_class.ps.last }

    it "contains a cmdline member and returns the expected value" do
      expect(subject).to respond_to(:cmdline)
      expect(subject.cmdline).to be_kind_of(String)
    end

    it "contains a cwd member and returns the expected value" do
      expect(subject).to respond_to(:cwd)
      expect(subject.cwd).to be_kind_of(String) if subject.cwd
    end

    it "contains a environ member and returns the expected value" do
      expect(subject).to respond_to(:environ)
      expect(subject.environ).to be_kind_of(Hash)
    end

    it "contains an exe member and returns the expected value" do
      expect(subject).to respond_to(:exe)
      expect(subject.exe).to be_kind_of(String) if subject.exe
    end

    it "contains an fd member and returns the expected value" do
      expect(subject).to respond_to(:fd)
      expect(subject.fd).to be_kind_of(Hash)
    end

    it "contains a root member and returns the expected value" do
      expect(subject).to respond_to(:root)
      expect(subject.root).to be_kind_of(String) if subject.root
    end

    it "contains a pid member and returns the expected value" do
      expect(subject).to respond_to(:pid)
      expect(subject.pid).to be_kind_of(Integer)
    end

    it "contains a comm member and returns the expected value" do
      expect(subject).to respond_to(:comm)
      expect(subject.comm).to be_kind_of(String)
    end

    it "contains a state member and returns the expected value" do
      expect(subject).to respond_to(:state)
      expect(subject.state).to be_kind_of(String)
    end

    it "contains a ppid member and returns the expected value" do
      expect(subject).to respond_to(:state)
      expect(subject.state).to be_kind_of(String)
    end

    it "contains a pgrp member and returns the expected value" do
      expect(subject).to respond_to(:pgrp)
      expect(subject.pgrp).to be_kind_of(Integer)
    end

    it "contains a session member and returns the expected value" do
      expect(subject).to respond_to(:session)
      expect(subject.session).to be_kind_of(Integer)
    end

    it "contains a tty_nr member and returns the expected value" do
      expect(subject).to respond_to(:tty_nr)
      expect(subject.tty_nr).to be_kind_of(Integer)
    end

    it "contains a tpgid member and returns the expected value" do
      expect(subject).to respond_to(:tpgid)
      expect(subject.tpgid).to be_kind_of(Integer)
    end

    it "contains a flags member and returns the expected value" do
      expect(subject).to respond_to(:flags)
      expect(subject.flags).to be_kind_of(Numeric)
    end

    it "contains a minflt member and returns the expected value" do
      expect(subject).to respond_to(:minflt)
      expect(subject.minflt).to be_kind_of(Numeric)
    end

    it "contains a cminflt member and returns the expected value" do
      expect(subject).to respond_to(:cminflt)
      expect(subject.cminflt).to be_kind_of(Numeric)
    end

    it "contains a majflt member and returns the expected value" do
      expect(subject).to respond_to(:majflt)
      expect(subject.majflt).to be_kind_of(Numeric)
    end

    it "contains a cmajflt member and returns the expected value" do
      expect(subject).to respond_to(:cmajflt)
      expect(subject.cmajflt).to be_kind_of(Numeric)
    end

    it "contains a utime member and returns the expected value" do
      expect(subject).to respond_to(:utime)
      expect(subject.utime).to be_kind_of(Numeric)
    end

    it "contains a stime member and returns the expected value" do
      expect(subject).to respond_to(:stime)
      expect(subject.stime).to be_kind_of(Numeric)
    end

    it "contains a cutime member and returns the expected value" do
      expect(subject).to respond_to(:cutime)
      expect(subject.cutime).to be_kind_of(Numeric)
    end

    it "contains a cstime member and returns the expected value" do
      expect(subject).to respond_to(:cstime)
      expect(subject.cstime).to be_kind_of(Numeric)
    end

    it "contains a priority member and returns the expected value" do
      expect(subject).to respond_to(:priority)
      expect(subject.priority).to be_kind_of(Numeric)
    end

    it "contains a nice member and returns the expected value" do
      expect(subject).to respond_to(:nice)
      expect(subject.nice).to be_kind_of(Numeric)
    end

    it "contains a num_threads member and returns the expected value" do
      expect(subject).to respond_to(:num_threads)
      expect(subject.num_threads).to be_kind_of(Numeric)
    end

    it "contains a itrealvalue member and returns the expected value" do
      expect(subject).to respond_to(:itrealvalue)
      expect(subject.itrealvalue).to be_kind_of(Numeric)
    end

    it "contains a starttime member and returns the expected value" do
      expect(subject).to respond_to(:starttime)
      expect(subject.starttime).to be_kind_of(Numeric)
    end

    it "contains a vsize member and returns the expected value" do
      expect(subject).to respond_to(:vsize)
      expect(subject.vsize).to be_kind_of(Numeric)
    end

    it "contains a rss member and returns the expected value" do
      expect(subject).to respond_to(:rss)
      expect(subject.rss).to be_kind_of(Numeric)
    end

    it "contains an rsslim member and returns the expected value" do
      expect(subject).to respond_to(:rsslim)
      expect(subject.rsslim).to be_kind_of(Numeric)
      expect(subject.rsslim).to eq(subject.rlim)
    end

    it "contains an startcode member and returns the expected value" do
      expect(subject).to respond_to(:startcode)
      expect(subject.startcode).to be_kind_of(Numeric)
    end

    it "contains an endcode member and returns the expected value" do
      expect(subject).to respond_to(:endcode)
      expect(subject.endcode).to be_kind_of(Numeric)
    end

    it "contains a startstack member and returns the expected value" do
      expect(subject).to respond_to(:startstack)
      expect(subject.startstack).to be_kind_of(Integer)
    end

    it "contains a kstkesp member and returns the expected value" do
      expect(subject).to respond_to(:kstkesp)
      expect(subject.kstkesp).to be_kind_of(Integer)
    end

    it "contains a kstkeip member and returns the expected value" do
      expect(subject).to respond_to(:kstkeip)
      expect(subject.kstkeip).to be_kind_of(Integer)
    end

    it "contains a signal member and returns the expected value" do
      expect(subject).to respond_to(:signal)
      expect(subject.signal).to be_kind_of(Integer)
    end

    it "contains a blocked member and returns the expected value" do
      expect(subject).to respond_to(:blocked)
      expect(subject.blocked).to be_kind_of(Integer)
    end

    it "contains a sigignore member and returns the expected value" do
      expect(subject).to respond_to(:sigignore)
      expect(subject.sigignore).to be_kind_of(Integer)
    end

    it "contains a sigcatch member and returns the expected value" do
      expect(subject).to respond_to(:sigcatch)
      expect(subject.sigcatch).to be_kind_of(Integer)
    end

    it "contains a wchan member and returns the expected value" do
      expect(subject).to respond_to(:wchan)
      expect(subject.wchan).to be_kind_of(Integer)
    end

    it "contains a nswap member and returns the expected value" do
      expect(subject).to respond_to(:nswap)
      expect(subject.nswap).to be_kind_of(Integer)
    end

    it "contains a cnswap member and returns the expected value" do
      expect(subject).to respond_to(:cnswap)
      expect(subject.cnswap).to be_kind_of(Integer)
    end

    it "contains a exit_signal member and returns the expected value" do
      expect(subject).to respond_to(:exit_signal)
      expect(subject.exit_signal).to be_kind_of(Integer)
    end

    it "contains a processor member and returns the expected value" do
      expect(subject).to respond_to(:processor)
      expect(subject.processor).to be_kind_of(Integer)
    end

    it "contains a rt_priority member and returns the expected value" do
      expect(subject).to respond_to(:rt_priority)
      expect(subject.rt_priority).to be_kind_of(Integer)
    end

    it "contains a policy member and returns the expected value" do
      expect(subject).to respond_to(:policy)
      expect(subject.policy).to be_kind_of(Integer)
    end

    it "contains a delayacct_blkio_ticks member and returns the expected value" do
      expect(subject).to respond_to(:delayacct_blkio_ticks)
      expect(subject.delayacct_blkio_ticks).to be_kind_of(Integer)
    end

    it "contains a guest_time member and returns the expected value" do
      expect(subject).to respond_to(:guest_time)
      expect(subject.guest_time).to be_kind_of(Integer)
    end

    it "contains a cguest_time member and returns the expected value" do
      expect(subject).to respond_to(:cguest_time)
      expect(subject.cguest_time).to be_kind_of(Integer)
    end

    it "contains a name member and returns the expected value" do
      expect(subject).to respond_to(:name)
      expect(subject.name).to be_kind_of(String)
    end

    it "contains a uid member and returns the expected value" do
      expect(subject).to respond_to(:uid)
      expect(subject.uid).to be_kind_of(Integer)
    end

    it "contains a euid member and returns the expected value" do
      expect(subject).to respond_to(:euid)
      expect(subject.euid).to be_kind_of(Integer)
    end

    it "contains a gid member and returns the expected value" do
      expect(subject).to respond_to(:gid)
      expect(subject.gid).to be_kind_of(Integer)
    end

    it "contains a egid member and returns the expected value" do
      expect(subject).to respond_to(:egid)
      expect(subject.egid).to be_kind_of(Integer)
    end

    it "contains a pctmem member and returns the expected value" do
      expect(subject).to respond_to(:pctmem)
      expect(subject.pctmem).to be_kind_of(Float)
    end

    it "contains a pctcpu member and returns the expected value" do
      expect(subject).to respond_to(:pctcpu)
      expect(subject.pctcpu).to be_kind_of(Float)
    end

    it "contains a nlwp member and returns the expected value" do
      expect(subject).to respond_to(:nlwp)
      expect(subject.nlwp).to be_kind_of(Integer)
    end
  end

  context "custom structs" do
    subject{ described_class.ps.last }

    it "contains a cgroup member and returns the expected value" do
      expect(subject).to respond_to(:cgroup)
      expect(subject.cgroup).to be_kind_of(Array)
      expect(subject.cgroup.first).to be_kind_of(Sys::ProcTable::CgroupEntry)
    end

    it "contains a smaps member and returns the expected value" do
      expect(subject).to respond_to(:cgroup)
      expect(subject.smaps).to be_kind_of(Sys::ProcTable::Smaps)
    end
  end

  context "fields" do
    it "has a fields method that returns the expected results" do
      expect(described_class).to respond_to(:fields)
      expect(described_class.fields).to be_kind_of(Array)
      expect(described_class.fields).to match_array(fields)
    end
  end
end
