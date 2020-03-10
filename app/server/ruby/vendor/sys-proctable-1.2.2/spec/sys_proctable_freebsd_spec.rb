################################################################
# sys_proctable_freebsd_rspec.rb
#
# Test suite for FreeBSD for the sys-proctable library.
# You should run these tests via 'rake test'.
################################################################
require 'rspec'
require 'sys/proctable'
require_relative 'sys_proctable_all_spec'

describe Sys::ProcTable do
  let(:fields){
    %w[
      pid ppid pgid tpgid sid tsid jobc uid ruid rgid
      ngroups groups size rssize swrss tsize dsize ssize
      xstat acflag pctcpu estcpu slptime swtime runtime start
      flag state nice lock rqindex oncpu lastcpu wmesg login
      lockname comm ttynum ttydev jid priority usrpri cmdline
      utime stime maxrss ixrss idrss isrss minflt majflt nswap
      inblock oublock msgsnd msgrcv nsignals nvcsw nivcsw
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
      expect(subject.ppid).to be_kind_of(Fixnum)
    end

    it "contains a pgid member and returns the expected value" do
      expect(subject).to respond_to(:pgid)
      expect(subject.pgid).to be_kind_of(Fixnum)
    end

    it "contains a ruid member and returns the expected value" do
      expect(subject).to respond_to(:ruid)
      expect(subject.ruid).to be_kind_of(Fixnum)
    end

    it "contains a rgid member and returns the expected value" do
      expect(subject).to respond_to(:rgid)
      expect(subject.rgid).to be_kind_of(Fixnum)
    end

    it "contains a comm member and returns the expected value" do
      expect(subject).to respond_to(:comm)
      expect(subject.comm).to be_kind_of(String)
    end

    it "contains a state member and returns the expected value" do
      expect(subject).to respond_to(:state)
      expect(subject.state).to be_kind_of(String)
    end

    it "contains a pctcpu member and returns the expected value" do
      expect(subject).to respond_to(:pctcpu)
      expect(subject.pctcpu).to be_kind_of(Float)
    end

    it "contains a oncpu member and returns the expected value" do
      expect(subject).to respond_to(:oncpu)
      expect(subject.oncpu).to be_kind_of(Fixnum)
    end

    it "contains a ttynum member and returns the expected value" do
      expect(subject).to respond_to(:ttynum)
      expect(subject.ttynum).to be_kind_of(Fixnum)
    end

    it "contains a ttydev member and returns the expected value" do
      expect(subject).to respond_to(:ttydev)
      expect(subject.ttydev).to be_kind_of(String)
    end

    it "contains a wmesg member and returns the expected value" do
      expect(subject).to respond_to(:wmesg)
      expect(subject.wmesg).to be_kind_of(String)
    end

    it "contains a runtime member and returns the expected value" do
      expect(subject).to respond_to(:runtime)
      expect(subject.runtime).to be_kind_of(Fixnum)
    end

    it "contains a priority member and returns the expected value" do
      expect(subject).to respond_to(:priority)
      expect(subject.priority).to be_kind_of(Fixnum)
    end

    it "contains a usrpri member and returns the expected value" do
      expect(subject).to respond_to(:usrpri)
      expect(subject.usrpri).to be_kind_of(Fixnum)
    end

    it "contains a nice member and returns the expected value" do
      expect(subject).to respond_to(:nice)
      expect(subject.nice).to be_kind_of(Fixnum)
    end

    it "contains a cmdline member and returns the expected value" do
      expect(subject).to respond_to(:cmdline)
      expect(subject.cmdline).to be_kind_of(String)
    end

    it "contains a start member and returns the expected value" do
      expect(subject).to respond_to(:start)
      expect(subject.start).to be_kind_of(Time)
    end

    it "contains a maxrss member and returns the expected value" do
      expect(subject).to respond_to(:maxrss)
      expect(subject.maxrss).to be_kind_of(Fixnum)
    end

    it "contains a ixrss member and returns the expected value" do
      expect(subject).to respond_to(:ixrss)
      expect(subject.ixrss).to be_kind_of(Fixnum)
    end

    # TODO: The value returned on PC BSD 10 does not appear to be valid. Investigate.
    it "contains a idrss member and returns the expected value" do
      expect(subject).to respond_to(:idrss)
      expect(subject.idrss).to be_kind_of(Numeric)
    end

    it "contains a isrss member and returns the expected value" do
      expect(subject).to respond_to(:isrss)
      expect(subject.isrss).to be_kind_of(Fixnum)
    end

    it "contains a minflt member and returns the expected value" do
      expect(subject).to respond_to(:minflt)
      expect(subject.minflt).to be_kind_of(Fixnum)
    end

    it "contains a majflt member and returns the expected value" do
      expect(subject).to respond_to(:majflt)
      expect(subject.majflt).to be_kind_of(Fixnum)
    end

    it "contains a nswap member and returns the expected value" do
      expect(subject).to respond_to(:nswap)
      expect(subject.nswap).to be_kind_of(Fixnum)
    end

    it "contains a inblock member and returns the expected value" do
      expect(subject).to respond_to(:inblock)
      expect(subject.inblock).to be_kind_of(Fixnum)
    end

    it "contains a oublock member and returns the expected value" do
      expect(subject).to respond_to(:oublock)
      expect(subject.oublock).to be_kind_of(Fixnum)
    end

    it "contains a msgsnd member and returns the expected value" do
      expect(subject).to respond_to(:msgsnd)
      expect(subject.msgsnd).to be_kind_of(Fixnum)
    end

    it "contains a msgrcv member and returns the expected value" do
      expect(subject).to respond_to(:msgrcv)
      expect(subject.msgrcv).to be_kind_of(Fixnum)
    end

    it "contains a nsignals member and returns the expected value" do
      expect(subject).to respond_to(:nsignals)
      expect(subject.nsignals).to be_kind_of(Fixnum)
    end

    it "contains a nvcsw member and returns the expected value" do
      expect(subject).to respond_to(:nvcsw)
      expect(subject.nvcsw).to be_kind_of(Fixnum)
    end

    it "contains a nivcsw member and returns the expected value" do
      expect(subject).to respond_to(:nivcsw)
      expect(subject.nivcsw).to be_kind_of(Fixnum)
    end

    it "contains a utime member and returns the expected value" do
      expect(subject).to respond_to(:utime)
      expect(subject.utime).to be_kind_of(Fixnum)
    end

    it "contains a stime member and returns the expected value" do
      expect(subject).to respond_to(:stime)
      expect(subject.stime).to be_kind_of(Fixnum)
    end
  end
end
