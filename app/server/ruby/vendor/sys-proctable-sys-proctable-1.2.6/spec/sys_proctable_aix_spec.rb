#######################################################################
# sys_proctable_aix_spec.rb
#
# Test suite for the AIX version of the sys-proctable library. You
# should run these tests via the 'rake test' task.
#######################################################################
require 'rspec'
require 'sys/proctable'
require_relative 'sys_proctable_all_spec'

describe Sys::ProcTable do
  let(:fields){
    %w[
        addr argc argv bindpro cid clname cmd_args cmdline cwd egid environ
        envp euid fd flag flag2 fname gid lwpid map nice nlwp onpro pgid pid
        policy ppid pri psargs ptid rssize sid size sname start state time
        ttydev uid wchan wtype s_ttydev
    ]
  }

  let(:map_fields){
    %w[size vaddr mapname off mflags pathoff alias gp s_mflags path]
  }

  before(:all) do
    File.open('aix-child.rb', 'w') do |out|
      out.puts 'trap("HUP") { exit }'
      out.puts 'trap("TERM") { exit }'
      out.puts 'sleep(60)'
    end

    @myenv = ENV.to_hash
    @p1args = %w/aix-child.rb testing how well this works 1/
    @p2args = %w/aix-child.rb testing how well this works 2/

    @pid1 = fork do
      exec('ruby', *@p1args)
    end

    @pid2 = fork do
      exec('ruby', *@p2args)
    end

    sleep(2)  # wait to make sure the above execs have completed in children


    @p1info = ProcTable.ps(@pid1)
    @p2info = ProcTable.ps.select { |e| e.pid == @pid2 }

    if @p2info.size == 1
      @p2info = @p2info[0]
    else
      $stderr.puts "expected a single process, have #{@p2info.size}"
      exit 1
    end
  end

  after(:all) do
    Process.kill('TERM', @pid1)
    Process.kill('TERM', @pid2)
    File.unlink('aix-child.rb') rescue nil
  end

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
    it "contains a flag member and returns the expected value" do
      expect(@p1info).to respond_to(:flag)
      expect(@p1info.flag).to be_kind_of(Fixnum)
    end

    it "contains a flag2 member and returns the expected value" do
      expect(@p1info).to respond_to(:flag2)
      expect(@p1info.flag2).to be_kind_of(Fixnum)
    end

    it "contains a nlwp member and returns the expected value" do
      expect(@p1info).to respond_to(:nlwp)
      expect(@p1info.nlwp).to be_kind_of(Fixnum)
    end

    it "contains a uid member and returns the expected value" do
      expect(@p1info).to respond_to(:uid)
      expect(@p1info.uid).to be_kind_of(Integer)
      expect(@p1info.uid).to eql(@p1info.uid)
    end

    it "contains a euid member and returns the expected value" do
      expect(@p1info).to respond_to(:euid)
      expect(@p1info.euid).to be_kind_of(Integer)
      expect(@p1info.euid).to eql(@p1info.euid)
    end

    it "contains a gid member and returns the expected value" do
      expect(@p1info).to respond_to(:gid)
      expect(@p1info.gid).to be_kind_of(Integer)
      expect(@p1info.gid).to eql(@p1info.gid)
    end

    it "contains a egid member and returns the expected value" do
      expect(@p1info).to respond_to(:egid)
      expect(@p1info.egid).to be_kind_of(Integer)
      expect(@p1info.egid).to eql(@p1info.egid)
    end

    it "contains a pid member and returns the expected value" do
      expect(@p1info).to respond_to(:pid)
      expect(@p1info.pid).to be_kind_of(Integer)
      expect(@p1info.pid).to eql(@p1info.pid)
    end

    it "contains a ppid member and returns the expected value" do
      expect(@p1info).to respond_to(:ppid)
      expect(@p1info.ppid).to be_kind_of(Integer)
      expect(@p1info.ppid).to eql(@p1info.ppid)
    end

    it "contains a pgid member and returns the expected value" do
      expect(@p1info).to respond_to(:pgid)
      expect(@p1info.pgid).to be_kind_of(Integer)
      expect(@p1info.pgid).to eql(@p1info.pgid)
    end

    it "contains a sid member and returns the expected value" do
      expect(@p1info).to respond_to(:sid)
      expect(@p1info.sid).to be_kind_of(Integer)
    end

    it "contains a ttydev member and returns the expected value" do
      expect(@p1info).to respond_to(:ttydev)
      expect(@p1info.ttydev).to be_kind_of(Integer)
    end

    it "contains a addr member and returns the expected value" do
      expect(@p1info).to respond_to(:addr)
      expect(@p1info.addr).to be_kind_of(Integer)
    end

    it "contains a size member and returns the expected value" do
      expect(@p1info).to respond_to(:size)
      expect(@p1info.size).to be_kind_of(Integer)
    end

    it "contains a rssize member and returns the expected value" do
      expect(@p1info).to respond_to(:rssize)
      expect(@p1info.rssize).to be_kind_of(Integer)
    end

    it "contains a start member and returns the expected value" do
      expect(@p1info).to respond_to(:start)
      expect(@p1info.start).to be_kind_of(Time)
    end

    it "contains a time member and returns the expected value" do
      expect(@p1info).to respond_to(:time)
      expect(@p1info.time).to be_kind_of(Time)
    end

    it "contains a cid member and returns the expected value" do
      expect(@p1info).to respond_to(:cid)
      expect(@p1info.cid).to be_kind_of(Fixnum)
    end

    it "contains an argc member and returns the expected value" do
      expect(@p1info).to respond_to(:argc)
      expect(@p1info.argc).to be_kind_of(Fixnum)
      expect(@p1info.argc).to eql(@p1args.size + 1)
    end

    it "contains an argv member and returns the expected value" do
      expect(@p1info).to respond_to(:argv)
      expect(@p1info.argv).to be_kind_of(Integer)
    end

    it "contains an envp member and returns the expected value" do
      expect(@p1info).to respond_to(:envp)
      expect(@p1info.envp).to be_kind_of(Integer)
    end

    it "contains an fname member and returns the expected value" do
      expect(@p1info).to respond_to(:fname)
      expect(@p1info.fname).to be_kind_of(String)
    end

    it "contains an fname member and returns the expected value" do
      expect(@p1info).to respond_to(:psargs)
      expect(@p1info.psargs).to be_kind_of(String)
    end

    it "contains an lwpid member and returns the expected value" do
      expect(@p1info).to respond_to(:lwpid)
      expect(@p1info.lwpid).to be_kind_of(Integer)
    end

    it "contains a wchan member and returns the expected value" do
      expect(@p1info).to respond_to(:wchan)
      expect(@p1info.wchan).to be_kind_of(Integer)
    end

    it "contains a wtype member and returns the expected value" do
      expect(@p1info).to respond_to(:wtype)
      expect(@p1info.wchan).to be_kind_of(Fixnum)
    end

    it "contains a state member and returns the expected value" do
      expect(@p1info).to respond_to(:state)
      expect(@p1info.state).to be_kind_of(Fixnum)
    end

    it "contains an sname member and returns the expected value" do
      expect(@p1info).to respond_to(:sname)
      expect(@p1info.sname).to be_kind_of(String)
    end

    it "contains a nice member and returns the expected value" do
      expect(@p1info).to respond_to(:nice)
      expect(@p1info.nice).to be_kind_of(Fixnum)
    end

    it "contains a pri member and returns the expected value" do
      expect(@p1info).to respond_to(:pri)
      expect(@p1info.pri).to be_kind_of(Fixnum)
    end

    it "contains a policy member and returns the expected value" do
      expect(@p1info).to respond_to(:policy)
      expect(@p1info.policy).to be_kind_of(Fixnum)
    end

    it "contains a clname member and returns the expected value" do
      expect(@p1info).to respond_to(:clname)
      expect(@p1info.clname).to be_kind_of(String)
    end

    it "contains an onpro member and returns the expected value" do
      expect(@p1info).to respond_to(:onpro)
      expect(@p1info.onpro).to be_kind_of(Fixnum)
    end

    it "contains a bindpro member and returns the expected value" do
      expect(@p1info).to respond_to(:bindpro)
      expect(@p1info.bindpro).to be_kind_of(Fixnum)
    end

    it "contains a ptid member and returns the expected value" do
      expect(@p1info).to respond_to(:ptid)
      expect(@p1info.ptid).to be_kind_of(Fixnum)
    end

    it "contains a comm member and returns the expected value" do
      expect(@p1info).to respond_to(:comm)
      expect(@p1info.comm).to be_kind_of(String)
    end

    it "contains a fd member and returns the expected value" do
      expect(@p1info).to respond_to(:fd)
      expect(@p1info.fd).to be_kind_of(Array)
    end

    it "contains a cmd_args member and returns the expected value" do
      expect(@p1info).to respond_to(:cmd_args)
      expect(@p1info.cmd_args).to be_kind_of(Array)
      expect(@p1info.cmd_args).to_eql(['ruby', @p1args].flatten)

      expect(@p2info).to respond_to(:cmd_args)
      expect(@p2info.cmd_args).to be_kind_of(Array)
      expect(@p2info.cmd_args).to_eql(['ruby', @p2args].flatten)
    end

    it "contains an environ member and returns the expected value" do
      expect(@p1info).to respond_to(:environ)
      expect(@p1info.environ).to be_kind_of(Hash)
      expect(@p1info.environ).to eql(@myenv)

      expect(@p2info).to respond_to(:environ)
      expect(@p2info.environ).to be_kind_of(Hash)
      expect(@p2info.environ).to eql(@myenv)
    end

    it "contains a cmdline member and returns the expected value" do
      expect(@p1info).to respond_to(:cmdline)
      expect(@p1info.cmdline).to be_kind_of(String)
    end

    it "contains a cwd member and returns the expected value" do
      expect(@p1info).to respond_to(:cwd)
      expect(@p1info.cwd).to be_kind_of(String) if @p1info.cwd
    end

    it "contains a map member and returns the expected value" do
      expect(@p1info).to respond_to(:map)
      expect(@p1info.map).to be_kind_of(Array) if @p1info.map
    end
  end

  context "Struct::ProcTableMapStruct" do
    it "contains the expected members" do
      expect(@p1info.map).to be_kind_of(Array)
      expect(map_fields.sort).to eql(@p1info.map[0].members.sort)
    end

    it "has members of the expected type" do
      expect(@p1info.map).to be_kind_of(Array)
      p1info_map = @p1info.map

      expect(p1info_map).to be_kind_of(Struct::ProcTableMapStruct)
      expect(p1info_map.size).to be_kind_of(Integer)
      expect(p1info_map.vaddr).to be_kind_of(Integer)
      expect(p1info_map.mapname).to be_kind_of(String)
      expect(p1info_map.off).to be_kind_of(Integer)
      expect(p1info_map.mflags).to be_kind_of(Integer)
      expect(p1info_map.s_mflags).to be_kind_of(String)
      expect(p1info_map.pathoff).to be_kind_of(Integer)
      expect(p1info_map.alias).to be_kind_of(Integer)
      expect(p1info_map.gp).to be_kind_of(Integer)
      expect(p1info_map.path).to be_kind_of(String)
    end
  end
end
