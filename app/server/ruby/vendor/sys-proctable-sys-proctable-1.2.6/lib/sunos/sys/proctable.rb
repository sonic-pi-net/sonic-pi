########################################################################
# proctable.rb
#
# A pure Ruby version of sys-proctable for SunOS 5.8 or later.
########################################################################
require 'ffi'
require 'sys/proctable/version'

# The Sys module serves as a namespace only.
module Sys

  # The ProcTable class encapsulates process table information.
  class ProcTable
    extend FFI::Library

    class Error < StandardError; end

    # There is no constructor
    private_class_method :new

    private

    class Timeval < FFI::Struct
      layout(:tv_sec, :time_t, :tv_usec, :time_t)
    end

    class LWPSInfo < FFI::Struct
      layout(
        :pr_flag, :int,
        :pr_lwpid, :id_t,
        :pr_addr, :uintptr_t,
        :pr_wchan, :uintptr_t,
        :pr_stype, :char,
        :pr_state, :char,
        :pr_sname, :char,
        :pr_nice, :char,
        :pr_syscall, :short,
        :pr_oldpri, :char,
        :pr_cpu, :char,
        :pr_pri, :int,
        :pr_pctcpu, :ushort_t,
        :pr_pad, :ushort_t,
        :pr_start, Timeval,
        :pr_time, Timeval,
        :pr_clname, [:char, 8],
        :pr_name, [:char, 16],
        :pr_onpro, :int,
        :pr_bindpro, :int,
        :pr_bindpset, :int,
        :pr_filler, [:int, 5]
      )
    end

    class PSInfo < FFI::Struct
      layout(
        :pr_flag, :int,
        :pr_nlwp, :int,
        :pr_pid, :pid_t,
        :pr_ppid, :pid_t,
        :pr_pgid, :pid_t,
        :pr_sid, :pid_t,
        :pr_uid, :uid_t,
        :pr_euid, :uid_t,
        :pr_gid, :gid_t,
        :pr_egid, :gid_t,
        :pr_addr, :uintptr_t,
        :pr_size, :size_t,
        :pr_rssize, :size_t,
        :pr_pad1, :size_t,
        :pr_ttydev, :dev_t,
        :pr_pctcpu, :ushort_t,
        :pr_pctmem, :ushort_t,
        :pr_start, Timeval,
        :pr_time, Timeval,
        :pr_ctime, Timeval,
        :pr_fname, [:char, 16],
        :pr_psargs, [:char, 80],
        :pr_wstat, :int,
        :pr_argc, :int,
        :pr_argv, :uintptr_t,
        :pr_envp, :uintptr_t,
        :pr_dmodel, :char,
        :pr_pad2, [:char, 3],
        :pr_taskid, :taskid_t,
        :pr_projid, :projid_t,
        :pr_nzomb, :int,
        :pr_poolid, :poolid_t,
        :pr_zoneid, :zoneid_t,
        :pr_contract, :id_t,
        :pr_filler, [:int, 1],
        :pr_lwp, LWPSInfo
      )
    end

    class PRUsage < FFI::Struct
      layout(
        :pr_lwpid, :id_t,
        :pr_count, :int,
        :pr_tstamp, Timeval,
        :pr_create, Timeval,
        :pr_term, Timeval,
        :pr_rtime, Timeval,
        :pr_utime, Timeval,
        :pr_stime, Timeval,
        :pr_ttime, Timeval,
        :pr_tftime, Timeval,
        :pr_dftime, Timeval,
        :pr_kftime, Timeval,
        :pr_ltime, Timeval,
        :pr_slptime, Timeval,
        :pr_wtime, Timeval,
        :pr_stoptime, Timeval,
        :pr_filetime, [Timeval,6],
        :pr_minf, :ulong_t,
        :pr_majf, :ulong_t,
        :pr_nswap, :ulong_t,
        :pr_inblk, :ulong_t,
        :pr_oublk, :ulong_t,
        :pr_msnd, :ulong_t,
        :pr_mrcv, :ulong_t,
        :pr_sigs, :ulong_t,
        :pr_vctx, :ulong_t,
        :pr_ictx, :ulong_t,
        :pr_sysc, :ulong_t,
        :pr_ioch, :ulong_t,
        :filler, [:ulong_t, 10]
      )
    end

    PRNODEV = (1<<FFI::Platform::ADDRESS_SIZE)-1

    @fields = [
      :flag,      # process flags (deprecated)
      :nlwp,      # number of active lwp's in the process
      :pid,       # unique process id
      :ppid,      # process id of parent
      :pgid,      # pid of session leader
      :sid,       # session id
      :uid,       # real user id
      :euid,      # effective user id
      :gid,       # real group id
      :egid,      # effective group id
      :addr,      # address of the process
      :size,      # size of process in kbytes
      :rssize,    # resident set size in kbytes
      :ttydev,    # tty device (or PRNODEV)
      :pctcpu,    # % of recent cpu used by all lwp's
      :pctmem,    # % of system memory used by process
      :start,     # absolute process start time
      :time,      # usr + sys cpu time for this process
      :ctime,     # usr + sys cpu time for reaped children
      :fname,     # name of the exec'd file
      :psargs,    # initial characters argument list - same as cmdline
      :wstat,     # if a zombie, the wait status
      :argc,      # initial argument count
      :argv,      # address of initial argument vector
      :envp,      # address of initial environment vector
      :dmodel,    # data model of the process
      :taskid,    # task id
      :projid,    # project id
      :nzomb,     # number of zombie lwp's in the process
      :poolid,    # pool id
      :zoneid,    # zone id
      :contract,  # process contract
      :lwpid,     # lwp id
      :wchan,     # wait address for sleeping lwp
      :stype,     # synchronization event type
      :state,     # numeric lwp state
      :sname,     # printable character for state
      :nice,      # nice for cpu usage
      :syscall,   # system call number (if in syscall)
      :pri,       # priority
      :clname,    # scheduling class name
      :name,      # name of system lwp
      :onpro,     # processor which last ran thsi lwp
      :bindpro,   # processor to which lwp is bound
      :bindpset,  # processor set to which lwp is bound
      :count,     # number of contributing lwp's
      :tstamp,    # current time stamp
      :create,    # process/lwp creation time stamp
      :term,      # process/lwp termination time stamp
      :rtime,     # total lwp real (elapsed) time
      :utime,     # user level cpu time
      :stime,     # system call cpu time
      :ttime,     # other system trap cpu time
      :tftime,    # text page fault sleep time
      :dftime,    # text page fault sleep time
      :kftime,    # kernel page fault sleep time
      :ltime,     # user lock wait sleep time
      :slptime,   # all other sleep time
      :wtime,     # wait-cpu (latency) time
      :stoptime,  # stopped time
      :minf,      # minor page faults
      :majf,      # major page faults
      :nswap,     # swaps
      :inblk,     # input blocks
      :oublk,     # output blocks
      :msnd,      # messages sent
      :mrcv,      # messages received
      :sigs,      # signals received
      :vctx,      # voluntary context switches
      :ictx,      # involuntary context switches
      :sysc,      # system calls
      :ioch,      # chars read and written
      :path,      # array of symbolic link paths from /proc/<pid>/path
      :contracts, # array symbolic link paths from /proc/<pid>/contracts
      :fd,        # array of used file descriptors
      :cmd_args,  # array of command line arguments
      :environ,   # hash of environment associated with the process,
      :cmdline    # joined cmd_args if present, otherwise psargs
    ]

    public

    ProcTableStruct = Struct.new("ProcTableStruct", *@fields) do
      alias comm fname
    end

    # In block form, yields a ProcTableStruct for each process entry that you
    # have rights to. This method returns an array of ProcTableStruct's in
    # non-block form.
    #
    # If a +pid+ is provided, then only a single ProcTableStruct is yielded or
    # returned, or nil if no process information is found for that +pid+.
    #
    # Example:
    #
    #   # Iterate over all processes
    #   ProcTable.ps do |proc_info|
    #      p proc_info
    #   end
    #
    #   # Print process table information for only pid 1001
    #   p ProcTable.ps(pid: 1001)
    #
    #   # Skip prusage information
    #   p ProcTable.ps(prusage: false)
    #
    def self.ps(**kwargs)
      pid = kwargs[:pid]
      prusage_info = kwargs[:prusage]

      raise TypeError unless pid.is_a?(Numeric) if pid

      array  = block_given? ? nil : []
      struct = nil

      Dir.foreach("/proc") do |file|
        next if file =~ /\D/ # Skip non-numeric entries under /proc

        # Only return information for a given pid, if provided
        next unless file.to_i == pid if pid

        # Skip over any entries we don't have permissions to read
        next unless File.readable?("/proc/#{file}/psinfo")

        data = IO.read("/proc/#{file}/psinfo") rescue next
        psinfo = PSInfo.new(FFI::MemoryPointer.from_string(data))

        struct = ProcTableStruct.new

        struct.flag   = psinfo[:pr_flag]
        struct.nlwp   = psinfo[:pr_nlwp]
        struct.pid    = psinfo[:pr_pid]
        struct.ppid   = psinfo[:pr_ppid]
        struct.pgid   = psinfo[:pr_pgid]
        struct.sid    = psinfo[:pr_sid]
        struct.uid    = psinfo[:pr_uid]
        struct.euid   = psinfo[:pr_euid]
        struct.gid    = psinfo[:pr_gid]
        struct.egid   = psinfo[:pr_egid]
        struct.addr   = psinfo[:pr_addr]
        struct.size   = psinfo[:pr_size] * 1024 # bytes
        struct.rssize = psinfo[:pr_rssize] * 1024 # bytes
        struct.ttydev = psinfo[:pr_ttydev] == PRNODEV ? -1 : psinfo[:pr_ttydev]
        struct.pctcpu = (psinfo[:pr_pctcpu] * 100).to_f / 0x8000
        struct.pctmem = (psinfo[:pr_pctmem] * 100).to_f / 0x8000

        struct.start = Time.at(psinfo[:pr_start][:tv_sec])
        struct.time  = psinfo[:pr_time][:tv_sec]
        struct.ctime = psinfo[:pr_ctime][:tv_sec]

        struct.fname  = psinfo[:pr_fname].to_s
        struct.psargs = psinfo[:pr_psargs].to_s
        struct.wstat  = psinfo[:pr_wstat]
        struct.argc   = psinfo[:pr_argc]
        struct.argv   = psinfo[:pr_argv]
        struct.envp   = psinfo[:pr_envp]
        struct.dmodel = psinfo[:pr_dmodel]

        struct.taskid   = psinfo[:pr_taskid]
        struct.projid   = psinfo[:pr_projid]
        struct.nzomb    = psinfo[:pr_nzomb]
        struct.poolid   = psinfo[:pr_poolid]
        struct.zoneid   = psinfo[:pr_zoneid]
        struct.contract = psinfo[:pr_contract]

        ### LWPSINFO struct info

        struct.lwpid    = psinfo[:pr_lwp][:pr_lwpid]
        struct.wchan    = psinfo[:pr_lwp][:pr_wchan]
        struct.stype    = psinfo[:pr_lwp][:pr_stype]
        struct.state    = psinfo[:pr_lwp][:pr_state]
        struct.sname    = psinfo[:pr_lwp][:pr_sname].chr
        struct.nice     = psinfo[:pr_lwp][:pr_nice]
        struct.syscall  = psinfo[:pr_lwp][:pr_syscall]
        struct.pri      = psinfo[:pr_lwp][:pr_pri]
        struct.clname   = psinfo[:pr_lwp][:pr_clname].to_s
        struct.name     = psinfo[:pr_lwp][:pr_name].to_s
        struct.onpro    = psinfo[:pr_lwp][:pr_onpro]
        struct.bindpro  = psinfo[:pr_lwp][:pr_bindpro]
        struct.bindpset = psinfo[:pr_lwp][:pr_bindpset]

        # Get the full command line out of /proc/<pid>/as.
        begin
          File.open("/proc/#{file}/as") do |fd|
            fd.sysseek(struct.argv, IO::SEEK_SET)
            address = fd.sysread(struct.argc * 4).unpack("L")[0]

            struct.cmd_args = []

            0.upto(struct.argc - 1){ |i|
              fd.sysseek(address, IO::SEEK_SET)
              data = fd.sysread(128)[/^[^\0]*/] # Null strip
              struct.cmd_args << data
              address += data.length + 1 # Add 1 for the space
            }

            # Get the environment hash associated with the process.
            struct.environ = {}

            fd.sysseek(struct.envp, IO::SEEK_SET)

            env_address = fd.sysread(128).unpack("L")[0]

            # TODO: Optimization potential here.
            loop do
              fd.sysseek(env_address, IO::SEEK_SET)
              data = fd.sysread(1024)[/^[^\0]*/] # Null strip
              break if data.empty?
              key, value = data.split('=')
              struct.environ[key] = value
              env_address += data.length + 1 # Add 1 for the space
            end
          end
        rescue Errno::EACCES, Errno::EOVERFLOW, EOFError, RangeError
          # Skip this if we don't have proper permissions, if there's
          # no associated environment, or if there's a largefile issue.
        rescue Errno::ENOENT
          next # The process has terminated. Bail out!
        end

        ### struct prusage

        if prusage_info != false
          begin
            data = IO.read("/proc/#{file}/usage")
            prusage = PRUsage.new(FFI::MemoryPointer.from_string(data))

            struct.count    = prusage[:pr_count]
            struct.tstamp   = prusage[:pr_tstamp][:tv_sec]
            struct.create   = prusage[:pr_create][:tv_sec]
            struct.term     = prusage[:pr_term][:tv_sec]
            struct.rtime    = prusage[:pr_rtime][:tv_sec]
            struct.utime    = prusage[:pr_utime][:tv_sec]
            struct.stime    = prusage[:pr_stime][:tv_sec]
            struct.ttime    = prusage[:pr_ttime][:tv_sec]
            struct.tftime   = prusage[:pr_tftime][:tv_sec]
            struct.dftime   = prusage[:pr_dftime][:tv_sec]
            struct.kftime   = prusage[:pr_kftime][:tv_sec]
            struct.ltime    = prusage[:pr_ltime][:tv_sec]
            struct.slptime  = prusage[:pr_slptime][:tv_sec]
            struct.wtime    = prusage[:pr_wtime][:tv_sec]
            struct.stoptime = prusage[:pr_stoptime][:tv_sec]
            struct.minf     = prusage[:pr_minf]
            struct.majf     = prusage[:pr_majf]
            struct.nswap    = prusage[:pr_nswap]
            struct.inblk    = prusage[:pr_inblk]
            struct.oublk    = prusage[:pr_oublk]
            struct.msnd     = prusage[:pr_msnd]
            struct.mrcv     = prusage[:pr_mrcv]
            struct.sigs     = prusage[:pr_sigs]
            struct.vctx     = prusage[:pr_vctx]
            struct.ictx     = prusage[:pr_ictx]
            struct.sysc     = prusage[:pr_sysc]
            struct.ioch     = prusage[:pr_ioch]
          rescue Errno::EACCES
            # Do nothing if we lack permissions. Just move on.
          rescue Errno::ENOENT
            next # The process has terminated. Bail out!
          end
        end

        # Information from /proc/<pid>/path. This is represented as a hash,
        # with the symbolic link name as the key, and the file it links to
        # as the value, or nil if it cannot be found.
        #--
        # Note that cwd information can be gathered from here, too.
        struct.path = {}

        Dir["/proc/#{file}/path/*"].each{ |entry|
          link = File.readlink(entry) rescue nil
          struct.path[File.basename(entry)] = link
        }

        # Information from /proc/<pid>/contracts. This is represented as
        # a hash, with the symbolic link name as the key, and the file
        # it links to as the value.
        struct.contracts = {}

        Dir["/proc/#{file}/contracts/*"].each{ |entry|
          link = File.readlink(entry) rescue nil
          struct.contracts[File.basename(entry)] = link
        }

        # Information from /proc/<pid>/fd. This returns an array of
        # numeric file descriptors used by the process.
        struct.fd = Dir["/proc/#{file}/fd/*"].map{ |f| File.basename(f).to_i }

        # Use the cmd_args as the cmdline if available. Otherwise use
        # the psargs. This struct member is provided to provide a measure
        # of consistency with the other platform implementations.
        if struct.cmd_args && struct.cmd_args.length > 0
          struct.cmdline = struct.cmd_args.join(' ')
        else
          struct.cmdline = struct.psargs
        end

        # This is read-only data
        struct.freeze

        if block_given?
          yield struct
        else
          array << struct
        end
      end

      pid ? struct : array
    end

    # Returns an array of fields that each ProcTableStruct will contain. This
    # may be useful if you want to know in advance what fields are available
    # without having to perform at least one read of the /proc table.
    #
    # Example:
    #
    #   Sys::ProcTable.fields.each{ |field|
    #      puts "Field: #{field}"
    #   }
    #
    def self.fields
      @fields.map(&:to_s)
    end
  end
end
