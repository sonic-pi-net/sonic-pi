#######################################################################
# proctable.rb
#
# A pure Ruby version of sys-proctable for AIX 5.3 or later.
########################################################################
require 'sys/proctable/version'

# The Sys module serves as a namespace only.
module Sys

  # The ProcTable class encapsulates process table information.
  class ProcTable

    class Error < StandardError; end

    # There is no constructor
    private_class_method :new

    private

    @fields = [
      # --- psinfo_t ---
      :flag,      # process flags from proc struct p_flag
      :flag2,     # process flags from proc struct p_flag2
      :nlwp,      # number of threads in process
      #:pad1,     # reserved for future use
      :uid,       # real user id
      :euid,      # effective user id
      :gid,       # real group id
      :egid,      # effective group id
      :pid,       # unique process id
      :ppid,      # process id of parent
      :pgid,      # pid of process group leader
      :sid,       # session id
      :ttydev,    # controlling tty device (device #)
      :s_ttydev,  # controlling tty device name or '-'
      :addr,      # internal address of proc struct
      :size,      # process image size in KB (1024) units
      :rssize,    # resident set size in KB (1024) units
      :start,     # process start time, time since epoch
      :time,      # usr+sys cpu time for this process
      :cid,       # corral id
      #:pad2,     # reserved for future use
      :argc,      # initial argument count
      :argv,      # address of initial argument vector in user process
      :envp,      # address of initial environment vector in user process
      :fname,     # last component of exec()ed pathname
      :psargs,    # initial characters of arg list
      #:pad,      # reserved for future use

      # --- lwpsinfo_t ---
      :lwpid,     # thread id
      #:addr,     # internal address of thread
      :wchan,     # wait addr for sleeping thread
      #:flag,     # thread flags
      :wtype,     # type of thread wait
      :state,     # thread state
      :sname,     # printable thread state character
      :nice,      # nice for cpu usage
      :pri,       # priority, high value = high priority
      :policy,    # scheduling policy
      :clname,    # printable scheduling policy string
      :onpro,     # processor on which thread last ran
      :bindpro,   # processor to which thread is bound
      :ptid,      # pthread id
      #:pad1,     # reserved for future use
      #:pad,      # reserved for future use

      # --- prmap_t ---
      :map,       # array of prmap_t structures

       # --- lwp ---
       #:lwp        # array of lwp information

       # other...
       :fd,        # array of used file descriptors
       :cmd_args,  # array of command line arguments
       :environ,   # hash of environment associated with the process
       :cmdline,   # joined cmd_args if present, otherwise psargs
       :cwd,       # current working directory
      ]

    @psinfo_pack_directive = [
      'L',   # pr_flag
      'L',   # pr_flag2
      'L',   # pr_nlwp
      'L',   # pr__pad1
      'Q',   # pr_uid
      'Q',   # pr_euid
      'Q',   # pr_gid
      'Q',   # pr_egid
      'Q',   # pr_pid
      'Q',   # pr_ppid
      'Q',   # pr_pgid
      'Q',   # pr_sid
      'Q',   # pr_ttydev
      'Q',   # pr_addr
      'Q',   # pr_size
      'Q',   # pr_rssize
      'QlL', # pr_start
      'QlL', # pr_time
      'S',   # pr_cid
      'S',   # pr__pad2
      'L',   # pr_argc
      'Q',   # pr_argv
      'Q',   # pr_envp
      'A16', # pr_fname[PRFNSZ]
      'A80', # pr_psargs[PRARGSZ]
      'Q8',  # pr__pad[8]
      # --- lwpsinfo_t --- pr_lwp
      'Q',   # pr_lwpid
      'Q',   # pr_addr
      'Q',   # pr_wchan
      'L',   # pr_flag
      'C',   # pr_wtype
      'c',   # pr_state
      'A',   # pr_sname
      'C',   # pr_nice
      'l',   # pr_pri
      'L',   # pr_policy
      'A8',  # pr_clname
      'l',   # pr_onpro
      'l',   # pr_bindpro
      'L',   # pr_ptid
      'L',   # pr__pad1
      'Q7'   # pr__pad[7]
    ].join

    # --- prmap_t ---
    @map_fields = [
      :size,
      :vaddr,
      :mapname,
      :off,
      :mflags,
      :s_mflags,
      :pathoff,
      :alias,
      :gp,
      #:pad,
      :path,
    ]

    @prmap_pack_directive = [
      'Q',   # pr_size
      'Q',   # pr_vaddr
      'A64', # pr_mapname[PRMAPSZ]
      'Q',   # pr_off
      'L',   # pr_mflags
      'L',   # pr_pathoff
      'Q',   # pr_alias
      'Q',   # pr_gp
      'Q8',  # pr__pad[8]
    ].join

    # prmap_t pr_mflags

    PR_MFLAGS =
     [
       [ 0x80000000, 'main'   ],  # MA_MAINEXEC - main executable
       [ 0x40000000, 'kernel' ],  # MA_KERNTEXT - kernel text
       [ 0x00000004, 'read'   ],  # MA_READ - readable
       [ 0x00000002, 'write'  ],  # MA_WRITE - writable
       [ 0x00000001, 'exec'   ],  # MA_EXEC - executable
       [ 0x00000008, 'shared' ],  # MA_SHARED - shared memory region
       [ 0x00000010, 'heap'   ],  # MA_BREAK - heap -- grown by brk
       [ 0x00000020, 'stack'  ],  # MA_STACK - stack -- grows on stack faults
     ]

    @devs = {}

    Dir['/dev/**/*'].map do |filename|
      begin
        rdev = File.stat(filename).rdev
      rescue
        next
      end

      @devs[rdev] = filename[5..-1] if rdev.nonzero?
    end

    public

    ProcTableStruct = Struct.new("ProcTableStruct", *@fields) do
      alias comm fname
    end

    ProcTableMapStruct = Struct.new("ProcTableMapStruct", *@map_fields)

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
    def self.ps(**kwargs)
      pid = kwargs[:pid]

      raise TypeError unless pid.is_a?(Numeric) if pid

      array  = block_given? ? nil : []
      struct = nil

      Dir.foreach("/proc") do |file|
        next if file =~ /\D/ # Skip non-numeric entries under /proc

        # Only return information for a given pid, if provided
        if pid
          next unless file.to_i == pid
        end

        # Skip over any entries we don't have permissions to read
        next unless File.readable?("/proc/#{file}/psinfo")

        psinfo = IO.read("/proc/#{file}/psinfo") rescue next

        psinfo_array = psinfo.unpack(@psinfo_pack_directive)

        struct = ProcTableStruct.new

        struct.flag    = psinfo_array[0]         # pr_flag
        struct.flag2   = psinfo_array[1]         # pr_flag2
        struct.nlwp    = psinfo_array[2]         # pr_nlwp
        # pr__pad1
        struct.uid     = psinfo_array[4]         # pr_uid
        struct.euid    = psinfo_array[5]         # pr_euid
        struct.gid     = psinfo_array[6]         # pr_gid
        struct.egid    = psinfo_array[7]         # pr_egid
        struct.pid     = psinfo_array[8]         # pr_pid
        struct.ppid    = psinfo_array[9]         # pr_ppid
        struct.pgid    = psinfo_array[10]        # pr_pgid
        struct.sid     = psinfo_array[11]        # pr_sid
        struct.ttydev  = psinfo_array[12]        # pr_ttydev

        # convert from 64-bit dev_t to 32-bit dev_t and then map the device
        # number to a name
        ttydev = struct.ttydev
        ttydev = (((ttydev & 0x0000FFFF00000000) >> 16) | (ttydev & 0xFFFF))
        struct.s_ttydev = @devs.has_key?(ttydev) ? @devs[ttydev] : '-'

        struct.addr    = psinfo_array[13]        # pr_addr
        struct.size    = psinfo_array[14] * 1024 # pr_size
        struct.rssize  = psinfo_array[15] * 1024 # pr_rssize
        struct.start   = Time.at(psinfo_array[16], psinfo_array[17]) # pr_start
        # skip pr_start.__pad
        struct.time    = psinfo_array[19]        # pr_time
        # skip pr_time.tv_nsec and pr_time.__pad
        struct.cid     = psinfo_array[22]        # pr_cid
        # skip pr__pad2
        struct.argc    = psinfo_array[24]        # pr_argc
        struct.argv    = psinfo_array[25]        # pr_argv
        struct.envp    = psinfo_array[26]        # pr_envp
        struct.fname   = psinfo_array[27]        # pr_fname
        struct.psargs  = psinfo_array[28]        # pr_psargs
        # skip pr__pad

        ### lwpsinfo_t info

        struct.lwpid   = psinfo_array[37]        # pr_lwpid
        # skip pr_addr
        struct.wchan   = psinfo_array[39]        # pr_wchan
        # skip pr_flag
        struct.wtype   = psinfo_array[41]        # pr_wtype
        struct.state   = psinfo_array[42]        # pr_state
        struct.sname   = psinfo_array[43]        # pr_sname
        struct.nice    = psinfo_array[44]        # pr_nice
        struct.pri     = psinfo_array[45]        # pr_pri
        struct.policy  = psinfo_array[46]        # pr_policy
        struct.clname  = psinfo_array[47]        # pr_clname
        struct.onpro   = psinfo_array[48]        # pr_onpro
        struct.bindpro = psinfo_array[49]        # pr_bindpro
        struct.ptid    = psinfo_array[50]        # pr_ptid
        # skip pr__pad1
        # skip pr__pad

        # Get the full command line out of /proc/<pid>/as.
        begin
          File.open("/proc/#{file}/as", 'rb') do |fd|
            np = fd.sysseek(struct.argv, IO::SEEK_SET)

            if np != struct.argv
              raise Error, "argv seek to #{struct.argv}, result #{np}", caller
            end

            argv = fd.sysread(4).unpack('L')[0]

            np = fd.sysseek(argv, IO::SEEK_SET)

            if np != argv
              raise Error, "*argv seek to #{argv}, result #{np}", caller
            end

            argv = fd.sysread(4 * struct.argc).unpack("L#{struct.argc}")

            struct.cmd_args = []

            argv.each_with_index do |address, i|
              np = fd.sysseek(address, IO::SEEK_SET)

              if np != address
                raise Error, "argv[#{i}] seek to #{address}, result #{np}",
                      caller
              end

              data = fd.sysread(512)[/^[^\0]*/] # Null strip
              struct.cmd_args << data
            end

            # Get the environment hash associated with the process.
            struct.environ = {}

            # First have to go to the address given by struct.envp. That will
            # give us the address of the environment pointer array.

            np = fd.sysseek(struct.envp, IO::SEEK_SET)

            if np != struct.envp
              raise Error, "envp seek to #{struct.envp}, result #{np}", caller
            end

            envloc = fd.sysread(4).unpack('L')[0]
            n = 0

            loop do
              np = fd.sysseek(envloc, IO::SEEK_SET)

              if np != envloc
                raise Error, "envp[#{n}] seek to #{envloc}, result #{np}",
                      caller
              end

              envp = fd.sysread(4).unpack("L")[0]
              break if envp.zero?
              np = fd.sysseek(envp, IO::SEEK_SET)
              data = fd.sysread(1024)[/^[^\0]*/] # Null strip
              key, value = data.split('=')
              struct.environ[key] = value
              envloc += 4
              n += 1
            end
          end
        rescue Errno::EACCES, Errno::EOVERFLOW, EOFError
          # Skip this if we don't have proper permissions, if there's
          # no associated environment, or if there's a largefile issue.
        rescue Errno::ENOENT
          next # The process has terminated. Bail out!
        end

        # Information from /proc/<pid>/fd. This returns an array of
        # numeric file descriptors used by the process.
        struct.fd = Dir["/proc/#{file}/fd/*"].map { |f| File.basename(f).to_i }

        # Use the cmd_args as the cmdline if available. Otherwise use
        # the psargs. This struct member is provided to provide a measure
        # of consistency with the other platform implementations.
        if struct.cmd_args.nil? || struct.cmd_args.empty?
          struct.cmdline = struct.psargs
        else
          struct.cmdline = struct.cmd_args.join(' ')
        end

        # get current working directory from /proc/<pid>/cwd
        struct.cwd = File.readlink("/proc/#{file}/cwd") rescue nil

        # get virtual address map from /proc/<pid>/map
        begin
          struct.map = []

          File.open("/proc/#{file}/map", 'rb') do |fd|
            loop do
              prmap_array = fd.sysread(176).unpack(@prmap_pack_directive)
              break if prmap_array[0].zero?

              map_struct = ProcTableMapStruct.new

              map_struct.size     = prmap_array[0]  # pr_size
              map_struct.vaddr    = prmap_array[1]  # pr_vaddr
              map_struct.mapname  = prmap_array[2]  # pr_mapname
              map_struct.off      = prmap_array[3]  # pr_off
              map_struct.mflags   = prmap_array[4]  # pr_mflags

              # convert pr_mflags value to string sort of like procmap outputs
              mflags = map_struct.mflags
              map_struct.s_mflags = ''
              sep = ''

              PR_MFLAGS.each do |flag|
                if (mflags & flag[0]).nonzero?
                  map_struct.s_mflags << sep << flag[1]
                  sep = '/'
                  mflags &= ~flag[0]
                end
              end

              if mflags.nonzero?
                map_struct.s_mflags << sep << sprintf('%08x', mflags)
              end

              map_struct.pathoff  = prmap_array[5]  # pr_pathoff
              map_struct.alias    = prmap_array[6]  # pr_alias
              map_struct.gp       = prmap_array[7]  # pr_gp

              struct.map << map_struct
            end

            struct.map.each do |m|
              next if m.pathoff.zero?
              fd.sysseek(m.pathoff, IO::SEEK_SET)
              buf = fd.sysread(4096)
              buf =~ /^([^\0]*)\0([^\0]*)\0/
              m.path = $2.empty? ? $1 : "#{$1}(#{$2})"
            end
          end

          struct.map = nil if struct.map.empty?
        rescue
          struct.map = nil
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
    #   Sys::ProcTable.fields.each do |field|
    #      puts "Field: #{field}"
    #   end
    #
    def self.fields
      @fields.map{ |f| f.to_s }
    end
  end
end
