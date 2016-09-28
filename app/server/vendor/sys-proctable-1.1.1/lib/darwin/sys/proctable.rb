require 'sys/proctable/version'
require 'ffi'

module Sys
  class ProcTable
    extend FFI::Library

    # Error typically raised if the ProcTable.ps method fails.
    class Error < StandardError; end

    # There is no constructor
    private_class_method :new

    private

    PROC_PIDTASKALLINFO = 2
    PROC_PIDTHREADINFO  = 5
    PROC_PIDLISTTHREADS = 6

    CTL_KERN       = 1
    KERN_PROCARGS  = 38
    MAXCOMLEN      = 16
    MAXPATHLEN     = 256

    MAXTHREADNAMESIZE = 64
    PROC_PIDPATHINFO_MAXSIZE = MAXPATHLEN * 4

    class ProcBsdInfo < FFI::Struct
      layout(
        :pbi_flags, :uint32_t,
        :pbi_status, :uint32_t,
        :pbi_xstatus, :uint32_t,
        :pbi_pid, :uint32_t,
        :pbi_ppid, :uint32_t,
        :pbi_uid, :uid_t,
        :pbi_gid, :uid_t,
        :pbi_ruid, :uid_t,
        :pbi_rgid, :gid_t,
        :pbi_svuid, :uid_t,
        :pbi_svgid, :gid_t,
        :rfu1, :uint32_t,
        :pbi_comm, [:char, MAXCOMLEN],
        :pbi_name, [:char, MAXCOMLEN * 2],
        :pbi_nfiles, :uint32_t,
        :pbi_pgid, :uint32_t,
        :pbi_pjobc, :uint32_t,
        :e_tdev, :uint32_t,
        :e_tpgid, :uint32_t,
        :pbi_nice, :int32_t,
        :pbi_start_tvsec, :uint64_t,
        :pbi_start_tvusec, :uint64_t
      )
    end

    class ProcTaskInfo < FFI::Struct
      layout(
        :pti_virtual_size, :uint64_t,
        :pti_resident_size, :uint64_t,
        :pti_total_user, :uint64_t,
        :pti_total_system, :uint64_t,
        :pti_threads_user, :uint64_t,
        :pti_threads_system, :uint64_t,
        :pti_policy, :int32_t,
        :pti_faults, :int32_t,
        :pti_pageins, :int32_t,
        :pti_cow_faults, :int32_t,
        :pti_messages_sent, :int32_t,
        :pti_messages_received, :int32_t,
        :pti_syscalls_mach, :int32_t,
        :pti_syscalls_unix, :int32_t,
        :pti_csw, :int32_t,
        :pti_threadnum, :int32_t,
        :pti_numrunning, :int32_t,
        :pti_priority, :int32_t
      )
    end

    class ProcThreadInfo < FFI::Struct
      layout(
        :pth_user_time, :uint64_t,
        :pth_system_time, :uint64_t,
        :pth_cpu_usage, :int32_t,
        :pth_policy, :int32_t,
        :pth_run_state, :int32_t,
        :pth_flags, :int32_t,
        :pth_sleep_time, :int32_t,
        :pth_curpri, :int32_t,
        :pth_priority, :int32_t,
        :pth_maxpriority, :int32_t,
        :pth_name, [:char, MAXTHREADNAMESIZE]
      )
    end

    class ProcTaskAllInfo < FFI::Struct
      layout(:pbsd, ProcBsdInfo, :ptinfo, ProcTaskInfo)
    end

    ffi_lib 'proc'

    attach_function :proc_listallpids, [:pointer, :int], :int
    attach_function :proc_pidinfo, [:int, :int, :uint64_t, :pointer, :int], :int

    ffi_lib FFI::Library::LIBC

    attach_function :sysctl, [:pointer, :uint, :pointer, :pointer, :pointer, :size_t], :int

    # These mostly mimic the struct members, but we've added a few custom ones as well.
    @fields = %w[
      flags status xstatus pid ppid uid gid ruid rgid svuid svgid rfu1 comm
      name nfiles pgid pjobc tdev tpgid nice start_tvsec start_tvusec
      virtual_size resident_size total_user total_system threads_user
      threads_system policy faults pageins cow_faults messages_sent
      messages_received syscalls_mach syscalls_unix csw threadnum numrunning
      priority cmdline exe environ threadinfo
    ]

    # Add a couple aliases to make it similar to Linux
    ProcTableStruct = Struct.new("ProcTableStruct", *@fields) do
      alias vsize virtual_size
      alias rss resident_size
    end

    ThreadInfoStruct = Struct.new("ThreadInfo", :user_time, :system_time,
      :cpu_usage, :policy, :run_state, :flags, :sleep_time, :curpri,
      :priority, :maxpriority, :name
    )

    public

    # Returns an array of fields that each ProcTableStruct will contain. This
    # may be useful if you want to know in advance what fields are available
    # without having to perform at least one read of the process table.
    #
    # Example:
    #
    #   Sys::ProcTable.fields.each{ |field|
    #      puts "Field: #{field}"
    #   }
    #
    def self.fields
      @fields
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
    #   p ProcTable.ps(1001)
    #
    def self.ps(pid = nil)
      raise TypeError unless pid.is_a?(Fixnum) if pid

      num = proc_listallpids(nil, 0)
      ptr = FFI::MemoryPointer.new(:pid_t, num)
      num = proc_listallpids(ptr, ptr.size)

      raise SystemCallError.new('proc_listallpids', FFI.errno) if num == 0

      pids  = ptr.get_array_of_int32(0, num).sort
      array = block_given? ? nil : []

      pids.each do |lpid|
        next unless pid == lpid if pid
        info = ProcTaskAllInfo.new

        if proc_pidinfo(lpid, PROC_PIDTASKALLINFO, 0, info, info.size) <= 0
          if [Errno::EPERM::Errno, Errno::ESRCH::Errno].include?(FFI.errno)
            next # Either we don't have permission, or the pid no longer exists
          else
            raise SystemCallError.new('proc_pidinfo', FFI.errno)
          end
        end

        struct = ProcTableStruct.new

        # Pass by reference
        get_cmd_args_and_env(lpid, struct)
        get_thread_info(lpid, struct, info[:ptinfo])

        # Chop the leading xx_ from the FFI struct members for our ruby struct.
        info.members.each do |nested|
          info[nested].members.each do |member|
            temp = member.to_s.split('_')
            sproperty = temp.size > 1 ? temp[1..-1].join('_') : temp.first
            if info[nested][member].is_a?(FFI::StructLayout::CharArray)
              struct[sproperty.to_sym] = info[nested][member].to_s
            else
              struct[sproperty.to_sym] = info[nested][member]
            end
          end
        end

        struct.freeze

        if block_given?
          yield struct
        else
          array << struct
        end
      end

      return nil if array.nil?
      pid ? array.first : array
    end

    private

    # Returns an array of ThreadInfo objects for the given pid.
    #
    def self.get_thread_info(pid, struct, ptinfo)
      buf = FFI::MemoryPointer.new(:uint64_t, ptinfo[:pti_threadnum])
      num = proc_pidinfo(pid, PROC_PIDLISTTHREADS, 0, buf, buf.size)

      if num <= 0
        if [Errno::EPERM::Errno, Errno::ESRCH::Errno].include?(FFI.errno)
          return # Either we don't have permission, or the pid no longer exists
        else
          raise SystemCallError.new('proc_pidinfo', FFI.errno)
        end
      end

      max = ptinfo[:pti_threadnum]
      struct[:threadinfo] = []

      0.upto(max-1) do |index|
        tinfo = ProcThreadInfo.new
        nb = proc_pidinfo(pid, PROC_PIDTHREADINFO, buf[index].read_uint64, tinfo, tinfo.size)

        if nb <= 0
          if [Errno::EPERM::Errno, Errno::ESRCH::Errno].include?(FFI.errno)
            return # Either we don't have permission, or the pid no longer exists
          else
            raise SystemCallError.new('proc_pidinfo', FFI.errno)
          end
        end

        tinfo_struct = ThreadInfoStruct.new(
          tinfo[:pth_user_time],
          tinfo[:pth_system_time],
          tinfo[:pth_cpu_usage],
          tinfo[:pth_policy],
          tinfo[:pth_run_state], 
          tinfo[:pth_flags], 
          tinfo[:pth_sleep_time], 
          tinfo[:pth_curpri],
          tinfo[:pth_priority],
          tinfo[:pth_maxpriority],
          tinfo[:pth_name].to_s,
        )

        struct[:threadinfo] << tinfo_struct
      end
    end

    # Get the command line arguments, as well as the environment settings,
    # for the given PID.
    #
    def self.get_cmd_args_and_env(pid, struct)
      len = FFI::MemoryPointer.new(:size_t)
      mib = FFI::MemoryPointer.new(:int, 3)

      # Since we may not have access to the process information due
      # to improper privileges, just bail if we see a failure here.

      mib.write_array_of_int([CTL_KERN, KERN_PROCARGS, pid])
      return if sysctl(mib, 3, nil, len, nil, 0) < 0

      buf = FFI::MemoryPointer.new(:char, len.read_ulong)
      return if sysctl(mib, 3, buf, len, nil, 0) < 0

      exe = buf.read_string # Read up to first null, does not include args
      struct[:exe] = exe

      # Parse the rest of the information out of a big, ugly string
      array = buf.read_bytes(len.read_ulong).split(0.chr)
      array.shift      # Delete first exe
      array.delete('') # Delete empty strings

      cmdline = ''

      # Anything that doesn't include a '=' sign is a cmdline argument.
      while array[0] && !array[0].include?('=')
        cmdline << ' ' + array.shift
      end

      struct[:cmdline] = File.basename(cmdline.strip)

      # Anything remaining at this point is a collection of key=value
      # pairs which we convert into a hash.
      environ = array.inject({}) do |hash, string|
        if string && string.include?('=')
          key, value = string.split('=')
          hash[key] = value
        end
        hash
      end

      struct[:environ] = environ
    end
  end
end
