require 'win32ole'
require 'socket'
require 'date'
require 'sys/proctable/version'

# The Sys module serves as a namespace only
module Sys

  # The ProcTable class encapsulates process table information
  class ProcTable

    # There is no constructor
    private_class_method :new

    # Error typically raised if one of the Sys::ProcTable methods fails
    class Error < StandardError; end

    # The comm field corresponds to the 'name' field.  The 'cmdline' field
    # is the CommandLine attribute on Windows XP or later, or the
    # 'executable_path' field on Windows 2000 or earlier.
    #
    @fields = %w[
      caption
      cmdline
      comm
      creation_class_name
      creation_date
      cs_creation_class_name
      cs_name
      description
      executable_path
      execution_state
      handle
      handle_count
      install_date
      kernel_mode_time
      maximum_working_set_size
      minimum_working_set_size
      name
      os_creation_class_name
      os_name
      other_operation_count
      other_transfer_count
      page_faults
      page_file_usage
      ppid
      peak_page_file_usage
      peak_virtual_size
      peak_working_set_size
      priority
      private_page_count
      pid
      quota_non_paged_pool_usage
      quota_paged_pool_usage
      quota_peak_non_paged_pool_usage
      quota_peak_paged_pool_usage
      read_operation_count
      read_transfer_count
      session_id
      status
      termination_date
      thread_count
      user_mode_time
      virtual_size
      windows_version
      working_set_size
      write_operation_count
      write_transfer_count
    ]

    ProcTableStruct = Struct.new("ProcTableStruct", *@fields)

    # call-seq:
    #    ProcTable.fields
    #
    # Returns an array of fields that each ProcTableStruct will contain.  This
    # may be useful if you want to know in advance what fields are available
    # without having to perform at least one read of the /proc table.
    #
    def self.fields
       @fields
    end

    # call-seq:
    #    ProcTable.ps(pid=nil)
    #    ProcTable.ps(pid=nil){ |ps| ... }
    #
    # In block form, yields a ProcTableStruct for each process entry that you
    # have rights to.  This method returns an array of ProcTableStruct's in
    # non-block form.
    #
    # If a +pid+ is provided, then only a single ProcTableStruct is yielded or
    # returned, or nil if no process information is found for that +pid+.
    #
    def self.ps(pid=nil, host=Socket.gethostname)
      if pid
        raise TypeError unless pid.kind_of?(Fixnum)
      end

      array  = block_given? ? nil : []
      struct = nil

      begin
        wmi = WIN32OLE.connect("winmgmts://#{host}/root/cimv2")
      rescue WIN32OLERuntimeError => e
        raise Error, e # Re-raise as ProcTable::Error
      else
        wmi.InstancesOf("Win32_Process").each{ |wproc|
          if pid
            next unless wproc.ProcessId == pid
          end

          # Some fields are added later, and so are nil initially
          struct = ProcTableStruct.new(
            wproc.Caption,
            nil, # Added later, based on OS version
            wproc.Name,
            wproc.CreationClassName,
            self.parse_ms_date(wproc.CreationDate),
            wproc.CSCreationClassName,
            wproc.CSName,
            wproc.Description,
            wproc.ExecutablePath,
            wproc.ExecutionState,
            wproc.Handle,
            wproc.HandleCount,
            self.parse_ms_date(wproc.InstallDate),
            self.convert(wproc.KernelModeTime),
            wproc.MaximumWorkingSetSize,
            wproc.MinimumWorkingSetSize,
            wproc.Name,
            wproc.OSCreationClassName,
            wproc.OSName,
            self.convert(wproc.OtherOperationCount),
            self.convert(wproc.OtherTransferCount),
            wproc.PageFaults,
            wproc.PageFileUsage,
            wproc.ParentProcessId,
            self.convert(wproc.PeakPageFileUsage),
            self.convert(wproc.PeakVirtualSize),
            self.convert(wproc.PeakWorkingSetSize),
            wproc.Priority,
            self.convert(wproc.PrivatePageCount),
            wproc.ProcessId,
            wproc.QuotaNonPagedPoolUsage,
            wproc.QuotaPagedPoolUsage,
            wproc.QuotaPeakNonPagedPoolUsage,
            wproc.QuotaPeakPagedPoolUsage,
            self.convert(wproc.ReadOperationCount),
            self.convert(wproc.ReadTransferCount),
            wproc.SessionId,
            wproc.Status,
            self.parse_ms_date(wproc.TerminationDate),
            wproc.ThreadCount,
            self.convert(wproc.UserModeTime),
            self.convert(wproc.VirtualSize),
            wproc.WindowsVersion,
            self.convert(wproc.WorkingSetSize),
            self.convert(wproc.WriteOperationCount),
            self.convert(wproc.WriteTransferCount)
          )

          ###############################################################
          # On Windows XP or later, set the cmdline to the CommandLine
          # attribute.  Otherwise, set it to the ExecutablePath
          # attribute.
          ###############################################################
          if wproc.WindowsVersion.to_f < 5.1
            struct.cmdline = wproc.ExecutablePath
          else
            struct.cmdline = wproc.CommandLine
          end

          struct.freeze # This is read-only data

          if block_given?
            yield struct
          else
            array << struct
          end
        }
      end

      pid ? struct : array
    end

    private

    #######################################################################
    # Converts a string in the format '20040703074625.015625-360' into a
    # Ruby Time object.
    #######################################################################
    def self.parse_ms_date(str)
      return if str.nil?
      return DateTime.parse(str)
    end

    #####################################################################
    # There is a bug in win32ole where uint64 types are returned as a
    # String instead of a Fixnum.  This method deals with that for now.
    #####################################################################
    def self.convert(str)
      return nil if str.nil? # Return nil, not 0
      return str.to_i
    end
  end
end
