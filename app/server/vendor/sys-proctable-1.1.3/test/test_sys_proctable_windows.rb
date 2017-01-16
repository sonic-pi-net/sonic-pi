############################################################################
# test_sys_proctable_windows.rb
#
# Test suite for the sys-proctable library for MS Windows. This should be
# run via the 'rake test' task.
############################################################################
require 'rubygems'
gem 'test-unit'

require 'sys/proctable'
require 'socket'
require 'test/unit'
require 'test/test_sys_proctable_all'

class TC_ProcTable_MSWindows < Test::Unit::TestCase
   def self.startup
      @@hostname = Socket.gethostname

      @@fields = %w/
         caption
         cmdline
         comm
         creation_class_name
         creation_date
         cs_creation_class_name
         cs_name description
         executable_path
         execution_state
         handle
         handle_count
         install_date
         kernel_mode_time
         maximum_working_set_size
         minimum_working_set_size name
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
      /

      @@ptable = ProcTable.ps.first
   end

   def test_write_transfer_count
      assert_respond_to(@@ptable, :write_transfer_count)
      assert_kind_of(Integer, @@ptable.write_transfer_count)
   end

   def test_write_operation_count
      assert_respond_to(@@ptable, :write_operation_count)
      assert_kind_of(Integer, @@ptable.write_operation_count)
   end

   def test_working_set_size
      assert_respond_to(@@ptable, :working_set_size)
      assert_kind_of(Integer, @@ptable.working_set_size)
   end

   def test_windows_version
      assert_respond_to(@@ptable, :windows_version)
      assert_kind_of(String, @@ptable.windows_version)
   end

   def test_virtual_size
      assert_respond_to(@@ptable, :virtual_size)
      assert_kind_of(Integer, @@ptable.virtual_size)
   end

   def test_user_mode_time
      assert_respond_to(@@ptable, :user_mode_time)
      assert_kind_of(Integer, @@ptable.user_mode_time)
   end

   def test_thread_count
      assert_respond_to(@@ptable, :thread_count)
      assert_kind_of(Integer, @@ptable.thread_count)
   end

   def test_termination_date
      assert_respond_to(@@ptable, :termination_date)
      assert_true([NilClass, Date].include?(@@ptable.termination_date.class))
   end

   def test_status
      assert_respond_to(@@ptable, :status)
      assert_nil(@@ptable.status) # Always null according to MSDN
   end

   def test_session_id
      assert_respond_to(@@ptable, :session_id)
      assert_kind_of(Integer, @@ptable.session_id)
   end

   def test_read_transfer_count
      assert_respond_to(@@ptable, :read_transfer_count)
      assert_kind_of(Integer, @@ptable.read_transfer_count)
   end

   def test_read_operation_count
      assert_respond_to(@@ptable, :read_operation_count)
      assert_kind_of(Integer, @@ptable.read_operation_count)
   end

   def test_quota_peak_paged_pool_usage
      assert_respond_to(@@ptable, :quota_peak_paged_pool_usage)
      assert_kind_of(Integer, @@ptable.quota_peak_paged_pool_usage)
   end

   def test_quota_peak_non_paged_pool_usage
      assert_respond_to(@@ptable, :quota_peak_non_paged_pool_usage)
      assert_kind_of(Integer, @@ptable.quota_peak_non_paged_pool_usage)
   end

   def test_quota_paged_pool_usage
      assert_respond_to(@@ptable, :quota_paged_pool_usage)
      assert_kind_of(Integer, @@ptable.quota_paged_pool_usage)
   end

   def test_quota_non_paged_pool_usage
      assert_respond_to(@@ptable, :quota_non_paged_pool_usage)
      assert_kind_of(Integer, @@ptable.quota_non_paged_pool_usage)
   end

   def test_pid
      assert_respond_to(@@ptable, :pid)
      assert_kind_of(Integer, @@ptable.pid)
   end

   def test_private_page_count
      assert_respond_to(@@ptable, :private_page_count)
      assert_kind_of(Integer, @@ptable.private_page_count)
   end

   def test_priority
      assert_respond_to(@@ptable, :priority)
      assert_kind_of(Integer, @@ptable.priority)
   end

   def test_peak_working_set_size
      assert_respond_to(@@ptable, :peak_working_set_size)
      assert_kind_of(Integer, @@ptable.peak_working_set_size)
   end

   def test_peak_virtual_size
      assert_respond_to(@@ptable, :peak_virtual_size)
      assert_kind_of(Integer, @@ptable.peak_virtual_size)
   end

   def test_peak_page_file_usage
      assert_respond_to(@@ptable, :peak_page_file_usage)
      assert_kind_of(Integer, @@ptable.peak_page_file_usage)
   end

   def test_ppid
      assert_respond_to(@@ptable, :ppid)
      assert_kind_of(Integer, @@ptable.ppid)
   end

   def test_page_file_usage
      assert_respond_to(@@ptable, :page_file_usage)
      assert_kind_of(Integer, @@ptable.page_file_usage)
   end

   def test_page_faults
      assert_respond_to(@@ptable, :page_faults)
      assert_kind_of(Integer, @@ptable.page_faults)
   end

   def test_other_transfer_count
      assert_respond_to(@@ptable, :other_transfer_count)
      assert_kind_of(Integer, @@ptable.other_transfer_count)
   end

   def test_other_operation_count
      assert_respond_to(@@ptable, :other_operation_count)
      assert_kind_of(Integer, @@ptable.other_operation_count)
   end

   def test_os_name
      assert_respond_to(@@ptable, :os_name)
      assert_kind_of(String, @@ptable.os_name)
   end

   def test_os_creation_class_name
      assert_respond_to(@@ptable, :os_creation_class_name)
      assert_kind_of(String, @@ptable.os_creation_class_name)
   end

   def test_name
      assert_respond_to(@@ptable, :name)
      assert_kind_of(String, @@ptable.name)
   end

   def test_minimum_working_set_size
      assert_respond_to(@@ptable, :minimum_working_set_size)
      assert_true([NilClass, Integer].include?(@@ptable.minimum_working_set_size.class))
   end

   def test_maximum_working_set_size
      assert_respond_to(@@ptable, :maximum_working_set_size)
      assert_true([NilClass, Integer].include?(@@ptable.maximum_working_set_size.class))
   end

   def test_kernel_mode_time
      assert_respond_to(@@ptable, :kernel_mode_time)
      assert_kind_of(Integer, @@ptable.kernel_mode_time)
   end

   def test_install_date
      assert_respond_to(@@ptable, :install_date)
      assert_true([NilClass, Date].include?(@@ptable.install_date.class))
   end

   def test_handle_count
      assert_respond_to(@@ptable, :handle_count)
      assert_kind_of(Integer, @@ptable.handle_count)
   end

   def test_handle
      assert_respond_to(@@ptable, :handle)
      assert_kind_of(String, @@ptable.handle) # MSDN says it's a String
   end

   def test_execution_state
      assert_respond_to(@@ptable, :execution_state)
      assert_nil(@@ptable.execution_state) # Always NULL according to MSDN
   end

   def test_executable_path
      assert_respond_to(@@ptable, :executable_path)
      assert_true([NilClass, String].include?(@@ptable.executable_path.class))
   end

   def test_description
      assert_respond_to(@@ptable, :description)
      assert_kind_of(String, @@ptable.description)
   end

   def test_cs_name
      assert_respond_to(@@ptable, :cs_name)
      assert_kind_of(String, @@ptable.cs_name)
   end

   def test_cs_creation_class_name
      assert_respond_to(@@ptable, :cs_creation_class_name)
      assert_kind_of(String, @@ptable.cs_creation_class_name)
   end

   def test_creation_date
      assert_respond_to(@@ptable, :creation_date)
      assert_true([NilClass, Date].include?(@@ptable.creation_date.class))
   end

   def test_creation_class_name
      assert_respond_to(@@ptable, :creation_class_name)
      assert_kind_of(String, @@ptable.creation_class_name)
   end

   def test_comm
      assert_respond_to(@@ptable, :comm)
      assert_kind_of(String, @@ptable.comm)
   end

   def test_cmdline
      assert_respond_to(@@ptable, :cmdline)
      assert_true([NilClass, String].include?(@@ptable.cmdline.class))
   end

   def test_caption
      assert_respond_to(@@ptable, :caption)
      assert_kind_of(String, @@ptable.caption)
   end

   def test_field_members
      assert_equal(@@fields.length, @@ptable.length)
      assert_equal(@@fields, ProcTable.fields)
      if RUBY_VERSION.to_f >= 1.9
        assert_equal(@@fields.map(&:to_sym), @@ptable.members)
      else
        assert_equal(@@fields, @@ptable.members)
      end
   end

   # Only Windows supports a hostname as a second argument
   def test_ps_with_pid_and_host
      assert_nothing_raised{ ProcTable.ps(0, @@hostname) }
      assert_kind_of(Struct::ProcTableStruct, ProcTable.ps(0, @@hostname))
   end

   def test_ps_expected_errors
      assert_raise(ArgumentError){ ProcTable.ps(0, @@hostname, 0) }
   end

   def self.shutdown
      @@ptable   = nil
      @@hostname = nil
      @@fields   = nil
   end
end
