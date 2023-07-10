# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'
require 'fileutils'
require 'pathname'
require 'tmpdir'

module DataSources
  class TCZoneinfoDataSource < Minitest::Test
    include TZInfo
    include TZInfo::DataSources

    ZONEINFO_DIR = File.expand_path(File.join(File.dirname(__FILE__), '..', 'zoneinfo'))
    RubyCoreSupport.untaint(ZONEINFO_DIR)

    def setup
      @orig_search_path = ZoneinfoDataSource.search_path.clone
      @orig_alternate_iso3166_tab_search_path = ZoneinfoDataSource.alternate_iso3166_tab_search_path.clone
      @orig_pwd = FileUtils.pwd

      # A zoneinfo directory containing files needed by the tests.
      # The symlinks in this directory are set up in test_utils.rb.
      @data_source = ZoneinfoDataSource.new(ZONEINFO_DIR)
    end

    def teardown
      ZoneinfoDataSource.search_path = @orig_search_path
      ZoneinfoDataSource.alternate_iso3166_tab_search_path = @orig_alternate_iso3166_tab_search_path
      FileUtils.chdir(@orig_pwd)
    end

    def test_default_search_path
      assert_equal(['/usr/share/zoneinfo', '/usr/share/lib/zoneinfo', '/etc/zoneinfo'], ZoneinfoDataSource.search_path)
      assert_equal(false, ZoneinfoDataSource.search_path.frozen?)
    end

    def test_set_search_path_default
      ZoneinfoDataSource.search_path = ['/tmp/zoneinfo1', '/tmp/zoneinfo2']
      assert_equal(['/tmp/zoneinfo1', '/tmp/zoneinfo2'], ZoneinfoDataSource.search_path)

      ZoneinfoDataSource.search_path = nil
      assert_equal(['/usr/share/zoneinfo', '/usr/share/lib/zoneinfo', '/etc/zoneinfo'], ZoneinfoDataSource.search_path)
      assert_equal(false, ZoneinfoDataSource.search_path.frozen?)
    end

    def test_set_search_path_array
      path = ['/tmp/zoneinfo1', '/tmp/zoneinfo2']
      ZoneinfoDataSource.search_path = path
      assert_equal(['/tmp/zoneinfo1', '/tmp/zoneinfo2'], ZoneinfoDataSource.search_path)
      refute_same(path, ZoneinfoDataSource.search_path)
    end

    def test_set_search_path_array_to_s
      ZoneinfoDataSource.search_path = [Pathname.new('/tmp/zoneinfo3')]
      assert_equal(['/tmp/zoneinfo3'], ZoneinfoDataSource.search_path)
    end

    def test_set_search_path_string
      ZoneinfoDataSource.search_path = ['/tmp/zoneinfo4', '/tmp/zoneinfo5'].join(File::PATH_SEPARATOR)
      assert_equal(['/tmp/zoneinfo4', '/tmp/zoneinfo5'], ZoneinfoDataSource.search_path)
    end

    def test_default_alternate_iso3166_tab_search_path
      assert_equal(['/usr/share/misc/iso3166.tab', '/usr/share/misc/iso3166'], ZoneinfoDataSource.alternate_iso3166_tab_search_path)
      assert_equal(false, ZoneinfoDataSource.alternate_iso3166_tab_search_path.frozen?)
    end

    def test_set_alternate_iso3166_tab_search_path_default
      ZoneinfoDataSource.alternate_iso3166_tab_search_path = ['/tmp/iso3166.tab', '/tmp/iso3166']
      assert_equal(['/tmp/iso3166.tab', '/tmp/iso3166'], ZoneinfoDataSource.alternate_iso3166_tab_search_path)

      ZoneinfoDataSource.alternate_iso3166_tab_search_path = nil
      assert_equal(['/usr/share/misc/iso3166.tab', '/usr/share/misc/iso3166'], ZoneinfoDataSource.alternate_iso3166_tab_search_path)
      assert_equal(false, ZoneinfoDataSource.alternate_iso3166_tab_search_path.frozen?)
    end

    def test_set_alternate_iso3166_tab_search_path_array
      path = ['/tmp/iso3166.tab', '/tmp/iso3166']
      ZoneinfoDataSource.alternate_iso3166_tab_search_path = path
      assert_equal(['/tmp/iso3166.tab', '/tmp/iso3166'], ZoneinfoDataSource.alternate_iso3166_tab_search_path)
      refute_same(path, ZoneinfoDataSource.alternate_iso3166_tab_search_path)
    end

    def test_set_alternate_iso3166_tab_search_path_array_to_s
      ZoneinfoDataSource.alternate_iso3166_tab_search_path = [Pathname.new('/tmp/iso3166.tab')]
      assert_equal(['/tmp/iso3166.tab'], ZoneinfoDataSource.alternate_iso3166_tab_search_path)
    end

    def test_set_alternate_iso3166_tab_search_path_string
      ZoneinfoDataSource.alternate_iso3166_tab_search_path = ['/tmp/iso3166.tab', '/tmp/iso3166'].join(File::PATH_SEPARATOR)
      assert_equal(['/tmp/iso3166.tab', '/tmp/iso3166'], ZoneinfoDataSource.alternate_iso3166_tab_search_path)
    end

    def test_new_search
      Dir.mktmpdir('tzinfo_test_dir1') do |dir1|
        Dir.mktmpdir('tzinfo_test_dir2') do |dir2|
          Dir.mktmpdir('tzinfo_test_dir3') do |dir3|
            Dir.mktmpdir('tzinfo_test_dir4') do |dir4|
              file = File.join(dir1, 'file')
              FileUtils.touch(File.join(dir2, 'zone.tab'))
              FileUtils.touch(File.join(dir3, 'iso3166.tab'))
              FileUtils.touch(File.join(dir4, 'zone.tab'))
              FileUtils.touch(File.join(dir4, 'iso3166.tab'))

              ZoneinfoDataSource.search_path = [file, dir2, dir3, dir4]
              ZoneinfoDataSource.alternate_iso3166_tab_search_path = []

              data_source = ZoneinfoDataSource.new
              assert_equal(dir4, data_source.zoneinfo_dir)
            end
          end
        end
      end
    end

    def test_new_search_zone1970
      Dir.mktmpdir('tzinfo_test_dir1') do |dir1|
        Dir.mktmpdir('tzinfo_test_dir2') do |dir2|
          Dir.mktmpdir('tzinfo_test_dir3') do |dir3|
            Dir.mktmpdir('tzinfo_test_dir4') do |dir4|
              file = File.join(dir1, 'file')
              FileUtils.touch(File.join(dir2, 'zone1970.tab'))
              FileUtils.touch(File.join(dir3, 'iso3166.tab'))
              FileUtils.touch(File.join(dir4, 'zone1970.tab'))
              FileUtils.touch(File.join(dir4, 'iso3166.tab'))

              ZoneinfoDataSource.search_path = [file, dir2, dir3, dir4]
              ZoneinfoDataSource.alternate_iso3166_tab_search_path = []

              data_source = ZoneinfoDataSource.new
              assert_equal(dir4, data_source.zoneinfo_dir)
            end
          end
        end
      end
    end

    def test_new_search_solaris_tab_files
      # Solaris names the tab files 'tab/country.tab' (iso3166.tab) and
      # 'tab/zone_sun.tab' (zone.tab).

      Dir.mktmpdir('tzinfo_test_dir') do |dir|
        tab = File.join(dir, 'tab')
        FileUtils.mkdir(tab)
        FileUtils.touch(File.join(tab, 'country.tab'))
        FileUtils.touch(File.join(tab, 'zone_sun.tab'))

        ZoneinfoDataSource.search_path = [dir]
        ZoneinfoDataSource.alternate_iso3166_tab_search_path = []

        data_source = ZoneinfoDataSource.new
        assert_equal(dir, data_source.zoneinfo_dir)
      end
    end

    def test_new_search_alternate_iso3166_path
      Dir.mktmpdir('tzinfo_test_dir_zoneinfo') do |zoneinfo_dir|
        Dir.mktmpdir('tzinfo_test_dir_tab') do |tab_dir|
          FileUtils.touch(File.join(zoneinfo_dir, 'zone.tab'))

          tab_file = File.join(tab_dir, 'iso3166')

          ZoneinfoDataSource.search_path = [zoneinfo_dir]
          ZoneinfoDataSource.alternate_iso3166_tab_search_path = [tab_file]

          assert_raises_directory_not_found { ZoneinfoDataSource.new }

          FileUtils.touch(tab_file)

          data_source = ZoneinfoDataSource.new
          assert_equal(zoneinfo_dir, data_source.zoneinfo_dir)
        end
      end
    end

    def test_new_search_not_found
      Dir.mktmpdir('tzinfo_test_dir1') do |dir1|
        Dir.mktmpdir('tzinfo_test_dir2') do |dir2|
          Dir.mktmpdir('tzinfo_test_dir3') do |dir3|
            Dir.mktmpdir('tzinfo_test_dir4') do |dir4|
              Dir.mktmpdir('tzinfo_test_dir5') do |dir5|
                file = File.join(dir1, 'file')
                FileUtils.touch(file)
                FileUtils.touch(File.join(dir2, 'zone.tab'))
                FileUtils.touch(File.join(dir3, 'zone1970.tab'))
                FileUtils.touch(File.join(dir4, 'iso3166.tab'))

                ZoneinfoDataSource.search_path = [file, dir2, dir3, dir4, dir5]
                ZoneinfoDataSource.alternate_iso3166_tab_search_path = []

                assert_raises_directory_not_found { ZoneinfoDataSource.new }
              end
            end
          end
        end
      end
    end

    def test_new_search_relative
      Dir.mktmpdir('tzinfo_test') do |dir|
        FileUtils.touch(File.join(dir, 'zone.tab'))
        FileUtils.touch(File.join(dir, 'iso3166.tab'))

        FileUtils.chdir(dir)

        ZoneinfoDataSource.search_path = ['.']
        ZoneinfoDataSource.alternate_iso3166_tab_search_path = []
        data_source = ZoneinfoDataSource.new
        assert_equal(Pathname.new(dir).realpath.to_s, data_source.zoneinfo_dir)

        # Change out of the directory to allow it to be deleted on Windows.
        FileUtils.chdir(@orig_pwd)
      end
    end

    def test_new_dir
      Dir.mktmpdir('tzinfo_test') do |dir|
        FileUtils.touch(File.join(dir, 'zone.tab'))
        FileUtils.touch(File.join(dir, 'iso3166.tab'))

        data_source = ZoneinfoDataSource.new(dir)
        assert_equal(dir, data_source.zoneinfo_dir)
      end
    end

    def test_new_dir_zone1970
      Dir.mktmpdir('tzinfo_test') do |dir|
        FileUtils.touch(File.join(dir, 'zone1970.tab'))
        FileUtils.touch(File.join(dir, 'iso3166.tab'))

        data_source = ZoneinfoDataSource.new(dir)
        assert_equal(dir, data_source.zoneinfo_dir)
      end
    end

    def test_new_dir_solaris_tab_files
      # Solaris names the tab files 'tab/country.tab' (iso3166.tab) and
      # 'tab/zone_sun.tab' (zone.tab).

      Dir.mktmpdir('tzinfo_test') do |dir|
        tab = File.join(dir, 'tab')
        FileUtils.mkdir(tab)
        FileUtils.touch(File.join(tab, 'country.tab'))
        FileUtils.touch(File.join(tab, 'zone_sun.tab'))

        data_source = ZoneinfoDataSource.new(dir)
        assert_equal(dir, data_source.zoneinfo_dir)
      end
    end

    def test_new_dir_alternate_iso3166_path
      Dir.mktmpdir('tzinfo_test_dir_zoneinfo') do |zoneinfo_dir|
        Dir.mktmpdir('tzinfo_test_dir_tab') do |tab_dir|
          FileUtils.touch(File.join(zoneinfo_dir, 'zone.tab'))

          tab_file = File.join(tab_dir, 'iso3166')
          FileUtils.touch(tab_file)

          ZoneinfoDataSource.alternate_iso3166_tab_search_path = [tab_file]

          assert_raises_invalid_directory(zoneinfo_dir) do
            # The alternate_iso3166_tab_search_path should not be used. This should raise
            # an exception.
            ZoneinfoDataSource.new(zoneinfo_dir)
          end

          data_source = ZoneinfoDataSource.new(zoneinfo_dir, tab_file)
          assert_equal(zoneinfo_dir, data_source.zoneinfo_dir)
        end
      end
    end

    def test_new_dir_invalid
      Dir.mktmpdir('tzinfo_test') do |dir|
        assert_raises_invalid_directory(dir) { ZoneinfoDataSource.new(dir) }
      end
    end

    def test_new_dir_invalid_alternate_iso3166_path
      Dir.mktmpdir('tzinfo_test_dir_zoneinfo') do |zoneinfo_dir|
        Dir.mktmpdir('tzinfo_test_dir_tab') do |tab_dir|
          FileUtils.touch(File.join(zoneinfo_dir, 'zone.tab'))

          assert_raises_invalid_directory(zoneinfo_dir) do
            ZoneinfoDataSource.new(zoneinfo_dir, File.join(tab_dir, 'iso3166'))
          end
        end
      end
    end

    def test_new_dir_invalid_alternate_iso3166_path_overrides_valid
      Dir.mktmpdir('tzinfo_test_dir_zoneinfo') do |zoneinfo_dir|
        Dir.mktmpdir('tzinfo_test_dir_tab') do |tab_dir|
          FileUtils.touch(File.join(zoneinfo_dir, 'iso3166.tab'))
          FileUtils.touch(File.join(zoneinfo_dir, 'zone.tab'))

          assert_raises_invalid_directory(zoneinfo_dir) do
            ZoneinfoDataSource.new(zoneinfo_dir, File.join(tab_dir, 'iso3166'))
          end
        end
      end
    end

    def test_new_file
      Dir.mktmpdir('tzinfo_test') do |dir|
        file = File.join(dir, 'file')
        FileUtils.touch(file)

        assert_raises_invalid_directory(file) do
          ZoneinfoDataSource.new(file)
        end
      end
    end

    def test_new_dir_relative
      Dir.mktmpdir('tzinfo_test') do |dir|
        FileUtils.touch(File.join(dir, 'zone.tab'))
        FileUtils.touch(File.join(dir, 'iso3166.tab'))

        FileUtils.chdir(dir)

        data_source = ZoneinfoDataSource.new('.')
        assert_equal(Pathname.new(dir).realpath.to_s, data_source.zoneinfo_dir)

        # Change out of the directory to allow it to be deleted on Windows.
        FileUtils.chdir(@orig_pwd)
      end
    end

    def test_zoneinfo_dir
      Dir.mktmpdir('tzinfo_test') do |dir|
        FileUtils.touch(File.join(dir, 'zone.tab'))
        FileUtils.touch(File.join(dir, 'iso3166.tab'))

        data_source = ZoneinfoDataSource.new(dir)
        assert_equal(dir, data_source.zoneinfo_dir)
        assert_equal(true, data_source.zoneinfo_dir.frozen?)
      end
    end

    def test_load_timezone_info_transitions
      o1 = TimezoneOffset.new(-17900,    0, 'TESTLMT')
      o2 = TimezoneOffset.new(-18000, 3600, 'TESTD')
      o3 = TimezoneOffset.new(-18000,    0, 'TESTS')

      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000, 4,1,1,0,0).to_i)
      t2 = TimezoneTransition.new(o3, o2, Time.utc(2000,10,1,1,0,0).to_i)
      t3 = TimezoneTransition.new(o2, o3, Time.utc(2001, 3,1,1,0,0).to_i)

      transitions = [t1, t2, t3]

      reader_mock = Minitest::Mock.new
      reader_mock.expect(:read, transitions, [File.join(ZONEINFO_DIR, 'Europe', 'London')])
      @data_source.instance_variable_set(:@zoneinfo_reader, reader_mock)

      info = @data_source.send(:load_timezone_info, 'Europe/London')

      reader_mock.verify
      assert_kind_of(TransitionsDataTimezoneInfo, info)
      assert_equal('Europe/London', info.identifier)
      assert_equal(transitions, info.transitions)
    end

    def test_load_timezone_info_constant_offset
      offset = TimezoneOffset.new(-17900, 0, 'TESTLMT')

      reader_mock = Minitest::Mock.new
      reader_mock.expect(:read, offset, [File.join(ZONEINFO_DIR, 'Europe', 'London')])
      @data_source.instance_variable_set(:@zoneinfo_reader, reader_mock)

      info = @data_source.send(:load_timezone_info, 'Europe/London')

      reader_mock.verify
      assert_kind_of(ConstantOffsetDataTimezoneInfo, info)
      assert_equal('Europe/London', info.identifier)
      assert_same(offset, info.constant_offset)
    end

    def test_load_timezone_info_data
      info = @data_source.send(:load_timezone_info, 'Europe/London')
      assert_kind_of(TransitionsDataTimezoneInfo, info)
      assert_equal('Europe/London', info.identifier)
    end

    def test_load_timezone_info_linked
      info = @data_source.send(:load_timezone_info, 'UTC')

      # On platforms that don't support symlinks, 'UTC' will be created as a copy.
      # Either way, a ConstantOffsetDataTimezoneInfo should be returned.

      assert_kind_of(ConstantOffsetDataTimezoneInfo, info)
      assert_equal('UTC', info.identifier)
    end

    def test_load_timezone_info_does_not_exist
      error = assert_raises(InvalidTimezoneIdentifier) do
        @data_source.send(:load_timezone_info, 'Nowhere/Special')
      end

      assert_match(/\bNowhere\/Special\b/, error.message)
    end

    test_encodings('UTF-8', 'UTF-16').each do |encoding|
      define_method("test_load_timezone_info_file_does_not_exist_with_#{encoding.to_method}_encoded_identifier") do
        # Override the index so that an attempt is made to load the file.
        def @data_source.validate_timezone_identifier(identifier)
          identifier = identifier.encode(Encoding::UTF_8)
          raise "Unexpected identifier passed to validate_timezone_identifier: #{identifier}" unless identifier == 'Nowhere/Special'
          'Nowhere/Special'.freeze
        end

        error = assert_raises(InvalidTimezoneIdentifier) do
          @data_source.send(:load_timezone_info, 'Nowhere/Special'.encode(encoding.name))
        end

        assert_match(/\bNowhere\/Special\b/, error.message)
        assert_equal(Encoding::UTF_8, error.message.encoding)
        assert_kind_of(Errno::ENOENT, error.cause) if error.respond_to?(:cause)
      end

      define_method("test_load_timezone_info_path_component_not_dir_with_#{encoding.to_method}_encoded_identifier") do
        skip('JRuby 9.0.5.0 raises an unhandled Java IOException when encountering a path component that is not a directory') if RUBY_ENGINE == 'jruby' && JRUBY_VERSION.start_with?('9.0.')

        # Override the index so that an attempt is made to load the file.
        def @data_source.validate_timezone_identifier(identifier)
          identifier = identifier.encode(Encoding::UTF_8)
          raise "Unexpected identifier passed to validate_timezone_identifier: #{identifier}" unless identifier == 'UTC/File'
          'UTC/File'.freeze
        end

        error = assert_raises(InvalidTimezoneIdentifier) do
          @data_source.send(:load_timezone_info, 'UTC/File'.encode(encoding.name))
        end

        assert_match(/\bUTC\/File\b/, error.message)
        assert_equal(Encoding::UTF_8, error.message.encoding)

        # The cause is usually Errno::ENOTDIR. On Windows it is Errno::ENOENT
        # instead.
        if error.respond_to?(:cause)
          expected_cause = get_expected_file_open_and_read_cause(File.join(ZONEINFO_DIR, 'UTC', 'File'))
          assert_kind_of(expected_cause, error.cause)
        end
      end

      define_method("test_load_timezone_info_name_to_long_with_#{encoding.to_method}_encoded_identifier") do
        # Override the read method to raise Errno::ENAMETOOLONG and check that
        # this is handled correctly.

        zoneinfo_reader = @data_source.instance_variable_get(:@zoneinfo_reader)
        def zoneinfo_reader.read(file_path)
          raise Errno::ENAMETOOLONG, 'Test'
        end

        error = assert_raises(InvalidTimezoneIdentifier) do
          @data_source.send(:load_timezone_info, 'Europe/London'.encode(encoding.name))
        end

        assert_match(/\bEurope\/London\b/, error.message)
        assert_equal(Encoding::UTF_8, error.message.encoding)
        assert_kind_of(Errno::ENAMETOOLONG, error.cause) if error.respond_to?(:cause)
      end
    end

    def test_load_timezone_info_invalid
      error = assert_raises(InvalidTimezoneIdentifier) do
        @data_source.send(:load_timezone_info, '../zoneinfo/Europe/London')
      end

      assert_match(/\W\.\.\/zoneinfo\/Europe\/London\b/, error.message)
    end

    def test_load_timezone_info_nil
      error = assert_raises(InvalidTimezoneIdentifier) do
        @data_source.send(:load_timezone_info, nil)
      end

      assert_match(/\bnil\b/, error.message)
    end

    def test_load_timezone_info_false
      error = assert_raises(InvalidTimezoneIdentifier) do
        @data_source.send(:load_timezone_info, false)
      end

      assert_match(/\bfalse\b/, error.message)
    end

    def test_load_timezone_info_case
      error = assert_raises(InvalidTimezoneIdentifier) do
        @data_source.send(:load_timezone_info, 'europe/london')
      end

      assert_match(/\beurope\/london/, error.message)
    end

    test_encodings('UTF-8', 'UTF-16').each do |encoding|
      define_method("test_load_timezone_info_permission_denied_with_#{encoding.to_method}_encoded_identifier") do
        Dir.mktmpdir('tzinfo_test') do |dir|
          FileUtils.touch(File.join(dir, 'zone.tab'))
          FileUtils.touch(File.join(dir, 'iso3166.tab'))

          file = File.join(dir, 'UTC')
          FileUtils.touch(file)
          FileUtils.chmod(0200, file)

          if File.stat(file).mode & 0400 == 0400
            # chmod failed to remove read permissions. Assume this is Windows and
            # try setting permissions with icacls instead.
            `icacls "#{file}" /deny Everyone:R`
          end

          data_source = ZoneinfoDataSource.new(dir)

          error = assert_raises(InvalidTimezoneIdentifier) do
            data_source.send(:load_timezone_info, 'UTC'.encode(encoding.name))
          end

          assert_match(/\bUTC\b/, error.message)
          assert_equal(Encoding::UTF_8, error.message.encoding)
          assert_kind_of(Errno::EACCES, error.cause) if error.respond_to?(:cause)
        end
      end
    end

    def test_load_timezone_info_directory
      Dir.mktmpdir('tzinfo_test') do |dir|
        FileUtils.touch(File.join(dir, 'zone.tab'))
        FileUtils.touch(File.join(dir, 'iso3166.tab'))

        subdir = File.join(dir, 'Subdir')
        FileUtils.mkdir(subdir)

        data_source = ZoneinfoDataSource.new(dir)

        error = assert_raises(InvalidTimezoneIdentifier) do
          data_source.send(:load_timezone_info, 'Subdir')
        end

        assert_match(/\bSubdir\b/, error.message)
      end
    end

    test_encodings('UTF-8', 'UTF-16').each do |encoding|
      define_method("test_load_timezone_info_file_is_directory_with_#{encoding.to_method}_encoded_identifier") do
        skip('JRuby 9.0.5.0 hangs when attempting to read a directory') if RUBY_ENGINE == 'jruby' && JRUBY_VERSION.start_with?('9.0.')

        Dir.mktmpdir('tzinfo_test') do |dir|
          FileUtils.touch(File.join(dir, 'zone.tab'))
          FileUtils.touch(File.join(dir, 'iso3166.tab'))

          subdir = File.join(dir, 'Subdir')
          FileUtils.mkdir(subdir)

          data_source = ZoneinfoDataSource.new(dir)

          # Override the index so that an attempt is made to load the file.
          def data_source.validate_timezone_identifier(identifier)
            identifier = identifier.encode(Encoding::UTF_8)
            raise "Unexpected identifier passed to validate_timezone_identifier: #{identifier}" unless identifier == 'Subdir'
            'Subdir'.freeze
          end

          error = assert_raises(InvalidTimezoneIdentifier) do
            data_source.send(:load_timezone_info, 'Subdir'.encode(encoding.name))
          end

          assert_match(/\bSubdir\b/, error.message)
          assert_equal(Encoding::UTF_8, error.message.encoding)

          if error.respond_to?(:cause)
            # Errno::EISDIR is normally raised either when attempting to read or
            # when opening the file (Windows). However, JRuby on Windows 9.1.14.0
            # raises Errno::EACCES instead.
            #
            # Once https://github.com/jruby/jruby/pull/4818 has been fixed/merged,
            # this can be changed to:
            #
            # assert_kind_of(Errno::EISDIR, error.cause)
            expected_cause = get_expected_file_open_and_read_cause(File.join(dir, 'Subdir'))
            assert_kind_of(expected_cause, error.cause)
          end
        end
      end
    end

    def test_load_timezone_info_linked_absolute_outside
      Dir.mktmpdir('tzinfo_test') do |dir|
        Dir.mktmpdir('tzinfo_test') do |outside|
          outside_file = File.join(outside, 'EST')
          FileUtils.cp(File.join(@data_source.zoneinfo_dir, 'EST'), outside_file)

          FileUtils.touch(File.join(dir, 'zone.tab'))
          FileUtils.touch(File.join(dir, 'iso3166.tab'))

          file = File.join(dir, 'EST')

          begin
            FileUtils.ln_s(outside_file, file)
          rescue NotImplementedError, Errno::EACCES
            # Symlinks not supported on this platform, or permission denied
            # (administrative rights are required on some versions of Windows).
            skip('Symlinks are not supported on this platform, or permission was denied creating a symlink')
          end

          data_source = ZoneinfoDataSource.new(dir)

          info = data_source.send(:load_timezone_info, 'EST')
          assert_kind_of(ConstantOffsetDataTimezoneInfo, info)
          assert_equal('EST', info.identifier)

          # JRuby 9.1.14.0 on Windows (running elevated) fails to clean up symlinks.
          File.delete(file)
        end
      end
    end

    def test_load_timezone_info_linked_absolute_inside
      Dir.mktmpdir('tzinfo_test') do |dir|
        FileUtils.touch(File.join(dir, 'zone.tab'))
        FileUtils.touch(File.join(dir, 'iso3166.tab'))

        FileUtils.cp(File.join(@data_source.zoneinfo_dir, 'EST'), File.join(dir, 'EST'))

        link = File.join(dir, 'Link')

        begin
          FileUtils.ln_s(File.join(File.expand_path(dir), 'EST'), link)
        rescue NotImplementedError, Errno::EACCES
          # Symlinks not supported on this platform, or permission denied
          # (administrative rights are required on some versions of Windows).
          skip('Symlinks are not supported on this platform, or permission was denied creating a symlink')
        end

        data_source = ZoneinfoDataSource.new(dir)

        info = data_source.send(:load_timezone_info, 'Link')
        assert_kind_of(ConstantOffsetDataTimezoneInfo, info)
        assert_equal('Link', info.identifier)

        # JRuby 9.1.14.0 on Windows (running elevated) fails to clean up symlinks.
        File.delete(link)
      end
    end

    def test_load_timezone_info_linked_relative_outside
      Dir.mktmpdir('tzinfo_test') do |root|
        FileUtils.cp(File.join(@data_source.zoneinfo_dir, 'EST'), File.join(root, 'outside'))

        dir = File.join(root, 'zoneinfo')
        FileUtils.mkdir(dir)

        FileUtils.touch(File.join(dir, 'zone.tab'))
        FileUtils.touch(File.join(dir, 'iso3166.tab'))

        link = File.join(dir, 'Link')

        begin
          FileUtils.ln_s('../outside', link)
        rescue NotImplementedError, Errno::EACCES
          # Symlinks not supported on this platform, or permission denied
          # (administrative rights are required on some versions of Windows).
          skip('Symlinks are not supported on this platform, or permission was denied creating a symlink')
        end

        subdir = File.join(dir, 'Subdir')
        subdir_link = File.join(subdir, 'Link')
        FileUtils.mkdir(subdir)
        FileUtils.ln_s('../../outside', subdir_link)

        data_source = ZoneinfoDataSource.new(dir)

        info = data_source.send(:load_timezone_info, 'Link')
        assert_kind_of(ConstantOffsetDataTimezoneInfo, info)
        assert_equal('Link', info.identifier)

        info = data_source.send(:load_timezone_info, 'Subdir/Link')
        assert_kind_of(ConstantOffsetDataTimezoneInfo, info)
        assert_equal('Subdir/Link', info.identifier)

        # JRuby 9.1.14.0 on Windows (running elevated) fails to clean up symlinks.
        File.delete(link)
        File.delete(subdir_link)
      end
    end

    def test_load_timezone_info_linked_relative_parent_inside
      Dir.mktmpdir('tzinfo_test') do |dir|
        FileUtils.touch(File.join(dir, 'zone.tab'))
        FileUtils.touch(File.join(dir, 'iso3166.tab'))

        FileUtils.cp(File.join(@data_source.zoneinfo_dir, 'EST'), File.join(dir, 'EST'))

        subdir = File.join(dir, 'Subdir')
        FileUtils.mkdir(subdir)
        FileUtils.cp(File.join(@data_source.zoneinfo_dir, 'EST'), File.join(subdir, 'EST'))

        subdir_link = File.join(subdir, 'Link')
        begin
          FileUtils.ln_s('../Subdir/EST', subdir_link)
        rescue NotImplementedError, Errno::EACCES
          # Symlinks not supported on this platform, or permission denied
          # (administrative rights are required on some versions of Windows).
          skip('Symlinks are not supported on this platform, or permission was denied creating a symlink')
        end

        subdir_link2 = File.join(subdir, 'Link2')
        FileUtils.ln_s('../EST', subdir_link2)

        subdir2 = File.join(dir, 'Subdir2')
        FileUtils.mkdir(subdir2)
        subdir2_link = File.join(subdir2, 'Link')
        FileUtils.ln_s('../Subdir/EST', subdir2_link)

        data_source = ZoneinfoDataSource.new(dir)

        info = data_source.send(:load_timezone_info, 'Subdir/Link')
        assert_kind_of(ConstantOffsetDataTimezoneInfo, info)
        assert_equal('Subdir/Link', info.identifier)

        info = data_source.send(:load_timezone_info, 'Subdir/Link2')
        assert_kind_of(ConstantOffsetDataTimezoneInfo, info)
        assert_equal('Subdir/Link2', info.identifier)

        info = data_source.send(:load_timezone_info, 'Subdir2/Link')
        assert_kind_of(ConstantOffsetDataTimezoneInfo, info)
        assert_equal('Subdir2/Link', info.identifier)

        # JRuby 9.1.14.0 on Windows (running elevated) fails to clean up symlinks.
        File.delete(subdir_link)
        File.delete(subdir_link2)
        File.delete(subdir2_link)
      end
    end

    test_encodings('UTF-8', 'UTF-16').each do |encoding|
      define_method("test_load_timezone_info_invalid_file_with_#{encoding.to_method}_encoded_identifier") do
        Dir.mktmpdir('tzinfo_test') do |dir|
          FileUtils.touch(File.join(dir, 'zone.tab'))
          FileUtils.touch(File.join(dir, 'iso3166.tab'))

          File.open(File.join(dir, 'Zone'), 'wb') do |file|
            file.write('NotAValidTZifFile')
          end

          data_source = ZoneinfoDataSource.new(dir)

          error = assert_raises(InvalidTimezoneIdentifier) do
            data_source.send(:load_timezone_info, 'Zone'.encode(encoding.name))
          end

          assert_match(/\bZone\b/, error.message)
          assert_equal(Encoding::UTF_8, error.message.encoding)
        end
      end

      define_method("test_load_timezone_info_invalid_file_2_with_#{encoding.to_method}_encoded_identifier") do
        Dir.mktmpdir('tzinfo_test') do |dir|
          FileUtils.touch(File.join(dir, 'zone.tab'))
          FileUtils.touch(File.join(dir, 'iso3166.tab'))

          zone = File.join(dir, 'Zone')

          File.open(File.join(@data_source.zoneinfo_dir, 'EST')) do |src|
            # Change header to TZif1 (which is not a valid header).
            File.open(zone, 'wb') do |dest|
              dest.write('TZif1')
              src.pos = 5
              FileUtils.copy_stream(src, dest)
            end
          end

          data_source = ZoneinfoDataSource.new(dir)

          error = assert_raises(InvalidTimezoneIdentifier) do
            data_source.send(:load_timezone_info, 'Zone'.encode(encoding.name))
          end

          assert_match(/\bZone\b/, error.message)
          assert_equal(Encoding::UTF_8, error.message.encoding)
        end
      end
    end

    def test_load_timezone_info_tainted
      skip_if_taint_is_undefined_or_no_op

      safe_test(unavailable: :skip) do
        identifier = 'Europe/Amsterdam'.dup.taint
        assert(identifier.tainted?)
        info = @data_source.send(:load_timezone_info, identifier)
        assert_equal('Europe/Amsterdam', info.identifier)
        assert(identifier.tainted?)
      end
    end

    def test_load_timezone_info_tainted_and_frozen
      skip_if_taint_is_undefined_or_no_op

      safe_test do
        info = @data_source.send(:load_timezone_info, 'Europe/Amsterdam'.dup.taint.freeze)
        assert_equal('Europe/Amsterdam', info.identifier)
      end
    end

    def test_load_timezone_info_tainted_zoneinfo_dir_safe_mode
      skip_if_taint_is_undefined_or_no_op

      safe_test(unavailable: :skip) do
        assert_raises(SecurityError) do
          ZoneinfoDataSource.new(@data_source.zoneinfo_dir.dup.taint)
        end
      end
    end

    def test_load_timezone_info_tainted_zoneinfo_dir
      skip_if_taint_is_undefined_or_no_op
      data_source = ZoneinfoDataSource.new(@data_source.zoneinfo_dir.dup.taint)
      info = data_source.send(:load_timezone_info, 'Europe/London')
      assert_kind_of(TransitionsDataTimezoneInfo, info)
      assert_equal('Europe/London', info.identifier)
    end

    def test_load_timezone_info_returned_identifier_frozen
      info = @data_source.send(:load_timezone_info, 'Europe/London')
      assert(info.identifier.frozen?)
    end

    def test_load_timezone_info_parameter_remains_unfrozen
      identifier = 'Europe/London'.dup
      info = @data_source.send(:load_timezone_info, identifier)
      assert_equal('Europe/London', info.identifier)
      refute_same(identifier, info.identifier)
      assert(!identifier.frozen?)
    end

    test_encodings('UTF-8', 'UTF-16', 'ISO-8859-1') do |encoding|
      define_method("test_load_timezone_info_with_#{encoding.to_method}_encoded_identifier") do
        identifier = 'Europe/London'.encode(encoding.name).freeze
        info = @data_source.send(:load_timezone_info, identifier)
        assert_equal('Europe/London', info.identifier)
      end
    end

    def test_get_timezone_info
      info = @data_source.get_timezone_info('Europe/London')
      assert_equal('Europe/London', info.identifier)
    end

    def get_timezone_filenames(directory)
      entries = Dir.glob(File.join(directory, '**', '*'))

      entries = entries.select do |file|
        RubyCoreSupport.untaint(file)
        File.file?(file)
      end

      entries = entries.collect {|file| file[directory.length + File::SEPARATOR.length, file.length - directory.length - File::SEPARATOR.length]}

      # Exclude right (with leapseconds) and posix (copy) directories; .tab files; leapseconds, localtime and posixrules files.
      entries = entries.select do |file|
        file !~ /\A(posix|right)\// &&
          file !~ /\.tab\z/ &&
          !%w(leapseconds localtime posixrules).include?(file)
      end

      entries.sort
    end

    def test_timezone_identifiers
      expected = get_timezone_filenames(@data_source.zoneinfo_dir).sort!
      all = @data_source.timezone_identifiers
      assert_kind_of(Array, all)
      assert_equal(expected, all)
      assert(all.frozen?)
      assert(all.all?(&:frozen?))
      assert(all.all? {|s| s.encoding == Encoding::UTF_8})
      assert_same(all, @data_source.timezone_identifiers)
    end

    def test_data_timezone_identifiers
      expected = get_timezone_filenames(@data_source.zoneinfo_dir).sort!
      all_data = @data_source.data_timezone_identifiers
      assert_kind_of(Array, all_data)
      assert_equal(expected, all_data)
      assert(all_data.frozen?)
      assert(all_data.all?(&:frozen?))
      assert(all_data.all? {|s| s.encoding == Encoding::UTF_8})
      assert_same(all_data, @data_source.data_timezone_identifiers)
    end

    def test_linked_timezone_identifiers
      all_linked = @data_source.linked_timezone_identifiers
      assert_kind_of(Array, all_linked)
      assert_equal([], all_linked)
      assert(all_linked.frozen?)
    end

    ['UTF-8', 'ISO-8859-1', nil].each do |encoding|
      define_method("test_timezone_identifiers_loaded_as_utf_8_when_internal_encoding_is_#{encoding ? encoding.downcase.gsub('-', '_') : 'nil'}") do
        Dir.mktmpdir('tzinfo_test') do |dir|
          FileUtils.touch(File.join(dir, 'zone.tab'))
          FileUtils.touch(File.join(dir, 'iso3166.tab'))

          subdir = File.join(dir, 'Subdir')
          FileUtils.mkdir(subdir)
          FileUtils.touch(File.join(subdir, 'Zone'))

          orig_verbose = $VERBOSE
          orig_default_internal_encoding = Encoding.default_internal

          begin
            $VERBOSE = nil
            Encoding.default_internal = encoding

            data_source = ZoneinfoDataSource.new(dir)
            all = data_source.timezone_identifiers
            assert_equal(1, all.length)
            assert_equal('Subdir/Zone', all[0])
            assert_equal(Encoding::UTF_8, all[0].encoding)
          ensure
            Encoding.default_internal = orig_default_internal_encoding
            $VERBOSE = orig_verbose
          end
        end
      end
    end

    ['UTF-8', 'ISO-8859-1'].each do |encoding|
      define_method("test_timezone_identifiers_loaded_when_zoneinfo_dir_encoded_as_#{encoding.downcase.gsub('-', '_')}") do
        Dir.mktmpdir('tzinfo_test') do |dir|
          FileUtils.touch(File.join(dir, 'zone.tab'))
          FileUtils.touch(File.join(dir, 'iso3166.tab'))

          subdir = File.join(dir, 'Subdir')
          FileUtils.mkdir(subdir)
          FileUtils.touch(File.join(subdir, 'Zone'))

          data_source = ZoneinfoDataSource.new(dir.encode(encoding))
          all = data_source.timezone_identifiers
          assert_equal(1, all.length)
          assert_equal('Subdir/Zone', all[0])
          assert_equal(Encoding::UTF_8, all[0].encoding)
        end
      end
    end

    def test_timezone_identifiers_excludes_those_that_cannot_be_utf8_encoded
      file_expand_path = ->(path) do
        raise "Unexpected path: #{path}" unless path == '/test'
        path
      end

      file_directory = ->(path) do
        path == '/test'
      end

      file_file = ->(path) do
        path =~ /\A\/test\/((iso3166|zone).tab|Zone[123])\z/
      end

      file_read = ->(path, options = {}) do
        raise "Unexpected path: #{path}" unless path == '/test/iso3166.tab' || path == '/test/zone.tab'
        ''
      end

      dir_foreach = ->(path, &block) do
        raise "Unexpected path: #{path}" unless path == '/test'
        block.call('Zone1'.dup.force_encoding(Encoding::UTF_16)) # Invalid, can't be converted.
        block.call('Zone2'.dup)
        block.call('Zone3'.dup.force_encoding(Encoding::UTF_16)) # Invalid, can't be converted.
      end

      File.stub(:expand_path, file_expand_path) do
        File.stub(:directory?, file_directory) do
          File.stub(:file?, file_file) do
            File.stub(:read, file_read) do
              Dir.stub(:foreach, dir_foreach) do
                data_source = ZoneinfoDataSource.new('/test')
                all = data_source.timezone_identifiers
                assert_equal(1, all.length)
                assert_equal('Zone2', all[0])
                assert_equal(Encoding::UTF_8, all[0].encoding)
              end
            end
          end
        end
      end
    end

    def test_timezone_identifiers_are_utf8_when_file_join_returns_non_utf8
      file_join = ->(*paths) do
        if paths.length == 2 && paths[0] == 'Test' && paths[1] == 'Zone'
          'Test/Zone'.encode(Encoding::ISO_8859_1)
        else
          File.__minitest_stub__join(*paths)
        end
      end

      Dir.mktmpdir('tzinfo_test') do |dir|
        FileUtils.touch(File.join(dir, 'zone.tab'))
        FileUtils.touch(File.join(dir, 'iso3166.tab'))
        test_path = File.join(dir, 'Test')
        FileUtils.mkdir(test_path)
        zone_path = File.join(test_path, 'Zone')
        FileUtils.touch(zone_path)

        File.stub(:join, file_join) do
          data_source = ZoneinfoDataSource.new(dir)
          assert_equal(['Test/Zone'], data_source.timezone_identifiers)
          assert_equal(Encoding::UTF_8, data_source.timezone_identifiers[0].encoding)
        end
      end
    end

    def test_timezone_identifiers_ignored_plus_version_file
      # Mac OS X includes a file named +VERSION containing the tzdata version.

      Dir.mktmpdir('tzinfo_test') do |dir|
        FileUtils.touch(File.join(dir, 'zone.tab'))
        FileUtils.touch(File.join(dir, 'iso3166.tab'))
        FileUtils.cp(File.join(@data_source.zoneinfo_dir, 'EST'), File.join(dir, 'EST'))

        File.open(File.join(dir, '+VERSION'), 'w') do |f|
          f.binmode
          f.write("2013a\n")
        end

        data_source = ZoneinfoDataSource.new(dir)
        assert_equal(['EST'], data_source.timezone_identifiers)
      end
    end

    def test_timezone_identifiers_ignored_timeconfig_symlink
      # Slackware includes a symlink named timeconfig that points at /usr/sbin/timeconfig.

      Dir.mktmpdir('tzinfo_test_target') do |target_dir|
        target_path = File.join(target_dir, 'timeconfig')

        File.open(target_path, 'w') do |f|
          f.write("#!/bin/sh\n")
          f.write("#\n")
          f.write('# timeconfig         Slackware Linux timezone configuration utility.\n')
        end

        Dir.mktmpdir('tzinfo_test') do |dir|
          FileUtils.touch(File.join(dir, 'zone.tab'))
          FileUtils.touch(File.join(dir, 'iso3166.tab'))
          FileUtils.cp(File.join(@data_source.zoneinfo_dir, 'EST'), File.join(dir, 'EST'))

          symlink_path = File.join(dir, 'timeconfig')
          begin
            FileUtils.ln_s(target_path, symlink_path)
          rescue NotImplementedError, Errno::EACCES
            # Symlinks not supported on this platform, or permission denied
            # (administrative rights are required on Windows). Copy instead.
            FileUtils.cp(target_path, symlink_path)
          end

          data_source = ZoneinfoDataSource.new(dir)
          assert_equal(['EST'], data_source.timezone_identifiers)
        end
      end
    end

    def test_timezone_identifiers_ignored_src_directory
      # Solaris includes a src directory containing the source timezone data files
      # from the tzdata distribution. These should be ignored.

      Dir.mktmpdir('tzinfo_test') do |dir|
        FileUtils.touch(File.join(dir, 'zone.tab'))
        FileUtils.touch(File.join(dir, 'iso3166.tab'))
        FileUtils.cp(File.join(@data_source.zoneinfo_dir, 'EST'), File.join(dir, 'EST'))

        src_dir = File.join(dir, 'src')
        FileUtils.mkdir(src_dir)

        File.open(File.join(src_dir, 'europe'), 'w') do |f|
          f.binmode
          f.write("Zone\tEurope/London\t0:00\tEU\tGMT/BST\n")
        end

        data_source = ZoneinfoDataSource.new(dir)
        assert_equal(['EST'], data_source.timezone_identifiers)
      end
    end

    def test_timezone_identifiers_ignored_security_file
      # The Arch linux tzdata package includes a file named SECURITY giving
      # instructions for reporting security-related bugs.

      Dir.mktmpdir('tzinfo_test') do |dir|
        FileUtils.touch(File.join(dir, 'zone.tab'))
        FileUtils.touch(File.join(dir, 'iso3166.tab'))
        FileUtils.cp(File.join(@data_source.zoneinfo_dir, 'EST'), File.join(dir, 'EST'))

        File.open(File.join(dir, 'SECURITY'), 'w') do |f|
          f.binmode
          f.write("Please report any sensitive security-related bugs...\n")
        end

        data_source = ZoneinfoDataSource.new(dir)
        assert_equal(['EST'], data_source.timezone_identifiers)
      end
    end

    def test_load_country_info
      info = @data_source.send(:load_country_info, 'GB')
      assert_equal('GB', info.code)
      assert_equal('Britain (UK)', info.name)
    end

    def test_load_country_info_not_exist
      error = assert_raises(InvalidCountryCode) do
        @data_source.send(:load_country_info, 'ZZ')
      end

      assert_match(/\bZZ\b/, error.message)
    end

    def test_load_country_info_invalid
      error = assert_raises(InvalidCountryCode) do
        @data_source.send(:load_country_info, '../Countries/GB')
      end

      assert_match(/\W\.\.\/Countries\/GB\b/, error.message)
    end

    def test_load_country_info_nil
      error = assert_raises(InvalidCountryCode) do
        @data_source.send(:load_country_info, nil)
      end

      assert_match(/\bnil\b/, error.message)
    end

    def test_load_country_info_false
      error = assert_raises(InvalidCountryCode) do
        @data_source.send(:load_country_info, false)
      end

      assert_match(/\bfalse\b/, error.message)
    end

    def test_load_country_info_case
      error = assert_raises(InvalidCountryCode) do
        @data_source.send(:load_country_info, 'gb')
      end

      assert_match(/\bgb\b/, error.message)
    end

    def test_load_country_info_tainted
      skip_if_taint_is_undefined_or_no_op

      safe_test(unavailable: :skip) do
        code = 'NL'.dup.taint
        assert(code.tainted?)
        info = @data_source.send(:load_country_info, code)
        assert_equal('NL', info.code)
        assert(code.tainted?)
      end
    end

    def test_load_country_info_returned_strings_frozen
      info = @data_source.send(:load_country_info, 'US')
      assert(info.code.frozen?)
      assert(info.name.frozen?)
      assert(info.zones.map(&:identifier).all?(&:frozen?))
      assert(info.zones.map(&:description).all?(&:frozen?))
    end

    def test_load_country_info_check_zones
      Dir.mktmpdir('tzinfo_test') do |dir|
        File.open(File.join(dir, 'iso3166.tab'), 'w', external_encoding: Encoding::UTF_8) do |iso3166|
          iso3166.puts('# iso3166.tab')
          iso3166.puts('')
          iso3166.puts("FC\tFake Country")
          iso3166.puts("OC\tOther Country")
        end

        File.open(File.join(dir, 'zone.tab'), 'w', external_encoding: Encoding::UTF_8) do |zone|
          zone.puts('# zone.tab')
          zone.puts('')
          zone.puts("FC\t+513030-0000731\tFake/One\tDescription of one")
          zone.puts("FC\t+353916+1394441\tFake/Two\tAnother description")
          zone.puts("FC\t-2332-04637\tFake/Three\tThis is Three")
          zone.puts("OC\t+5005+01426\tOther/One")
        end

        data_source = ZoneinfoDataSource.new(dir)

        info = data_source.send(:load_country_info, 'FC')
        assert_equal('FC', info.code)
        assert_equal('Fake Country', info.name)
        assert_equal([
          CountryTimezone.new('Fake/One', Rational(6181, 120), Rational(-451, 3600), 'Description of one'),
          CountryTimezone.new('Fake/Two', Rational(32089, 900), Rational(503081, 3600), 'Another description'),
          CountryTimezone.new('Fake/Three', Rational(-353, 15), Rational(-2797, 60), 'This is Three')], info.zones)
        assert_equal(true, info.zones.frozen?)

        info = data_source.send(:load_country_info, 'OC')
        assert_equal('OC', info.code)
        assert_equal('Other Country', info.name)
        assert_equal([CountryTimezone.new('Other/One', Rational(601, 12), Rational(433, 30))], info.zones)
        assert_equal(true, info.zones.frozen?)
      end
    end

    def test_load_country_info_check_zones_zone1970
      Dir.mktmpdir('tzinfo_test') do |dir|
        File.open(File.join(dir, 'iso3166.tab'), 'w', external_encoding: Encoding::UTF_8) do |iso3166|
          iso3166.puts('# iso3166.tab')
          iso3166.puts('')
          iso3166.puts("AC\tAnother Country")
          iso3166.puts("FC\tFake Country")
          iso3166.puts("OC\tOther Country")
        end

        # zone.tab will be ignored.
        File.open(File.join(dir, 'zone.tab'), 'w', external_encoding: Encoding::UTF_8) do |zone|
          zone.puts('# zone.tab')
          zone.puts('')
          zone.puts("FC\t+513030-0000731\tFake/One\tDescription of one")
          zone.puts("FC\t+353916+1394441\tFake/Two\tAnother description")
          zone.puts("FC\t-2332-04637\tFake/Three\tThis is Three")
          zone.puts("OC\t+5005+01426\tOther/One")
        end

        # zone1970.tab will be used.
        File.open(File.join(dir, 'zone1970.tab'), 'w', external_encoding: Encoding::UTF_8) do |zone|
          zone.puts('# zone1970.tab')
          zone.puts('')
          zone.puts("AC,OC\t+0000+00000\tMiddle/Another/One\tAnother's One")
          zone.puts("FC\t+513030-0000731\tFake/One\tDescription of one")
          zone.puts("FC,OC\t+353916+1394441\tFake/Two\tAnother description")
          zone.puts("FC,OC\t-2332-04637\tFake/Three\tThis is Three")
          zone.puts("OC\t+5005+01426\tOther/One")
          zone.puts("OC\t+5015+11426\tOther/Two")
        end

        data_source = ZoneinfoDataSource.new(dir)

        info = data_source.send(:load_country_info, 'AC')
        assert_equal('AC', info.code)
        assert_equal('Another Country', info.name)
        assert_equal([CountryTimezone.new('Middle/Another/One', Rational(0, 1), Rational(0, 1), "Another's One")], info.zones)
        assert_equal(true, info.zones.frozen?)

        info = data_source.send(:load_country_info, 'FC')
        assert_equal('FC', info.code)
        assert_equal('Fake Country', info.name)
        assert_equal([
          CountryTimezone.new('Fake/One', Rational(6181, 120), Rational(-451, 3600), 'Description of one'),
          CountryTimezone.new('Fake/Two', Rational(32089, 900), Rational(503081, 3600), 'Another description'),
          CountryTimezone.new('Fake/Three', Rational(-353, 15), Rational(-2797, 60), 'This is Three')], info.zones)
        assert_equal(true, info.zones.frozen?)

        # Testing the ordering of zones. A zone can either be primary (country
        # code is the first in the first column), or secondary (country code is
        # not the first). Should return all the primaries first in the order they
        # appeared in the file, followed by all the secondaries in the order they
        # appeared in file.

        info = data_source.send(:load_country_info, 'OC')
        assert_equal('OC', info.code)
        assert_equal('Other Country', info.name)
        assert_equal([
          CountryTimezone.new('Other/One', Rational(601, 12), Rational( 433, 30)),
          CountryTimezone.new('Other/Two', Rational(201,  4), Rational(3433, 30)),
          CountryTimezone.new('Middle/Another/One', Rational(0, 1), Rational(0, 1), "Another's One"),
          CountryTimezone.new('Fake/Two', Rational(32089, 900), Rational(503081, 3600), 'Another description'),
          CountryTimezone.new('Fake/Three', Rational(-353, 15), Rational(-2797, 60), 'This is Three')], info.zones)
        assert_equal(true, info.zones.frozen?)
      end
    end

    def test_load_country_info_check_zones_solaris_tab_files
      # Solaris uses 5 columns instead of the usual 4 in zone_sun.tab.
      # An extra column before the comment gives an optional linked/alternate
      # timezone identifier (or '-' if not set).
      #
      # Additionally, there is a section at the end of the file for timezones
      # covering regions. These are given lower-case "country" codes. The timezone
      # identifier column refers to a continent instead of an identifier. These
      # lines will be ignored by TZInfo.

      Dir.mktmpdir('tzinfo_test') do |dir|
        tab_dir = File.join(dir, 'tab')
        FileUtils.mkdir(tab_dir)

        File.open(File.join(tab_dir, 'country.tab'), 'w', external_encoding: Encoding::UTF_8) do |country|
          country.puts('# country.tab')
          country.puts('# Solaris')
          country.puts("FC\tFake Country")
          country.puts("OC\tOther Country")
        end

        File.open(File.join(tab_dir, 'zone_sun.tab'), 'w', external_encoding: Encoding::UTF_8) do |zone_sun|
          zone_sun.puts('# zone_sun.tab')
          zone_sun.puts('# Solaris')
          zone_sun.puts('# Countries')
          zone_sun.puts("FC\t+513030-0000731\tFake/One\t-\tDescription of one")
          zone_sun.puts("FC\t+353916+1394441\tFake/Two\tFake/Alias/Two\tAnother description")
          zone_sun.puts("FC\t-2332-04637\tFake/Three\tFake/Alias/Three\tThis is Three")
          zone_sun.puts("OC\t+5005+01426\tOther/One\tOther/Linked/One")
          zone_sun.puts("OC\t+5015+01436\tOther/Two\t-")
          zone_sun.puts('# Regions')
          zone_sun.puts("ee\t+0000+00000\tEurope/\tEET")
          zone_sun.puts("me\t+0000+00000\tEurope/\tMET")
          zone_sun.puts("we\t+0000+00000\tEurope/\tWET")
        end

        data_source = ZoneinfoDataSource.new(dir)

        info = data_source.send(:load_country_info, 'FC')
        assert_equal('FC', info.code)
        assert_equal('Fake Country', info.name)
        assert_equal([
          CountryTimezone.new('Fake/One', Rational(6181, 120), Rational(-451, 3600), 'Description of one'),
          CountryTimezone.new('Fake/Two', Rational(32089, 900), Rational(503081, 3600), 'Another description'),
          CountryTimezone.new('Fake/Three', Rational(-353, 15), Rational(-2797, 60), 'This is Three')], info.zones)
        assert_equal(true, info.zones.frozen?)

        info = data_source.send(:load_country_info, 'OC')
        assert_equal('OC', info.code)
        assert_equal('Other Country', info.name)
        assert_equal([
          CountryTimezone.new('Other/One', Rational(601, 12), Rational(433, 30)),
          CountryTimezone.new('Other/Two', Rational(201, 4), Rational(73, 5))], info.zones)
        assert_equal(true, info.zones.frozen?)
      end
    end

    def test_load_country_info_check_zones_alternate_iso3166_file
      Dir.mktmpdir('tzinfo_test') do |dir|
        zoneinfo_dir = File.join(dir, 'zoneinfo')
        tab_dir = File.join(dir, 'tab')
        FileUtils.mkdir(zoneinfo_dir)
        FileUtils.mkdir(tab_dir)

        tab_file = File.join(tab_dir, 'iso3166')
        File.open(tab_file, 'w', external_encoding: Encoding::UTF_8) do |iso3166|
          # Use the BSD 4 column format (alternate iso3166 is used on BSD).
          iso3166.puts("FC\tFCC\t001\tFake Country")
          iso3166.puts("OC\tOCC\t002\tOther Country")
        end

        File.open(File.join(zoneinfo_dir, 'zone.tab'), 'w', external_encoding: Encoding::UTF_8) do |zone|
          zone.puts("FC\t+513030-0000731\tFake/One\tDescription of one")
          zone.puts("FC\t+353916+1394441\tFake/Two\tAnother description")
          zone.puts("FC\t-2332-04637\tFake/Three\tThis is Three")
          zone.puts("OC\t+5005+01426\tOther/One")
        end

        data_source = ZoneinfoDataSource.new(zoneinfo_dir, tab_file)

        info = data_source.send(:load_country_info, 'FC')
        assert_equal('FC', info.code)
        assert_equal('Fake Country', info.name)
        assert_equal([
          CountryTimezone.new('Fake/One', Rational(6181, 120), Rational(-451, 3600), 'Description of one'),
          CountryTimezone.new('Fake/Two', Rational(32089, 900), Rational(503081, 3600), 'Another description'),
          CountryTimezone.new('Fake/Three', Rational(-353, 15), Rational(-2797, 60), 'This is Three')], info.zones)
        assert_equal(true, info.zones.frozen?)

        info = data_source.send(:load_country_info, 'OC')
        assert_equal('OC', info.code)
        assert_equal('Other Country', info.name)
        assert_equal([CountryTimezone.new('Other/One', Rational(601, 12), Rational(433, 30))], info.zones)
        assert_equal(true, info.zones.frozen?)
      end
    end

    def test_load_country_info_four_column_iso31611
      # OpenBSD and FreeBSD use a 4 column iso3166.tab file that includes
      # alpha-3 and numeric-3 codes in addition to the alpha-2 and name in the
      # tz database version.

      Dir.mktmpdir('tzinfo_test') do |dir|
        File.open(File.join(dir, 'iso3166.tab'), 'w', external_encoding: Encoding::UTF_8) do |iso3166|
          iso3166.puts("FC\tFCC\t001\tFake Country")
          iso3166.puts("OC\tOCC\t002\tOther Country")
        end

        File.open(File.join(dir, 'zone.tab'), 'w', external_encoding: Encoding::UTF_8) do |zone|
          zone.puts("FC\t+513030-0000731\tFake/One\tDescription of one")
          zone.puts("OC\t+5005+01426\tOther/One")
        end

        data_source = ZoneinfoDataSource.new(dir)

        info = data_source.send(:load_country_info, 'FC')
        assert_equal('FC', info.code)
        assert_equal('Fake Country', info.name)

        info = data_source.send(:load_country_info, 'OC')
        assert_equal('OC', info.code)
        assert_equal('Other Country', info.name)
      end
    end

    def test_load_country_info_utf8
      # iso3166.tab is currently in ASCII (as of tzdata 2014f), but will be
      # changed to UTF-8 in the future.

      # zone.tab is in ASCII, with no plans to change. Since ASCII is a subset of
      # UTF-8, test that this is loaded in UTF-8 anyway.

      Dir.mktmpdir('tzinfo_test') do |dir|
        File.open(File.join(dir, 'iso3166.tab'), 'w', external_encoding: Encoding::UTF_8) do |iso3166|
          iso3166.puts("UT\tUnicode Test ✓")
        end

        File.open(File.join(dir, 'zone.tab'), 'w', external_encoding: Encoding::UTF_8) do |zone|
          zone.puts("UT\t+513030-0000731\tUnicode✓/One\tUnicode Description ✓")
        end

        data_source = ZoneinfoDataSource.new(dir)

        info = data_source.send(:load_country_info, 'UT')
        assert_equal('UT', info.code)
        assert_equal('Unicode Test ✓', info.name)
        assert_equal([CountryTimezone.new('Unicode✓/One', Rational(6181, 120), Rational(-451, 3600), 'Unicode Description ✓')], info.zones)
      end
    end

    def test_load_country_info_utf8_zone1970
      # iso3166.tab is currently in ASCII (as of tzdata 2014f), but will be
      # changed to UTF-8 in the future.

      # zone1970.tab is in UTF-8.

      Dir.mktmpdir('tzinfo_test') do |dir|
        File.open(File.join(dir, 'iso3166.tab'), 'w', external_encoding: Encoding::UTF_8) do |iso3166|
          iso3166.puts("UT\tUnicode Test ✓")
        end

        File.open(File.join(dir, 'zone1970.tab'), 'w', external_encoding: Encoding::UTF_8) do |zone|
          zone.puts("UT\t+513030-0000731\tUnicode✓/One\tUnicode Description ✓")
        end

        data_source = ZoneinfoDataSource.new(dir)

        info = data_source.send(:load_country_info, 'UT')
        assert_equal('UT', info.code)
        assert_equal('Unicode Test ✓', info.name)
        assert_equal([CountryTimezone.new('Unicode✓/One', Rational(6181, 120), Rational(-451, 3600), 'Unicode Description ✓')], info.zones)
      end
    end

    test_encodings('UTF-8', 'UTF-16', 'ISO-8859-1') do |encoding|
      define_method("test_load_country_info_with_#{encoding.to_method}_encoded_code") do
        code = 'GB'.encode(encoding.name).freeze
        info = @data_source.send(:load_country_info, code)
        assert_equal('GB', info.code)
      end
    end

    def test_get_country_info
      info = @data_source.get_country_info('GB')
      assert_equal('GB', info.code)
    end

    def test_country_codes
      file_codes = []

      file = File.read(File.join(@data_source.zoneinfo_dir, 'iso3166.tab'), external_encoding: Encoding::UTF_8, internal_encoding: Encoding::UTF_8)
      file.each_line do |line|
        line.chomp!
        file_codes << $1 if line =~ /\A([A-Z]{2})\t/
      end

      file_codes.sort!

      codes = @data_source.country_codes
      assert_kind_of(Array, codes)
      assert_equal(file_codes, codes)
      assert(codes.frozen?)
      assert(codes.all?(&:frozen?))
      assert_same(codes, @data_source.country_codes)
    end

    def test_country_codes_four_column_iso3166
      # OpenBSD and FreeBSD use a 4 column iso3166.tab file that includes
      # alpha-3 and numeric-3 codes in addition to the alpha-2 and name in the
      # tz database version.

      Dir.mktmpdir('tzinfo_test') do |dir|
        File.open(File.join(dir, 'iso3166.tab'), 'w', external_encoding: Encoding::UTF_8) do |iso3166|
          iso3166.puts("FC\tFCC\t001\tFake Country")
          iso3166.puts("OC\tOCC\t002\tOther Country")
        end

        File.open(File.join(dir, 'zone.tab'), 'w', external_encoding: Encoding::UTF_8) do |zone|
          zone.puts("FC\t+513030-0000731\tFake/One\tDescription of one")
          zone.puts("OC\t+5005+01426\tOther/One")
        end

        data_source = ZoneinfoDataSource.new(dir)

        codes = data_source.country_codes
        assert_equal(%w(FC OC), codes)
      end
    end

    def test_to_s
      assert_equal("Zoneinfo DataSource: #{ZONEINFO_DIR}", @data_source.to_s)
    end

    def test_inspect
      assert_equal("#<TZInfo::DataSources::ZoneinfoDataSource: #{ZONEINFO_DIR}>", @data_source.inspect)
    end

    def test_country_info_strings_deduped
      Dir.mktmpdir('tzinfo_test') do |dir|
        File.open(File.join(dir, 'iso3166.tab'), 'w', external_encoding: Encoding::UTF_8) do |iso3166|
          iso3166.puts("FC\tFake Country")
          iso3166.puts("OC\tOther Country")
        end

        File.open(File.join(dir, 'zone.tab'), 'w', external_encoding: Encoding::UTF_8) do |zone|
          zone.puts("FC\t+513030-0000731\tFake/One\tDescription of onex")
          zone.puts("OC\t+513030-0000731\tFake/One\tDescription of onex")
          zone.puts("OC,FC\t+513030-0000731\tFake/Two\tDescription of two")
        end

        fake_dir = File.join(dir, 'Fake')
        FileUtils.mkdir(fake_dir)
        FileUtils.touch(File.join(fake_dir, 'Two'))

        data_source = ZoneinfoDataSource.new(dir)

        fc = data_source.send(:load_country_info, 'FC')
        oc = data_source.send(:load_country_info, 'OC')

        assert_same(fc.zones[0].identifier, oc.zones[0].identifier)
        assert_same(fc.zones[0].description, oc.zones[0].description)
        assert_same(fc.zones[1].identifier, oc.zones[1].identifier)
        assert_same(fc.zones[1].description, oc.zones[1].description)

        assert_same(data_source.timezone_identifiers[0], fc.zones[1].identifier)
      end

      def test_timezone_abbreviations_deduped
        london = @data_source.load_timezone_info('Europe/London').transitions[0].previous_offset
        new_york = @data_source.load_timezone_info('America/New_York').transitions[0].previous_offset

        assert_equal('LMT', london)
        assert_same(london, new_york)
      end
    end

    private

    def assert_raises_directory_not_found(&block)
      error = assert_raises(ZoneinfoDirectoryNotFound, &block)
      assert_equal('None of the paths included in TZInfo::DataSources::ZoneinfoDataSource.search_path are valid zoneinfo directories.', error.message)
    end

    def assert_raises_invalid_directory(zoneinfo_dir, &block)
      error = assert_raises(InvalidZoneinfoDirectory, &block)
      assert_equal("#{zoneinfo_dir} is not a directory or doesn't contain a iso3166.tab file and a zone1970.tab or zone.tab file.", error.message)
    end

    def get_expected_file_open_and_read_cause(path)
      expected_error = assert_raises(SystemCallError) do
        File.open(path, 'r') {|f| f.read(1) }
      end
      expected_error.class
    end
  end
end
