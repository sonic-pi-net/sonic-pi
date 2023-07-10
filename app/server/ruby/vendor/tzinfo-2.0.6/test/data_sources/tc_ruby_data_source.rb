# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'

module DataSources
  class TCRubyDataSource < Minitest::Test
    include TZInfo
    include TZInfo::DataSources

    def setup
      @data_source = RubyDataSource.new
    end

    def test_initialize_not_found
      code = <<-EOF
        begin
          ds = TZInfo::DataSources::RubyDataSource.new
          puts "No exception raised, returned \#{ds} instead"
        rescue Exception => e
          puts e.class
          puts e.message
        end
      EOF

      assert_sub_process_returns([
        'TZInfo::DataSources::TZInfoDataNotFound',
        'The tzinfo-data gem could not be found (require \'tzinfo/data\' failed).'], code)
    end

    def test_load_timezone_info_data
      info = @data_source.send(:load_timezone_info, 'Europe/London')
      assert_kind_of(DataTimezoneInfo, info)
      assert_equal('Europe/London', info.identifier)
    end

    def test_load_timezone_info_linked
      info = @data_source.send(:load_timezone_info, 'UTC')
      assert_kind_of(LinkedTimezoneInfo, info)
      assert_equal('UTC', info.identifier)
      assert_equal('Etc/UTC', info.link_to_identifier)
    end

    def test_load_timezone_info_does_not_exist
      error = assert_raises(InvalidTimezoneIdentifier) do
        @data_source.send(:load_timezone_info, 'Nowhere/Special')
      end

      assert_match(/\bNowhere\/Special\b/, error.message)
    end

    test_encodings('UTF-8', 'UTF-16').each do |encoding|
      define_method("test_load_timezone_info_in_index_but_file_does_not_exist_with_#{encoding.to_method}_encoded_identifier") do
        error = assert_raises(InvalidTimezoneIdentifier) do
          @data_source.send(:load_timezone_info, 'Europe/Berlin'.encode(encoding.name))
        end

        assert_match(/\bEurope\/Berlin\b/, error.message)
        assert_equal(Encoding::UTF_8, error.message.encoding)
        assert_kind_of(LoadError, error.cause) if error.respond_to?(:cause)
      end

      define_method("test_load_timezone_info_module_names_do_not_match_zone_with_#{encoding.to_method}_encoded_identifier") do
        def @data_source.validate_timezone_identifier(identifier)
          identifier = identifier.encode(Encoding::UTF_8)
          raise "Unexpected identifier passed to validate_timezone_identifier: #{identifier}" unless identifier == 'Invalid/Incorrect_Module'
          'Invalid/Incorrect_Module'.freeze
        end

        error = assert_raises(InvalidTimezoneIdentifier) do
          @data_source.send(:load_timezone_info, 'Invalid/Incorrect_Module'.encode(encoding.name))
        end

        assert_match(/\bInvalid\/Incorrect_Module\b/, error.message)
        assert_kind_of(NameError, error.cause) if error.respond_to?(:cause)
      end
    end

    def test_load_timezone_info_invalid
      error = assert_raises(InvalidTimezoneIdentifier) do
        @data_source.send(:load_timezone_info, '../definitions/UTC')
      end

      assert_match(/\W\.\.\/definitions\/UTC\b/, error.message)
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

      assert_match(/\beurope\/london\b/, error.message)
    end

    def test_load_timezone_info_plus
      info = @data_source.send(:load_timezone_info, 'Etc/GMT+1')
      assert_equal('Etc/GMT+1', info.identifier)
    end

    def test_load_timezone_info_minus
      info = @data_source.send(:load_timezone_info, 'Etc/GMT-1')
      assert_equal('Etc/GMT-1', info.identifier)
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

    def test_get_timezone_info
      info = @data_source.get_timezone_info('Europe/London')
      assert_equal('Europe/London', info.identifier)
    end

    test_encodings('UTF-8', 'UTF-16', 'ISO-8859-1') do |encoding|
      define_method("test_load_timezone_info_with_#{encoding.to_method}_encoded_identifier") do
        identifier = 'Europe/London'.encode(encoding.name).freeze
        info = @data_source.send(:load_timezone_info, identifier)
        assert_equal('Europe/London', info.identifier)
      end
    end

    def test_timezone_identifiers
      all = @data_source.timezone_identifiers
      assert_equal((TZInfo::Data::Indexes::Timezones.data_timezones + TZInfo::Data::Indexes::Timezones.linked_timezones).sort, all)
      assert(all.frozen?)
      assert(all.all?(&:frozen?))
    end

    def test_data_timezone_identifiers
      all_data = @data_source.data_timezone_identifiers
      assert_same(TZInfo::Data::Indexes::Timezones.data_timezones, all_data)
      assert(all_data.frozen?)
      assert(all_data.all?(&:frozen?))
    end

    def test_linked_timezone_identifiers
      all_linked = @data_source.linked_timezone_identifiers
      assert_same(TZInfo::Data::Indexes::Timezones.linked_timezones, all_linked)
      assert(all_linked.frozen?)
      assert(all_linked.all?(&:frozen?))
    end

    def test_load_country_info
      info = @data_source.send(:load_country_info, 'GB')
      assert_equal('GB', info.code)
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

    def test_load_country_info_tainted_and_frozen
      skip_if_taint_is_undefined_or_no_op

      safe_test do
        info = @data_source.send(:load_country_info, 'NL'.dup.taint.freeze)
        assert_equal('NL', info.code)
      end
    end

    def test_load_country_info_returned_strings_frozen
      info = @data_source.send(:load_country_info, 'US')
      assert(info.code.frozen?)
      assert(info.name.frozen?)
      assert(info.zones.map(&:identifier).all?(&:frozen?))
      assert(info.zones.map(&:description).all?(&:frozen?))
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
      codes = @data_source.country_codes
      assert_equal(TZInfo::Data::Indexes::Countries.countries.keys.sort, codes)
      assert(codes.frozen?)
      assert(codes.all?(&:frozen?))
      assert_same(codes, @data_source.country_codes)
    end

    def test_to_s
      assert_equal("Ruby DataSource: tzdb v2020d, tzinfo-data v#{defined?(TZINFO_TEST_DATA_FORMAT) ? TZINFO_TEST_DATA_FORMAT : 2}.2020.4.test", @data_source.to_s)
    end

    def test_inspect
      assert_equal("#<TZInfo::DataSources::RubyDataSource: tzdb v2020d, tzinfo-data v#{defined?(TZINFO_TEST_DATA_FORMAT) ? TZINFO_TEST_DATA_FORMAT : 2}.2020.4.test>", @data_source.inspect)
    end

    if defined?(TZINFO_TEST_DATA_FORMAT) && TZINFO_TEST_DATA_FORMAT == 1
      # The TZInfo::Data::VERSION and TZInfo::Data::Version::STRING constants
      # are only available from v1.2014.8 onwards.

      def test_to_s_no_tzinfo_data_version
        code = <<-EOF
          $:.unshift('#{TZINFO_TEST_DATA_DIR}')
          require 'tzinfo/data'
          TZInfo::Data.send(:remove_const, :VERSION)
          TZInfo::Data::Version.send(:remove_const, :STRING)
          ds = TZInfo::DataSources::RubyDataSource.new
          puts ds.to_s
        EOF

        assert_sub_process_returns(['Ruby DataSource: tzdb v2020d'], code)
      end

      def test_inspect_no_tzinfo_data_version
        code = <<-EOF
          $:.unshift('#{TZINFO_TEST_DATA_DIR}')
          require 'tzinfo/data'
          TZInfo::Data.send(:remove_const, :VERSION)
          TZInfo::Data::Version.send(:remove_const, :STRING)
          ds = TZInfo::DataSources::RubyDataSource.new
          puts ds.inspect
        EOF

        assert_sub_process_returns(['#<TZInfo::DataSources::RubyDataSource: tzdb v2020d>'], code)
      end
    end

    def test_tzinfo_data_not_found_in_loaded_features
      code = <<-EOF
        def require(name)
          result = super
          if name == 'tzinfo/data'
            $".delete_if {|p| p.end_with?(File.join('', 'tzinfo', 'data.rb')) }
          end
          result
        end

        begin
          $:.unshift('#{TZINFO_TEST_DATA_DIR}')
          ds = TZInfo::DataSources::RubyDataSource.new
          c = ds.get_country_info('GB')
          puts c.code
          tz = ds.get_timezone_info('Europe/London')
          puts tz.identifier
        rescue Exception => e
          puts e.class
          puts e.message
          puts e.backtrace
        end
      EOF

      assert_sub_process_returns(['GB', 'Europe/London'], code)
    end
  end
end
