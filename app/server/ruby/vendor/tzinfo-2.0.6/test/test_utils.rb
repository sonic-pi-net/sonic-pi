# encoding: UTF-8
# frozen_string_literal: true

raise 'Tests must be run with bundler, e.g. bundle exec rake test' unless defined?(Bundler)

# Workaround an issue on JRuby where the stdlib directory can end up before the
# minitest gem in the load path. This causes an old built-in version of minitest
# to be loaded instead of the gem. https://github.com/jruby/jruby/issues/5038
if RUBY_ENGINE == 'jruby'
  stdlib_index = $:.find_index(RbConfig::CONFIG['rubylibdir'])
  minitest_index = $:.find_index(File.join(Bundler.rubygems.find_name('minitest').first.full_gem_path, 'lib'))

  if stdlib_index < minitest_index
    # The stdlib directory is before the minitest gem. Move stdlib to the end of
    # the load path.
    $:.push($:.delete_at(stdlib_index))
  end
end

COVERAGE_ENABLED = ENV['TEST_COVERAGE'] == '1'

if COVERAGE_ENABLED && defined?(COVERAGE_TYPE)
  string_unary_minus_does_dedupe = if '0'.respond_to?(:-@)
    s1 = -('0'.dup)
    s2 = -('0'.dup)
    s1.object_id == s2.object_id
  else
    false
  end

  untaint_is_functional = Object.new.respond_to?(:untaint) && RUBY_VERSION < '2.7'

  feature_support = [
    ['deduping_string_unary_minus', string_unary_minus_does_dedupe],
    ['functional_untaint', untaint_is_functional]
  ].map do |feature, available|
    "#{available ? '' : 'no_'}#{feature}"
  end

  method_support = [[[], :bsearch], [[], :bsearch_index], ['', :-@]].map do |object, method|
    "#{object.respond_to?(method) ? '' : 'no_'}#{Regexp.escape(object.class.name.downcase)}_#{Regexp.escape(method)}"
  end

  COVERAGE_NOCOV_TOKEN = "nocov_(#{(feature_support + method_support).join('|')})"

  require 'simplecov'

  SimpleCov.start do
    command_name COVERAGE_TYPE
    add_filter 'test'
    nocov_token COVERAGE_NOCOV_TOKEN
    project_name 'TZInfo'
    self.formatters = [SimpleCov::Formatter::HTMLFormatter]
  end
end

tests_dir = File.expand_path(File.dirname(__FILE__))
tests_dir.untaint if RUBY_VERSION < '2.7'
TESTS_DIR = tests_dir
TZINFO_TEST_ZONEINFO_DIR = File.join(TESTS_DIR, 'zoneinfo')

unless defined? TZINFO_TEST_DATA_DIR
  TZINFO_TEST_DATA_DIR = File.join(TESTS_DIR, "tzinfo-data#{(defined? TZINFO_TEST_DATA_FORMAT) ? TZINFO_TEST_DATA_FORMAT : 2}")
  $:.unshift(TZINFO_TEST_DATA_DIR) unless $:.include?(TZINFO_TEST_DATA_DIR)
end

def nil.freeze
  warn('nil is being frozen')
  super
end

require 'minitest/autorun'
require 'tzinfo'
require 'date'
require 'fileutils'
require 'rbconfig'

module TestUtils
  ZONEINFO_SYMLINKS = [
    ['localtime', File.join('America', 'New_York')],
    ['UTC', File.join('Etc', 'UTC')]]


  def self.prepare_test_zoneinfo_dir
    ZONEINFO_SYMLINKS.each do |file, target|
      path = File.join(TZINFO_TEST_ZONEINFO_DIR, file)

      File.delete(path) if File.exist?(path)

      begin
        FileUtils.ln_s(target, path)
      rescue NotImplementedError, Errno::EACCES
        # Symlinks not supported on this platform, or permission denied
        # (administrative rights are required on Windows). Copy instead.
        target_path = File.join(TZINFO_TEST_ZONEINFO_DIR, target)
        FileUtils.cp(target_path, path)
      end
    end
  end

  # A helper class used to define test methods for different encodings.
  class TestEncoding
    attr_reader :name

    def initialize(name)
      @name = name
    end

    def find
      Encoding.find(@name)
    end

    def to_method
      @name.downcase.gsub('-', '_')
    end
  end

  # With MRI and Rubinius, a Time constructed with a zero offset will have
  # Time#utc? == false. utc? only returns true for Times explicitly initialized
  # as UTC (e.g with Time.utc). JRuby's Time#utc? returns true when the offset
  # is zero.
  TIME_SUPPORTS_DISTINCT_UTC = !Time.new(2017,1,1,0,0,0,0).utc?

  # Test class to simulate Ruby 2.6's internal Time::TM.
  # Timestamp.for will accept objects that have value, subsec and optionally
  # utc_offset methods and return Time results.
  class TimeLike
    attr_reader :subsec

    def initialize(value, subsec)
      @value = value
      @subsec = subsec
    end

    def to_i
      @value
    end
  end

  # An extended version of TimeLike that adds a utc_offset attribute.
  class TimeLikeWithOffset < TimeLike
    attr_reader :utc_offset

    def initialize(value, subsec, utc_offset)
      super(value, subsec)
      @utc_offset = utc_offset
    end
  end

  class TimeTypesHelper
    def supports?(feature)
      self.class.supports?(feature)
    end

    def expected_zero_offset
      # On JRuby Time#utc? is always true when utc_offset is 0 (but not on MRI or Rubinius).
      supports?(:utc) && !supports?(:distinct_utc) ? :utc : 0
    end
  end

  class TimeTypesOutputHelper < TimeTypesHelper
    def self.supports?(feature)
      feature == :output || feature == :offset
    end

    def output_time(*args)
      time(*args)
    end
  end

  class TimeTypesTimeHelper < TimeTypesOutputHelper
    def self.supports?(feature)
      super(feature) || (feature == :distinct_utc && TIME_SUPPORTS_DISTINCT_UTC) || feature == :utc
    end

    def type
      :time
    end

    def time(year, month, day, hour, minute, second, sub_second = 0, utc_offset = 0)
      if utc_offset == :utc
        Time.utc(year, month, day, hour, minute, second + sub_second)
      else
        Time.new(year, month, day, hour, minute, second + sub_second, utc_offset)
      end
    end

    def time_with_offset(period, year, month, day, hour, minute, second, sub_second = 0)
      TZInfo::TimeWithOffset.new(year, month, day, hour, minute, second + sub_second, period.observed_utc_offset).set_timezone_offset(period)
    end
  end

  class TimeTypesDateTimeHelper < TimeTypesOutputHelper
    def type
      :datetime
    end

    def time(year, month, day, hour, minute, second, sub_second = 0, utc_offset = 0)
      utc_offset = 0 if utc_offset == :utc
      DateTime.new(year, month, day, hour, minute, second + sub_second, utc_offset.to_r / 86400)
    end

    def time_with_offset(offset, year, month, day, hour, minute, second, sub_second = 0)
      TZInfo::DateTimeWithOffset.new(year, month, day, hour, minute, second + sub_second, offset.observed_utc_offset.to_r / 86400).set_timezone_offset(offset)
    end
  end

  class TimeTypesTimestampHelper < TimeTypesOutputHelper
    def self.supports?(feature)
      super(feature) || feature == :distinct_utc || feature == :unspecified_offset || feature == :utc
    end

    def type
      :timestamp
    end

    def time(year, month, day, hour, minute, second, sub_second = 0, utc_offset = nil)
      TZInfo::Timestamp.create(year, month, day, hour, minute, second, sub_second, utc_offset)
    end

    def time_with_offset(offset, year, month, day, hour, minute, second, sub_second = 0)
      TZInfo::TimestampWithOffset.create(year, month, day, hour, minute, second, sub_second, offset.observed_utc_offset).set_timezone_offset(offset)
    end
  end

  class TimeTypesBaseTimeLikeHelper < TimeTypesHelper
    def initialize
      @time_helper = TimeTypesTimeHelper.new
    end

    def output_time(*args)
      @time_helper.time(*args)
    end

    def time_with_offset(*args)
      @time_helper.time_with_offset(*args)
    end
  end

  class TimeTypesTimeLikeHelper < TimeTypesBaseTimeLikeHelper
    def self.supports?(feature)
      false
    end

    def type
      :time_like
    end

    def time(year, month, day, hour, minute, second, sub_second = 0, utc_offset = 0)
      raise 'TimeLike does not support non-zero/non-UTC offsets' unless utc_offset == 0 || utc_offset == :utc
      TimeLike.new(Time.utc(year, month, day, hour, minute, second).to_i, sub_second)
    end
  end

  class TimeTypesTimeLikeWithOffsetHelper < TimeTypesBaseTimeLikeHelper
    def self.supports?(feature)
      feature == :offset
    end

    def type
      :time_like_with_offset
    end

    def time(year, month, day, hour, minute, second, sub_second = 0, utc_offset = 0)
      utc_offset = 0 if utc_offset == :utc
      TimeLikeWithOffset.new(Time.new(year, month, day, hour, minute, second, utc_offset).to_i, sub_second, utc_offset)
    end
  end

  module Helpers
    def self.append_features(base)
      super
      base.extend(ClassMethods)
    end

    # Runs tests with each of the supported time representation types (DateTime,
    # Time or Timestamp). Types can be restricted by requiring features
    # (:distinct_utc, :unspecified_offset or :utc).
    def time_types_test(*required_features, &block)
      self.class.time_types_helpers(*required_features, &block)
    end

    # Suppresses any warnings raised in a specified block.
    def without_warnings
      old_verbose = $VERBOSE
      begin
        $VERBOSE = nil
        yield
      ensure
        $-v = old_verbose
      end
    end

    # Runs a test with safe mode enabled ($SAFE = 1).
    def safe_test(options = {})
      # Ruby >= 2.7, JRuby, Rubinius and TruffleRuby don't support SAFE levels.
      available = RUBY_VERSION < '2.7' && !%w(jruby rbx truffleruby).include?(RUBY_ENGINE)

      if !available && options[:unavailable] == :skip
        skip('Ruby >= 2.7, JRuby, Rubinius and TruffleRuby don\'t support SAFE levels')
      end

      thread = Thread.new do
        orig_diff = Minitest::Assertions.diff

        if available
          orig_safe = $SAFE
          $SAFE = options[:level] || 1
        end
        begin
          # Disable the use of external diff tools during safe mode tests (since
          # safe mode will prevent their use). The initial value is retrieved
          # before activating safe mode because the first time
          # Minitest::Assertions.diff is called, it will attempt to find a diff
          # tool. Finding the diff tool will also fail in safe mode.
          Minitest::Assertions.diff = nil
          begin
            yield
          ensure
            Minitest::Assertions.diff = orig_diff
          end
        ensure
          if available
            # On Ruby < 2.6, setting $SAFE affects only the current thread
            # and the $SAFE level cannot be downgraded. Catch and ignore the
            # SecurityError.
            # On Ruby >= 2.6, setting $SAFE is global, and the $SAFE level
            # can be downgraded. Restore $SAFE back to the original level.
            begin
              $SAFE = orig_safe
            rescue SecurityError
            end
          end
        end
      end

      thread.join
    end

    module ClassMethods
      # Yields instances of the TimeTypesHelper subclasses. Types can be
      # restricted by requiring features (:distinct_utc, :unspecified_offset or
      # :utc).
      def time_types_helpers(*required_features)
        [
          TestUtils::TimeTypesTimeHelper,
          TestUtils::TimeTypesDateTimeHelper,
          TestUtils::TimeTypesTimestampHelper,
          TestUtils::TimeTypesTimeLikeHelper,
          TestUtils::TimeTypesTimeLikeWithOffsetHelper
        ].each do |helper_class|
          if required_features.all? {|f| helper_class.supports?(f) }
            yield helper_class.new
          end
        end
      end

      # Gets instances of TestEncoding for the given names.
      def test_encodings(*names)
        names.map {|n| TestEncoding.new(n) }
      end
    end
  end

  module Assertions
    # Assert that an array contains the same items independent of ordering.
    def assert_array_same_items(expected, actual, msg = nil)
      full_message = message(msg, '') { diff(expected, actual) }
      condition = (expected.size == actual.size) && (expected - actual == [])
      assert(condition, full_message)
    end

    # Keeps track of the number of times assert_sub_process_returns has been
    # called in order to name each SimpleCov run.
    @@assert_sub_process_returns_count = 0

    # Assert that starting a Ruby sub process to run code returns the output
    # contained in the expected_lines array. Directories in load_path are added
    # to the start of the load path before running requires. Each item in
    # required is passed to require before running the specified code.
    def assert_sub_process_returns(expected_lines, code, load_path = [], required = ['tzinfo'])
      if RUBY_ENGINE == 'jruby' && JRUBY_VERSION.start_with?('9.0.') && RbConfig::CONFIG['host_os'] =~ /mswin/
        skip('JRuby 9.0 on Windows cannot handle writing to the IO instance returned by popen')
      end

      ruby = File.join(RbConfig::CONFIG['bindir'],
        RbConfig::CONFIG['ruby_install_name'] + RbConfig::CONFIG['EXEEXT'])

      if RUBY_ENGINE == 'rbx'
        # Stop Rubinius from operating as irb.
        args = ' -'
      else
        args = ''
      end

      @@assert_sub_process_returns_count += 1

      IO.popen("\"#{ruby}\"#{args} 2>&1", 'r+') do |process|
        load_path.each do |p|
          process.puts("$:.unshift('#{p.gsub("'", "\\\\'")}')")
        end

        if RUBY_ENGINE == 'rbx'
          # Bundler doesn't get set up automatically on Rubinius.
          process.puts("require 'bundler/setup'")
        end

        if COVERAGE_ENABLED && defined?(COVERAGE_TYPE)
          process.puts("require 'simplecov'")
          process.puts('SimpleCov.start do')
          process.puts("  command_name '#{COVERAGE_TYPE.gsub("'", "\\\\'")}:sp#{@@assert_sub_process_returns_count}'")
          process.puts("  add_filter 'test'")
          process.puts("  nocov_token '#{COVERAGE_NOCOV_TOKEN.gsub("'", "\\\\'")}'")
          process.puts("  project_name 'TZInfo'")
          process.puts('  self.formatters = []')
          process.puts('end')
        end

        required.each do |r|
          process.puts("require '#{r.gsub("'", "\\\\'")}'")
        end

        process.puts(code)
        process.flush
        process.close_write

        actual_lines = process.readlines
        actual_lines = actual_lines.collect(&:chomp)

        # Ignore warnings from RubyGems >= 3.1.0 that cause test failures with
        # JRuby 9.2.9.0:
        # https://travis-ci.org/tzinfo/tzinfo/jobs/628170481#L2437
        #
        # Ignore warnings from JRuby 1.7 and 9.0 on modern versions of Java:
        # https://github.com/tzinfo/tzinfo/runs/1664655982#step:8:1893
        #
        # Ignore untaint deprecation warnings from Bundler 1 on Ruby 3.0.
        actual_lines = actual_lines.reject do |l|
          l.start_with?('Gem::Specification#rubyforge_project= called from') ||
            l.start_with?('NOTE: Gem::Specification#rubyforge_project= is deprecated with no replacement.') ||
            l.end_with?('warning: constant Gem::ConfigMap is deprecated') ||
            l.start_with?('unsupported Java version') ||
            l.start_with?('WARNING: An illegal reflective access operation has occurred') ||
            l.start_with?('WARNING: Illegal reflective access by') ||
            l.start_with?('WARNING: Please consider reporting this to the maintainers of') ||
            l.start_with?('WARNING: All illegal access operations will be denied in a future release') ||
            l.start_with?('WARNING: Use --illegal-access=warn to enable warnings of further illegal reflective access operations') ||
            l.start_with?('io/console on JRuby shells out to stty for most operations') ||
            l =~ /\/bundler-1\..*\/lib\/bundler\/.*\.rb:\d+: warning: (Object|Pathname)#untaint is deprecated and will be removed in Ruby 3\.2\.\z/
        end

        if RUBY_ENGINE == 'jruby' && ENV['_JAVA_OPTIONS'] && actual_lines.first == "Picked up _JAVA_OPTIONS: #{ENV['_JAVA_OPTIONS']}"
          actual_lines.shift
        end

        assert_equal(expected_lines, actual_lines)
      end
    end

    # Asserts that a block does not raise an exception.
    def assert_nothing_raised(msg = nil)
      begin
        yield
      rescue => e
        full_message = message(msg) { exception_details(e, 'Exception raised: ') }
        assert(false, full_message)
      end
    end

    # If expected is nil, asserts that actual is nil, otherwise asserts that
    # expected equals actual.
    def assert_nil_or_equal(expected, actual, msg = nil)
      if expected.nil?
        assert_nil(actual, msg)
      else
        assert_equal(expected, actual, msg)
      end
    end

    # Asserts that Time, DateTime or Timestamp instances are equal and that
    # their offsets are equal. The actual instance is allowed to be a subclass
    # of the expected class.
    def assert_equal_with_offset(expected, actual)
      assert_kind_of(expected.class, actual)
      assert_equal(expected, actual)

      # Time, DateTime and Timestamp don't require identical offsets for equality.
      # Test the offsets explicitly.
      if expected.respond_to?(:utc_offset)
        assert_nil_or_equal(expected.utc_offset, actual.utc_offset, 'utc_offset')
      elsif expected.respond_to?(:offset)
        assert_nil_or_equal(expected.offset, actual.offset, 'offset')
      end

      # Time (on MRI and Rubinius, but not JRuby) and Timestamp distinguish between
      # UTC and a local time with 0 offset from UTC.
      if expected.respond_to?(:utc?)
        assert_nil_or_equal(expected.utc?, actual.utc?, 'utc?')
      end
    end

    # Asserts that TimeWithOffset, DateTimeWithOffset or TimestampWithOffset
    # instances are equal and that their associated TimezoneOffsets are also
    # equal.
    def assert_equal_with_offset_and_timezone_offset(expected, actual)
      assert_equal_with_offset(expected, actual)
      assert_kind_of(TZInfo::TimezoneOffset, actual.timezone_offset)
      assert_equal(expected.timezone_offset, actual.timezone_offset)
    end

    # Asserts that Time, DateTime or Timestamp instances are equal and that
    # their classes are identical.
    def assert_equal_with_offset_and_class(expected, actual)
      assert_equal_with_offset(expected, actual)
      assert_equal(expected.class, actual.class)
    end

    # Object#taint is deprecated in Ruby >= 2.7 and will be removed in 3.2.
    # 2.7 makes it a no-op with a warning.
    # Define a method that will skip for use in tests that deal with tainted
    # objects.
    if Object.respond_to?(:taint)
      if RUBY_VERSION >= '2.7'
        def skip_if_taint_is_undefined_or_no_op
          skip('Object#taint is a no-op')
        end
      else
        def skip_if_taint_is_undefined_or_no_op
        end
      end
    else
      def skip_if_taint_is_undefined_or_no_op
        skip('Object#taint is not defined')
      end
    end
  end
end

TestUtils.prepare_test_zoneinfo_dir

class Minitest::Test
  include TestUtils::Helpers
  include TestUtils::Assertions
end
