# encoding: UTF-8
# frozen_string_literal: true

require_relative '../test_utils'
require 'tempfile'

module DataSources
  class TCZoneinfoReader < Minitest::Test
    include TZInfo
    include TZInfo::DataSources

    class FakePosixTimeZoneParser
      def initialize(&block)
        @on_parse = block
      end

      def parse(tz_string)
        @on_parse.call(tz_string)
      end
    end

    MIN_FORMAT = 1
    MAX_FORMAT = 3

    def convert_times_to_i(items, key = :at)
      items.each do |item|
        if item[key].kind_of?(Time)
          item[key] = item[key].utc.to_i
        end
      end
    end

    def select_with_32bit_values(items, key = :at)
      items.select do |item|
        i = item[key]
        i >= -2147483648 && i <= 2147483647
      end
    end

    def pack_int64_network_order(values)
      values.collect {|value| [value >> 32, value & 0xFFFFFFFF]}.flatten.pack('NN' * values.length)
    end

    def pack_int64_signed_network_order(values)
      # Convert to the equivalent 64-bit unsigned integer with the same bit representation
      pack_int64_network_order(values.collect {|value| value < 0 ? value + 0x10000000000000000 : value})
    end

    def write_tzif(format, offsets, transitions, tz_string, leaps, options = {})

      # Options for testing malformed zoneinfo files.
      magic = options[:magic]
      section2_magic = options[:section2_magic]
      abbrev_separator = options[:abbrev_separator] || "\0"
      abbrev_offset_base = options[:abbrev_offset_base] || 0
      omit_tz_string_start_new_line = options[:omit_tz_string_start_new_line]
      omit_tz_string_end_new_line = options[:omit_tz_string_end_new_line]

      unless magic
        if format == 1
          magic = "TZif\0"
        elsif format >= 2
          magic = "TZif#{format}"
        else
          raise ArgumentError, 'Invalid format specified'
        end
      end

      if section2_magic.kind_of?(Proc)
        section2_magic = section2_magic.call(format)
      else
        section2_magic = magic unless section2_magic
      end

      convert_times_to_i(transitions)
      convert_times_to_i(leaps)

      abbrevs = offsets.collect {|o| o[:abbrev]}.uniq

      if abbrevs.length > 0
        abbrevs = abbrevs.collect {|a| a.encode(Encoding::UTF_8)}

        if abbrevs.first.respond_to?(:bytesize)
          abbrevs_length = abbrevs.inject(0) {|sum, a| sum + a.bytesize + abbrev_separator.bytesize}
        else
          abbrevs_length = abbrevs.inject(0) {|sum, a| sum + a.length + abbrev_separator.length}
        end
      else
        abbrevs_length = 0
      end

      b32_transitions = select_with_32bit_values(transitions)
      b32_leaps = select_with_32bit_values(leaps)

      Tempfile.open('tzinfo-test-zone') do |file|
        file.binmode

        file.write(
          [magic, offsets.length, offsets.length, leaps.length,
          b32_transitions.length, offsets.length, abbrevs_length].pack('a5 x15 NNNNNN'))

        unless b32_transitions.empty?
          file.write(b32_transitions.collect {|t| t[:at]}.pack('N' * b32_transitions.length))
          file.write(b32_transitions.collect {|t| t[:offset_index]}.pack('C' * b32_transitions.length))
        end

        offsets.each do |offset|
          index = abbrevs.index(offset[:abbrev])
          abbrev_offset = abbrev_offset_base
          0.upto(index - 1) {|i| abbrev_offset += abbrevs[i].length + 1}

          file.write([offset[:gmtoff], offset[:isdst] ? 1 : 0, abbrev_offset].pack('NCC'))
        end

        abbrevs.each do |a|
          file.write(a)
          file.write(abbrev_separator)
        end

        b32_leaps.each do |leap|
          file.write([leap[:at], leap[:seconds]].pack('NN'))
        end

        unless offsets.empty?
          file.write("\0" * offsets.length * 2)
        end

        if format >= 2
          file.write(
            [section2_magic, offsets.length, offsets.length, leaps.length,
            transitions.length, offsets.length, abbrevs_length].pack('a5 x15 NNNNNN'))

          unless transitions.empty?
            file.write(pack_int64_signed_network_order(transitions.collect {|t| t[:at]}))
            file.write(transitions.collect {|t| t[:offset_index]}.pack('C' * transitions.length))
          end

          offsets.each do |offset|
            index = abbrevs.index(offset[:abbrev])
            abbrev_offset = abbrev_offset_base
            0.upto(index - 1) {|i| abbrev_offset += abbrevs[i].length + 1}

            file.write([offset[:gmtoff], offset[:isdst] ? 1 : 0, abbrev_offset].pack('NCC'))
          end

          abbrevs.each do |a|
            file.write(a)
            file.write(abbrev_separator)
          end

          leaps.each do |leap|
            file.write(pack_int64_signed_network_order([leap[:at]]))
            file.write([leap[:seconds]].pack('N'))
          end

          unless offsets.empty?
            file.write("\0" * offsets.length * 2)
          end

          file.write("\n") unless omit_tz_string_start_new_line
          file.write(tz_string.encode(Encoding::UTF_8))
          file.write("\n") unless omit_tz_string_end_new_line
        end

        file.flush

        yield file.path
      end
    end

    def tzif_test(offsets, transitions, options = {}, &block)
      rules = options[:rules]
      tz_string = options[:tz_string] || (rules ? "TEST_TZ_STRING_#{rand(1000000)}" : '')
      leaps = options[:leaps] || []
      min_format = options[:min_format] || (tz_string.empty? ? MIN_FORMAT : 2)

      min_format.upto(MAX_FORMAT) do |format|
        write_tzif(format, offsets, transitions, tz_string, leaps, options) do |path|
          if format >= 2
            @tz_parse_result = rules
            @expect_tz_string = tz_string
          end
          begin
            yield path, format
          ensure
            @tz_parse_result = nil
            @expect_tz_string = nil
          end
        end
      end
    end

    def setup
      @expect_tz_string = nil
      @tz_parse_result = nil
      @posix_tz_parser = FakePosixTimeZoneParser.new do |tz_string|
        raise "Unexpected tz_string passed to PosixTimeZoneParser: #{tz_string}" unless tz_string == @expect_tz_string
        raise InvalidPosixTimeZone, 'FakePosixTimeZoneParser Failure.' if @tz_parse_result == :fail
        @tz_parse_result
      end
      @reader = ZoneinfoReader.new(@posix_tz_parser, StringDeduper.new)
    end

    def test_read
      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST'},
        {gmtoff: 7200, isdst: true,  abbrev: 'XDT'},
        {gmtoff:    0, isdst: false, abbrev: 'XNST'}]

      transitions = [
        {at: Time.utc(1971,  1,  2), offset_index: 1},
        {at: Time.utc(1980,  4, 22), offset_index: 2},
        {at: Time.utc(1980, 10, 21), offset_index: 1},
        {at: Time.utc(2000, 12, 31), offset_index: 3}]

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'XST')
      o2 = TimezoneOffset.new(3600, 3600, 'XDT')
      o3 = TimezoneOffset.new(   0,    0, 'XNST')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(1971,  1,  2).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(1980,  4, 22).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(1980, 10, 21).to_i)
      t3 = TimezoneTransition.new(o3, o1, Time.utc(2000, 12, 31).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2,t3], @reader.read(path))
      end
    end

    def test_read_negative_utc_offset
      offsets = [
        {gmtoff: -12492, isdst: false, abbrev: 'LMT'},
        {gmtoff: -12000, isdst: false, abbrev: 'XST'},
        {gmtoff: -8400,  isdst: true,  abbrev: 'XDT'},
        {gmtoff: -8400,  isdst: false, abbrev: 'XNST'}]

      transitions = [
        {at: Time.utc(1971,  7,  9, 3,  0, 0), offset_index: 1},
        {at: Time.utc(1972, 10, 12, 3,  0, 0), offset_index: 2},
        {at: Time.utc(1973,  4, 29, 3,  0, 0), offset_index: 1},
        {at: Time.utc(1992,  4,  1, 4, 30, 0), offset_index: 3}]

      o0 = TimezoneOffset.new(-12492,    0, 'LMT')
      o1 = TimezoneOffset.new(-12000,    0, 'XST')
      o2 = TimezoneOffset.new(-12000, 3600, 'XDT')
      o3 = TimezoneOffset.new( -8400,    0, 'XNST')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(1971,  7,  9, 3,  0, 0).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(1972, 10, 12, 3,  0, 0).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(1973,  4, 29, 3,  0, 0).to_i)
      t3 = TimezoneTransition.new(o3, o1, Time.utc(1992,  4,  1, 4, 30, 0).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2,t3], @reader.read(path))
      end
    end

    def test_read_dst_first
      offsets = [
        {gmtoff: 7200, isdst: true,  abbrev: 'XDT'},
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST'},
        {gmtoff:    0, isdst: false, abbrev: 'XNST'}]

      transitions = [
        {at: Time.utc(1979,  1,  2), offset_index: 2},
        {at: Time.utc(1980,  4, 22), offset_index: 0},
        {at: Time.utc(1980, 10, 21), offset_index: 2},
        {at: Time.utc(2000, 12, 31), offset_index: 3}]

      o1 = TimezoneOffset.new(3542, 0, 'LMT')
      o2 = TimezoneOffset.new(3600, 0, 'XST')
      t0 = TimezoneTransition.new(o2, o1, Time.utc(1979, 1, 2).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal(t0, @reader.read(path).first)
      end
    end

    def test_read_no_transitions
      offsets = [{gmtoff: -12094, isdst: false, abbrev: 'LT'}]
      o0 = TimezoneOffset.new(-12094, 0, 'LT')

      tzif_test(offsets, []) do |path, format|
        assert_equal(o0, @reader.read(path))
      end
    end

    def test_read_no_transitions_dst_first
      offsets = [
        {gmtoff: -10800, isdst: true, abbrev: 'XDT'},
        {gmtoff: -12094, isdst: false, abbrev: 'LT'}]

      o1 = TimezoneOffset.new(-12094, 0, 'LT')

      tzif_test(offsets, []) do |path, format|
        assert_equal(o1, @reader.read(path))
      end
    end

    def test_read_no_transitions_dst_only
      offsets = [{gmtoff: -10800, isdst: true, abbrev: 'XDT'}]
      o0 = TimezoneOffset.new(-14400, 3600, 'XDT')

      tzif_test(offsets, []) do |path, format|
        assert_equal(o0, @reader.read(path))
      end
    end

    def test_read_initial_transition_to_first_offset
      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST'}]

      transitions = [
        {at: Time.utc(1950, 1, 1), offset_index: 0},
        {at: Time.utc(2000, 1, 1), offset_index: 1}]

      o0 = TimezoneOffset.new(3542, 0, 'LMT')
      o1 = TimezoneOffset.new(3600, 0, 'XST')

      t0 = TimezoneTransition.new(o0, o0, Time.utc(1950, 1, 1).to_i)
      t1 = TimezoneTransition.new(o1, o0, Time.utc(2000, 1, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1], @reader.read(path))
      end
    end

    def test_read_no_offsets
      offsets = []
      transitions = [{at: Time.utc(2000, 12, 31), offset_index: 0}]

      tzif_test(offsets, transitions) do |path, format|
        error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
        assert_equal("Invalid offset referenced by transition in file '#{path}'.", error.message)
      end
    end

    def test_read_invalid_offset_index
      offsets = [{gmtoff: -0, isdst: false, abbrev: 'LMT'}]
      transitions = [{at: Time.utc(2000, 12, 31), offset_index: 2}]

      tzif_test(offsets, transitions) do |path, format|
        error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
        assert_equal("Invalid offset referenced by transition in file '#{path}'.", error.message)
      end
    end

    def test_read_with_leap_seconds
      offsets = [{gmtoff: -0, isdst: false, abbrev: 'LMT'}]
      leaps = [{at: Time.utc(1972,6,30,23,59,60), seconds: 1}]

      tzif_test(offsets, [], leaps: leaps) do |path, format|
        error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
        assert_equal("The file '#{path}' contains leap second data. TZInfo requires zoneinfo files that omit leap seconds.", error.message)
      end
    end

    def test_read_invalid_magic
      ['tzif2', '12345'].each do |magic|
        offsets = [{gmtoff: -12094, isdst: false, abbrev: 'LT'}]

        tzif_test(offsets, [], magic: magic) do |path, format|
          error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
          assert_equal("The file '#{path}' does not start with the expected header.", error.message)
        end
      end
    end

    def test_read_invalid_version
      offsets = [{gmtoff: -12094, isdst: false, abbrev: 'LT'}]

      tzif_test(offsets, [], magic: 'TZif4') do |path, format|
        error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
        assert_equal("The file '#{path}' contains a version of the zoneinfo format that is not currently supported.", error.message)
      end
    end

    def test_read_invalid_section2_magic
      ['TZif4', 'tzif2', '12345'].each do |section2_magic|
        offsets = [{gmtoff: -12094, isdst: false, abbrev: 'LT'}]

        tzif_test(offsets, [], min_format: 2, section2_magic: section2_magic) do |path, format|
          error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
          assert_equal("The file '#{path}' contains an invalid 64-bit section header.", error.message)
        end
      end
    end

    def test_read_mismatched_section2_magic
      minus_one = Proc.new {|f| f == 2 ? "TZif\0" : "TZif#{f - 1}" }
      plus_one = Proc.new {|f| "TZif#{f + 1}" }

      [minus_one, plus_one].each do |section2_magic|
        offsets = [{gmtoff: -12094, isdst: false, abbrev: 'LT'}]

        tzif_test(offsets, [], min_format: 2, section2_magic: section2_magic) do |path, format|
          error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
          assert_equal("The file '#{path}' contains an invalid 64-bit section header.", error.message)
        end
      end
    end

    def test_read_invalid_format
      Tempfile.open('tzinfo-test-zone') do |file|
        file.write('Invalid')
        file.flush

        error = assert_raises(InvalidZoneinfoFile) { @reader.read(file.path) }
        assert_match(/Expected \d+ bytes reading/, error.message)
      end
    end

    def test_read_missing_abbrev_null_termination
      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false,  abbrev: 'XST'}]

      transitions = [
        {at: Time.utc(2000, 1, 1), offset_index: 1}]

      tzif_test(offsets, transitions, abbrev_separator: '^') do |path, format|
        error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
        assert_equal("Missing abbreviation null terminator in file '#{path}'.", error.message)
      end
    end

    def test_read_out_of_range_abbrev_offsets
      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false,  abbrev: 'XST'}]

      transitions = [
        {at: Time.utc(2000, 1, 1), offset_index: 1}]

      tzif_test(offsets, transitions, abbrev_offset_base: 8) do |path, format|
        error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
        assert_equal("Abbreviation index is out of range in file '#{path}'.", error.message)
      end
    end

    def test_read_subsequent_transition_same
      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST'},
        {gmtoff: 7200, isdst: true,  abbrev: 'XDT'}]

      transitions = [
        {at: Time.utc(2000, 1, 1, 12, 0, 0), offset_index: 1},
        {at: Time.utc(2000, 1, 1, 12, 0, 0), offset_index: 2}]

      tzif_test(offsets, transitions) do |path, format|
        error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
        assert_equal("Transition at #{Time.utc(2000, 1, 1, 12, 0, 0).to_i} is not later than the previous transition at #{Time.utc(2000, 1, 1, 12, 0, 0).to_i} in file '#{path}'.", error.message)
      end
    end

    def test_read_subsequent_transition_less
      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST'},
        {gmtoff: 7200, isdst: true,  abbrev: 'XDT'}]

      transitions = [
        {at: Time.utc(2000, 1, 1, 12,  0,  0), offset_index: 1},
        {at: Time.utc(2000, 1, 1, 11, 59, 59), offset_index: 2}]

      tzif_test(offsets, transitions) do |path, format|
        error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
        assert_equal("Transition at #{Time.utc(2000, 1, 1, 11, 59, 59).to_i} is not later than the previous transition at #{Time.utc(2000, 1, 1, 12, 0, 0).to_i} in file '#{path}'.", error.message)
      end
    end

    def test_read_before_epoch
      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST'},
        {gmtoff: 7200, isdst: true,  abbrev: 'XDT'},
        {gmtoff:    0, isdst: false, abbrev: 'XNST'}]

      transitions = [
        {at: Time.utc(1948,  1,  2), offset_index: 1},
        {at: Time.utc(1969,  4, 22), offset_index: 2},
        {at: Time.utc(1970, 10, 21), offset_index: 1},
        {at: Time.utc(2000, 12, 31), offset_index: 3}]

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'XST')
      o2 = TimezoneOffset.new(3600, 3600, 'XDT')
      o3 = TimezoneOffset.new(   0,    0, 'XNST')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(1948,  1,  2).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(1969,  4, 22).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(1970, 10, 21).to_i)
      t3 = TimezoneTransition.new(o3, o1, Time.utc(2000, 12, 31).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2,t3], @reader.read(path))
      end
    end

    def test_read_on_epoch
      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST'},
        {gmtoff: 7200, isdst: true,  abbrev: 'XDT'},
        {gmtoff:    0, isdst: false, abbrev: 'XNST'}]

      transitions = [
        {at: Time.utc(1948,  1,  2), offset_index: 1},
        {at: Time.utc(1969,  4, 22), offset_index: 2},
        {at: Time.utc(1970,  1,  1), offset_index: 1},
        {at: Time.utc(2000, 12, 31), offset_index: 3}]

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'XST')
      o2 = TimezoneOffset.new(3600, 3600, 'XDT')
      o3 = TimezoneOffset.new(   0,    0, 'XNST')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(1948,  1,  2).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(1969,  4, 22).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(1970,  1,  1).to_i)
      t3 = TimezoneTransition.new(o3, o1, Time.utc(2000, 12, 31).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2,t3], @reader.read(path))
      end
    end

    def test_read_64bit
      # TZif format 2 and later contains both 32-bit and 64-bit times. Where a
      # TZif 2 or later file is provided, the 64-bit times should be used.

      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST'},
        {gmtoff: 7200, isdst: true,  abbrev: 'XDT'},
        {gmtoff:    0, isdst: false, abbrev: 'XNST'}]

      transitions = [
        {at: Time.utc(1850,  1,  2), offset_index: 1},
        {at: Time.utc(2003,  4, 22), offset_index: 2},
        {at: Time.utc(2003, 10, 21), offset_index: 1},
        {at: Time.utc(2040, 12, 31), offset_index: 3}]

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'XST')
      o2 = TimezoneOffset.new(3600, 3600, 'XDT')
      o3 = TimezoneOffset.new(   0,    0, 'XNST')

      t0    = TimezoneTransition.new(o1, o0, Time.utc(1850,  1,  2).to_i)
      t1_f1 = TimezoneTransition.new(o2, o0, Time.utc(2003,  4, 22).to_i)
      t1_f2 = TimezoneTransition.new(o2, o1, Time.utc(2003,  4, 22).to_i)
      t2    = TimezoneTransition.new(o1, o2, Time.utc(2003, 10, 21).to_i)
      t3    = TimezoneTransition.new(o3, o1, Time.utc(2040, 12, 31).to_i)

      tzif_test(offsets, transitions) do |path, format|
        transitions = @reader.read(path)

        if format >= 2
          assert_equal([t0,t1_f2,t2,t3], transitions)
        else
          assert_equal([t1_f1,t2], transitions)
        end
      end
    end

    def test_read_64bit_range
      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST'},
        {gmtoff: 7200, isdst: false, abbrev: 'XNST'}]

      transitions = [
        {at: -2**63,                offset_index: 1},
        {at: Time.utc(2014, 5, 27), offset_index: 2},
        {at: 2**63 - 1,             offset_index: 0}]

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'XST')
      o2 = TimezoneOffset.new(7200,    0, 'XNST')

      t0    = TimezoneTransition.new(o1, o0,                     -2**63)
      t1_f1 = TimezoneTransition.new(o2, o0, Time.utc(2014, 5, 27).to_i)
      t1_f2 = TimezoneTransition.new(o2, o1, Time.utc(2014, 5, 27).to_i)
      t2    = TimezoneTransition.new(o0, o2,                  2**63 - 1)

      tzif_test(offsets, transitions) do |path, format|
        transitions = @reader.read(path)

        if format >= 2
          assert_equal([t0, t1_f2, t2], transitions)
        else
          assert_equal([t1_f1], transitions)
        end
      end
    end

    def test_read_32bit_range
      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST'},
        {gmtoff: 7200, isdst: false, abbrev: 'XNST'}]

      transitions = [
        {at: -2**31,                offset_index: 1},
        {at: Time.utc(2014, 5, 27), offset_index: 2},
        {at: 2**31 - 1,             offset_index: 0}]

      o0 = TimezoneOffset.new(3542, 0, 'LMT')
      o1 = TimezoneOffset.new(3600, 0, 'XST')
      o2 = TimezoneOffset.new(7200, 0, 'XNST')

      t0 = TimezoneTransition.new(o1, o0,                     -2**31)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(2014, 5, 27).to_i)
      t2 = TimezoneTransition.new(o0, o2,                  2**31 - 1)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2], @reader.read(path))
      end
    end

    def test_read_std_offset_changes
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff:  3542, isdst: false, abbrev: 'LMT'},
        {gmtoff:  3600, isdst: false, abbrev: 'XST'},
        {gmtoff:  7200, isdst: true,  abbrev: 'XDT'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDDT'}]

      transitions = [
        {at: Time.utc(2000, 1, 1), offset_index: 1},
        {at: Time.utc(2000, 2, 1), offset_index: 2},
        {at: Time.utc(2000, 3, 1), offset_index: 3},
        {at: Time.utc(2000, 4, 1), offset_index: 1}]

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'XST')
      o2 = TimezoneOffset.new(3600, 3600, 'XDT')
      o3 = TimezoneOffset.new(3600, 7200, 'XDDT')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2000, 1, 1).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000, 2, 1).to_i)
      t2 = TimezoneTransition.new(o3, o2, Time.utc(2000, 3, 1).to_i)
      t3 = TimezoneTransition.new(o1, o3, Time.utc(2000, 4, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2,t3], @reader.read(path))
      end
    end

    def test_read_std_offset_changes_jump_to_double_dst
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff:  3542, isdst: false, abbrev: 'LMT'},
        {gmtoff:  3600, isdst: false, abbrev: 'XST'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDDT'}]

      transitions = [
        {at: Time.utc(2000, 4, 1), offset_index: 1},
        {at: Time.utc(2000, 5, 1), offset_index: 2},
        {at: Time.utc(2000, 6, 1), offset_index: 1}]

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'XST')
      o2 = TimezoneOffset.new(3600, 7200, 'XDDT')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2000, 4, 1).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000, 5, 1).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(2000, 6, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2], @reader.read(path))
      end
    end

    def test_read_std_offset_changes_negative
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff: -10821, isdst: false, abbrev: 'LMT'},
        {gmtoff: -10800, isdst: false, abbrev: 'XST'},
        {gmtoff:  -7200, isdst: true,  abbrev: 'XDT'},
        {gmtoff:  -3600, isdst: true,  abbrev: 'XDDT'}]

      transitions = [
        {at: Time.utc(2000, 1, 1), offset_index: 1},
        {at: Time.utc(2000, 2, 1), offset_index: 2},
        {at: Time.utc(2000, 3, 1), offset_index: 3},
        {at: Time.utc(2000, 4, 1), offset_index: 1},
        {at: Time.utc(2000, 5, 1), offset_index: 3},
        {at: Time.utc(2000, 6, 1), offset_index: 1}]

      o0 = TimezoneOffset.new(-10821,    0, 'LMT')
      o1 = TimezoneOffset.new(-10800,    0, 'XST')
      o2 = TimezoneOffset.new(-10800, 3600, 'XDT')
      o3 = TimezoneOffset.new(-10800, 7200, 'XDDT')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2000, 1, 1).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000, 2, 1).to_i)
      t2 = TimezoneTransition.new(o3, o2, Time.utc(2000, 3, 1).to_i)
      t3 = TimezoneTransition.new(o1, o3, Time.utc(2000, 4, 1).to_i)
      t4 = TimezoneTransition.new(o3, o1, Time.utc(2000, 5, 1).to_i)
      t5 = TimezoneTransition.new(o1, o3, Time.utc(2000, 6, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2,t3,t4,t5], @reader.read(path))
      end
    end

    def test_read_starts_two_hour_std_offset
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff:  3542, isdst: false, abbrev: 'LMT'},
        {gmtoff:  3600, isdst: false, abbrev: 'XST'},
        {gmtoff:  7200, isdst: true,  abbrev: 'XDT'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDDT'}]

      transitions = [
        {at: Time.utc(2000, 1, 1), offset_index: 3},
        {at: Time.utc(2000, 2, 1), offset_index: 2},
        {at: Time.utc(2000, 3, 1), offset_index: 1}]

      o0 = TimezoneOffset.new(3542, 0, 'LMT')
      o1 = TimezoneOffset.new(3600, 0, 'XST')
      o2 = TimezoneOffset.new(3600, 3600, 'XDT')
      o3 = TimezoneOffset.new(3600, 7200, 'XDDT')

      t0 = TimezoneTransition.new(o3, o0, Time.utc(2000, 1, 1).to_i)
      t1 = TimezoneTransition.new(o2, o3, Time.utc(2000, 2, 1).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(2000, 3, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2], @reader.read(path))
      end
    end

    def test_read_starts_only_dst_transition_with_lmt
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 7200, isdst: true,  abbrev: 'XDT'}]

      transitions = [{at: Time.utc(2000, 1, 1), offset_index: 1}]

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3542, 3658, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2000, 1, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0], @reader.read(path))
      end
    end

    def test_read_starts_only_dst_transition_without_lmt
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [{gmtoff: 7200, isdst: true,  abbrev: 'XDT'}]

      transitions = [{at: Time.utc(2000, 1, 1), offset_index: 0}]

      o0 = TimezoneOffset.new(3600, 3600, 'XDT')

      t0 = TimezoneTransition.new(o0, o0, Time.utc(2000, 1, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0], @reader.read(path))
      end
    end

    def test_read_switch_to_dst_and_change_utc_offset
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      # Switch from non-DST to DST at the same time as moving the UTC offset
      # back an hour (i.e. wall clock time doesn't change).

      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'YST'},
        {gmtoff: 3600, isdst: true,  abbrev: 'XDT'}]

      transitions = [
        {at: Time.utc(2000, 1, 1), offset_index: 1},
        {at: Time.utc(2000, 2, 1), offset_index: 2}]

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'YST')
      o2 = TimezoneOffset.new(   0, 3600, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2000, 1, 1).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000, 2, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1], @reader.read(path))
      end
    end

    def test_read_apia_international_dateline_change
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      # Pacific/Apia moved across the International Date Line whilst observing
      # daylight savings time.

      offsets = [
        {gmtoff:  45184, isdst: false, abbrev: 'LMT'},
        {gmtoff: -39600, isdst: false, abbrev: '-11'},
        {gmtoff: -36000, isdst: true,  abbrev: '-10'},
        {gmtoff:  50400, isdst: true,  abbrev: '+14'},
        {gmtoff:  46800, isdst: false, abbrev: '+13'}]

      transitions = [
        {at: Time.utc(2011,  4,  2, 14, 0, 0), offset_index: 1},
        {at: Time.utc(2011,  9, 24, 14, 0, 0), offset_index: 2},
        {at: Time.utc(2011, 12, 30, 10, 0, 0), offset_index: 3},
        {at: Time.utc(2012,  3, 31, 14, 0, 0), offset_index: 4}]

      o0 = TimezoneOffset.new( 45184,    0, 'LMT')
      o1 = TimezoneOffset.new(-39600,    0, '-11')
      o2 = TimezoneOffset.new(-39600, 3600, '-10')
      o3 = TimezoneOffset.new( 46800, 3600, '+14')
      o4 = TimezoneOffset.new( 46800,    0, '+13')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2011,  4,  2, 14, 0, 0).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(2011,  9, 24, 14, 0, 0).to_i)
      t2 = TimezoneTransition.new(o3, o2, Time.utc(2011, 12, 30, 10, 0, 0).to_i)
      t3 = TimezoneTransition.new(o4, o3, Time.utc(2012,  3, 31, 14, 0, 0).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2,t3], @reader.read(path))
      end
    end

    def test_read_offset_split_for_different_utc_offset
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff:  3542, isdst: false, abbrev: 'LMT'},
        {gmtoff:  3600, isdst: false, abbrev: 'XST1'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST2'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}]

      transitions = [
        {at: Time.utc(2000,  1, 1), offset_index: 1},
        {at: Time.utc(2000,  2, 1), offset_index: 3},
        {at: Time.utc(2000,  3, 1), offset_index: 1},
        {at: Time.utc(2000,  4, 1), offset_index: 2},
        {at: Time.utc(2000,  5, 1), offset_index: 3},
        {at: Time.utc(2000,  6, 1), offset_index: 2},
        {at: Time.utc(2000,  7, 1), offset_index: 1},
        {at: Time.utc(2000,  8, 1), offset_index: 3},
        {at: Time.utc(2000,  9, 1), offset_index: 1},
        {at: Time.utc(2000, 10, 1), offset_index: 2},
        {at: Time.utc(2000, 11, 1), offset_index: 3},
        {at: Time.utc(2000, 12, 1), offset_index: 2}]

      # XDT will be split and defined according to its surrounding standard time
      # offsets.

      o0   = TimezoneOffset.new(3542,    0, 'LMT')
      o1   = TimezoneOffset.new(3600,    0, 'XST1')
      o2   = TimezoneOffset.new(7200,    0, 'XST2')
      o3_1 = TimezoneOffset.new(3600, 7200, 'XDT')
      o3_2 = TimezoneOffset.new(7200, 3600, 'XDT')

      t0  = TimezoneTransition.new(o1,   o0,   Time.utc(2000,  1, 1).to_i)
      t1  = TimezoneTransition.new(o3_1, o1,   Time.utc(2000,  2, 1).to_i)
      t2  = TimezoneTransition.new(o1,   o3_1, Time.utc(2000,  3, 1).to_i)
      t3  = TimezoneTransition.new(o2,   o1,   Time.utc(2000,  4, 1).to_i)
      t4  = TimezoneTransition.new(o3_2, o2,   Time.utc(2000,  5, 1).to_i)
      t5  = TimezoneTransition.new(o2,   o3_2, Time.utc(2000,  6, 1).to_i)
      t6  = TimezoneTransition.new(o1,   o2,   Time.utc(2000,  7, 1).to_i)
      t7  = TimezoneTransition.new(o3_1, o1,   Time.utc(2000,  8, 1).to_i)
      t8  = TimezoneTransition.new(o1,   o3_1, Time.utc(2000,  9, 1).to_i)
      t9  = TimezoneTransition.new(o2,   o1,   Time.utc(2000, 10, 1).to_i)
      t10 = TimezoneTransition.new(o3_2, o2,   Time.utc(2000, 11, 1).to_i)
      t11 = TimezoneTransition.new(o2,   o3_2, Time.utc(2000, 12, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        transitions = @reader.read(path)
        assert_equal([t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11], transitions)
        0.upto(5) do |i|
          assert_same(transitions[i].offset, transitions[i + 6].offset, "Offsets for transitions #{i} and #{i + 6} are not the same")
        end
      end
    end

    def test_read_offset_utc_offset_taken_from_minimum_difference_minimum_after
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff:  3542, isdst: false, abbrev: 'LMT'},
        {gmtoff:  3600, isdst: false, abbrev: 'XST1'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST2'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}]

      transitions = [
        {at: Time.utc(2000,  1, 1), offset_index: 1},
        {at: Time.utc(2000,  2, 1), offset_index: 3},
        {at: Time.utc(2000,  3, 1), offset_index: 2}]

      # XDT should use the closest utc_offset (7200) (and not an equivalent
      # utc_offset of 3600 and std_offset of 7200).

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'XST1')
      o2 = TimezoneOffset.new(7200,    0, 'XST2')
      o3 = TimezoneOffset.new(7200, 3600, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2000,  1, 1).to_i)
      t1 = TimezoneTransition.new(o3, o1, Time.utc(2000,  2, 1).to_i)
      t2 = TimezoneTransition.new(o2, o3, Time.utc(2000,  3, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2], @reader.read(path))
      end
    end

    def test_read_offset_utc_offset_taken_from_minimum_difference_minimum_before
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff:  3542, isdst: false, abbrev: 'LMT'},
        {gmtoff:  3600, isdst: false, abbrev: 'XST1'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST2'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}]

      transitions = [
        {at: Time.utc(2000,  1, 1), offset_index: 2},
        {at: Time.utc(2000,  2, 1), offset_index: 3},
        {at: Time.utc(2000,  3, 1), offset_index: 1}]

      # XDT should use the closest utc_offset (7200) (and not an equivalent
      # utc_offset of 3600 and std_offset of 7200).

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'XST1')
      o2 = TimezoneOffset.new(7200,    0, 'XST2')
      o3 = TimezoneOffset.new(7200, 3600, 'XDT')

      t0 = TimezoneTransition.new(o2, o0, Time.utc(2000,  1, 1).to_i)
      t1 = TimezoneTransition.new(o3, o2, Time.utc(2000,  2, 1).to_i)
      t2 = TimezoneTransition.new(o1, o3, Time.utc(2000,  3, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2], @reader.read(path))
      end
    end

    def test_read_offset_does_not_use_equal_observed_utc_offset_equal_after
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST1'},
        {gmtoff: 7200, isdst: false, abbrev: 'XST2'},
        {gmtoff: 7200, isdst: true,  abbrev: 'XDT'}]

      transitions = [
        {at: Time.utc(2000,  1, 1), offset_index: 1},
        {at: Time.utc(2000,  2, 1), offset_index: 3},
        {at: Time.utc(2000,  3, 1), offset_index: 2}]

      # XDT will be based on the utc_offset of XST1 even though XST2 has an
      # equivalent (or greater) observed_utc_offset.

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'XST1')
      o2 = TimezoneOffset.new(7200,    0, 'XST2')
      o3 = TimezoneOffset.new(3600, 3600, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2000,  1, 1).to_i)
      t1 = TimezoneTransition.new(o3, o1, Time.utc(2000,  2, 1).to_i)
      t2 = TimezoneTransition.new(o2, o3, Time.utc(2000,  3, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2], @reader.read(path))
      end
    end

    def test_read_offset_does_not_use_equal_observed_utc_offset_equal_before
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST1'},
        {gmtoff: 7200, isdst: false, abbrev: 'XST2'},
        {gmtoff: 7200, isdst: true,  abbrev: 'XDT'}]

      transitions = [
        {at: Time.utc(2000,  1, 1), offset_index: 2},
        {at: Time.utc(2000,  2, 1), offset_index: 3},
        {at: Time.utc(2000,  3, 1), offset_index: 1}]

      # XDT will be based on the utc_offset of XST1 even though XST2 has an
      # equivalent (or greater) observed_utc_offset.

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'XST1')
      o2 = TimezoneOffset.new(7200,    0, 'XST2')
      o3 = TimezoneOffset.new(3600, 3600, 'XDT')

      t0 = TimezoneTransition.new(o2, o0, Time.utc(2000,  1, 1).to_i)
      t1 = TimezoneTransition.new(o3, o2, Time.utc(2000,  2, 1).to_i)
      t2 = TimezoneTransition.new(o1, o3, Time.utc(2000,  3, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2], @reader.read(path))
      end
    end

    def test_read_offset_both_adjacent_non_dst_equal_observed_utc_offset
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff: 7142, isdst: false, abbrev: 'LMT'},
        {gmtoff: 7200, isdst: false, abbrev: 'XST'},
        {gmtoff: 7200, isdst: true,  abbrev: 'XDT'}]

      transitions = [
        {at: Time.utc(2000,  1, 1), offset_index: 1},
        {at: Time.utc(2000,  2, 1), offset_index: 2},
        {at: Time.utc(2000,  3, 1), offset_index: 1}]

      # XDT will just assume an std_offset of +1 hour and calculate the utc_offset
      # from observed_utc_offset - std_offset.

      o0 = TimezoneOffset.new(7142,    0, 'LMT')
      o1 = TimezoneOffset.new(7200,    0, 'XST')
      o2 = TimezoneOffset.new(3600, 3600, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2000,  1, 1).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000,  2, 1).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(2000,  3, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2], @reader.read(path))
      end
    end

    def test_read_offset_utc_offset_preserved_from_next
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff:  3542, isdst: false, abbrev: 'LMT'},
        {gmtoff:  3600, isdst: false, abbrev: 'XST1'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST2'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT1'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT2'}]

      transitions = [
        {at: Time.utc(2000,  1, 1), offset_index: 1},
        {at: Time.utc(2000,  2, 1), offset_index: 3},
        {at: Time.utc(2000,  3, 1), offset_index: 4},
        {at: Time.utc(2000,  4, 1), offset_index: 2}]

      # Both XDT1 and XDT2 should both use the closest utc_offset (7200) (and not
      # an equivalent utc_offset of 3600 and std_offset of 7200).

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'XST1')
      o2 = TimezoneOffset.new(7200,    0, 'XST2')
      o3 = TimezoneOffset.new(7200, 3600, 'XDT1')
      o4 = TimezoneOffset.new(7200, 3600, 'XDT2')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2000,  1, 1).to_i)
      t1 = TimezoneTransition.new(o3, o1, Time.utc(2000,  2, 1).to_i)
      t2 = TimezoneTransition.new(o4, o3, Time.utc(2000,  3, 1).to_i)
      t3 = TimezoneTransition.new(o2, o4, Time.utc(2000,  4, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2,t3], @reader.read(path))
      end
    end

    def test_read_offset_utc_offset_preserved_from_previous
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff:  3542, isdst: false, abbrev: 'LMT'},
        {gmtoff:  3600, isdst: false, abbrev: 'XST1'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST2'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT1'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT2'}]

      transitions = [
        {at: Time.utc(2000,  1, 1), offset_index: 2},
        {at: Time.utc(2000,  2, 1), offset_index: 3},
        {at: Time.utc(2000,  3, 1), offset_index: 4},
        {at: Time.utc(2000,  4, 1), offset_index: 1}]

      # Both XDT1 and XDT2 should both use the closest utc_offset (7200) (and not
      # an equivalent utc_offset of 3600 and std_offset of 7200).

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600,    0, 'XST1')
      o2 = TimezoneOffset.new(7200,    0, 'XST2')
      o3 = TimezoneOffset.new(7200, 3600, 'XDT1')
      o4 = TimezoneOffset.new(7200, 3600, 'XDT2')

      t0 = TimezoneTransition.new(o2, o0, Time.utc(2000,  1, 1).to_i)
      t1 = TimezoneTransition.new(o3, o2, Time.utc(2000,  2, 1).to_i)
      t2 = TimezoneTransition.new(o4, o3, Time.utc(2000,  3, 1).to_i)
      t3 = TimezoneTransition.new(o1, o4, Time.utc(2000,  4, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2,t3], @reader.read(path))
      end
    end

    def test_read_offset_negative_std_offset_dst
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff: -100, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST'},
        {gmtoff:    0, isdst: true,  abbrev: 'XWT'}]

      transitions = [
        {at: Time.utc(2000,  1, 1), offset_index: 1},
        {at: Time.utc(2000,  2, 1), offset_index: 2},
        {at: Time.utc(2000,  3, 1), offset_index: 1},
        {at: Time.utc(2000,  4, 1), offset_index: 2},
        {at: Time.utc(2000,  5, 1), offset_index: 1}]

      o0 = TimezoneOffset.new(-100,     0, 'LMT')
      o1 = TimezoneOffset.new(3600,     0, 'XST')
      o2 = TimezoneOffset.new(3600, -3600, 'XWT')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2000,  1, 1).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000,  2, 1).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(2000,  3, 1).to_i)
      t3 = TimezoneTransition.new(o2, o1, Time.utc(2000,  4, 1).to_i)
      t4 = TimezoneTransition.new(o1, o2, Time.utc(2000,  5, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2,t3,t4], @reader.read(path))
      end
    end

    def test_read_offset_negative_std_offset_dst_initial_dst
      # The zoneinfo files don't include the offset from standard time, so this
      # has to be derived by looking at changes in the total UTC offset.

      offsets = [
        {gmtoff: -100, isdst: false, abbrev: 'LMT'},
        {gmtoff:    0, isdst: true,  abbrev: 'XWT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST'}]

      transitions = [
        {at: Time.utc(2000,  1, 1), offset_index: 1},
        {at: Time.utc(2000,  2, 1), offset_index: 2},
        {at: Time.utc(2000,  3, 1), offset_index: 1},
        {at: Time.utc(2000,  4, 1), offset_index: 2},
        {at: Time.utc(2000,  5, 1), offset_index: 1}]

      o0 = TimezoneOffset.new(-100,     0, 'LMT')
      o1 = TimezoneOffset.new(3600, -3600, 'XWT')
      o2 = TimezoneOffset.new(3600,     0, 'XST')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2000,  1, 1).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000,  2, 1).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.utc(2000,  3, 1).to_i)
      t3 = TimezoneTransition.new(o2, o1, Time.utc(2000,  4, 1).to_i)
      t4 = TimezoneTransition.new(o1, o2, Time.utc(2000,  5, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2,t3,t4], @reader.read(path))
      end
    end

    def test_read_offset_prefer_base_offset_moves_to_dst_not_hour
      offsets = [
        {gmtoff: -100, isdst: false, abbrev: 'LMT'},
        {gmtoff:    0, isdst: false, abbrev: 'XST'},
        {gmtoff: 1800, isdst: true,  abbrev: 'XDT'},
        {gmtoff: 1800, isdst: false, abbrev: 'XST'}]

      transitions = [
        {at: Time.utc(2000,  1, 1), offset_index: 1},
        {at: Time.utc(2000,  2, 1), offset_index: 2},
        {at: Time.utc(2000,  3, 1), offset_index: 3}]

      o0 = TimezoneOffset.new(-100,    0, 'LMT')
      o1 = TimezoneOffset.new(   0,    0, 'XST')
      o2 = TimezoneOffset.new(   0, 1800, 'XDT')
      o3 = TimezoneOffset.new(1800,    0, 'XST')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2000,  1, 1).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000,  2, 1).to_i)
      t2 = TimezoneTransition.new(o3, o2, Time.utc(2000,  3, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2], @reader.read(path))
      end
    end

    def test_read_offset_prefer_base_offset_moves_from_dst_not_hour
      offsets = [
        {gmtoff: -100, isdst: false, abbrev: 'LMT'},
        {gmtoff: 1800, isdst: false, abbrev: 'XST'},
        {gmtoff: 1800, isdst: true,  abbrev: 'XDT'},
        {gmtoff:    0, isdst: false, abbrev: 'XST'}]

      transitions = [
        {at: Time.utc(2000,  1, 1), offset_index: 1},
        {at: Time.utc(2000,  2, 1), offset_index: 2},
        {at: Time.utc(2000,  3, 1), offset_index: 3}]

      o0 = TimezoneOffset.new(-100,    0, 'LMT')
      o1 = TimezoneOffset.new(1800,    0, 'XST')
      o2 = TimezoneOffset.new(   0, 1800, 'XDT')
      o3 = TimezoneOffset.new(   0,    0, 'XST')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2000,  1, 1).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.utc(2000,  2, 1).to_i)
      t2 = TimezoneTransition.new(o3, o2, Time.utc(2000,  3, 1).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0,t1,t2], @reader.read(path))
      end
    end

    def test_read_untainted_in_safe_mode
      offsets = [{gmtoff: -12094, isdst: false, abbrev: 'LT'}]

      o0 = TimezoneOffset.new(-12094, 0, 'LT')

      tzif_test(offsets, []) do |path, format|
        safe_test do
          # Temp file path is tainted with Ruby >= 2.3 & < 2.7. Untaint for this test.
          RubyCoreSupport.untaint(path)

          assert_equal(o0, @reader.read(path))
        end
      end
    end

    def test_read_tainted_in_safe_mode
      skip_if_taint_is_undefined_or_no_op

      offsets = [{gmtoff: -12094, isdst: false, abbrev: 'LT'}]

      tzif_test(offsets, []) do |path, format|
        safe_test(unavailable: :skip) do
          # Temp file path is only tainted with Ruby >= 2.3 & < 2.7. Taint for this test.
          path.taint

          assert_raises(SecurityError) { @reader.read(path) }
        end
      end
    end

    def test_read_abbreviations_not_tainted
      skip_if_taint_is_undefined_or_no_op

      offsets = [{gmtoff: -12094, isdst: false, abbrev: 'LT'}]

      tzif_test(offsets, []) do |path, format|
        safe_test(unavailable: :skip) do
          # Temp file path is only tainted with Ruby >= 2.3 & < 2.7. Untaint for this test.
          path.untaint

          refute(@reader.read(path).abbreviation.tainted?)
        end
      end
    end

    def test_read_encoding
      # tzfile.5 doesn't specify an encoding, but the source data is in ASCII.
      # ZoneinfoTimezoneInfo will load as UTF-8 (a superset of ASCII).

      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST©'}]

      transitions = [
        {at: Time.utc(1971, 1, 2), offset_index: 1}]

      o0 = TimezoneOffset.new(3542, 0, 'LMT')
      o1 = TimezoneOffset.new(3600, 0, 'XST©')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(1971, 1, 2).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0], @reader.read(path))
      end
    end

    def test_read_binmode
      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 3600, isdst: false, abbrev: 'XST'}]

      # Transition time that includes CRLF (4EFF0D0A).
      # Test that this doesn't get corrupted by translating CRLF to LF.
      transitions = [
        {at: Time.utc(2011, 12, 31, 13, 24, 26), offset_index: 1}]

      o0 = TimezoneOffset.new(3542, 0, 'LMT')
      o1 = TimezoneOffset.new(3600, 0, 'XST')

      t0 = TimezoneTransition.new(o1, o0, Time.utc(2011, 12, 31, 13, 24, 26).to_i)

      tzif_test(offsets, transitions) do |path, format|
        assert_equal([t0], @reader.read(path))
      end
    end

    def test_read_returns_deduped_strings
      offsets = [{gmtoff: 3542, isdst: false, abbrev: 'ZIDDSMT'}]

      tzif_test(offsets, []) do |path, format|
        abbreviations = 2.times.collect do
          @reader.read(path).abbreviation
        end

        assert_same(abbreviations[0], abbreviations[1])
      end
    end

    def test_read_invalid_tz_string
      offsets = [{gmtoff: 0, isdst: false, abbrev: 'UTC'}]

      tzif_test(offsets, [], rules: :fail) do |path, format|
        error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
        assert_equal("Failed to parse POSIX-style TZ string in file '#{path}': FakePosixTimeZoneParser Failure.", error.message)
      end
    end

    def test_read_tz_string_missing_start_newline
      offsets = [{gmtoff: 0, isdst: false, abbrev: 'UTC'}]
      rules = TimezoneOffset.new(0, 0, 'UTC')

      tzif_test(offsets, [], rules: rules, omit_tz_string_start_new_line: true) do |path, format|
        error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
        assert_equal("Expected newline starting POSIX-style TZ string in file '#{path}'.", error.message)
      end
    end

    def test_read_tz_string_missing_end_newline
      offsets = [{gmtoff: 0, isdst: false, abbrev: 'UTC'}]
      rules = TimezoneOffset.new(0, 0, 'UTC')

      tzif_test(offsets, [], rules: rules, omit_tz_string_end_new_line: true) do |path, format|
        error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
        assert_equal("Expected newline ending POSIX-style TZ string in file '#{path}'.", error.message)
      end
    end

    [
      [false, 1, 0, 'TEST'],
      [false, 0, 1, 'TEST'],
      [false, 0, 0, 'TEST2'],
      [false, -1, 1, 'TEST'],
      [true, 0, 0, 'TEST']
    ].each do |(isdst, base_utc_offset, std_offset, abbreviation)|
      define_method "test_read_tz_string_does_not_match_#{isdst ? 'dst' : 'std'}_constant_offset_#{base_utc_offset}_#{std_offset}_#{abbreviation}" do
        offsets = [{gmtoff: 0, isdst: isdst, abbrev: 'TEST'}]
        rules = TimezoneOffset.new(base_utc_offset, std_offset, abbreviation)

        tzif_test(offsets, [], rules: rules) do |path, format|
          error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
          assert_equal("Constant offset POSIX-style TZ string does not match constant offset in file '#{path}'.", error.message)
        end
      end
    end

    [
      [3601, 'XST'],
      [3600, 'YST']
    ].each do |(base_utc_offset, abbreviation)|
      define_method "test_read_tz_string_does_not_match_final_std_transition_offset_#{base_utc_offset}_#{abbreviation}" do
        offsets = [
          {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
          {gmtoff: 3600, isdst: false, abbrev: 'XST'},
          {gmtoff: 7200, isdst: true,  abbrev: 'XDT'}
        ]

        transitions = [
          {at: Time.new(1971,  1,  2, 2, 0, 0, 3542), offset_index: 1},
          {at: Time.new(1981,  4, 10, 2, 0, 0, 3600), offset_index: 2},
          {at: Time.new(1981, 10, 27, 2, 0, 0, 7200), offset_index: 1}
        ]

        rules = AnnualRules.new(
          TimezoneOffset.new(base_utc_offset, 0, abbreviation),
          TimezoneOffset.new(3600, 3600, 'XDT'),
          JulianDayOfYearTransitionRule.new(100, 7200),
          JulianDayOfYearTransitionRule.new(300, 7200)
        )

        tzif_test(offsets, transitions, rules: rules) do |path, format|
          error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
          assert_equal("The first offset indicated by the POSIX-style TZ string did not match the final defined offset in file '#{path}'.", error.message)
        end
      end
    end

    [
      [3601, 0, 'XST'],
      [3600, 1, 'XST'],
      [3600, 0, 'YST']
    ].each do |(base_utc_offset, std_offset, abbreviation)|
      define_method "test_read_tz_string_does_not_match_final_dst_transition_offset_#{base_utc_offset}_#{std_offset}_#{abbreviation}" do
        offsets = [
          {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
          {gmtoff: 3600, isdst: false, abbrev: 'XST'},
          {gmtoff: 7200, isdst: true,  abbrev: 'XDT'}
        ]

        transitions = [
          {at: Time.new(1971,  1,  2, 2, 0, 0, 3542), offset_index: 1},
          {at: Time.new(1981,  4, 10, 2, 0, 0, 3600), offset_index: 2}
        ]

        rules = AnnualRules.new(
          TimezoneOffset.new(3600, 0, 'XST'),
          TimezoneOffset.new(base_utc_offset, std_offset, abbreviation),
          JulianDayOfYearTransitionRule.new(100, 7200),
          JulianDayOfYearTransitionRule.new(300, 7200)
        )

        tzif_test(offsets, transitions, rules: rules) do |path, format|
          error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
          assert_equal("The first offset indicated by the POSIX-style TZ string did not match the final defined offset in file '#{path}'.", error.message)
        end
      end
    end

    [
      [3600, 0],
      [3600, 3600],
      [10800, -3600]
    ].each do |(base_utc_offset, std_offset)|
      rules = TimezoneOffset.new(base_utc_offset, std_offset, 'TEST')

      define_method "test_read_tz_string_uses_constant_offset_with_no_transitions_#{base_utc_offset}_#{std_offset}" do
        offsets = [{gmtoff: base_utc_offset + std_offset, isdst: std_offset != 0, abbrev: 'TEST'}]

        tzif_test(offsets, [], rules: rules) do |path, format|
          assert_equal(rules, @reader.read(path))
        end
      end

      define_method "test_read_tz_string_uses_constant_offset_after_last_transition_#{base_utc_offset}_#{std_offset}" do
        offsets = [
          {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
          {gmtoff: base_utc_offset + std_offset, isdst: std_offset != 0, abbrev: 'TEST'}
        ]

        transitions = [
          {at: Time.new(1971, 1, 2, 2, 0, 0, 3542), offset_index: 1}
        ]

        o0 = TimezoneOffset.new(3542, 0, 'LMT')
        t0 = TimezoneTransition.new(rules, o0, Time.new(1971, 1, 2, 2, 0, 0, 3542).to_i)

        tzif_test(offsets, transitions, rules: rules) do |path, format|
          assert_equal([t0], @reader.read(path))
        end
      end
    end

    def test_read_tz_string_uses_rules_to_generate_all_transitions_when_none_defined
      offsets = [{gmtoff: 7200, isdst: false, abbrev: 'XST'}]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200, 0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(100, 3600),
        JulianDayOfYearTransitionRule.new(300, 7200)
      )

      o0 = TimezoneOffset.new(7200,    0, 'XST')
      o1 = TimezoneOffset.new(7200, 3600, 'XDT')

      t = 1970.upto(ZoneinfoReader.const_get(:GENERATE_UP_TO)).flat_map do |year|
        [
          TimezoneTransition.new(o1, o0, Time.new(year,  4, 10, 1, 0, 0,  7200).to_i),
          TimezoneTransition.new(o0, o1, Time.new(year, 10, 27, 2, 0, 0, 10800).to_i)
        ]
      end

      tzif_test(offsets, [], rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_uses_rules_to_generate_all_transitions_when_none_defined_omitting_first_if_matches_first_offset
      offsets = [{gmtoff: 10800, isdst: true, abbrev: 'XDT'}]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200, 0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(100, 3600),
        JulianDayOfYearTransitionRule.new(300, 7200)
      )

      o0 = TimezoneOffset.new(7200, 3600, 'XDT')
      o1 = TimezoneOffset.new(7200,    0, 'XST')

      t0 = TimezoneTransition.new(o1, o0, Time.new(1970, 10, 27, 2, 0, 0, 10800).to_i)
      tn = 1971.upto(ZoneinfoReader.const_get(:GENERATE_UP_TO)).flat_map do |year|
        [
          TimezoneTransition.new(o0, o1, Time.new(year,  4, 10, 1, 0, 0,  7200).to_i),
          TimezoneTransition.new(o1, o0, Time.new(year, 10, 27, 2, 0, 0, 10800).to_i)
        ]
      end
      t = [t0] + tn

      tzif_test(offsets, [], rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_uses_rules_to_generate_all_transitions_when_none_defined_with_previous_offset_of_first_matching_first_offset
      offsets = [{gmtoff: 7142, isdst: false, abbrev: 'LMT'}]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200, 0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(100, 3600),
        JulianDayOfYearTransitionRule.new(300, 7200)
      )

      o0 = TimezoneOffset.new(7142,    0, 'LMT')
      o1 = TimezoneOffset.new(7200,    0, 'XST')
      o2 = TimezoneOffset.new(7200, 3600, 'XDT')

      t0 = TimezoneTransition.new(o2, o0, Time.new(1970,  4, 10, 1, 0, 0,  7200).to_i)
      t1 = TimezoneTransition.new(o1, o2, Time.new(1970, 10, 27, 2, 0, 0, 10800).to_i)
      tn = 1971.upto(ZoneinfoReader.const_get(:GENERATE_UP_TO)).flat_map do |year|
        [
          TimezoneTransition.new(o2, o1, Time.new(year,  4, 10, 1, 0, 0,  7200).to_i),
          TimezoneTransition.new(o1, o2, Time.new(year, 10, 27, 2, 0, 0, 10800).to_i)
        ]
      end
      t = [t0, t1] + tn

      tzif_test(offsets, [], rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_uses_rules_to_generate_all_transitions_when_none_defined_correcting_initial_offset
      offsets = [{gmtoff: 10800, isdst: true, abbrev: 'XDDT'}]

      rules = AnnualRules.new(
        TimezoneOffset.new(3600, 0, 'XST'),
        TimezoneOffset.new(3600, 7200, 'XDDT'),
        JulianDayOfYearTransitionRule.new(100, 3600),
        JulianDayOfYearTransitionRule.new(300, 7200)
      )

      o0 = TimezoneOffset.new(3600, 7200, 'XDDT')
      o1 = TimezoneOffset.new(3600,    0, 'XST')

      t0 = TimezoneTransition.new(o1, o0, Time.new(1970, 10, 27, 2, 0, 0, 10800).to_i)
      tn = 1971.upto(ZoneinfoReader.const_get(:GENERATE_UP_TO)).flat_map do |year|
        [
          TimezoneTransition.new(o0, o1, Time.new(year,  4, 10, 1, 0, 0,  3600).to_i),
          TimezoneTransition.new(o1, o0, Time.new(year, 10, 27, 2, 0, 0, 10800).to_i)
        ]
      end
      t = [t0] + tn

      tzif_test(offsets, [], rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_extends_transitions_starting_from_std_to_dst_following_year
      offsets = [
        {gmtoff:  7142, isdst: false, abbrev: 'LMT'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}
      ]

      transitions = [
        {at: Time.new(1971,  1,  2, 2, 0, 0,  7142), offset_index: 1},
        {at: Time.new(1981,  4, 10, 1, 0, 0,  7200), offset_index: 2},
        {at: Time.new(1981, 10, 27, 2, 0, 0, 10800), offset_index: 1}
      ]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200, 0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(100, 3600),
        JulianDayOfYearTransitionRule.new(300, 7200)
      )

      o0 = TimezoneOffset.new(7142,    0, 'LMT')
      o1 = TimezoneOffset.new(7200,    0, 'XST')
      o2 = TimezoneOffset.new(7200, 3600, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.new(1971, 1, 2, 2, 0, 0, 7142).to_i)
      tn = 1981.upto(ZoneinfoReader.const_get(:GENERATE_UP_TO)).flat_map do |year|
        [
          TimezoneTransition.new(o2, o1, Time.new(year,  4, 10, 1, 0, 0,  7200).to_i),
          TimezoneTransition.new(o1, o2, Time.new(year, 10, 27, 2, 0, 0, 10800).to_i)
        ]
      end
      t = [t0] + tn

      tzif_test(offsets, transitions, rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_extends_transitions_starting_from_dst_to_std_same_year
      offsets = [
        {gmtoff:  7142, isdst: false, abbrev: 'LMT'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}
      ]

      transitions = [
        {at: Time.new(1971,  1,  2, 2, 0, 0,  7142), offset_index: 1},
        {at: Time.new(1981,  4, 10, 1, 0, 0,  7200), offset_index: 2},
        {at: Time.new(1981, 10, 27, 2, 0, 0, 10800), offset_index: 1},
        {at: Time.new(1982,  4, 10, 1, 0, 0,  7200), offset_index: 2}
      ]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200, 0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(100, 3600),
        JulianDayOfYearTransitionRule.new(300, 7200)
      )

      o0 = TimezoneOffset.new(7142,    0, 'LMT')
      o1 = TimezoneOffset.new(7200,    0, 'XST')
      o2 = TimezoneOffset.new(7200, 3600, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.new(1971, 1, 2, 2, 0, 0, 7142).to_i)
      tn = 1981.upto(ZoneinfoReader.const_get(:GENERATE_UP_TO)).flat_map do |year|
        [
          TimezoneTransition.new(o2, o1, Time.new(year,  4, 10, 1, 0, 0,  7200).to_i),
          TimezoneTransition.new(o1, o2, Time.new(year, 10, 27, 2, 0, 0, 10800).to_i)
        ]
      end
      t = [t0] + tn

      tzif_test(offsets, transitions, rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_extends_transitions_starting_from_dst_to_std_following_year
      offsets = [
        {gmtoff:  7142, isdst: false, abbrev: 'LMT'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}
      ]

      transitions = [
        {at: Time.new(1971,  1,  2, 1, 0, 0,  7142), offset_index: 1},
        {at: Time.new(1981, 10, 27, 1, 0, 0,  7200), offset_index: 2},
        {at: Time.new(1982,  4, 10, 2, 0, 0, 10800), offset_index: 1},
        {at: Time.new(1982, 10, 27, 1, 0, 0,  7200), offset_index: 2}
      ]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200,    0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(300, 3600),
        JulianDayOfYearTransitionRule.new(100, 7200)
      )

      o0 = TimezoneOffset.new(7142,    0, 'LMT')
      o1 = TimezoneOffset.new(7200,    0, 'XST')
      o2 = TimezoneOffset.new(7200, 3600, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.new(1971,  1,  2, 1, 0, 0, 7142).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.new(1981, 10, 27, 1, 0, 0, 7200).to_i)
      tn = 1982.upto(ZoneinfoReader.const_get(:GENERATE_UP_TO)).flat_map do |year|
        [
          TimezoneTransition.new(o1, o2, Time.new(year,  4, 10, 2, 0, 0, 10800).to_i),
          TimezoneTransition.new(o2, o1, Time.new(year, 10, 27, 1, 0, 0,  7200).to_i)
        ]
      end
      t = [t0, t1] + tn

      tzif_test(offsets, transitions, rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_extends_transitions_starting_from_std_to_dst_same_year
      offsets = [
        {gmtoff:  7142, isdst: false, abbrev: 'LMT'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}
      ]

      transitions = [
        {at: Time.new(1971,  1,  2, 1, 0, 0,  7142), offset_index: 1},
        {at: Time.new(1981, 10, 27, 1, 0, 0,  7200), offset_index: 2},
        {at: Time.new(1982,  4, 10, 2, 0, 0, 10800), offset_index: 1},
        {at: Time.new(1982, 10, 27, 1, 0, 0,  7200), offset_index: 2},
        {at: Time.new(1983,  4, 10, 2, 0, 0, 10800), offset_index: 1}
      ]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200,    0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(300, 3600),
        JulianDayOfYearTransitionRule.new(100, 7200)
      )

      o0 = TimezoneOffset.new(7142,    0, 'LMT')
      o1 = TimezoneOffset.new(7200,    0, 'XST')
      o2 = TimezoneOffset.new(7200, 3600, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.new(1971,  1,  2, 1, 0, 0, 7142).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.new(1981, 10, 27, 1, 0, 0, 7200).to_i)
      tn = 1982.upto(ZoneinfoReader.const_get(:GENERATE_UP_TO)).flat_map do |year|
        [
          TimezoneTransition.new(o1, o2, Time.new(year,  4, 10, 2, 0, 0, 10800).to_i),
          TimezoneTransition.new(o2, o1, Time.new(year, 10, 27, 1, 0, 0,  7200).to_i)
        ]
      end
      t = [t0, t1] + tn

      tzif_test(offsets, transitions, rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_extends_transitions_negative_dst
      offsets = [
        {gmtoff:  7142, isdst: false, abbrev: 'LMT'},
        {gmtoff:  7200, isdst: true,  abbrev: 'XDT'},
        {gmtoff: 10800, isdst: false, abbrev: 'XST'}
      ]

      transitions = [
        {at: Time.new(1971,  1,  2, 2, 0, 0,  7142), offset_index: 1},
        {at: Time.new(1981,  4, 10, 1, 0, 0,  7200), offset_index: 2},
        {at: Time.new(1981, 10, 27, 2, 0, 0, 10800), offset_index: 1}
      ]

      rules = AnnualRules.new(
        TimezoneOffset.new(10800,     0, 'XST'),
        TimezoneOffset.new(10800, -3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(300, 7200),
        JulianDayOfYearTransitionRule.new(100, 3600)
      )

      o0 = TimezoneOffset.new( 7142,     0, 'LMT')
      o1 = TimezoneOffset.new(10800, -3600, 'XDT')
      o2 = TimezoneOffset.new(10800,     0, 'XST')

      t0 = TimezoneTransition.new(o1, o0, Time.new(1971, 1, 2, 2, 0, 0, 7142).to_i)
      tn = 1981.upto(ZoneinfoReader.const_get(:GENERATE_UP_TO)).flat_map do |year|
        [
          TimezoneTransition.new(o2, o1, Time.new(year,  4, 10, 1, 0, 0,  7200).to_i),
          TimezoneTransition.new(o1, o2, Time.new(year, 10, 27, 2, 0, 0, 10800).to_i)
        ]
      end
      t = [t0] + tn

      tzif_test(offsets, transitions, rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_extends_single_transition_in_final_year
      offsets = [
        {gmtoff:  7142, isdst: false, abbrev: 'LMT'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}
      ]

      generate_up_to = ZoneinfoReader.const_get(:GENERATE_UP_TO)

      transitions = [
        {at: Time.new(              1971,  1,  2, 2, 0, 0,  7142), offset_index: 1},
        {at: Time.new(generate_up_to - 1,  4, 10, 1, 0, 0,  7200), offset_index: 2},
        {at: Time.new(generate_up_to - 1, 10, 27, 2, 0, 0, 10800), offset_index: 1},
        {at: Time.new(generate_up_to,      4, 10, 1, 0, 0,  7200), offset_index: 2}
      ]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200, 0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(100, 3600),
        JulianDayOfYearTransitionRule.new(300, 7200)
      )

      o0 = TimezoneOffset.new(7142,    0, 'LMT')
      o1 = TimezoneOffset.new(7200,    0, 'XST')
      o2 = TimezoneOffset.new(7200, 3600, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.new(1971, 1, 2, 2, 0, 0, 7142).to_i)
      tn = (generate_up_to - 1).upto(generate_up_to).flat_map do |year|
        [
          TimezoneTransition.new(o2, o1, Time.new(year,  4, 10, 1, 0, 0,  7200).to_i),
          TimezoneTransition.new(o1, o2, Time.new(year, 10, 27, 2, 0, 0, 10800).to_i)
        ]
      end
      t = [t0] + tn

      tzif_test(offsets, transitions, rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_adds_nothing_if_transitions_up_to_final_year
      offsets = [
        {gmtoff:  7142, isdst: false, abbrev: 'LMT'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}
      ]

      generate_up_to = ZoneinfoReader.const_get(:GENERATE_UP_TO)

      transitions = [
        {at: Time.new(          1971,  1,  2, 2, 0, 0,  7142), offset_index: 1},
        {at: Time.new(generate_up_to,  4, 10, 1, 0, 0,  7200), offset_index: 2},
        {at: Time.new(generate_up_to, 10, 27, 2, 0, 0, 10800), offset_index: 1},
      ]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200, 0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(100, 3600),
        JulianDayOfYearTransitionRule.new(300, 7200)
      )

      o0 = TimezoneOffset.new(7142,    0, 'LMT')
      o1 = TimezoneOffset.new(7200,    0, 'XST')
      o2 = TimezoneOffset.new(7200, 3600, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.new(          1971,  1,  2, 2, 0, 0,  7142).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.new(generate_up_to,  4, 10, 1, 0, 0,  7200).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.new(generate_up_to, 10, 27, 2, 0, 0, 10800).to_i)

      tzif_test(offsets, transitions, rules: rules) do |path, format|
        assert_equal([t0,t1,t2], @reader.read(path))
      end
    end

    def test_read_tz_string_corrects_offset_of_final_transition_same_year
      offsets = [
        {gmtoff: 3542, isdst: false, abbrev: 'LMT'},
        {gmtoff: 7200, isdst: true,  abbrev: 'XDT'}]

      transitions = [{at: Time.new(2000, 4, 10, 1, 0, 0, 3542), offset_index: 1}]

      rules = AnnualRules.new(
        TimezoneOffset.new(3600,    0, 'XST'),
        TimezoneOffset.new(3600, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(100, 3600),
        JulianDayOfYearTransitionRule.new(300, 7200)
      )

      o0 = TimezoneOffset.new(3542,    0, 'LMT')
      o1 = TimezoneOffset.new(3600, 3600, 'XDT') # without tz_string would be 3542, 3658, 'XDT'
      o2 = TimezoneOffset.new(3600,    0, 'XST')

      t0 = TimezoneTransition.new(o1, o0, Time.new(2000,  4, 10, 1, 0, 0, 3542).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.new(2000, 10, 27, 2, 0, 0, 7200).to_i)
      tn = 2001.upto(ZoneinfoReader.const_get(:GENERATE_UP_TO)).flat_map do |year|
        [
          TimezoneTransition.new(o1, o2, Time.new(year,  4, 10, 1, 0, 0, 3600).to_i),
          TimezoneTransition.new(o2, o1, Time.new(year, 10, 27, 2, 0, 0, 7200).to_i)
        ]
      end
      t = [t0, t1] + tn

      tzif_test(offsets, transitions, rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_specifies_transition_to_offset_of_final_transition_same_year_skip_dst_start
      # TZInfo v2.0.3 considered this to be an error. However, this is a valid
      # situation with Africa/Casablanca in 2018e.
      #
      # The last defined transitions are:
      #   At 2037-03-29 02:00Z change to WEST UTC+1
      #   At 2037-10-04 02:00Z change to WET  UTC+0
      #
      # The rules define the end of DST to be at 03:00 local time on the last
      # Sunday of October (2037-10-31). This later transition needs to be
      # ignored.

      offsets = [
        {gmtoff:  7142, isdst: false, abbrev: 'LMT'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}
      ]

      transitions = [
        {at: Time.new(1971,  1,  2, 2, 0, 0,  7142), offset_index: 1},
        {at: Time.new(1981,  4, 10, 1, 0, 0,  7200), offset_index: 2},
        {at: Time.new(1981, 10, 27, 2, 0, 0, 10800), offset_index: 1},
        {at: Time.new(1982,  4, 10, 1, 0, 0,  7200), offset_index: 2}
      ]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200, 0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(101, 3600),
        JulianDayOfYearTransitionRule.new(300, 7200)
      )

      o0 = TimezoneOffset.new(7142,    0, 'LMT')
      o1 = TimezoneOffset.new(7200,    0, 'XST')
      o2 = TimezoneOffset.new(7200, 3600, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.new(1971,  1,  2, 2, 0, 0,  7142).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.new(1981,  4, 10, 1, 0, 0,  7200).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.new(1981, 10, 27, 2, 0, 0, 10800).to_i)
      t3 = TimezoneTransition.new(o2, o1, Time.new(1982,  4, 10, 1, 0, 0,  7200).to_i)
      t4 = TimezoneTransition.new(o1, o2, Time.new(1982, 10, 27, 2, 0, 0, 10800).to_i)
      tn = 1983.upto(ZoneinfoReader.const_get(:GENERATE_UP_TO)).flat_map do |year|
        [
          TimezoneTransition.new(o2, o1, Time.new(year,  4, 11, 1, 0, 0,  7200).to_i),
          TimezoneTransition.new(o1, o2, Time.new(year, 10, 27, 2, 0, 0, 10800).to_i)
        ]
      end
      t = [t0, t1, t2, t3, t4] + tn

      tzif_test(offsets, transitions, rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_specifies_transition_to_offset_of_final_transition_same_year_skip_dst_end
      # TZInfo v2.0.3 considered this to be an error. However, this is a valid
      # situation with Africa/Casablanca in 2018e.
      #
      # The last defined transitions are:
      #   At 2037-03-29 02:00Z change to WEST UTC+1
      #   At 2037-10-04 02:00Z change to WET  UTC+0
      #
      # The rules define the end of DST to be at 03:00 local time on the last
      # Sunday of October (2037-10-31). This later transition needs to be
      # ignored.

      offsets = [
        {gmtoff:  7142, isdst: false, abbrev: 'LMT'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}
      ]

      transitions = [
        {at: Time.new(1971,  1,  2, 2, 0, 0,  7142), offset_index: 1},
        {at: Time.new(1981,  4, 10, 1, 0, 0,  7200), offset_index: 2},
        {at: Time.new(1981, 10, 27, 2, 0, 0, 10800), offset_index: 1}
      ]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200, 0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(100, 3600),
        JulianDayOfYearTransitionRule.new(301, 7200)
      )

      o0 = TimezoneOffset.new(7142,    0, 'LMT')
      o1 = TimezoneOffset.new(7200,    0, 'XST')
      o2 = TimezoneOffset.new(7200, 3600, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.new(1971,  1,  2, 2, 0, 0,  7142).to_i)
      t1 = TimezoneTransition.new(o2, o1, Time.new(1981,  4, 10, 1, 0, 0,  7200).to_i)
      t2 = TimezoneTransition.new(o1, o2, Time.new(1981, 10, 27, 2, 0, 0, 10800).to_i)
      tn = 1982.upto(ZoneinfoReader.const_get(:GENERATE_UP_TO)).flat_map do |year|
        [
          TimezoneTransition.new(o2, o1, Time.new(year,  4, 10, 1, 0, 0,  7200).to_i),
          TimezoneTransition.new(o1, o2, Time.new(year, 10, 28, 2, 0, 0, 10800).to_i)
        ]
      end
      t = [t0, t1, t2] + tn

      tzif_test(offsets, transitions, rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_specifies_transition_to_offset_of_final_transition_following_year
      offsets = [
        {gmtoff:  7142, isdst: false, abbrev: 'LMT'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}
      ]

      transitions = [
        {at: Time.new(1971,  1,  2, 2, 0, 0, 7142), offset_index: 1},
        {at: Time.new(1981, 10, 27, 2, 0, 0, 7200), offset_index: 2},
      ]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200, 0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(100, 3600),
        JulianDayOfYearTransitionRule.new(299, 7200)
      )

      tzif_test(offsets, transitions, rules: rules) do |path, format|
        error = assert_raises(InvalidZoneinfoFile) { @reader.read(path) }
        assert_equal("The first offset indicated by the POSIX-style TZ string did not match the final defined offset in file '#{path}'.", error.message)
      end
    end

    def test_read_tz_string_generates_from_last_transition_if_before_1970
      offsets = [
        {gmtoff:  7142, isdst: false, abbrev: 'LMT'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}
      ]

      transitions = [
        {at: Time.new(1961,  1,  2, 2, 0, 0,  7142), offset_index: 1},
        {at: Time.new(1962,  4, 10, 1, 0, 0,  7200), offset_index: 2},
        {at: Time.new(1962, 10, 27, 2, 0, 0, 10800), offset_index: 1}
      ]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200, 0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(100, 3600),
        JulianDayOfYearTransitionRule.new(300, 7200)
      )

      o0 = TimezoneOffset.new(7142,    0, 'LMT')
      o1 = TimezoneOffset.new(7200,    0, 'XST')
      o2 = TimezoneOffset.new(7200, 3600, 'XDT')

      t0 = TimezoneTransition.new(o1, o0, Time.new(1961, 1, 2, 2, 0, 0, 7142).to_i)
      tn = 1962.upto(ZoneinfoReader.const_get(:GENERATE_UP_TO)).flat_map do |year|
        [
          TimezoneTransition.new(o2, o1, Time.new(year,  4, 10, 1, 0, 0,  7200).to_i),
          TimezoneTransition.new(o1, o2, Time.new(year, 10, 27, 2, 0, 0, 10800).to_i)
        ]
      end
      t = [t0] + tn

      tzif_test(offsets, transitions, rules: rules) do |path, format|
        assert_equal(t, @reader.read(path))
      end
    end

    def test_read_tz_string_reuses_offset_instances_when_adding_to_exsisting_transitions
      offsets = [
        {gmtoff:  7142, isdst: false, abbrev: 'LMT'},
        {gmtoff:  7200, isdst: false, abbrev: 'XST'},
        {gmtoff: 10800, isdst: true,  abbrev: 'XDT'}
      ]

      transitions = [
        {at: Time.new(1971,  1,  2, 2, 0, 0,  7142), offset_index: 1},
        {at: Time.new(1981,  4, 10, 1, 0, 0,  7200), offset_index: 2},
        {at: Time.new(1981, 10, 27, 2, 0, 0, 10800), offset_index: 1}
      ]

      rules = AnnualRules.new(
        TimezoneOffset.new(7200, 0, 'XST'),
        TimezoneOffset.new(7200, 3600, 'XDT'),
        JulianDayOfYearTransitionRule.new(100, 3600),
        JulianDayOfYearTransitionRule.new(300, 7200)
      )

      tzif_test(offsets, transitions, rules: rules) do |path, format|
        transitions = @reader.read(path)
        xst = transitions[0].offset
        xdt = transitions[1].offset
        assert_equal(TimezoneOffset.new(7200,    0, 'XST'), xst)
        assert_equal(TimezoneOffset.new(7200, 3600, 'XDT'), xdt)

        1.upto((transitions.length - 1) / 2) do |i| # 2, 4, 6, ...
          assert_same(xst, transitions[i * 2].offset)
          assert_same(xdt, transitions[i * 2].previous_offset)
        end

        1.upto(transitions.length / 2 - 1) {|i| assert_same(xdt, transitions[i * 2 + 1].offset) } # 3, 5, 7, ...
        1.upto(transitions.length / 2) {|i| assert_same(xst, transitions[i * 2 - 1].previous_offset) } # 1, 3, 5, ...
      end
    end

    def test_read_tz_string_as_utf8
      offsets = [{gmtoff: 3600, isdst: false, abbrev: 'áccént'}]
      rules = TimezoneOffset.new(3600, 0, 'áccént')

      tzif_test(offsets, [], tz_string: '<áccént>1', rules: rules) do |path, format|
        # FakePosixTimeZoneParser will test that the tz_string matches.
        assert_same(rules, @reader.read(path))
      end
    end
  end
end
