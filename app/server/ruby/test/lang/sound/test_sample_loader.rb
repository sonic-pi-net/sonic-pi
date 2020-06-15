#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/sample_loader"

module SonicPi
  class SampleLoaderTester < Minitest::Test
    def setup
      @fake_built_in_sample_dir = File.expand_path("#{File.dirname(__FILE__)}/../../fake_built_in_sample_dir")
      # Contains:
      # - beans.flac
      # - foo.wav
      # - quux.wav
      @fake_sample_dir = File.expand_path("#{File.dirname(__FILE__)}/../../fake_sample_dir")
      # Contains:
      # -  a_text_file.txt
      # -  bar_baz.aiff
      # -  buzz_100.flac
      # -  eggs.wav
      # -  first_foo.wav
      # -  foo.wav
      # -  woo_100.aiff
      # -  xylophone-test-samp.wave
      # -  subdir
      #    |
      #    - quux.wav
      @loader = SampleLoader.new(@fake_built_in_sample_dir)
    end

    def test_ls_candidates
      files = [
        @fake_built_in_sample_dir + "/beans.flac",
        @fake_built_in_sample_dir + "/foo.wav",
        @fake_built_in_sample_dir + "/quux.wav"]

      assert_equal(@loader.ls_samples(@fake_built_in_sample_dir), files)
      # test cache
      assert_equal(@loader.ls_samples(@fake_built_in_sample_dir), files)
    end

    def test_split_candidates_and_filts_simple
      filts_and_sources = [@fake_sample_dir, 1]
      candidates, filts = @loader.split_candidates_and_filts(filts_and_sources)
      assert_equal([@fake_sample_dir], candidates)
      assert_equal([1], filts)
    end

    def test_split_candidates_and_filts_no_filts
      filts_and_sources = [@fake_sample_dir]
      candidates, filts = @loader.split_candidates_and_filts(filts_and_sources)
      assert_equal([@fake_sample_dir], candidates)
      assert_equal([], filts)
    end

    def test_split_candidates_and_filts_multi_candidates_no_filts
      filts_and_sources = [@fake_sample_dir, @fake_sample_dir + "/eggs.wav"]
      candidates, filts = @loader.split_candidates_and_filts(filts_and_sources)
      assert_equal([@fake_sample_dir, @fake_sample_dir + "/eggs.wav"], candidates)
      assert_equal([], filts)
    end

    def test_split_candidates_and_filts_no_candidates_no_filts
      filts_and_sources = []
      candidates, filts = @loader.split_candidates_and_filts(filts_and_sources)
      assert_equal([], candidates)
      assert_equal([], filts)
    end

    def test_split_candidates_and_filts_no_candidates_one_filt
      filts_and_sources = [:foo]
      candidates, filts = @loader.split_candidates_and_filts(filts_and_sources)
      assert_equal([], candidates)
      assert_equal([:foo], filts)
    end

    def test_split_candidates_and_filts_no_candidates_one_filt2
      filts_and_sources = [:woo_100]
      candidates, filts = @loader.split_candidates_and_filts(filts_and_sources)
      assert_equal([], candidates)
      assert_equal([:woo_100], filts)
    end

    def test_split_candidates_and_filts_no_candidates_one_string_filt
      filts_and_sources = ["foobar"]
      candidates, filts = @loader.split_candidates_and_filts(filts_and_sources)
      assert_equal([], candidates)
      assert_equal(["foobar"], filts)
    end

    def test_split_candidates_and_filts_no_candidates_multi_filts
      filts_and_sources = ["foobar", 1, :foo, /bar/]
      candidates, filts = @loader.split_candidates_and_filts(filts_and_sources)
      assert_equal([], candidates)
      assert_equal(["foobar", 1, :foo, /bar/], filts)
    end

    def test_split_candidates_and_filts_glob_candidate
      filts_and_sources = [@fake_sample_dir + "/**"]
      candidates, filts = @loader.split_candidates_and_filts(filts_and_sources)
      assert_equal([@fake_sample_dir + "/**"], candidates)
      assert_equal([], filts)
    end

    def test_nil
      res = @loader.find_candidates([nil])
      assert_equal([], res)
    end

    def test_single_builtin_symbol
      res = @loader.find_candidates([:beans])
      assert_equal(res, ["#{@fake_built_in_sample_dir}/beans.flac"])
    end

    def test_single_builtin_symbol_cache
      res = @loader.find_candidates([:foo])
      assert_equal(res, ["#{@fake_built_in_sample_dir}/foo.wav"])
      res = @loader.find_candidates([:foo])
      assert_equal(res, ["#{@fake_built_in_sample_dir}/foo.wav"])
      res = @loader.find_candidates([:foo])
      assert_equal(res, ["#{@fake_built_in_sample_dir}/foo.wav"])
      res = @loader.find_candidates([:quux])
      assert_equal(res, ["#{@fake_built_in_sample_dir}/quux.wav"])
      res = @loader.find_candidates([:quux])
      assert_equal(res, ["#{@fake_built_in_sample_dir}/quux.wav"])
    end

    def test_path_and_symbol_does_not_include_builtin_samples
      res = @loader.find_candidates([@fake_sample_dir, :foo])
      refute_includes(res, "#{@fake_built_in_sample_dir}/foo.wav")
    end


    def test_globs_count
      res = @loader.find_candidates(["#{@fake_sample_dir}/**"])
      assert_equal(res.count, 8)
    end

    def test_non_globs_count
      res = @loader.find_candidates([@fake_sample_dir])
      assert_equal(res.count, 7)
    end

    def test_globs_filt
      res = @loader.find_candidates(["#{@fake_sample_dir}/**", :quux])
      assert_equal(["#{@fake_sample_dir}/subdir/quux.wav"], res)
    end

    def test_non_globs_filt_does_not_include_subdir
      res = @loader.find_candidates([@fake_sample_dir, :quux])
      assert_equal([], res)
    end

    def test_non_globs_symbol_matches_name_exactly
      res = @loader.find_candidates([@fake_sample_dir, :foo])
      assert_equal(res, ["#{@fake_sample_dir}/foo.wav"])
    end

    def test_idx
      res = @loader.find_candidates([@fake_sample_dir, 1])
      assert_equal(["#{@fake_sample_dir}/buzz_100.flac"], res)
    end

    def test_mult_idx_overrides
      res = @loader.find_candidates([@fake_sample_dir, 1, 3, 5, 1])
      assert_equal(["#{@fake_sample_dir}/buzz_100.flac"], res)
    end

    def test_idx_wrap
      res = @loader.find_candidates([@fake_sample_dir, 7])
      assert_equal(["#{@fake_sample_dir}/bar_baz.aiff"], res)
    end

    def test_glob_idx_wrap
      res = @loader.find_candidates([@fake_sample_dir + "/**", 8])
      assert_equal(["#{@fake_sample_dir}/bar_baz.aiff"], res)
    end

    def test_str_filter
      res = @loader.find_candidates([@fake_sample_dir, "100"])
      assert_equal(["#{@fake_sample_dir}/buzz_100.flac",
                    "#{@fake_sample_dir}/woo_100.aiff"], res)
    end

    def test_regex_filter
      res = @loader.find_candidates([@fake_sample_dir, /b[uU]zz/])
      assert_equal(["#{@fake_sample_dir}/buzz_100.flac"], res)
    end

    def test_1_arity_proc_filter
      proc = lambda {|c| c.keep_if { |v| v.include? "foo"}}
      res = @loader.find_candidates([@fake_sample_dir, proc])
      assert_equal(["#{@fake_sample_dir}/first_foo.wav",
                    "#{@fake_sample_dir}/foo.wav"], res)
    end

    def test_multi_filters
      identity_proc = lambda {|c| c }
      res = @loader.find_candidates([@fake_sample_dir, identity_proc, "100", /100/, 1])
      assert_equal(["#{@fake_sample_dir}/woo_100.aiff"], res)
    end

    def test_multi_filters_with_nil
      identity_proc = lambda {|c| c }
      res = @loader.find_candidates([@fake_sample_dir, nil, identity_proc, "100", /100/, nil, 1])
      assert_equal(["#{@fake_sample_dir}/woo_100.aiff"], res)
    end

    def test_arrays
      identity_proc = lambda {|c| c }
      res = @loader.find_candidates([@fake_sample_dir, [[identity_proc, "100"], /100/], [1]])
      assert_equal(["#{@fake_sample_dir}/woo_100.aiff"], res)
    end

    def test_string_filtering_with_hyphens
      res = @loader.find_candidates([@fake_sample_dir, "xylophone-"])
      assert_equal(["#{@fake_sample_dir}/xylophone-test-samp.wave"], res)
    end

    def test_empty_strings_dont_filter
      res = @loader.find_candidates([@fake_sample_dir, "", "xylophone-", "", ""])
      assert_equal(["#{@fake_sample_dir}/xylophone-test-samp.wave"], res)
    end

    def test_ring_source
      files = [
        @fake_built_in_sample_dir + "/beans.flac",
        @fake_built_in_sample_dir + "/foo.wav",
        @fake_built_in_sample_dir + "/quux.wav"].ring

      res = @loader.find_candidates([files])
      assert_equal(res, files.to_a)
    end
  end
end
