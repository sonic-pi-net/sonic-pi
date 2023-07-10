# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'
require 'concurrent'

class TCStringDeduper < Minitest::Test
  include TZInfo

  def test_string_deduper_dedupe
    dedupe_tests(StringDeduper.new)
  end

  def test_concurrent_string_deduper_dedupe
    dedupe_tests(ConcurrentStringDeduper.new)
  end

  if TZInfo.const_defined?(:UnaryMinusGlobalStringDeduper)
    def test_unary_minus_global_string_deduper
      dedupe_tests(UnaryMinusGlobalStringDeduper.new)
    end
  end

  def test_global_uses_unary_minus_when_available
    global = StringDeduper.global

    if RUBY_VERSION >= '2.5'
      assert_kind_of(UnaryMinusGlobalStringDeduper, global)
    else
      assert_kind_of(StringDeduper, global)
    end
  end

  def test_global_returns_same_instance
    global = StringDeduper.global
    assert_same(global, StringDeduper.global)
  end

  def test_global_dedupe
    dedupe_tests(StringDeduper.global)
  end

  private

  def dedupe_tests(sd)
    s1 = rand.to_s
    s2 = s1.dup
    s3 = s1.dup.freeze

    refute_same(s1, s2)
    refute_same(s1, s3)
    assert_equal(false, s1.frozen?)
    assert_equal(false, s2.frozen?)
    assert_equal(true, s3.frozen?)

    r1 = sd.dedupe(s1)

    assert_equal(s1, r1)
    refute_same(s1, r1)
    assert_equal(false, s1.frozen?)
    assert_equal(true, r1.frozen?)

    r2 = sd.dedupe(s2)

    assert_equal(false, s2.frozen?)
    assert_same(r1, r2)

    r3 = sd.dedupe(s3)

    assert_equal(s3, r3)
    refute_same(r1, r3)
  end

  def dedupe_tests_with_hash_class_check(sd, hash_class)
    assert_same(hash_class, sd.hash_class)
    dedupe_tests(sd)
  end
end
