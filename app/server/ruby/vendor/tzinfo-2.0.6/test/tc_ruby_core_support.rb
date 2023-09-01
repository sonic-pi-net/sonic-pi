# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCRubyCoreSupport < Minitest::Test
  def test_untaint_returns_object
    o = Object.new
    assert_same(o, TZInfo.const_get(:RubyCoreSupport).untaint(o))
  end

  def test_untaint_untaints_object
    skip_if_taint_is_undefined_or_no_op
    o = Object.new.taint
    assert(o.tainted?)
    TZInfo.const_get(:RubyCoreSupport).untaint(o)
    refute(o.tainted?)
  end
end
