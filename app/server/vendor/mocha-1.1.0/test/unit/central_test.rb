require File.expand_path('../../test_helper', __FILE__)

require 'mocha/central'
require 'mocha/mock'
require 'method_definer'

class CentralTest < Mocha::TestCase

  include Mocha

  def test_should_start_with_empty_stubba_methods
    stubba = Central.new

    assert_equal [], stubba.stubba_methods
  end

  def test_should_stub_method_if_not_already_stubbed
    method = build_mock
    method.expects(:stub)
    stubba = Central.new

    stubba.stub(method)

    assert method.__verified__?
  end

  def test_should_not_stub_method_if_already_stubbed
    method = build_mock
    method.stubs(:matches?).returns(true)
    method.expects(:stub).times(0)
    stubba = Central.new
    stubba.stubba_methods = [method]

    stubba.stub(method)

    assert method.__verified__?
  end

  def test_should_record_method
    method = build_mock
    method.expects(:stub)
    stubba = Central.new

    stubba.stub(method)

    assert_equal [method], stubba.stubba_methods
  end

  def test_should_unstub_specified_method
    stubba = Central.new
    method_1 = build_mock
    method_1.stubs(:matches?).returns(false)
    method_2 = build_mock
    method_2.stubs(:matches?).returns(true)
    method_2.expects(:unstub)
    stubba.stubba_methods = [method_1, method_2]

    stubba.unstub(method_2)

    assert_equal [method_1], stubba.stubba_methods
    assert method_2.__verified__?
  end

  def test_should_not_unstub_specified_method_if_not_already_stubbed
    stubba = Central.new
    method_1 = build_mock
    method_1.stubs(:matches?).returns(false)
    method_2 = build_mock
    method_2.expects(:unstub).never
    stubba.stubba_methods = [method_1]

    stubba.unstub(method_2)

    assert_equal [method_1], stubba.stubba_methods
    assert method_2.__verified__?
  end

  def test_should_unstub_all_methods
    stubba = Central.new
    method_1 = build_mock
    method_1.stubs(:matches?).returns(true)
    method_1.expects(:unstub)
    method_2 = build_mock
    method_2.stubs(:matches?).returns(true)
    method_2.expects(:unstub)
    stubba.stubba_methods = [method_1, method_2]

    stubba.unstub_all

    assert_equal [], stubba.stubba_methods
    assert method_1.__verified__?
    assert method_2.__verified__?
  end

  private

  def build_mock
    Mock.new(nil)
  end
end
