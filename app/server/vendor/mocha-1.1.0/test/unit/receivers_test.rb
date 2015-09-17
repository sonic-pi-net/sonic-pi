require File.expand_path('../../test_helper', __FILE__)
require 'mocha/receivers'

class ObjectReceiverTest < Mocha::TestCase
  include Mocha

  class FakeObject < Struct.new(:mocha)
    def is_a?(klass)
      false
    end
  end

  class FakeClass < Struct.new(:superclass, :mocha)
    def is_a?(klass)
      klass == Class
    end
  end

  def test_mocks_returns_mock_for_object
    object = FakeObject.new(:mocha)
    receiver = ObjectReceiver.new(object)
    assert_equal [:mocha], receiver.mocks
  end

  def test_mocks_returns_mocks_for_class_and_its_superclasses
    grandparent = FakeClass.new(nil, :grandparent_mocha)
    parent = FakeClass.new(grandparent, :parent_mocha)
    klass = FakeClass.new(parent, :mocha)
    receiver = ObjectReceiver.new(klass)
    assert_equal [:mocha, :parent_mocha, :grandparent_mocha], receiver.mocks
  end
end

class AnyInstanceReceiverTest < Mocha::TestCase
  include Mocha

  class FakeAnyInstanceClass
    attr_reader :superclass

    def initialize(superclass, mocha)
      @superclass, @mocha = superclass, mocha
    end

    def any_instance
      Struct.new(:mocha).new(@mocha)
    end
  end

  def test_mocks_returns_mocks_for_class_and_its_superclasses
    grandparent = FakeAnyInstanceClass.new(nil, :grandparent_mocha)
    parent = FakeAnyInstanceClass.new(grandparent, :parent_mocha)
    klass = FakeAnyInstanceClass.new(parent, :mocha)
    receiver = AnyInstanceReceiver.new(klass)
    assert_equal [:mocha, :parent_mocha, :grandparent_mocha], receiver.mocks
  end
end

class DefaultReceiverTest < Mocha::TestCase
  include Mocha

  def test_mocks_returns_mock
    mock = :mocha
    receiver = DefaultReceiver.new(mock)
    assert_equal [:mocha], receiver.mocks
  end
end
