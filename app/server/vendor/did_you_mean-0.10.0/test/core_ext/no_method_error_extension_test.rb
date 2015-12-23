require 'test_helper'

class NoMethodErrorExtensionTest < Minitest::Test
  def test_receiver_with_string
    receiver = "receiver"
    error = assert_raises(NoMethodError) do
      receiver.doesnt_exist
    end

    assert_same receiver, error.receiver
  end

  def test_receiver_with_class
    error = assert_raises(NoMethodError) do
      Object.doesnt_exist
    end

    assert_same Object, error.receiver
  end

  def test_receiver_with_class_after_calling_to_s
    error = assert_raises(NoMethodError) do
      Object.doesnt_exist
    end

    error.to_s
    assert_same Object, error.receiver
  end

  def test_receiver_with_class_after_calling_message
    error = assert_raises(NoMethodError) do
      Object.doesnt_exist
    end

    error.message
    assert_same Object, error.receiver
  end
end
