require "test_helper"

class ErrorsTest < Rugged::TestCase

  def test_rugged_error_classes_exist
    error_classes = [
      Rugged::NoMemError,
      Rugged::OSError,
      Rugged::InvalidError,
      Rugged::Error,
      Rugged::ReferenceError,
      Rugged::ZlibError,
      Rugged::RepositoryError,
      Rugged::ConfigError,
      Rugged::RegexError,
      Rugged::OdbError,
      Rugged::IndexError,
      Rugged::ObjectError,
      Rugged::NetworkError,
      Rugged::TagError,
      Rugged::TreeError,
      Rugged::IndexerError
    ]

    # All should descend from StandardError (correctly), except
    # Rugged::NoMemError which descends from Ruby's built-in NoMemoryError,
    # which descends from Exception
    error_classes.each do |klass|
      err = klass.new
      assert err.is_a?(Exception)
    end
  end
end

