require_relative 'test_helper'

class SimilarNameFinderTest < Minitest::Test
  class User
    def call_flrst_name;  f1rst_name; end
    def call_fr0m_module; fr0m_module; end
    def first_name; end
  end

  module UserModule
    def from_module; end
  end

  def setup
    user = User.new.extend(UserModule)

    @error_from_instance_method = assert_raises(NameError){ user.call_flrst_name }
    @error_from_module_method   = assert_raises(NameError){ user.call_fr0m_module }

    # Use begin + rescue as #assert_raises changes a scope.
    @error_from_local_variable  = begin
      userr
    rescue NameError => e
      e
    end
  end

  def test_similar_words
    assert_includes @error_from_instance_method.finder.similar_words, "first_name"
    assert_includes @error_from_module_method.finder.similar_words,   "from_module"
    assert_includes @error_from_local_variable.finder.similar_words,  "user"
  end

  def test_similar_instance_methods
    assert_includes @error_from_instance_method.finder.similar_methods, "first_name"
    assert_includes @error_from_module_method.finder.similar_methods,   "from_module"
  end

  def test_similar_local_variables
    assert_includes @error_from_local_variable.finder.similar_local_variables, "user"
  end

  def test_did_you_mean?
    assert_match "Did you mean? #first_name",  @error_from_instance_method.did_you_mean?
    assert_match "Did you mean? #from_module", @error_from_module_method.did_you_mean?
    assert_match "Did you mean? user",         @error_from_local_variable.did_you_mean?
  end
end
