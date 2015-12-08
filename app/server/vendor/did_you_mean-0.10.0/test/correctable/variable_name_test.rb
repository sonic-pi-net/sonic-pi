require 'test_helper'

class VariableNameTest < Minitest::Test
  class User
    def initialize
      @email_address = 'email_address@address.net'
    end

    def first_name; end
    def to_s
      "#{@first_name} #{@last_name} <#{email_address}>"
    end

    private

    def cia_codename; "Alexa" end
  end

  module UserModule
    def from_module; end
  end

  def setup
    @user = User.new.extend(UserModule)
  end

  def test_suggestions_include_instance_method
    error = assert_raises(NAME_ERROR) do
      @user.instance_eval { flrst_name }
    end

    assert_suggestion :first_name, error.suggestions
    assert_match "Did you mean? first_name", error.to_s
  end

  def test_suggestions_include_method_from_module
    error = assert_raises(NAME_ERROR) do
      @user.instance_eval { fr0m_module }
    end

    assert_suggestion :from_module, error.suggestions
    assert_match "Did you mean? from_module", error.to_s
  end

  def test_suggestions_include_local_variable_name
    person  = nil
    error = (eprson rescue $!) # Do not use @assert_raises here as it changes a scope.

    assert_suggestion :person, error.suggestions
    assert_match "Did you mean? person", error.to_s
  end

  def test_suggestions_include_instance_variable_name
    error = assert_raises(NAME_ERROR){ @user.to_s }

    assert_suggestion :@email_address, error.suggestions
    assert_match "Did you mean? @email_address", error.to_s
  end

  def test_suggestions_include_private_method
    error = assert_raises(NAME_ERROR) do
      @user.instance_eval { cia_code_name }
    end

    assert_suggestion :cia_codename,  error.suggestions
    assert_match "Did you mean? cia_codename",  error.to_s
  end

  @@does_exist = true

  def test_suggestions_include_class_variable_name
    skip if RUBY_ENGINE == 'rbx'

    error = assert_raises(NameError){ @@doesnt_exist }

    assert_suggestion :@@does_exist, error.suggestions
    assert_match "Did you mean? @@does_exist", error.to_s
  end
end
