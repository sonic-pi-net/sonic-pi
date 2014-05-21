require "test_helper"

class ConfigTest < Rugged::TestCase 
  include Rugged::RepositoryAccess

  def test_read_config_file
    config = @repo.config
    assert_equal 'false', config['core.bare']
    assert_nil config['not.exist']
  end

  def test_read_config_from_path
    config = Rugged::Config.new(File.join(@repo.path, 'config'))
    assert_equal 'false', config['core.bare']
  end

  def test_read_global_config_file
    config = Rugged::Config.global
    assert config['user.name'] != nil
    assert_nil config['core.bare']
  end
end

class ConfigWriteTest < Rugged::TestCase
  include Rugged::TempRepositoryAccess

  def test_write_config_values
    config = @repo.config
    config['custom.value'] = 'my value'

    config2 = @repo.config
    assert_equal 'my value', config2['custom.value']

    content = File.read(File.join(@repo.path, 'config'))
    assert_match(/value = my value/, content)
  end

  def test_delete_config_values
    config = @repo.config
    config.delete('core.bare')

    config2 = @repo.config
    assert_nil config2.get('core.bare')
  end
end
