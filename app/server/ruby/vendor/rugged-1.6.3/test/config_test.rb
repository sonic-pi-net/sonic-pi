require "test_helper"

class ConfigTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_rugged("testrepo.git")

    path = Dir.mktmpdir("rugged-global-config")
    cfg = Rugged::Config.new(File.join(path, ".gitconfig"))
    cfg['user.name'] = "The test suite"
    Rugged::Settings['search_path_global'] = path
    @glocalconfigdir = path
  end

  def cleanup
    FileUtils.remove_entry_secure(@globalconfigdir)
  end

  def test_multi_fetch
    config = @repo.config
    fetches = ["+refs/heads/*:refs/remotes/test_remote/*",
               "+refs/heads/*:refs/remotes/hello_remote/*"]
    assert_equal fetches, config.get_all("remote.test_multiple_fetches.fetch")
  end

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
    refute_nil config['user.name']
    assert_nil config['core.bare']
  end

  def test_snapshot
    config = Rugged::Config.new(File.join(@repo.path, 'config'))
    config['old.value'] = 5

    snapshot = config.snapshot
    assert_equal '5', snapshot['old.value']

    config['new.value'] = 42
    config['old.value'] = 1337

    assert_equal '5', snapshot['old.value']
    assert_nil snapshot['new.value']
  end

  def test_each_key_is_a_string
    config = @repo.config
    config.each_key do |key|
      assert key.is_a?(String)
    end
  end

  def test_each_pair_is_pairs
    config = @repo.config
    config.each_pair do |key, value|
      assert key.is_a?(String)
      assert value.is_a?(String)
    end
  end

  def test_transaction
    config = Rugged::Config.new(File.join(@repo.path, 'config'))
    config['section.name'] = 'value'

    config2 = Rugged::Config.new(File.join(@repo.path, 'config'))

    config.transaction do
      config['section.name'] = 'other value'
      config['section2.name3'] = 'more value'

      assert_equal 'value', config2['section.name']
      assert_nil config2['section2.name3']

      assert_equal 'value', config['section.name']
      assert_nil config['section2.name3']
    end

    assert_equal 'other value', config['section.name']
    assert_equal 'more value', config['section2.name3']
    assert_equal 'other value', config2['section.name']
    assert_equal 'more value', config2['section2.name3']
  end
end

class ConfigWriteTest < Rugged::TestCase
  def setup
    @source_repo = FixtureRepo.from_rugged("testrepo.git")
    @repo = FixtureRepo.clone(@source_repo)
    @path = @repo.workdir
  end

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

  def test_invalid_values
    Tempfile.open("config-value") do |file|
      file.write(<<-CONFIG)
      [section "subsection"]
      line1 = string1
      line2
      line3 = string2
      CONFIG
      file.flush

      config = Rugged::Config.new(file.path)
      config.each_pair.to_a # just checking it doesn't crash
      config.to_hash

      # We expect an empty string due to how we look up specific keys
      assert_equal "string1", config["section.subsection.line1"]
      assert_equal "", config["section.subsection.line2"]
      assert_equal "string2", config["section.subsection.line3"]
    end
  end
end
