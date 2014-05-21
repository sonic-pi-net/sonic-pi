require "test_helper"

class RepositoryIgnoreTest < Rugged::SandboxedTestCase
  def setup
    super

    @repo = sandbox_init "attr"
  end

  def teardown
    @repo.close

    super
  end

  def test_path_ignored
    assert @repo.path_ignored?("ign")
    refute @repo.path_ignored?("ignore_not")
    assert @repo.path_ignored?("dir")
    assert @repo.path_ignored?("dir/foo")
    assert @repo.path_ignored?("dir/foo/bar")
    refute @repo.path_ignored?("foo/dir")
    assert @repo.path_ignored?("foo/dir/bar")
    refute @repo.path_ignored?("direction")
  end
end

