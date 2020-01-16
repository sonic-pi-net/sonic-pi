require "test_helper"

class RepositoryIgnoreTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2 "attr"
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
