require "test_helper"

class RepositoryResetTest < Rugged::TestCase
  def setup
    @source_repo = FixtureRepo.from_rugged("testrepo.git")
    @repo = FixtureRepo.clone(@source_repo)
  end

  def repo_file_path; File.join('subdir', 'README') end
  def file_path;      File.join(@repo.workdir, 'subdir', 'README') end

  def test_reset_with_rugged_tag
    tag = @repo.lookup('0c37a5391bbff43c37f0d0371823a5509eed5b1d')
    @repo.reset(tag, :soft)
    assert_equal tag.target_id , @repo.head.target_id
  end

  def test_reset_with_invalid_mode
    assert_raises ArgumentError do
      @repo.reset('441034f860c1d5d90e4188d11ae0d325176869a8', :tender)
    end
  end

  def test_reset_soft
    original_content = File.open(file_path) { |f| f.read }

    @repo.reset('441034f860c1d5d90e4188d11ae0d325176869a8', :soft)
    assert_equal '441034f860c1d5d90e4188d11ae0d325176869a8', @repo.head.target_id
    assert_equal [:index_modified], @repo.status(repo_file_path)

    new_content = File.open(file_path) { |f| f.read }

    assert_equal original_content, new_content
  end

  def test_reset_mixed
    original_content = File.open(file_path) { |f| f.read }

    @repo.reset('441034f860c1d5d90e4188d11ae0d325176869a8', :mixed)
    assert_equal [:worktree_modified], @repo.status(repo_file_path)

    new_content = File.open(file_path) { |f| f.read }

    assert_equal original_content, new_content
  end

  def test_reset_hard
    original_content = File.open(file_path) { |f| f.read }

    @repo.reset('441034f860c1d5d90e4188d11ae0d325176869a8', :hard)
    assert_empty @repo.status(repo_file_path)

    new_content = File.open(file_path) { |f| f.read }

    refute_equal original_content, new_content
  end

  def test_reset_path
    File.open(file_path, 'w') do |f|
      f.puts "test content"
    end
    @repo.index.add(repo_file_path)
    @repo.index.write

    @repo.reset_path(repo_file_path, '441034f860c1d5d90e4188d11ae0d325176869a8')
    assert_equal [:index_modified, :worktree_modified], @repo.status(repo_file_path)
  end

  def test_reset_path_no_target
    File.open(file_path, 'w') do |f|
      f.puts "test content"
    end
    @repo.index.add(repo_file_path)
    @repo.index.write

    @repo.reset_path(repo_file_path)
    assert_equal [:index_deleted, :worktree_new], @repo.status(repo_file_path)
  end

  def test_reset_path_invalid_pathspec
    assert_raises TypeError do
      @repo.reset_path([:invalid_reset_path], '441034f860c1d5d90e4188d11ae0d325176869a8')
    end
  end
end
