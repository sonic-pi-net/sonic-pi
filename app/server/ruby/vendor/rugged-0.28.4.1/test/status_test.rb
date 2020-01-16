# encoding: UTF-8
require "test_helper"

class LibgitRepositoryStatusTest < Rugged::TestCase
  STATUSES = {
    "staged_changes" => [:index_modified],
    "staged_changes_file_deleted" => [:index_modified, :worktree_deleted],
    "staged_changes_modified_file" => [:index_modified, :worktree_modified],
    "staged_delete_file_deleted" => [:index_deleted],
    "staged_delete_modified_file" => [:index_deleted, :worktree_new],
    "staged_new_file" => [:index_new],
    "staged_new_file_deleted_file" => [:index_new, :worktree_deleted],
    "staged_new_file_modified_file" => [:index_new, :worktree_modified],
    "file_deleted" => [:worktree_deleted],
    "modified_file" => [:worktree_modified],
    "new_file" => [:worktree_new],
    "ignored_file" => [:ignored],
    "subdir/deleted_file" => [:worktree_deleted],
    "subdir/modified_file" => [:worktree_modified],
    "subdir/new_file" => [:worktree_new],
    "\xe8\xbf\x99" => [:worktree_new]
  }

  STATUSES.each do |file,expected_statuses|
    name = "test_" + file.gsub("/", "__")
    define_method name do
      actual_status = @repo.status file
      assert_equal expected_statuses, actual_status
    end
  end

  def setup
    @repo = FixtureRepo.from_libgit2 "status"
  end

  class TestException < RuntimeError
  end

  def test_status_block_raises
    assert_raises(TestException) do
      @repo.status do |file, status|
        raise TestException, "wow"
      end
    end
  end

  def test_status_block_breaks
    yielded = 0
    @repo.status do |file, status|
      yielded += 1
      break
    end
    assert_equal 1, yielded
  end

  def test_status_with_callback
    actual_statuses = {}
    @repo.status do |file, status|
      assert_nil actual_statuses[file]
      actual_statuses[file] = status
      assert_equal STATUSES[file], actual_statuses[file]
    end
    assert_equal STATUSES, actual_statuses
  end

  def test_status_with_invalid_file_path
    invalid_file = "something_that_doesnt_exist"
    assert_raises Rugged::InvalidError do
      @repo.status(invalid_file)
    end
  end
end
