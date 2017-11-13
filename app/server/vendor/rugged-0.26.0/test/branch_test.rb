# encoding: UTF-8
require "test_helper"

class BranchTest < Rugged::TestCase
  def setup
    @source_repo = FixtureRepo.from_rugged("testrepo.git")
    @repo = FixtureRepo.clone(@source_repo)
  end

  def test_list_all_names
    assert_equal [
      "master",
      "origin/HEAD",
      "origin/master",
      "origin/packed",
    ], @repo.branches.each_name.sort
  end

  def test_lookup_with_ambiguous_names
    @repo.branches.create("origin/master", "41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9")

    assert_equal "41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9", @repo.branches["origin/master"].target_id

    assert_equal "41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9", @repo.branches["heads/origin/master"].target_id
    assert_equal "36060c58702ed4c2a40832c51758d5344201d89a", @repo.branches["remotes/origin/master"].target_id

    assert_equal "41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9", @repo.branches["refs/heads/origin/master"].target_id
    assert_equal "36060c58702ed4c2a40832c51758d5344201d89a", @repo.branches["refs/remotes/origin/master"].target_id
  end

  def test_list_only_local_branches
    assert_equal ["master"], @repo.branches.each_name(:local).sort
  end

  def test_list_only_remote_branches
    assert_equal [
      "origin/HEAD",
      "origin/master",
      "origin/packed",
    ], @repo.branches.each_name(:remote).sort
  end

  def test_get_latest_commit_in_branch
    target = @repo.branches["master"].target

    assert_kind_of Rugged::Commit, target
    assert_equal "36060c58702ed4c2a40832c51758d5344201d89a", target.oid
  end

  def test_lookup_local_branch
    branch = @repo.branches["master"]
    refute_nil branch

    assert_equal "master", branch.name
    assert_equal "refs/heads/master", branch.canonical_name
    assert_equal "36060c58702ed4c2a40832c51758d5344201d89a", branch.target.oid
  end

  def test_lookup_remote_branches
    branch = @repo.branches["origin/packed"]
    refute_nil branch

    assert_equal "origin/packed", branch.name
    assert_equal "refs/remotes/origin/packed", branch.canonical_name
    assert_equal "41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9", branch.target.oid
  end

  def test_lookup_unicode_branch_name
    new_branch = @repo.create_branch("Ångström", "5b5b025afb0b4c913b4c338a42934a3863bf3644")
    refute_nil new_branch

    retrieved_branch = @repo.branches["Ångström"]
    refute_nil retrieved_branch

    assert_equal new_branch, retrieved_branch
  end

  def test_delete_branch
    branch = @repo.create_branch("test_branch")
    @repo.branches.delete(branch)
    assert_nil @repo.branches["test_branch"]
  end

  def test_is_head
    assert @repo.branches["master"].head?
    refute @repo.branches["origin/master"].head?
    refute @repo.branches["origin/packed"].head?
    refute @repo.create_branch("test_branch").head?
  end

  def test_rename_branch
    branch = @repo.create_branch("test_branch")

    @repo.branches.move(branch, 'other_branch')

    assert_nil @repo.branches["test_branch"]
    refute_nil @repo.branches["other_branch"]
  end

  def test_create_new_branch
    new_branch = @repo.create_branch("test_branch", "5b5b025afb0b4c913b4c338a42934a3863bf3644")

    refute_nil new_branch
    assert_equal "test_branch", new_branch.name
    assert_equal "refs/heads/test_branch", new_branch.canonical_name

    refute_nil new_branch.target
    assert_equal "5b5b025afb0b4c913b4c338a42934a3863bf3644", new_branch.target.oid

    refute_nil @repo.branches.find { |p| p.name == "test_branch" }
  end

  def test_create_unicode_branch_nfc
    branch_name = "\xC3\x85\x73\x74\x72\xC3\xB6\x6D"

    new_branch = @repo.create_branch(branch_name, "5b5b025afb0b4c913b4c338a42934a3863bf3644")
    refute_nil new_branch

    assert_equal branch_name, new_branch.name
    assert_equal "refs/heads/#{branch_name}", new_branch.canonical_name

    refute_nil @repo.branches[branch_name]
  end

  def test_create_unicode_branch_nfd
    branch_name = "\x41\xCC\x8A\x73\x74\x72\x6F\xCC\x88\x6D"

    new_branch = @repo.create_branch(branch_name, "5b5b025afb0b4c913b4c338a42934a3863bf3644")
    refute_nil new_branch

    if @repo.config["core.precomposeunicode"] == "true"
      expected_name = "\xC3\x85\x73\x74\x72\xC3\xB6\x6D"
    else
      expected_name = branch_name
    end

    assert_equal expected_name, new_branch.name
    assert_equal "refs/heads/#{expected_name}", new_branch.canonical_name

    refute_nil @repo.branches[branch_name]
    refute_nil @repo.branches[expected_name]
  end

  def test_create_branch_short_sha
    new_branch = @repo.create_branch("test_branch", "5b5b025")

    refute_nil new_branch
    assert_equal "test_branch", new_branch.name
    assert_equal "refs/heads/test_branch", new_branch.canonical_name

    refute_nil new_branch.target
    assert_equal "5b5b025afb0b4c913b4c338a42934a3863bf3644", new_branch.target.oid
  end

  def test_create_branch_from_tag
    new_branch = @repo.create_branch("test_branch", "refs/tags/v0.9")

    refute_nil new_branch
    assert_equal "test_branch", new_branch.name
    assert_equal "refs/heads/test_branch", new_branch.canonical_name

    refute_nil new_branch.target
    assert_equal "5b5b025afb0b4c913b4c338a42934a3863bf3644", new_branch.target.oid
  end

  def test_create_branch_from_head
    new_branch = @repo.create_branch("test_branch")

    refute_nil new_branch
    assert_equal "test_branch", new_branch.name
    assert_equal "refs/heads/test_branch", new_branch.canonical_name

    refute_nil new_branch.target
    assert_equal "36060c58702ed4c2a40832c51758d5344201d89a", new_branch.target.oid
  end

  def test_create_branch_explicit_head
    new_branch = @repo.create_branch("test_branch", "HEAD")

    refute_nil new_branch
    assert_equal "test_branch", new_branch.name
    assert_equal "refs/heads/test_branch", new_branch.canonical_name

    refute_nil new_branch.target
    assert_equal "36060c58702ed4c2a40832c51758d5344201d89a", new_branch.target.oid
  end

  def test_create_branch_from_commit
    new_branch = @repo.create_branch("test_branch",
      Rugged::Commit.lookup(@repo, "5b5b025afb0b4c913b4c338a42934a3863bf3644"))

    refute_nil new_branch
    assert_equal "test_branch", new_branch.name
    assert_equal "refs/heads/test_branch", new_branch.canonical_name

    refute_nil new_branch.target
    assert_equal "5b5b025afb0b4c913b4c338a42934a3863bf3644", new_branch.target.oid
  end

  def test_create_branch_from_tree_fails
    assert_raises ArgumentError, Rugged::InvalidError do
      @repo.create_branch("test_branch",
        Rugged::Tree.lookup(@repo, "f60079018b664e4e79329a7ef9559c8d9e0378d1"))
    end
  end

  def test_create_branch_from_blob_fails
    assert_raises ArgumentError, Rugged::InvalidError do
      @repo.create_branch("test_branch",
        Rugged::Blob.lookup(@repo, "1385f264afb75a56a5bec74243be9b367ba4ca08"))
    end
  end

  def test_create_branch_from_unknown_ref_fails
    assert_raises Rugged::ReferenceError do
      @repo.create_branch("test_branch", "i_do_not_exist")
    end
  end

  def test_create_branch_from_unknown_commit_fails
    assert_raises Rugged::ReferenceError do
      @repo.create_branch("test_branch", "dd15de908706711b51b7acb24faab726d2b3cb16")
    end
  end

  def test_create_branch_from_non_canonical_fails
    assert_raises Rugged::ReferenceError do
      @repo.create_branch("test_branch", "packed")
    end
  end

  def test_branch_remote_remote_branch
    assert_equal 'origin',
      @repo.branches["origin/master"].remote.name
  end

  def test_branch_remote_local_tracking_remote_branch
    assert_equal 'origin',
      @repo.branches["master"].remote.name
  end

  def test_branch_remote_local_non_tracking_branch
    branch = @repo.create_branch('test_branch',
                                 '5b5b025afb0b4c913b4c338a42934a3863bf3644')
    assert_nil branch.remote
  end

  def test_branch_upstream
    upstream_branch = @repo.branches["master"].upstream
    assert_equal 'origin/master', upstream_branch.name
  end

  def test_branch_upstream_remote_branch
    assert_nil @repo.branches["origin/master"].upstream
  end

  def test_branch_upstream_no_tracking_branch
    branch = @repo.create_branch('test_branch',
      '5b5b025afb0b4c913b4c338a42934a3863bf3644')

    assert_nil branch.upstream
  end

  def test_branch_set_upstream_invalid
    branch = @repo.create_branch('test_branch',
      '5b5b025afb0b4c913b4c338a42934a3863bf3644')

    assert_raises TypeError do
      branch.upstream = :invalid_branch
    end
  end

  def test_branch_set_upstream_with_reference
    branch = @repo.create_branch('test_branch',
      '5b5b025afb0b4c913b4c338a42934a3863bf3644')

    branch.upstream = @repo.references["refs/heads/master"]
    assert_equal 'master',  branch.upstream.name
  end

  def test_branch_set_upstream_with_tag_reference
    branch = @repo.create_branch('test_branch',
      '5b5b025afb0b4c913b4c338a42934a3863bf3644')

    assert_raises Rugged::InvalidError do
      branch.upstream = @repo.references["refs/tags/v1.0"]
    end
  end

  def test_branch_set_upstream_local
    branch = @repo.create_branch('test_branch',
      '5b5b025afb0b4c913b4c338a42934a3863bf3644')

    branch.upstream =  @repo.branches["master"]
    assert_equal 'master',  branch.upstream.name
  end

  def test_branch_set_upstream_remote
    branch = @repo.create_branch('test_branch',
      '5b5b025afb0b4c913b4c338a42934a3863bf3644')

    branch.upstream =  @repo.branches["origin/master"]
    assert_equal 'origin/master',  branch.upstream.name
  end

  def test_branch_unset_upstream
    branch = @repo.branches["master"]
    assert branch.upstream
    branch.upstream = nil
    assert_nil branch.upstream
  end

  def test_branch_set_upstream_on_remote_branch
    branch = @repo.branches["origin/master"]

    assert_raises Rugged::InvalidError do
      branch.upstream = @repo.create_branch('test_branch',
                                            '5b5b025afb0b4c913b4c338a42934a3863bf3644')
    end
  end
end
