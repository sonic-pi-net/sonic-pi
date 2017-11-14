require "test_helper"

class RevertTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2("revert")
  end

  def verify_index(index, expected)
    assert index.is_a?(Rugged::Index)
    assert_equal expected.count, index.count
    expected.each_with_index do |(mode, oid, stage, path), i|
      entry = index[i]
      assert entry
      assert_equal path, entry[:path]
      assert_equal mode, entry[:mode]
      assert_equal oid, entry[:oid]
      assert_equal stage, entry[:stage]
    end
  end

  def test_revert_automerge
    expected = [
      [0100644, "caf99de3a49827117bb66721010eac461b06a80c", 0, "file1.txt"],
      [0100644, "0ab09ea6d4c3634bdf6c221626d8b6f7dd890767", 0, "file2.txt"],
      [0100644, "f4e107c230d08a60fb419d19869f1f282b272d9c", 0, "file3.txt"],
      [0100644, "0f5bfcf58c558d865da6be0281d7795993646cee", 0, "file6.txt"]]

    ours = Rugged::Commit.lookup(@repo, "72333f47d4e83616630ff3b0ffe4c0faebcc3c45")
    revert = Rugged::Commit.lookup(@repo, "d1d403d22cbe24592d725f442835cf46fe60c8ac")

    index = @repo.revert_commit(revert, ours)
    verify_index(index, expected)
  end

  def test_revert_with_conflicts
    expected = [
      [0100644, "7731926a337c4eaba1e2187d90ebfa0a93659382", 1, "file1.txt"],
      [0100644, "4b8fcff56437e60f58e9a6bc630dd242ebf6ea2c", 2, "file1.txt"],
      [0100644, "3a3ef367eaf3fe79effbfb0a56b269c04c2b59fe", 3, "file1.txt"],
      [0100644, "0ab09ea6d4c3634bdf6c221626d8b6f7dd890767", 0, "file2.txt"],
      [0100644, "f4e107c230d08a60fb419d19869f1f282b272d9c", 0, "file3.txt"],
      [0100644, "0f5bfcf58c558d865da6be0281d7795993646cee", 0, "file6.txt"]]

    revert = Rugged::Commit.lookup(@repo, "72333f47d4e83616630ff3b0ffe4c0faebcc3c45")

    index = @repo.revert_commit(revert, "HEAD")
    assert index.conflicts?
    verify_index(index, expected)

    index = @repo.revert_commit(revert, "HEAD", :fail_on_conflict => true)
    refute index
  end

  def test_revert_orphan
    expected = [
      [0100644, "296a6d3be1dff05c5d1f631d2459389fa7b619eb", 0, "file-mainline.txt"]]

    head = Rugged::Commit.lookup(@repo,"39467716290f6df775a91cdb9a4eb39295018145")
    revert = Rugged::Commit.lookup(@repo, "ebb03002cee5d66c7732dd06241119fe72ab96a5")

    index = @repo.revert_commit(revert, head)
    verify_index(index, expected)
  end

  # GH-566
  def test_reverted_index_does_not_cause_segfault_on_diff
    head = Rugged::Commit.lookup(@repo,"39467716290f6df775a91cdb9a4eb39295018145")
    revert = Rugged::Commit.lookup(@repo, "ebb03002cee5d66c7732dd06241119fe72ab96a5")

    index = @repo.revert_commit(revert, head)
    index.diff
  end
end
