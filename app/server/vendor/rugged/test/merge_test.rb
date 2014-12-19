require "test_helper"

class TrivialMergeTest < Rugged::SandboxedTestCase
  def test_2alt
    repo = sandbox_init("merge-resolve")

    ours = repo.rev_parse("trivial-2alt")
    theirs = repo.rev_parse("trivial-2alt-branch")

    analysis = repo.merge_analysis(theirs)
    assert_equal [:normal], analysis

    base = repo.rev_parse(repo.merge_base(ours, theirs))

    index = ours.tree.merge(theirs.tree, base.tree)

    refute index.conflicts?
  end

  def test_4
    repo = sandbox_init("merge-resolve")

    ours = repo.rev_parse("trivial-4")
    theirs = repo.rev_parse("trivial-4-branch")
    base = repo.rev_parse(repo.merge_base(ours, theirs))

    index = ours.tree.merge(theirs.tree, base.tree)

    assert index.conflicts?
    assert 2, index.count { |entry| entry[:stage] > 0 }
    assert index["new-and-different.txt", 2]
    assert index["new-and-different.txt", 3]
  end

  def test_analysis
    repo = sandbox_init("merge-resolve")

    analysis = repo.merge_analysis("HEAD")
    assert_equal [:up_to_date], analysis

    analysis = repo.merge_analysis(repo.rev_parse("HEAD~"))
    assert_equal [:up_to_date], analysis

    analysis = repo.merge_analysis("ff_branch")
    assert_equal [:normal, :fastforward], analysis

    analysis = repo.merge_analysis("branch")
    assert_equal [:normal], analysis

  end

end
