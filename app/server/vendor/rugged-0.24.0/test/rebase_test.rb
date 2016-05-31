require "test_helper"

class TestRebase < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2("rebase")
    @sig = {
      name: "Rebaser",
      email: "rebaser@rebaser.rb",
      time: Time.at(1405694510),
      time_offset: 0,
    }
  end

  def test_merge_next
    rebase = Rugged::Rebase.new(@repo, "refs/heads/beef", "refs/heads/master")

    op = rebase.next()
    assert_equal :pick, op[:type]
    assert_equal "da9c51a23d02d931a486f45ad18cda05cf5d2b94", op[:id]

    statuses = []
    @repo.status do |file, status|
      statuses << [file, status]
    end
    assert_equal 1, statuses.length
    assert_equal "beef.txt", statuses[0][0]

    rebase.abort()
  end

  def test_merge_finish
    rebase = Rugged::Rebase.new(@repo, "refs/heads/gravy", "refs/heads/veal")
    op = rebase.next()
    rebase.commit(committer: @sig)

    op = rebase.next()
    assert_equal nil, op

    rebase.finish(@sig)
  end

  def test_merge_options
    rebase = Rugged::Rebase.new(@repo, "refs/heads/asparagus", "refs/heads/master",
                                fail_on_conflict: true, skip_reuc: true)
    assert_raises(Rugged::MergeError) { rebase.next() }
    rebase.abort()
  end

  def test_inmemory_resolve_conflicts
    rebase = Rugged::Rebase.new(@repo, "refs/heads/asparagus", "refs/heads/master", inmemory: true)

    op = rebase.next()
    idx = rebase.inmemory_index

    assert idx
    assert_equal :pick, op[:type]
    assert_equal "33f915f9e4dbd9f4b24430e48731a59b45b15500", op[:id]
    assert !@repo.index.conflicts?
    assert idx.conflicts?

    # We can't commit with conflicts pending
    assert_raises(Rugged::RebaseError) { rebase.commit(committer: @sig) }

    # But we can fix those conflicts in the in-memory index
    idx.conflict_remove("asparagus.txt")
    idx.add(path: "asparagus.txt", oid: "414dfc71ead79c07acd4ea47fecf91f289afc4b9", mode: 0100644)
    assert !idx.conflicts?

    commit_id = rebase.commit(committer: @sig)
    assert_equal "db7af47222181e548810da2ab5fec0e9357c5637", commit_id
  end
end
