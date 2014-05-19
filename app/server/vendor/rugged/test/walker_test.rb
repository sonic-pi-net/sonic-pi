require "test_helper"
require 'base64'

class WalkerTest < Rugged::TestCase
  include Rugged::RepositoryAccess

  def setup
    super
    @walker = Rugged::Walker.new(@repo)
  end

  def test_walk_revlist
    @walker.push("9fd738e8f7967c078dceed8190330fc8648ee56a")
    data = @walker.each.to_a
    oids = data.sort { |a, b| a.oid <=> b.oid }.map {|a| a.oid[0,5]}.join('.')
    assert_equal "4a202.5b5b0.84960.9fd73", oids
  end

  def test_walk_partial_revlist
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    @walker.push(oid)
    walk = @walker.each.to_a
    assert_equal oid, walk[0].oid
    assert_equal 1, walk.count
  end

  def test_hide_part_of_list
    @walker.push("9fd738e8f7967c078dceed8190330fc8648ee56a")
    @walker.hide("5b5b025afb0b4c913b4c338a42934a3863bf3644")
    assert_equal 2, @walker.each.count
  end

  # resetting a walker emtpies the walking queue
  def test_resetting_walker
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    @walker.push(oid)
    walk = @walker.each.to_a
    assert_equal oid, walk[0].oid
    assert_equal 1, walk.count
    @walker.reset
    walk = @walker.each.to_a
    assert_equal 0, walk.count
  end

  def test_walk_is_enumberable
    @walker.push("9fd738e8f7967c078dceed8190330fc8648ee56a")
    enum = @walker.sort { |a, b| a.oid <=> b.oid }.map { |a| a.oid[0, 4] }.join('.')
    assert_equal "4a20.5b5b.8496.9fd7", enum
  end

  def do_sort(sorting)
    oid = "a4a7dce85cf63874e984719f4fdd239f5145052f"
    @walker.sorting(sorting)
    @walker.push(oid)
    @walker.each.to_a
  end

  def revlist_with_sorting(sorting)
    data = do_sort sorting
    data.map {|a| a.oid[0,5] if a }.join('.')
  end

  def is_toposorted(list)
    list.all? do |commit|
      commit.parents.all? { |parent| list.index(commit) < list.index(parent) }
    end
  end

  def test_sort_by_date
    time = revlist_with_sorting(Rugged::SORT_DATE)
    assert_equal "a4a7d.c4780.9fd73.4a202.5b5b0.84960", time
  end

  def test_sort_by_topo
    sort_list = do_sort(Rugged::SORT_TOPO)
    assert_equal is_toposorted(sort_list), true
  end

  def test_sort_by_date_reversed
    time = revlist_with_sorting(Rugged::SORT_DATE | Rugged::SORT_REVERSE)
    assert_equal "84960.5b5b0.4a202.9fd73.c4780.a4a7d", time
  end

  def test_sort_by_topo_reverse
    sort_list = do_sort(Rugged::SORT_TOPO | Rugged::SORT_REVERSE).reverse
    assert_equal is_toposorted(sort_list), true
  end
end

# testrepo (the non-bare repo) is the one with non-linear history,
# which we need in order to make sure that we are activating the
# first-parent simplification
class WalkerTest2 < Rugged::SandboxedTestCase
  def test_simplify_first_parent
    repo = sandbox_init("testrepo")
    walker = Rugged::Walker.new(repo)
    walker.push("099fabac3a9ea935598528c27f866e34089c2eff")
    walker.simplify_first_parent
    assert_equal 7, walker.each.to_a.length
  end
end
