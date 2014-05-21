require "test_helper"

class TreeTest < Rugged::TestCase
  include Rugged::RepositoryAccess

  def test_lookup_raises_error_if_object_type_does_not_match
    assert_raises Rugged::InvalidError do
      # blob
      Rugged::Tree.lookup(@repo, "fa49b077972391ad58037050f2a75f74e3671e92")
    end

    assert_raises Rugged::InvalidError do
      # commit
      Rugged::Tree.lookup(@repo, "8496071c1b46c854b31185ea97743be6a8774479")
    end

    assert_raises Rugged::InvalidError do
      # tag
      Rugged::Tree.lookup(@repo, "0c37a5391bbff43c37f0d0371823a5509eed5b1d")
    end

    subclass = Class.new(Rugged::Tree)

    assert_raises Rugged::InvalidError do
      # blob
      subclass.lookup(@repo, "fa49b077972391ad58037050f2a75f74e3671e92")
    end

    assert_raises Rugged::InvalidError do
      # commit
      subclass.lookup(@repo, "8496071c1b46c854b31185ea97743be6a8774479")
    end

    assert_raises Rugged::InvalidError do
      # tag
      subclass.lookup(@repo, "0c37a5391bbff43c37f0d0371823a5509eed5b1d")
    end
  end

  def setup
    super
    @oid = "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b"
    @tree = @repo.lookup(@oid)
  end

  def test_read_tree_data
    assert_equal @oid, @tree.oid
    assert_equal :tree, @tree.type
    assert_equal 3, @tree.count
    assert_equal "1385f264afb75a56a5bec74243be9b367ba4ca08", @tree[0][:oid]
    assert_equal "fa49b077972391ad58037050f2a75f74e3671e92", @tree[1][:oid]
  end

  def test_read_tree_entry_data
    bent = @tree[0]
    tent = @tree[2]

    assert_equal "README", bent[:name]
    assert_equal :blob, bent[:type]
    # assert_equal 33188, bent.attributes

    assert_equal "subdir", tent[:name]
    assert_equal :tree, tent[:type]
    assert_equal "619f9935957e010c419cb9d15621916ddfcc0b96", tent[:oid]
    assert_equal :tree, @repo.lookup(tent[:oid]).type
  end

  def test_get_entry_by_oid
    bent = @tree.get_entry_by_oid("1385f264afb75a56a5bec74243be9b367ba4ca08")
    assert_equal "README", bent[:name]
    assert_equal :blob, bent[:type]
  end

  def test_get_entry_by_oid_returns_nil_if_no_oid
    nada = @tree.get_entry_by_oid("1385f264afb75a56a5bec74243be9b367ba4ca07")
    assert_equal nil, nada
  end

  def test_get_entry_by_oid_throws_error_if_wrong_type
    assert_raises TypeError do
      @tree.get_entry_by_oid(:not_a_string)
    end
  end

  def test_tree_iteration
    enum_test = @tree.sort { |a, b| a[:oid] <=> b[:oid] }.map { |e| e[:name] }.join(':')
    assert_equal "README:subdir:new.txt", enum_test

    enum = @tree.each
    assert enum.kind_of? Enumerable
  end

  def test_tree_walk_only_trees
    @tree.walk_trees {|root, entry| assert_equal :tree, entry[:type]}
  end

  def test_tree_walk_only_blobs
    @tree.walk_blobs {|root, entry| assert_equal :blob, entry[:type]}
  end

  def test_iterate_subtrees
    @tree.each_tree {|tree| assert_equal :tree, tree[:type]}
  end

  def test_iterate_subtree_blobs
    @tree.each_blob {|tree| assert_equal :blob, tree[:type]}
  end
end

class TreeWriteTest < Rugged::TestCase
  include Rugged::TempRepositoryAccess

  def test_write_tree_data
    entry = {:type => :blob,
             :name => "README.txt",
             :oid  => "1385f264afb75a56a5bec74243be9b367ba4ca08",
             :filemode => 33188}

    builder = Rugged::Tree::Builder.new
    builder << entry
    sha = builder.write(@repo)
    obj = @repo.lookup(sha)
    assert_equal 38, obj.read_raw.len
  end
end
