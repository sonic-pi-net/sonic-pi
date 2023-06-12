require "test_helper"

class TreeTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_rugged("testrepo.git")
    @oid = "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b"
    @tree = @repo.lookup(@oid)
  end

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

  def test_read_tree_data
    assert_equal @oid, @tree.oid
    assert_equal :tree, @tree.type
    assert_equal 3, @tree.count
    assert_equal 6, @tree.count_recursive
    assert_equal 5, @tree.count_recursive(5)
    assert_equal 6, @tree.count_recursive(10)
    assert_raises(TypeError) do
      @tree.count_recursive("NaN")
    end
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
    assert_nil nada
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

  def test_tree_walk_only_trees_without_block
    assert_equal [:tree], @tree.walk_trees.map { |_root, entry| entry[:type] }.uniq
  end

  def test_tree_walk_only_blobs
    @tree.walk_blobs {|root, entry| assert_equal :blob, entry[:type]}
  end

  def test_tree_walk_only_blobs_without_block
    assert_equal [:blob], @tree.walk_blobs.map { |_root, entry| entry[:type] }.uniq
  end

  def test_iterate_subtrees
    @tree.each_tree {|tree| assert_equal :tree, tree[:type]}
  end

  def test_iterate_subtrees_without_block
    assert_equal [:tree], @tree.each_tree.map { |tree| tree[:type] }.uniq
  end

  def test_iterate_subtree_blobs
    @tree.each_blob {|tree| assert_equal :blob, tree[:type]}
  end

  def test_iterate_subtree_blobs_without_block
    assert_equal [:blob], @tree.each_blob.map { |tree| tree[:type] }.uniq
  end
end

class TreeWriteTest < Rugged::TestCase
  def setup
    @source_repo = FixtureRepo.from_rugged("testrepo.git")
    @repo = FixtureRepo.clone(@source_repo)
  end

  def test_write_tree_data
    entry = {:type => :blob,
             :name => "README.txt",
             :oid  => "1385f264afb75a56a5bec74243be9b367ba4ca08",
             :filemode => 33188}

    builder = Rugged::Tree::Builder.new(@repo)
    builder << entry
    sha = builder.write
    obj = @repo.lookup(sha)
    assert_equal 38, obj.read_raw.len
  end
end

class TreeUpdateTest < Rugged::TestCase
  def setup
    @source_repo = FixtureRepo.from_rugged("testrepo.git")
    @repo = FixtureRepo.clone(@source_repo)
  end

  def test_treebuilder_remove
    builder = Rugged::Tree::Builder.new(@repo, @repo.head.target.tree)
    assert_equal builder.remove("new.txt"), true
    assert_equal builder.remove("nonexistent file"), false
  end

  def test_treebuilder_add
    builder = Rugged::Tree::Builder.new(@repo, @repo.head.target.tree)
    builder << { :type => :blob, :name => "another-readme", :oid => "1385f264afb75a56a5bec74243be9b367ba4ca08", :filemode => 0100644 }
    newtree = builder.write
    assert_equal "71a3bbe701e60c1756edd23cfc0b207711dca1f2", newtree
  end

  def test_tree_updater_add
    updates = [{:action => :upsert, :path => "another-readme", :oid => "1385f264afb75a56a5bec74243be9b367ba4ca08", :filemode => 0100644}]
    newtree = @repo.head.target.tree.update(updates)
    assert_equal "71a3bbe701e60c1756edd23cfc0b207711dca1f2", newtree
  end

  def test_tree_updater_add_deeper
    baseline = @repo.head.target.tree
    file_oid = "1385f264afb75a56a5bec74243be9b367ba4ca08"
    file_mode = 0100644
    file_path = "some/file"

    idx = Rugged::Index.new
    idx.read_tree(baseline)
    idx.add({:oid => file_oid, :path => file_path, :stage => 0, :mode => file_mode})
    indexer_tree_id = idx.write_tree(@repo)

    updates = [{:action => :upsert, :path => file_path, :oid => file_oid, :filemode => file_mode}]
    newtree = baseline.update(updates)

    assert_equal indexer_tree_id, newtree
  end

  def test_tree_updater_remove
    baseline = Rugged::Tree.lookup(@repo, 'c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b')

    ["README", "subdir/README"].each do |file_path|
      idx = Rugged::Index.new
      idx.read_tree(baseline)
      idx.remove(file_path)
      indexer_tree_id = idx.write_tree(@repo)

      updates = [{:action => :remove, :path => file_path}]
      newtree = baseline.update(updates)

      assert_equal indexer_tree_id, newtree
    end
  end

  def test_treebuilder_add_nonexistent_fails
    builder = Rugged::Tree::Builder.new(@repo, @repo.head.target.tree)
    assert_raises Rugged::TreeError do
      builder << { :type => :blob, :name => "another-readme", :oid => "0000000000000000000000000000000000000001", :filemode => 0100644 }
    end
  end

  def test_treebuilder_add_submodules_always_succeeds
    builder = Rugged::Tree::Builder.new(@repo, @repo.head.target.tree)
    builder << { :type => :commit, :name => "submodule", :oid => "0000000000000000000000000000000000000001", :filemode => 0160000 }
    newtree = builder.write
    assert_equal "731d4e5d79f60ed64687b7828e1d0528839fed6e", newtree
  end

  def test_treebuilder_add_nonexistent_can_pass
    begin
      Rugged::Settings['strict_object_creation'] = false
      builder = Rugged::Tree::Builder.new(@repo, @repo.head.target.tree)
      builder << { :type => :blob, :name => "another-readme", :oid => "0000000000000000000000000000000000000001", :filemode => 0100644 }
      newtree = builder.write
      assert_equal "2e34ce696ea8d595b62b99ac20406f86c15d464a", newtree
    ensure
      Rugged::Settings['strict_object_creation'] = true
    end

    def test_emtpy_tree_lookup
      tree = Rugged::Tree.empty(@repo)
      assert_equal 0, tree.count
    end
  end
end
