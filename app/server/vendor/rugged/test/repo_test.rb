require 'test_helper'
require 'base64'

class RepositoryTest < Rugged::SandboxedTestCase
  def setup
    super

    @repo = sandbox_init "testrepo.git"
  end

  def teardown
    @repo.close

    super
  end

  def test_last_commit
    assert @repo.respond_to? :last_commit
    assert "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", @repo.last_commit.oid
  end

  def test_fails_to_open_unexisting_repos
    assert_raises IOError, Rugged::OSError do
      Rugged::Repository.new("fakepath/123/")
    end

    assert_raises Rugged::RepositoryError do
      Rugged::Repository.new("test")
    end
  end

  def test_can_check_if_objects_exist
    assert @repo.exists?("8496071c1b46c854b31185ea97743be6a8774479")
    assert @repo.exists?("1385f264afb75a56a5bec74243be9b367ba4ca08")
    assert !@repo.exists?("ce08fe4884650f067bd5703b6a59a8b3b3c99a09")
    assert !@repo.exists?("8496071c1c46c854b31185ea97743be6a8774479")
  end

  def test_can_read_a_raw_object
    rawobj = @repo.read("8496071c1b46c854b31185ea97743be6a8774479")
    assert_match 'tree 181037049a54a1eb5fab404658a3a250b44335d7', rawobj.data
    assert_equal 172, rawobj.len
    assert_equal :commit, rawobj.type
  end

  def test_can_read_object_headers
    hash = @repo.read_header("8496071c1b46c854b31185ea97743be6a8774479")
    assert_equal 172, hash[:len]
    assert_equal :commit, hash[:type]
  end

  def test_check_reads_fail_on_missing_objects
    assert_raises Rugged::OdbError do
      @repo.read("a496071c1b46c854b31185ea97743be6a8774471")
    end
  end

  def test_check_read_headers_fail_on_missing_objects
    assert_raises Rugged::OdbError do
      @repo.read_header("a496071c1b46c854b31185ea97743be6a8774471")
    end
  end

  def test_walking_with_block
    oid = "a4a7dce85cf63874e984719f4fdd239f5145052f"
    list = []
    @repo.walk(oid) { |c| list << c }
    assert list.map {|c| c.oid[0,5] }.join('.'), "a4a7d.c4780.9fd73.4a202.5b5b0.84960"
  end

  def test_walking_without_block
    commits = @repo.walk('a4a7dce85cf63874e984719f4fdd239f5145052f')

    assert commits.kind_of?(Enumerable)
    assert commits.count > 0
  end

  def test_lookup_object
    object = @repo.lookup("8496071c1b46c854b31185ea97743be6a8774479")
    assert object.kind_of?(Rugged::Commit)
  end

  def test_find_reference
    ref = @repo.ref('refs/heads/master')

    assert ref.kind_of?(Rugged::Reference)
    assert_equal 'refs/heads/master', ref.name
  end

  def test_match_all_refs
    refs = @repo.refs 'refs/heads/*'
    assert_equal 12, refs.count
  end

  def test_return_all_ref_names
    refs = @repo.ref_names
    refs.each {|name| assert name.kind_of?(String)}
    assert_equal 21, refs.count
  end

  def test_return_all_tags
    tags = @repo.tags
    assert_equal 7, tags.count
  end

  def test_return_matching_tags
    assert_equal 1, @repo.tags.each('e90810b').count
    assert_equal 4, @repo.tags.each('*tag*').count
  end

  def test_return_all_remotes
    remotes = @repo.remotes
    assert_equal 5, remotes.count
  end

  def test_lookup_head
    head = @repo.head
    assert_equal "refs/heads/master", head.name
    assert_equal "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", head.target_id
    assert_equal :direct, head.type
  end

  def test_set_head_ref
    @repo.head = "refs/heads/packed"
    assert_equal "refs/heads/packed", @repo.head.name
  end

  def test_set_head_invalid
    assert_raises Rugged::ReferenceError do
      @repo.head = "a65fedf39aefe402d3bb6e24df4d4f5fe4547750"
    end
  end

  def test_access_a_file
    sha = 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750'
    blob = @repo.blob_at(sha, 'new.txt')
    assert_equal "my new file\n", blob.content
  end

  def test_access_a_missing_file
    sha = 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750'
    blob = @repo.blob_at(sha, 'file-not-found.txt')
    assert_nil blob
  end

  def test_garbage_collection
    Rugged::Repository.new(@repo.path)
    ObjectSpace.garbage_collect
  end

  def test_enumerate_all_objects
    assert_equal 1687, @repo.each_id.count
  end

  def test_loading_alternates
    alt_path = File.dirname(__FILE__) + '/fixtures/alternate/objects'
    repo = Rugged::Repository.new(@repo.path, :alternates => [alt_path])
    begin
      assert_equal 1690, repo.each_id.count
      assert repo.read('146ae76773c91e3b1d00cf7a338ec55ae58297e2')
    ensure
      repo.close
    end
  end

  def test_alternates_with_invalid_path_type
    assert_raises TypeError do
      Rugged::Repository.new(@repo.path, :alternates => [:invalid_input])
    end
  end

  def test_find_merge_base_between_oids
    commit1 = 'a4a7dce85cf63874e984719f4fdd239f5145052f'
    commit2 = 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750'
    base    = 'c47800c7266a2be04c571c04d5a6614691ea99bd'
    assert_equal base, @repo.merge_base(commit1, commit2)
  end

  def test_find_merge_base_between_commits
    commit1 = @repo.lookup('a4a7dce85cf63874e984719f4fdd239f5145052f')
    commit2 = @repo.lookup('a65fedf39aefe402d3bb6e24df4d4f5fe4547750')
    base    = 'c47800c7266a2be04c571c04d5a6614691ea99bd'
    assert_equal base, @repo.merge_base(commit1, commit2)
  end

  def test_find_merge_base_between_ref_and_oid
    commit1 = 'a4a7dce85cf63874e984719f4fdd239f5145052f'
    commit2 = "refs/heads/master"
    base    = 'c47800c7266a2be04c571c04d5a6614691ea99bd'
    assert_equal base, @repo.merge_base(commit1, commit2)
  end

  def test_find_merge_base_between_many
    commit1 = 'a4a7dce85cf63874e984719f4fdd239f5145052f'
    commit2 = "refs/heads/packed"
    commit3 = @repo.lookup('a65fedf39aefe402d3bb6e24df4d4f5fe4547750')

    base    = 'c47800c7266a2be04c571c04d5a6614691ea99bd'
    assert_equal base, @repo.merge_base(commit1, commit2, commit3)
  end

  def test_ahead_behind_with_oids
    ahead, behind = @repo.ahead_behind(
      'a4a7dce85cf63874e984719f4fdd239f5145052f',
      'a65fedf39aefe402d3bb6e24df4d4f5fe4547750'
    )
    assert_equal 2, ahead
    assert_equal 1, behind
  end

  def test_ahead_behind_with_commits
    ahead, behind = @repo.ahead_behind(
      @repo.lookup('a4a7dce85cf63874e984719f4fdd239f5145052f'),
      @repo.lookup('a65fedf39aefe402d3bb6e24df4d4f5fe4547750')
    )
    assert_equal 2, ahead
    assert_equal 1, behind
  end
end

class MergeCommitsRepositoryTest < Rugged::SandboxedTestCase
  def setup
    super

    @repo = sandbox_init("merge-resolve")
  end

  def teardown
    @repo.close
  end

  def test_merge_commits
    our_commit = @repo.branches["master"].target_id
    their_commit = @repo.branches["branch"].target_id

    index = @repo.merge_commits(our_commit, their_commit)

    assert_equal 8, index.count

    assert_equal "233c0919c998ed110a4b6ff36f353aec8b713487", index["added-in-master.txt", 0][:oid]
    assert_equal "f2e1550a0c9e53d5811175864a29536642ae3821", index["automergeable.txt", 0][:oid]
    assert_equal "4eb04c9e79e88f6640d01ff5b25ca2a60764f216", index["changed-in-branch.txt", 0][:oid]
    assert_equal "11deab00b2d3a6f5a3073988ac050c2d7b6655e2", index["changed-in-master.txt", 0][:oid]

    assert_equal "d427e0b2e138501a3d15cc376077a3631e15bd46", index["conflicting.txt", 1][:oid]
    assert_equal "4e886e602529caa9ab11d71f86634bd1b6e0de10", index["conflicting.txt", 2][:oid]
    assert_equal "2bd0a343aeef7a2cf0d158478966a6e587ff3863", index["conflicting.txt", 3][:oid]

    assert_equal "c8f06f2e3bb2964174677e91f0abead0e43c9e5d", index["unchanged.txt", 0][:oid]

    assert index.conflicts?
  end
end

class ShallowRepositoryTest < Rugged::SandboxedTestCase
  def setup
    super

    @repo = sandbox_init("testrepo.git")
    @shallow_repo = sandbox_init("shallow.git")
  end

  def teardown
    @repo.close
    @shallow_repo.close

    super
  end

  def test_is_shallow
    refute @repo.shallow?
    assert @shallow_repo.shallow?
  end
end

class RepositoryWriteTest < Rugged::TestCase
  include Rugged::TempRepositoryAccess

  TEST_CONTENT = "my test data\n"
  TEST_CONTENT_TYPE = 'blob'

  def test_can_hash_data
    oid = Rugged::Repository.hash_data(TEST_CONTENT, TEST_CONTENT_TYPE)
    assert_equal "76b1b55ab653581d6f2c7230d34098e837197674", oid
  end

  def test_write_to_odb
    oid = @repo.write(TEST_CONTENT, TEST_CONTENT_TYPE)
    assert_equal "76b1b55ab653581d6f2c7230d34098e837197674", oid
    assert @repo.exists?("76b1b55ab653581d6f2c7230d34098e837197674")
  end

  def test_no_merge_base_between_unrelated_branches
    info = @repo.rev_parse('HEAD').to_hash
    baseless = Rugged::Commit.create(@repo, info.merge(:parents => []))
    assert_nil @repo.merge_base('HEAD', baseless)
  end

  def test_default_signature
    name = 'Rugged User'
    email = 'rugged@example.com'
    @repo.config['user.name'] = name
    @repo.config['user.email'] = email
    assert_equal name, @repo.default_signature[:name]
    assert_equal email, @repo.default_signature[:email]
  end
end

class RepositoryDiscoverTest < Rugged::TestCase
  def setup
    @tmpdir = Dir.mktmpdir
    Dir.mkdir(File.join(@tmpdir, 'foo'))
  end

  def teardown
    FileUtils.remove_entry_secure(@tmpdir)
  end

  def test_discover_false
    assert_raises Rugged::RepositoryError do
      Rugged::Repository.discover(@tmpdir)
    end
  end

  def test_discover_nested_false
    assert_raises Rugged::RepositoryError do
      Rugged::Repository.discover(File.join(@tmpdir, 'foo'))
    end
  end

  def test_discover_true
    repo = Rugged::Repository.init_at(@tmpdir, true)
    root = Rugged::Repository.discover(@tmpdir)
    begin
      assert root.bare?
      assert_equal repo.path, root.path
    ensure
      repo.close
      root.close
    end
  end

  def test_discover_nested_true
    repo = Rugged::Repository.init_at(@tmpdir, true)
    root = Rugged::Repository.discover(File.join(@tmpdir, 'foo'))
    begin
      assert root.bare?
      assert_equal repo.path, root.path
    ensure
      repo.close
      root.close
    end
  end
end

class RepositoryInitTest < Rugged::TestCase
  def setup
    @tmppath = Dir.mktmpdir
  end

  def teardown
    FileUtils.remove_entry_secure(@tmppath)
  end

  def test_init_bare_false
    repo = Rugged::Repository.init_at(@tmppath, false)
    begin
      refute repo.bare?
    ensure
      repo.close
    end
  end

  def test_init_bare_true
    repo = Rugged::Repository.init_at(@tmppath, true)
    begin
      assert repo.bare?
    ensure
      repo.close
    end
  end

  def test_init_bare_truthy
    repo = Rugged::Repository.init_at(@tmppath, :bare)
    begin
      assert repo.bare?
    ensure
      repo.close
    end
  end

  def test_init_non_bare_default
    repo = Rugged::Repository.init_at(@tmppath)
    begin
      refute repo.bare?
    ensure
      repo.close
    end
  end
end

class RepositoryCloneTest < Rugged::TestCase
  def setup
    @tmppath = Dir.mktmpdir
    @source_path = File.join(Rugged::TestCase::TEST_DIR, 'fixtures', 'testrepo.git')
  end

  def teardown
    FileUtils.remove_entry_secure(@tmppath)
  end

  def test_clone
    repo = Rugged::Repository.clone_at(@source_path, @tmppath)
    begin
      assert_equal "hey", File.read(File.join(@tmppath, "README")).chomp
      assert_equal "36060c58702ed4c2a40832c51758d5344201d89a", repo.head.target_id
      assert_equal "36060c58702ed4c2a40832c51758d5344201d89a", repo.ref("refs/heads/master").target_id
      assert_equal "36060c58702ed4c2a40832c51758d5344201d89a", repo.ref("refs/remotes/origin/master").target_id
      assert_equal "41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9", repo.ref("refs/remotes/origin/packed").target_id
    ensure
      repo.close
    end
  end

  def test_clone_bare
    repo = Rugged::Repository.clone_at(@source_path, @tmppath, :bare => true)
    begin
      assert repo.bare?
    ensure
      repo.close
    end
  end

  def test_clone_with_transfer_progress_callback
    total_objects = indexed_objects = received_objects = local_objects = total_deltas = indexed_deltas = received_bytes = nil
    callsback = 0
    repo = Rugged::Repository.clone_at(@source_path, @tmppath, {
      transfer_progress: lambda { |*args|
        total_objects, indexed_objects, received_objects, local_objects, total_deltas, indexed_deltas, received_bytes = args
        callsback += 1
      }
    })
    repo.close
    assert_equal 22,   callsback
    assert_equal 19,   total_objects
    assert_equal 19,   indexed_objects
    assert_equal 19,   received_objects
    assert_equal 0,    local_objects
    assert_equal 2,    total_deltas
    assert_equal 2,    indexed_deltas
    assert_equal 1563, received_bytes
  end


  def test_clone_with_update_tips_callback
    calls = 0
    updated_tips = {}

    repo = Rugged::Repository.clone_at(@source_path, @tmppath, {
      update_tips: lambda { |refname, a, b|
        calls += 1
        updated_tips[refname] = [a, b]
      }
    })
    repo.close

    assert_equal 4, calls
    assert_equal({
      "refs/remotes/origin/master" => [nil, "36060c58702ed4c2a40832c51758d5344201d89a"],
      "refs/remotes/origin/packed" => [nil, "41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9"],
      "refs/tags/v0.9"             => [nil, "5b5b025afb0b4c913b4c338a42934a3863bf3644"],
      "refs/tags/v1.0"             => [nil, "0c37a5391bbff43c37f0d0371823a5509eed5b1d"],
    }, updated_tips)
  end

  def test_clone_with_branch
    repo = Rugged::Repository.clone_at(@source_path, @tmppath, {checkout_branch: "packed"})
    begin
      assert_equal "what file?\n", File.read(File.join(@tmppath, "second.txt"))
      assert_equal repo.head.target_id, repo.ref("refs/heads/packed").target_id
      assert_equal "refs/heads/packed", repo.references["HEAD"].target_id
    ensure
      repo.close
    end
  end

  def test_clone_quits_on_error
    begin
      Rugged::Repository.clone_at(@source_path, @tmppath, {
        transfer_progress: lambda { |*_| raise 'boom' }
      })
    rescue => e
      assert_equal 'boom', e.message
    end
    assert_no_dotgit_dir(@tmppath)
  end

  def test_clone_with_bad_progress_callback
    assert_raises ArgumentError do
      Rugged::Repository.clone_at(@source_path, @tmppath, {
        transfer_progress: Object.new
      })
    end
    assert_no_dotgit_dir(@tmppath)
  end

  def assert_no_dotgit_dir(path)
    assert_equal [], Dir[File.join(path, ".git/**")], "new repository's .git dir should not exist"
  end
end

class RepositoryNamespaceTest < Rugged::SandboxedTestCase
  def setup
    super

    @repo = sandbox_init("testrepo.git")
  end

  def teardown
    @repo.close

    super
  end

  def test_no_namespace
    assert_nil @repo.namespace
  end

  def test_changing_namespace
    @repo.namespace = "foo"
    assert_equal "foo", @repo.namespace

    @repo.namespace = "bar"
    assert_equal "bar", @repo.namespace

    @repo.namespace = "foo/bar"
    assert_equal "foo/bar", @repo.namespace

    @repo.namespace = nil
    assert_equal nil, @repo.namespace
  end

  def test_refs_in_namespaces
    @repo.namespace = "foo"
    assert_equal [], @repo.refs.to_a
  end
end

class RepositoryPushTest < Rugged::SandboxedTestCase
  def setup
    super
    @remote_repo = sandbox_init("testrepo.git")
    # We can only push to bare repos
    @remote_repo.config['core.bare'] = 'true'

    @repo = sandbox_clone("testrepo.git", "testrepo")
    @repo.references.create("refs/heads/unit_test",
      "8496071c1b46c854b31185ea97743be6a8774479")
  end

  def teardown
    @repo.close
    @remote_repo.close

    super
  end

  def test_push_single_ref
    result = @repo.push("origin", ["refs/heads/master", "refs/heads/master:refs/heads/foobar", "refs/heads/unit_test"])
    assert_equal({}, result)

    assert_equal "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", @remote_repo.ref("refs/heads/foobar").target_id
    assert_equal "8496071c1b46c854b31185ea97743be6a8774479", @remote_repo.ref("refs/heads/unit_test").target_id
  end

  def test_push_to_remote_instance
    origin = Rugged::Remote.lookup(@repo, "origin")
    result = @repo.push(origin, ["refs/heads/master"])
    assert_equal({}, result)
  end

  def test_push_to_non_bare_raise_error
    @remote_repo.config['core.bare'] = 'false'

    exception = assert_raises Rugged::InvalidError do
      @repo.push("origin", ["refs/heads/master"])
    end

    assert_equal "Local push doesn't (yet) support pushing to non-bare repos.", exception.message
  end

  def test_push_non_forward_raise_error
    exception = assert_raises Rugged::ReferenceError do
      @repo.push("origin", ["refs/heads/unit_test:refs/heads/master"])
    end

    assert_equal "Cannot push non-fastforwardable reference", exception.message
    assert_equal "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", @remote_repo.ref("refs/heads/master").target_id
  end

  def test_push_non_forward_forced_raise_no_error
    result = @repo.push("origin", ["+refs/heads/unit_test:refs/heads/master"])
    assert_equal({}, result)

    assert_equal "8496071c1b46c854b31185ea97743be6a8774479", @remote_repo.ref("refs/heads/master").target_id
  end
end

class RepositoryCheckoutTest < Rugged::SandboxedTestCase
  def setup
    super

    @repo = sandbox_init("testrepo")
    @clone = sandbox_clone("testrepo", "cloned_testrepo")

    _bare = sandbox_init("testrepo.git")
    @bare = Rugged::Repository.bare(_bare.path)
    _bare.close
  end

  def teardown
    @bare.close
    @clone.close
    @repo.close

    super
  end

  def test_checkout_tree_with_commit
    @repo.checkout_tree(@repo.rev_parse("refs/heads/dir"), :strategy => :force)
    @repo.head = "refs/heads/dir"

    assert File.exist?(File.join(@repo.workdir, "README"))
    assert File.exist?(File.join(@repo.workdir, "branch_file.txt"))
    assert File.exist?(File.join(@repo.workdir, "new.txt"))
    assert File.exist?(File.join(@repo.workdir, "a/b.txt"))

    refute File.exist?(File.join(@repo.workdir, "ab"))

    @repo.checkout_tree(@repo.rev_parse("refs/heads/subtrees"), :strategy => :safe)
    @repo.head = "refs/heads/subtrees"

    assert File.exist?(File.join(@repo.workdir, "README"))
    assert File.exist?(File.join(@repo.workdir, "branch_file.txt"))
    assert File.exist?(File.join(@repo.workdir, "new.txt"))
    assert File.exist?(File.join(@repo.workdir, "ab/4.txt"))
    assert File.exist?(File.join(@repo.workdir, "ab/c/3.txt"))
    assert File.exist?(File.join(@repo.workdir, "ab/de/2.txt"))
    assert File.exist?(File.join(@repo.workdir, "ab/de/fgh/1.txt"))

    refute File.exist?(File.join(@repo.workdir, "a"))
  end

  def test_checkout_with_revspec_string
    @repo.checkout_tree("refs/heads/dir", :strategy => :force)
    @repo.head = "refs/heads/dir"

    assert File.exist?(File.join(@repo.workdir, "README"))
    assert File.exist?(File.join(@repo.workdir, "branch_file.txt"))
    assert File.exist?(File.join(@repo.workdir, "new.txt"))
    assert File.exist?(File.join(@repo.workdir, "a/b.txt"))

    refute File.exist?(File.join(@repo.workdir, "ab"))
  end

  def test_checkout_tree_raises_errors_in_notify_cb
    exception = assert_raises RuntimeError do
      @repo.checkout_tree(@repo.rev_parse("refs/heads/dir"), :strategy => :force,
        :notify_flags => :all,
        :notify => lambda { |*args| raise "fail" })
    end

    assert_equal exception.message, "fail"
  end

  def test_checkout_tree_raises_errors_in_progress_cb
    exception = assert_raises RuntimeError do
      @repo.checkout_tree(@repo.rev_parse("refs/heads/dir"), :strategy => :force,
        :progress => lambda { |*args| raise "fail" })
    end

    assert_equal exception.message, "fail"
  end

  def test_checkout_tree_subdirectory
    refute File.exist?(File.join(@repo.workdir, "ab"))

    @repo.checkout_tree(@repo.rev_parse("refs/heads/subtrees"), :strategy => :safe, :paths => "ab/de/")

    assert File.exist?(File.join(@repo.workdir, "ab"))
    assert File.exist?(File.join(@repo.workdir, "ab/de/2.txt"))
    assert File.exist?(File.join(@repo.workdir, "ab/de/fgh/1.txt"))
  end

  def test_checkout_tree_subtree_directory
    refute File.exist?(File.join(@repo.workdir, "de"))

    @repo.checkout_tree(@repo.rev_parse("refs/heads/subtrees:ab"), :strategy => :safe, :paths => "de/")

    assert File.exist?(File.join(@repo.workdir, "de"))
    assert File.exist?(File.join(@repo.workdir, "de/2.txt"))
    assert File.exist?(File.join(@repo.workdir, "de/fgh/1.txt"))
  end

  def test_checkout_tree_raises_with_bare_repo
    assert_raises Rugged::RepositoryError do
      @bare.checkout_tree("HEAD", :strategy => :safe_create)
    end
  end

  def test_checkout_tree_works_with_bare_repo_and_target_directory
    Dir.mktmpdir("alternative") do |dir|
      @bare.checkout_tree("HEAD", :strategy => :safe_create, :target_directory => dir)

      assert File.exist?(File.join(dir, "README"))
      assert File.exist?(File.join(dir, "new.txt"))
    end
  end

  def test_checkout_with_branch_updates_HEAD
    @repo.checkout("dir", :strategy => :force)
    assert_equal "refs/heads/dir", @repo.head.name
  end

  def test_checkout_with_HEAD
    @repo.checkout("dir", :strategy => :force)
    File.unlink(File.join(@repo.workdir, "README"))

    @repo.checkout("HEAD", :strategy => :force)

    assert File.exist?(File.join(@repo.workdir, "README"))
    assert_equal "refs/heads/dir", @repo.head.name
  end

  def test_checkout_with_commit_detaches_HEAD
    @repo.checkout(@repo.rev_parse_oid("refs/heads/dir"), :strategy => :force)

    assert @repo.head_detached?
    assert_equal @repo.rev_parse_oid("refs/heads/dir"), @repo.head.target_id
  end

  def test_checkout_with_remote_branch_detaches_HEAD
    @clone.checkout("origin/dir", :strategy => :force)

    assert @clone.head_detached?
    assert_equal @clone.rev_parse_oid("refs/remotes/origin/dir"), @clone.head.target_id
  end
end
