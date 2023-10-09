# encoding: utf-8
require "test_helper"

class ReferenceTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2("testrepo")
  end

  def test_reference_validity
    valid = "refs/foobar"
    invalid = "refs/~nope^*"

    assert Rugged::Reference.valid_name?(valid)
    assert !Rugged::Reference.valid_name?(invalid)
  end

  def test_each_can_handle_exceptions
    assert_raises Exception do
      @repo.references.each do
        raise Exception.new("fail")
      end
    end
  end

  def test_list_references
    assert_equal [
      "refs/heads/br2",
      "refs/heads/dir",
      "refs/heads/executable",
      "refs/heads/ident",
      "refs/heads/long-file-name",
      "refs/heads/master",
      "refs/heads/merge-conflict",
      "refs/heads/packed",
      "refs/heads/packed-test",
      "refs/heads/subtrees",
      "refs/heads/test",
      "refs/heads/testrepo-worktree",
      "refs/symref",
      "refs/tags/e90810b",
      "refs/tags/foo/bar",
      "refs/tags/foo/foo/bar",
      "refs/tags/packed-tag",
      "refs/tags/point_to_blob",
      "refs/tags/test"
    ], @repo.refs.map(&:name).sort
  end

  def test_can_filter_refs_with_glob
    assert_equal [
      "refs/tags/e90810b",
      "refs/tags/foo/bar",
      "refs/tags/foo/foo/bar",
      "refs/tags/packed-tag",
      "refs/tags/point_to_blob",
      "refs/tags/test"
    ], @repo.refs('refs/tags/*').map(&:name).sort
  end

  def test_target_id_encoding
    ref = @repo.references["refs/heads/master"]
    assert_equal "099fabac3a9ea935598528c27f866e34089c2eff", ref.target_id
    assert_equal Encoding::US_ASCII, ref.target_id.encoding
  end

  def test_can_open_reference
    ref = @repo.references["refs/heads/master"]
    assert_equal "099fabac3a9ea935598528c27f866e34089c2eff", ref.target_id
    assert_equal :direct, ref.type
    assert_equal "refs/heads/master", ref.name
    assert_equal "refs/heads/master", ref.canonical_name
    assert_nil ref.peel
  end

  def test_can_open_a_symbolic_reference
    ref = @repo.references["HEAD"]
    assert_equal "refs/heads/master", ref.target_id
    assert_equal :symbolic, ref.type

    resolved = ref.resolve
    assert_equal :direct, resolved.type
    assert_equal "099fabac3a9ea935598528c27f866e34089c2eff", resolved.target_id
    assert_equal resolved.target_id, ref.peel
  end

  def test_looking_up_missing_ref_returns_nil
    ref = @repo.references["lol/wut"]
    assert_nil ref
  end

  def test_reference_exists
    exists = @repo.references.exists?("refs/heads/master")
    assert exists

    exists = @repo.references.exists?("lol/wut")
    assert !exists
  end

  def test_load_packed_ref
    ref = @repo.references["refs/heads/packed"]
    assert_equal "41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9", ref.target_id
    assert_equal :direct, ref.type
    assert_equal "refs/heads/packed", ref.name
  end

  def test_resolve_head
    ref = @repo.references["HEAD"]
    assert_equal "refs/heads/master", ref.target_id
    assert_equal :symbolic, ref.type

    head = ref.resolve
    assert_equal "099fabac3a9ea935598528c27f866e34089c2eff", head.target_id
    assert_equal :direct, head.type
  end

  def test_reference_to_tag
    ref = @repo.references["refs/tags/test"]

    assert_equal "b25fa35b38051e4ae45d4222e795f9df2e43f1d1", ref.target_id
    assert_equal "e90810b8df3e80c413d903f631643c716887138d", ref.peel
  end

  def test_collection_delete_with_tag
    tag = @repo.tags["test"]

    @repo.references.delete(tag)
    refute @repo.references.exists?("refs/tags/test")
  end

  def test_collection_delete_with_branch
    branch = @repo.branches["master"]

    @repo.references.delete(branch)
    refute @repo.references.exists?("refs/heads/master")
  end

  def test_reference_is_branch
    repo = FixtureRepo.from_libgit2("testrepo.git")

    assert repo.references["refs/heads/master"].branch?

    refute repo.references["refs/remotes/test/master"].branch?
    refute repo.references["refs/tags/test"].branch?
  end

  def test_reference_is_remote
    repo = FixtureRepo.from_libgit2("testrepo.git")

    assert repo.references["refs/remotes/test/master"].remote?

    refute repo.references["refs/heads/master"].remote?
    refute repo.references["refs/tags/test"].remote?
  end

  def test_reference_is_tag
    repo = FixtureRepo.from_libgit2("testrepo.git")

    assert repo.references["refs/tags/test"].tag?

    refute repo.references["refs/heads/master"].tag?
    refute repo.references["refs/remotes/test/master"].tag?
  end
end

class ReferenceWriteTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_rugged("testrepo.git")
  end

  def test_create_force
    @repo.references.create("refs/heads/unit_test", "refs/heads/master")

    @repo.references.create("refs/heads/unit_test",
      "refs/heads/master", force: true)

    @repo.references.create("refs/heads/unit_test",
      "refs/heads/master", force: :force)
  end

  def test_create_unicode_reference_nfc
    ref_name = "refs/heads/\xC3\x85\x73\x74\x72\xC3\xB6\x6D"

    new_ref = @repo.references.create(ref_name, "5b5b025afb0b4c913b4c338a42934a3863bf3644")
    refute_nil new_ref

    assert_equal ref_name, new_ref.name
    assert_equal ref_name, new_ref.canonical_name

    refute_nil @repo.references[ref_name]
  end

  def test_create_unicode_reference_nfd
    ref_name = "refs/heads/\x41\xCC\x8A\x73\x74\x72\x6F\xCC\x88\x6D"

    new_ref = @repo.references.create(ref_name, "5b5b025afb0b4c913b4c338a42934a3863bf3644")
    refute_nil new_ref

    if @repo.config["core.precomposeunicode"] == "true"
      expected_name = "refs/heads/\xC3\x85\x73\x74\x72\xC3\xB6\x6D"
    else
      expected_name = ref_name
    end

    assert_equal expected_name, new_ref.name
    assert_equal expected_name, new_ref.canonical_name

    refute_nil @repo.references[ref_name]
    refute_nil @repo.references[expected_name]
  end

  def test_rename_unicode_reference_nfd
    ref_name = "refs/heads/\x41\xCC\x8A\x73\x74\x72\x6F\xCC\x88\x6D"

    @repo.references.create("refs/heads/unit_test", "36060c58702ed4c2a40832c51758d5344201d89a")
    new_ref = @repo.references.rename("refs/heads/unit_test", ref_name)
    refute_nil new_ref

    if @repo.config["core.precomposeunicode"] == "true"
      expected_name = "refs/heads/\xC3\x85\x73\x74\x72\xC3\xB6\x6D"
    else
      expected_name = ref_name
    end

    assert_equal expected_name, new_ref.name
    assert_equal expected_name, new_ref.canonical_name

    refute_nil @repo.references[ref_name]
    refute_nil @repo.references[expected_name]
  end

  def test_create_symbolic_ref
    ref = @repo.references.create("refs/heads/unit_test", "refs/heads/master")
    assert_equal "refs/heads/master", ref.target_id
    assert_equal :symbolic, ref.type
    assert_equal "refs/heads/unit_test", ref.name
    @repo.references.delete(ref)
  end

  def test_create_ref_from_oid
    ref = @repo.references.create(
      "refs/heads/unit_test",
      "36060c58702ed4c2a40832c51758d5344201d89a")

    assert_equal "36060c58702ed4c2a40832c51758d5344201d89a", ref.target_id
    assert_equal :direct, ref.type
    assert_equal "refs/heads/unit_test", ref.name
    @repo.references.delete(ref)
  end

  def test_rename_ref
    ref = @repo.references.create("refs/heads/unit_test",
      "36060c58702ed4c2a40832c51758d5344201d89a")

    assert_equal "36060c58702ed4c2a40832c51758d5344201d89a", ref.target_id
    assert_equal :direct, ref.type
    assert_equal "refs/heads/unit_test", ref.name

    new_ref = @repo.references.rename(ref, "refs/heads/rug_new_name")
    assert_equal "refs/heads/rug_new_name", new_ref.name
    @repo.references.delete(new_ref)
  end

  def test_set_ref_target
    ref = @repo.references.create("refs/heads/unit_test",
      "36060c58702ed4c2a40832c51758d5344201d89a")

    assert_equal "36060c58702ed4c2a40832c51758d5344201d89a", ref.target_id
    assert_equal :direct, ref.type
    assert_equal "refs/heads/unit_test", ref.name

    new_ref = @repo.references.update(ref, "5b5b025afb0b4c913b4c338a42934a3863bf3644")
    assert_equal "5b5b025afb0b4c913b4c338a42934a3863bf3644", new_ref.target_id
    @repo.references.delete(new_ref)
  end

  def test_write_and_read_unicode_refs
    ref1 = @repo.references.create("refs/heads/Ångström", "refs/heads/master")
    ref2 = @repo.references.create("refs/heads/foobar", "refs/heads/Ångström")

    assert_equal "refs/heads/Ångström", ref1.name
    assert_equal "refs/heads/Ångström", ref2.target_id
  end
end

class ReflogTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2("testrepo")

    @ident = {
      name: 'Rugged User',
      email: 'rugged@example.com'
    }

    @repo.config['user.name'] = @ident[:name]
    @repo.config['user.email'] = @ident[:email]

    @ref = @repo.references.create("refs/heads/test-reflog",
      "a65fedf39aefe402d3bb6e24df4d4f5fe4547750")
  end

  def test_create_default_log
    ref = @repo.references.create("refs/heads/test-reflog-default",
      "a65fedf39aefe402d3bb6e24df4d4f5fe4547750")
    reflog = ref.log

    assert_equal reflog.size, 1

    assert_equal '0000000000000000000000000000000000000000', reflog[0][:id_old]
    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[0][:id_new]
    assert_nil   reflog[0][:message]
    assert_equal @ident[:name], reflog[0][:committer][:name]
    assert_equal @ident[:email], reflog[0][:committer][:email]
    assert_kind_of Time, reflog[0][:committer][:time]
  end

  def test_create_default_log_custom_ident
    @repo.ident = {
      name: 'Other User',
      email: 'other@example.com'
    }

    ref = @repo.references.create("refs/heads/test-reflog-default",
      "a65fedf39aefe402d3bb6e24df4d4f5fe4547750")
    reflog = ref.log

    assert_equal reflog.size, 1

    assert_equal '0000000000000000000000000000000000000000', reflog[0][:id_old]
    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[0][:id_new]
    assert_nil   reflog[0][:message]
    assert_equal 'Other User', reflog[0][:committer][:name]
    assert_equal 'other@example.com', reflog[0][:committer][:email]
    assert_kind_of Time, reflog[0][:committer][:time]
  end

  def test_create_default_log_custom_log_message
    ref = @repo.references.create(
      "refs/heads/test-reflog-default",
      "a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
      message: "reference created"
      )
    reflog = ref.log

    assert_equal reflog.size, 1

    assert_equal '0000000000000000000000000000000000000000', reflog[0][:id_old]
    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[0][:id_new]
    assert_equal "reference created", reflog[0][:message]
    assert_equal @ident[:name], reflog[0][:committer][:name]
    assert_equal @ident[:email], reflog[0][:committer][:email]
    assert_kind_of Time, reflog[0][:committer][:time]
  end

  def test_create_default_log_custom_ident_and_log_message
    @repo.ident = {
      name: 'Other User',
      email: 'other@example.com'
    }

    ref = @repo.references.create(
      "refs/heads/test-reflog-default",
      "a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
      message: "reference created"
    )
    reflog = ref.log

    assert_equal reflog.size, 1

    assert_equal '0000000000000000000000000000000000000000', reflog[0][:id_old]
    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[0][:id_new]
    assert_equal "reference created", reflog[0][:message]
    assert_equal 'Other User', reflog[0][:committer][:name]
    assert_equal 'other@example.com', reflog[0][:committer][:email]
    assert_kind_of Time, reflog[0][:committer][:time]
  end

  def test_set_target_default_log
    @repo.references.update(@ref, "5b5b025afb0b4c913b4c338a42934a3863bf3644")

    reflog = @ref.log
    assert_equal reflog.size, 2

    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[1][:id_old]
    assert_equal '5b5b025afb0b4c913b4c338a42934a3863bf3644', reflog[1][:id_new]
    assert_nil   reflog[1][:message]
    assert_equal @ident[:name], reflog[1][:committer][:name]
    assert_equal @ident[:email], reflog[1][:committer][:email]
    assert_kind_of Time, reflog[1][:committer][:time]
  end

  def test_set_target_default_log_custom_signature
    @repo.ident = {
      name: "Other User",
      email: "other@exmaple.com"
    }

    @repo.references.update(@ref, "5b5b025afb0b4c913b4c338a42934a3863bf3644")

    reflog = @ref.log
    assert_equal reflog.size, 2

    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[1][:id_old]
    assert_equal '5b5b025afb0b4c913b4c338a42934a3863bf3644', reflog[1][:id_new]
    assert_nil   reflog[1][:message]
    assert_equal "Other User", reflog[1][:committer][:name]
    assert_equal "other@exmaple.com", reflog[1][:committer][:email]
    assert_kind_of Time, reflog[1][:committer][:time]
  end

  def test_set_target_default_log_custom_log_message
    @repo.references.update(
      @ref,
      "5b5b025afb0b4c913b4c338a42934a3863bf3644",
      message: "reference updated"
    )

    reflog = @ref.log
    assert_equal reflog.size, 2

    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[1][:id_old]
    assert_equal '5b5b025afb0b4c913b4c338a42934a3863bf3644', reflog[1][:id_new]
    assert_equal "reference updated", reflog[1][:message]
    assert_equal @ident[:name], reflog[1][:committer][:name]
    assert_equal @ident[:email], reflog[1][:committer][:email]
    assert_kind_of Time, reflog[1][:committer][:time]
  end
end
