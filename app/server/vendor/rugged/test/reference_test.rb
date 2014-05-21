# encoding: UTF-8
require "test_helper"

class ReferenceTest < Rugged::SandboxedTestCase
  UNICODE_REF_NAME = "A\314\212ngstro\314\210m"

  def setup
    super
    @repo = sandbox_init("testrepo")
  end

  def teardown
    @repo.close
    super
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
      "refs/heads/long-file-name",
      "refs/heads/master",
      "refs/heads/packed",
      "refs/heads/packed-test",
      "refs/heads/subtrees",
      "refs/heads/test",
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
    assert_equal nil, ref
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
    repo = sandbox_init("testrepo.git")

    begin
      assert repo.references["refs/heads/master"].branch?

      refute repo.references["refs/remotes/test/master"].branch?
      refute repo.references["refs/tags/test"].branch?
    ensure
      repo.close
    end
  end

  def test_reference_is_remote
    repo = sandbox_init("testrepo.git")
    begin
      assert repo.references["refs/remotes/test/master"].remote?

      refute repo.references["refs/heads/master"].remote?
      refute repo.references["refs/tags/test"].remote?
    ensure
      repo.close
    end
  end

  def test_reference_is_tag
    repo = sandbox_init("testrepo.git")
    begin
      assert repo.references["refs/tags/test"].tag?

      refute repo.references["refs/heads/master"].tag?
      refute repo.references["refs/remotes/test/master"].tag?
    ensure
      repo.close
    end
  end
end

class ReferenceWriteTest < Rugged::TestCase
  include Rugged::TempRepositoryAccess

  def test_create_force
    @repo.references.create("refs/heads/unit_test", "refs/heads/master")

    @repo.references.create("refs/heads/unit_test",
      "refs/heads/master", force: true)

    @repo.references.create("refs/heads/unit_test",
      "refs/heads/master", force: :force)
  end

  def test_list_unicode_refs
    @repo.references.create(
      "refs/heads/#{ReferenceTest::UNICODE_REF_NAME}",
      "refs/heads/master")

    refs = @repo.refs.map { |r| r.name.gsub("refs/", '') }
    assert refs.include? "heads/#{ReferenceTest::UNICODE_REF_NAME}"
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

class ReflogTest < Rugged::SandboxedTestCase
  def setup
    super

    @repo = sandbox_init("testrepo")

    @comitter = {
      name: 'Rugged User',
      email: 'rugged@example.com'
    }

    @repo.config['user.name'] = @comitter[:name]
    @repo.config['user.email'] = @comitter[:email]

    @ref = @repo.references.create("refs/heads/test-reflog",
      "a65fedf39aefe402d3bb6e24df4d4f5fe4547750")
  end

  def teardown
    @repo.close
    super
  end

  def test_create_default_log
    ref = @repo.references.create("refs/heads/test-reflog-default",
      "a65fedf39aefe402d3bb6e24df4d4f5fe4547750")
    reflog = ref.log

    assert_equal reflog.size, 1

    assert_equal '0000000000000000000000000000000000000000', reflog[0][:id_old]
    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[0][:id_new]
    assert_equal nil, reflog[0][:message]
    assert_equal @comitter[:name], reflog[0][:committer][:name]
    assert_equal @comitter[:email], reflog[0][:committer][:email]
    assert_kind_of Time, reflog[0][:committer][:time]
  end

  def test_create_default_log_custom_signature
    ref = @repo.references.create("refs/heads/test-reflog-default",
      "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", {
        signature: {
          name: "Other User",
          email: "other@exmaple.com"
        }
      })
    reflog = ref.log

    assert_equal reflog.size, 1

    assert_equal '0000000000000000000000000000000000000000', reflog[0][:id_old]
    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[0][:id_new]
    assert_equal nil, reflog[0][:message]
    assert_equal "Other User", reflog[0][:committer][:name]
    assert_equal "other@exmaple.com", reflog[0][:committer][:email]
    assert_kind_of Time, reflog[0][:committer][:time]
  end

  def test_create_default_log_custom_log_message
    ref = @repo.references.create(
      "refs/heads/test-reflog-default",
      "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", {
        message: "reference created"
      })
    reflog = ref.log

    assert_equal reflog.size, 1

    assert_equal '0000000000000000000000000000000000000000', reflog[0][:id_old]
    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[0][:id_new]
    assert_equal "reference created", reflog[0][:message]
    assert_equal @comitter[:name], reflog[0][:committer][:name]
    assert_equal @comitter[:email], reflog[0][:committer][:email]
    assert_kind_of Time, reflog[0][:committer][:time]
  end

  def test_create_default_log_custom_signature_and_log_message
    ref = @repo.references.create(
      "refs/heads/test-reflog-default",
      "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", {
        message: "reference created",
        signature: {
          name: "Other User",
          email: "other@exmaple.com"
        }
      })
    reflog = ref.log

    assert_equal reflog.size, 1

    assert_equal '0000000000000000000000000000000000000000', reflog[0][:id_old]
    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[0][:id_new]
    assert_equal "reference created", reflog[0][:message]
    assert_equal "Other User", reflog[0][:committer][:name]
    assert_equal "other@exmaple.com", reflog[0][:committer][:email]
    assert_kind_of Time, reflog[0][:committer][:time]
  end

  def test_set_target_default_log
    @repo.references.update(@ref, "5b5b025afb0b4c913b4c338a42934a3863bf3644")

    reflog = @ref.log
    assert_equal reflog.size, 2

    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[1][:id_old]
    assert_equal '5b5b025afb0b4c913b4c338a42934a3863bf3644', reflog[1][:id_new]
    assert_equal nil, reflog[1][:message]
    assert_equal @comitter[:name], reflog[1][:committer][:name]
    assert_equal @comitter[:email], reflog[1][:committer][:email]
    assert_kind_of Time, reflog[1][:committer][:time]
  end

  def test_set_target_default_log_custom_signature
    @repo.references.update(@ref, "5b5b025afb0b4c913b4c338a42934a3863bf3644", {
      signature: {
        name: "Other User",
        email: "other@exmaple.com"
      }
    })

    reflog = @ref.log
    assert_equal reflog.size, 2

    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[1][:id_old]
    assert_equal '5b5b025afb0b4c913b4c338a42934a3863bf3644', reflog[1][:id_new]
    assert_equal nil, reflog[1][:message]
    assert_equal "Other User", reflog[1][:committer][:name]
    assert_equal "other@exmaple.com", reflog[1][:committer][:email]
    assert_kind_of Time, reflog[1][:committer][:time]
  end

  def test_set_target_default_log_custom_log_message
    @repo.references.update(@ref, "5b5b025afb0b4c913b4c338a42934a3863bf3644", {
      message: "reference updated"
    })

    reflog = @ref.log
    assert_equal reflog.size, 2

    assert_equal 'a65fedf39aefe402d3bb6e24df4d4f5fe4547750', reflog[1][:id_old]
    assert_equal '5b5b025afb0b4c913b4c338a42934a3863bf3644', reflog[1][:id_new]
    assert_equal "reference updated", reflog[1][:message]
    assert_equal @comitter[:name], reflog[1][:committer][:name]
    assert_equal @comitter[:email], reflog[1][:committer][:email]
    assert_kind_of Time, reflog[1][:committer][:time]
  end
end

