require "test_helper"
require 'base64'

class ObjectTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_rugged("testrepo.git")
  end

  def test_lookup_can_lookup_any_object_type
    blob = Rugged::Object.lookup(@repo, "fa49b077972391ad58037050f2a75f74e3671e92")
    assert_instance_of Rugged::Blob, blob

    commit = Rugged::Object.lookup(@repo, "8496071c1b46c854b31185ea97743be6a8774479")
    assert_instance_of Rugged::Commit, commit

    tag = Rugged::Object.lookup(@repo, "0c37a5391bbff43c37f0d0371823a5509eed5b1d")
    assert_instance_of Rugged::Tag::Annotation, tag

    tree = Rugged::Object.lookup(@repo, "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")
    assert_instance_of Rugged::Tree, tree

    subclass = Class.new(Rugged::Object)

    blob = subclass.lookup(@repo, "fa49b077972391ad58037050f2a75f74e3671e92")
    assert_instance_of Rugged::Blob, blob

    commit = subclass.lookup(@repo, "8496071c1b46c854b31185ea97743be6a8774479")
    assert_instance_of Rugged::Commit, commit

    tag = subclass.lookup(@repo, "0c37a5391bbff43c37f0d0371823a5509eed5b1d")
    assert_instance_of Rugged::Tag::Annotation, tag

    tree = subclass.lookup(@repo, "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")
    assert_instance_of Rugged::Tree, tree
  end

  def test_fail_to_lookup_inexistant_object
    assert_raises Rugged::OdbError do
      @repo.lookup("a496071c1b46c854b31185ea97743be6a8774479")
    end
  end

  def test_lookup_object
    obj = @repo.lookup("8496071c1b46c854b31185ea97743be6a8774479")
    assert_equal :commit, obj.type
    assert_equal '8496071c1b46c854b31185ea97743be6a8774479', obj.oid
  end

  def test_objects_are_the_same
    obj = @repo.lookup("8496071c1b46c854b31185ea97743be6a8774479")
    obj2 = @repo.lookup("8496071c1b46c854b31185ea97743be6a8774479")
    assert_equal obj, obj2
  end

  def test_read_raw_data
    obj = @repo.lookup("8496071c1b46c854b31185ea97743be6a8774479")
    assert obj.read_raw
  end

  def test_lookup_by_rev
    obj = @repo.rev_parse("v1.0")
    assert "0c37a5391bbff43c37f0d0371823a5509eed5b1d", obj.oid
    obj = @repo.rev_parse("v1.0^1")
    assert "8496071c1b46c854b31185ea97743be6a8774479", obj.oid
  end

  def test_lookup_oid_by_rev
    oid = @repo.rev_parse_oid("v1.0")
    assert "0c37a5391bbff43c37f0d0371823a5509eed5b1d", oid
    @repo.rev_parse_oid("v1.0^1")
    assert "8496071c1b46c854b31185ea97743be6a8774479", oid
  end
end
