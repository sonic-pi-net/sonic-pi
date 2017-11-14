require "test_helper"
require 'base64'

class PackfileTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_rugged("testrepo.git")
  end

  def test_packfile_object_exists
    assert @repo.exists?("41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9")
    assert @repo.exists?("f82a8eb4cb20e88d1030fd10d89286215a715396")
  end

  def test_read_packed_object
    rawobj = @repo.read("41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9")
    assert_match 'tree f82a8eb4cb20e88d1030fd10d89286215a715396', rawobj.data
    assert_equal 230, rawobj.len
    assert_equal :commit, rawobj.type
  end

  def test_read_packed_header
    hash = @repo.read_header("41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9")
    assert_equal 230, hash[:len]
    assert_equal :commit, hash[:type]
  end
end
