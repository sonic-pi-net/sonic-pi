require "test_helper"

class TestCommit < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_rugged("testrepo.git")
    @repo.config['core.abbrev'] = 7
  end

  def test_lookup_raises_error_if_object_type_does_not_match
    assert_raises Rugged::InvalidError do
      # blob
      Rugged::Commit.lookup(@repo, "fa49b077972391ad58037050f2a75f74e3671e92")
    end

    assert_raises Rugged::InvalidError do
      # tag
      Rugged::Commit.lookup(@repo, "0c37a5391bbff43c37f0d0371823a5509eed5b1d")
    end

    assert_raises Rugged::InvalidError do
      # tree
      Rugged::Commit.lookup(@repo, "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")
    end

    subclass = Class.new(Rugged::Commit)

    assert_raises Rugged::InvalidError do
      # blob
      subclass.lookup(@repo, "fa49b077972391ad58037050f2a75f74e3671e92")
    end

    assert_raises Rugged::InvalidError do
      # tag
      subclass.lookup(@repo, "0c37a5391bbff43c37f0d0371823a5509eed5b1d")
    end

    assert_raises Rugged::InvalidError do
      # tree
      subclass.lookup(@repo, "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")
    end
  end

  def test_read_commit_data
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    obj = @repo.lookup(oid)

    assert_equal obj.oid, oid
    assert_equal obj.type, :commit
    assert_equal obj.message, "testing\n"
    assert_equal obj.time.to_i, 1273360386
    assert_equal obj.epoch_time, 1273360386

    c = obj.committer
    assert_equal c[:name], "Scott Chacon"
    assert_equal c[:time].to_i, 1273360386
    assert_equal c[:email], "schacon@gmail.com"

    c = obj.author
    assert_equal c[:name], "Scott Chacon"
    assert_equal c[:time].to_i, 1273360386
    assert_equal c[:email], "schacon@gmail.com"

    assert_equal obj.tree.oid, "181037049a54a1eb5fab404658a3a250b44335d7"
    assert_equal [], obj.parents
  end

  def test_commit_with_multiple_parents
    oid = "a4a7dce85cf63874e984719f4fdd239f5145052f"
    obj = @repo.lookup(oid)
    parents = obj.parents.map {|c| c.oid }
    assert parents.include?("9fd738e8f7967c078dceed8190330fc8648ee56a")
    assert parents.include?("c47800c7266a2be04c571c04d5a6614691ea99bd")
  end

  def test_get_parent_oids
    oid = "a4a7dce85cf63874e984719f4fdd239f5145052f"
    obj = @repo.lookup(oid)
    parents = obj.parent_oids
    assert parents.include?("9fd738e8f7967c078dceed8190330fc8648ee56a")
    assert parents.include?("c47800c7266a2be04c571c04d5a6614691ea99bd")
  end

  def test_get_tree_oid
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    obj = @repo.lookup(oid)

    assert_equal obj.tree_oid, "181037049a54a1eb5fab404658a3a250b44335d7"
  end

  def test_amend_commit
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    obj = @repo.lookup(oid)

    entry = {:type => :blob,
             :name => "README.txt",
             :oid  => "1385f264afb75a56a5bec74243be9b367ba4ca08",
             :filemode => 33188}

    builder = Rugged::Tree::Builder.new(@repo)
    builder << entry
    tree_oid = builder.write

    person = {:name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }

    commit_params = {
      :message => "This is the amended commit message\n\nThis commit is created from Rugged",
      :committer => person,
      :author => person,
      :tree => tree_oid
    }

    new_commit_oid = obj.amend(commit_params)

    amended_commit = @repo.lookup(new_commit_oid)
    assert_equal commit_params[:message], amended_commit.message
    assert_equal tree_oid, amended_commit.tree.oid
  end

  def test_amend_commit_blank_tree
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    obj = @repo.lookup(oid)

    person = {:name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }

    commit_params = {
      :message => "This is the amended commit message\n\nThis commit is created from Rugged",
      :committer => person,
      :author => person
    }

    new_commit_oid = obj.amend(commit_params)

    amended_commit = @repo.lookup(new_commit_oid)
    assert_equal commit_params[:message], amended_commit.message
  end

  def test_amend_commit_blank_author_and_committer
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    obj = @repo.lookup(oid)

    commit_params = {
      :message => "This is the amended commit message\n\nThis commit is created from Rugged"
    }

    new_commit_oid = obj.amend(commit_params)

    amended_commit = @repo.lookup(new_commit_oid)
    assert_equal commit_params[:message], amended_commit.message
  end

  def test_amend_commit_blank_message
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    obj = @repo.lookup(oid)

    entry = {:type => :blob,
             :name => "README.txt",
             :oid  => "1385f264afb75a56a5bec74243be9b367ba4ca08",
             :filemode => 33188}

    builder = Rugged::Tree::Builder.new(@repo)
    builder << entry
    tree_oid = builder.write

    person = {:name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }

    commit_params = {
      :committer => person,
      :author => person,
      :tree => tree_oid
    }

    new_commit_oid = obj.amend(commit_params)

    amended_commit = @repo.lookup(new_commit_oid)
    assert_equal tree_oid, amended_commit.tree.oid
  end

  def test_header_field
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    obj = @repo.lookup(oid)

    expected_header_field = "Scott Chacon <schacon@gmail.com> 1273360386 -0700"
    assert_equal expected_header_field, obj.header_field("author")
    assert_nil   obj.header_field("foobar")
  end

  def test_header_field?
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    obj = @repo.lookup(oid)

    assert_equal true, obj.header_field?("author")
    assert_equal false, obj.header_field?("foobar")
  end

  def test_header
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    obj = @repo.lookup(oid)

    expected_header = <<-HEADER
tree 181037049a54a1eb5fab404658a3a250b44335d7
author Scott Chacon <schacon@gmail.com> 1273360386 -0700
committer Scott Chacon <schacon@gmail.com> 1273360386 -0700
    HEADER

    assert_equal expected_header, obj.header
  end

  def test_extract_signature

    raw_commit = <<-COMMIT
tree 6b79e22d69bf46e289df0345a14ca059dfc9bdf6
parent 34734e478d6cf50c27c9d69026d93974d052c454
author Ben Burkert <ben@benburkert.com> 1358451456 -0800
committer Ben Burkert <ben@benburkert.com> 1358451456 -0800
gpgsig -----BEGIN PGP SIGNATURE-----
 Version: GnuPG v1.4.12 (Darwin)
 
 iQIcBAABAgAGBQJQ+FMIAAoJEH+LfPdZDSs1e3EQAJMjhqjWF+WkGLHju7pTw2al
 o6IoMAhv0Z/LHlWhzBd9e7JeCnanRt12bAU7yvYp9+Z+z+dbwqLwDoFp8LVuigl8
 JGLcnwiUW3rSvhjdCp9irdb4+bhKUnKUzSdsR2CK4/hC0N2i/HOvMYX+BRsvqweq
 AsAkA6dAWh+gAfedrBUkCTGhlNYoetjdakWqlGL1TiKAefEZrtA1TpPkGn92vbLq
 SphFRUY9hVn1ZBWrT3hEpvAIcZag3rTOiRVT1X1flj8B2vGCEr3RrcwOIZikpdaW
 who/X3xh/DGbI2RbuxmmJpxxP/8dsVchRJJzBwG+yhwU/iN3MlV2c5D69tls/Dok
 6VbyU4lm/ae0y3yR83D9dUlkycOnmmlBAHKIZ9qUts9X7mWJf0+yy2QxJVpjaTGG
 cmnQKKPeNIhGJk2ENnnnzjEve7L7YJQF6itbx5VCOcsGh3Ocb3YR7DMdWjt7f8pu
 c6j+q1rP7EpE2afUN/geSlp5i3x8aXZPDj67jImbVCE/Q1X9voCtyzGJH7MXR0N9
 ZpRF8yzveRfMH8bwAJjSOGAFF5XkcR/RNY95o+J+QcgBLdX48h+ZdNmUf6jqlu3J
 7KmTXXQcOVpN6dD3CmRFsbjq+x6RHwa8u1iGn+oIkX908r97ckfB/kHKH7ZdXIJc
 cpxtDQQMGYFpXK/71stq
 =ozeK
 -----END PGP SIGNATURE-----

a simple commit which works
COMMIT

    exp_signature = <<-SIGNATURE.strip
-----BEGIN PGP SIGNATURE-----
Version: GnuPG v1.4.12 (Darwin)

iQIcBAABAgAGBQJQ+FMIAAoJEH+LfPdZDSs1e3EQAJMjhqjWF+WkGLHju7pTw2al
o6IoMAhv0Z/LHlWhzBd9e7JeCnanRt12bAU7yvYp9+Z+z+dbwqLwDoFp8LVuigl8
JGLcnwiUW3rSvhjdCp9irdb4+bhKUnKUzSdsR2CK4/hC0N2i/HOvMYX+BRsvqweq
AsAkA6dAWh+gAfedrBUkCTGhlNYoetjdakWqlGL1TiKAefEZrtA1TpPkGn92vbLq
SphFRUY9hVn1ZBWrT3hEpvAIcZag3rTOiRVT1X1flj8B2vGCEr3RrcwOIZikpdaW
who/X3xh/DGbI2RbuxmmJpxxP/8dsVchRJJzBwG+yhwU/iN3MlV2c5D69tls/Dok
6VbyU4lm/ae0y3yR83D9dUlkycOnmmlBAHKIZ9qUts9X7mWJf0+yy2QxJVpjaTGG
cmnQKKPeNIhGJk2ENnnnzjEve7L7YJQF6itbx5VCOcsGh3Ocb3YR7DMdWjt7f8pu
c6j+q1rP7EpE2afUN/geSlp5i3x8aXZPDj67jImbVCE/Q1X9voCtyzGJH7MXR0N9
ZpRF8yzveRfMH8bwAJjSOGAFF5XkcR/RNY95o+J+QcgBLdX48h+ZdNmUf6jqlu3J
7KmTXXQcOVpN6dD3CmRFsbjq+x6RHwa8u1iGn+oIkX908r97ckfB/kHKH7ZdXIJc
cpxtDQQMGYFpXK/71stq
=ozeK
-----END PGP SIGNATURE-----
SIGNATURE

    exp_signed_data = <<-SIGNEDDATA
tree 6b79e22d69bf46e289df0345a14ca059dfc9bdf6
parent 34734e478d6cf50c27c9d69026d93974d052c454
author Ben Burkert <ben@benburkert.com> 1358451456 -0800
committer Ben Burkert <ben@benburkert.com> 1358451456 -0800

a simple commit which works
SIGNEDDATA

    commit_sha = @repo.write(raw_commit, :commit)

    signature, signed_data = Rugged::Commit.extract_signature(@repo, commit_sha, "gpgsig")
    assert_equal exp_signature, signature
    assert_equal exp_signed_data, signed_data

    signature, signed_data = Rugged::Commit.extract_signature(@repo, commit_sha)
    assert_equal exp_signature, signature
    assert_equal exp_signed_data, signed_data

    assert_nil Rugged::Commit.extract_signature(@repo, "8496071c1b46c854b31185ea97743be6a8774479")

    # Ask for a tree
    assert_raises(Rugged::InvalidError) { Rugged::Commit.extract_signature(@repo, "181037049a54a1eb5fab404658a3a250b44335d7") }

    # Ask for a non-existent object
    assert_raises(Rugged::OdbError) { Rugged::Commit.extract_signature(@repo, "181037049a54a1eb5fab404658a3a250b44335d8") }
  end

  def test_create_with_signature
    signed_commit = <<-COMMIT
tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904
parent 8496071c1b46c854b31185ea97743be6a8774479
author Ben Burkert <ben@benburkert.com> 1358451456 -0800
committer Ben Burkert <ben@benburkert.com> 1358451456 -0800
gpgsig -----BEGIN PGP SIGNATURE-----
 Version: GnuPG v1.4.12 (Darwin)
 
 iQIcBAABAgAGBQJQ+FMIAAoJEH+LfPdZDSs1e3EQAJMjhqjWF+WkGLHju7pTw2al
 o6IoMAhv0Z/LHlWhzBd9e7JeCnanRt12bAU7yvYp9+Z+z+dbwqLwDoFp8LVuigl8
 JGLcnwiUW3rSvhjdCp9irdb4+bhKUnKUzSdsR2CK4/hC0N2i/HOvMYX+BRsvqweq
 AsAkA6dAWh+gAfedrBUkCTGhlNYoetjdakWqlGL1TiKAefEZrtA1TpPkGn92vbLq
 SphFRUY9hVn1ZBWrT3hEpvAIcZag3rTOiRVT1X1flj8B2vGCEr3RrcwOIZikpdaW
 who/X3xh/DGbI2RbuxmmJpxxP/8dsVchRJJzBwG+yhwU/iN3MlV2c5D69tls/Dok
 6VbyU4lm/ae0y3yR83D9dUlkycOnmmlBAHKIZ9qUts9X7mWJf0+yy2QxJVpjaTGG
 cmnQKKPeNIhGJk2ENnnnzjEve7L7YJQF6itbx5VCOcsGh3Ocb3YR7DMdWjt7f8pu
 c6j+q1rP7EpE2afUN/geSlp5i3x8aXZPDj67jImbVCE/Q1X9voCtyzGJH7MXR0N9
 ZpRF8yzveRfMH8bwAJjSOGAFF5XkcR/RNY95o+J+QcgBLdX48h+ZdNmUf6jqlu3J
 7KmTXXQcOVpN6dD3CmRFsbjq+x6RHwa8u1iGn+oIkX908r97ckfB/kHKH7ZdXIJc
 cpxtDQQMGYFpXK/71stq
 =ozeK
 -----END PGP SIGNATURE-----

a simple commit which works
COMMIT

    signature = <<-SIGNATURE.strip
-----BEGIN PGP SIGNATURE-----
Version: GnuPG v1.4.12 (Darwin)

iQIcBAABAgAGBQJQ+FMIAAoJEH+LfPdZDSs1e3EQAJMjhqjWF+WkGLHju7pTw2al
o6IoMAhv0Z/LHlWhzBd9e7JeCnanRt12bAU7yvYp9+Z+z+dbwqLwDoFp8LVuigl8
JGLcnwiUW3rSvhjdCp9irdb4+bhKUnKUzSdsR2CK4/hC0N2i/HOvMYX+BRsvqweq
AsAkA6dAWh+gAfedrBUkCTGhlNYoetjdakWqlGL1TiKAefEZrtA1TpPkGn92vbLq
SphFRUY9hVn1ZBWrT3hEpvAIcZag3rTOiRVT1X1flj8B2vGCEr3RrcwOIZikpdaW
who/X3xh/DGbI2RbuxmmJpxxP/8dsVchRJJzBwG+yhwU/iN3MlV2c5D69tls/Dok
6VbyU4lm/ae0y3yR83D9dUlkycOnmmlBAHKIZ9qUts9X7mWJf0+yy2QxJVpjaTGG
cmnQKKPeNIhGJk2ENnnnzjEve7L7YJQF6itbx5VCOcsGh3Ocb3YR7DMdWjt7f8pu
c6j+q1rP7EpE2afUN/geSlp5i3x8aXZPDj67jImbVCE/Q1X9voCtyzGJH7MXR0N9
ZpRF8yzveRfMH8bwAJjSOGAFF5XkcR/RNY95o+J+QcgBLdX48h+ZdNmUf6jqlu3J
7KmTXXQcOVpN6dD3CmRFsbjq+x6RHwa8u1iGn+oIkX908r97ckfB/kHKH7ZdXIJc
cpxtDQQMGYFpXK/71stq
=ozeK
-----END PGP SIGNATURE-----
SIGNATURE

    base_data = <<-SIGNEDDATA
tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904
parent 8496071c1b46c854b31185ea97743be6a8774479
author Ben Burkert <ben@benburkert.com> 1358451456 -0800
committer Ben Burkert <ben@benburkert.com> 1358451456 -0800

a simple commit which works
SIGNEDDATA

    id1 = Rugged::Commit::create_with_signature(@repo, base_data, signature, "gpgsig")
    id2 = Rugged::Commit::create_with_signature(@repo, base_data, signature)
    sig, data = Rugged::Commit::extract_signature(@repo, id1)

    assert_equal id1, id2
    assert_equal base_data, data
    assert_equal signature, sig

    raw_commit = Rugged::Commit::lookup(@repo, id1).read_raw.data
    assert_equal signed_commit, raw_commit
  end

  def test_commit_summary
    person = {:name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }

    commit_id = Rugged::Commit.create(@repo,
      :message => "This is the commit message\n\nThis commit is created from Rugged",
      :committer => person,
      :author => person,
      :parents => [@repo.head.target],
      :tree => "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")

    commit = Rugged::Commit.lookup(@repo, commit_id)
    assert_equal "This is the commit message", commit.summary
  end
end

class CommitWriteTest < Rugged::TestCase
  def setup
    @source_repo = FixtureRepo.from_rugged("testrepo.git")
    @repo = FixtureRepo.clone(@source_repo)
    @repo.config['core.abbrev'] = 7
  end

  def test_write_commit_with_time
    person = {:name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }

    Rugged::Commit.create(@repo,
      :message => "This is the commit message\n\nThis commit is created from Rugged",
      :committer => person,
      :author => person,
      :parents => [@repo.head.target],
      :tree => "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")
  end

  def test_write_commit_with_time_offset
    person = {:name => 'Jake', :email => 'jake@github.com', :time => Time.now, :time_offset => 3600}

    oid = Rugged::Commit.create(@repo,
      :message => "This is the commit message\n\nThis commit is created from Rugged",
      :committer => person,
      :author => person,
      :parents => [@repo.head.target],
      :tree => "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")

    commit = @repo.lookup(oid)
    assert_equal 3600, commit.committer[:time].utc_offset
  end

  def test_write_commit_without_time
    person = {:name => 'Jake', :email => 'jake@github.com'}

    oid = Rugged::Commit.create(@repo,
      :message => "This is the commit message\n\nThis commit is created from Rugged",
      :committer => person,
      :author => person,
      :parents => [@repo.head.target],
      :tree => "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")

    commit = @repo.lookup(oid)
    assert_kind_of Time, commit.committer[:time]
  end

  def test_write_commit_without_signature
    name = 'Rugged User'
    email = 'rugged@example.com'
    @repo.config['user.name'] = name
    @repo.config['user.email'] = email

    oid = Rugged::Commit.create(@repo,
      :message => "This is the commit message\n\nThis commit is created from Rugged",
      :parents => [@repo.head.target],
      :tree => "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")

    commit = @repo.lookup(oid)
    assert_equal name, commit.committer[:name]
    assert_equal email, commit.committer[:email]
    assert_equal name, commit.author[:name]
    assert_equal email, commit.author[:email]
  end

  def test_write_invalid_parents
    person = {:name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }
    assert_raises TypeError do
      Rugged::Commit.create(@repo,
        :message => "This is the commit message\n\nThis commit is created from Rugged",
        :parents => [:invalid_parent],
        :committer => person,
        :author => person,
        :tree => "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")
    end
  end

  def test_write_empty_email_fails
    person = {:name => 'Jake', :email => '', :time => Time.now}
    assert_raises Rugged::InvalidError do
      Rugged::Commit.create(@repo,
      :message => "This is the commit message\n\nThis commit is created from Rugged",
      :committer => person,
      :author => person,
      :parents => [@repo.head.target],
      :tree => "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")
    end
  end

  def test_write_signature_raises_key_error_for_missing_keys
    person = {:name => 'Jake', :time => Time.now} # no :email
    assert_raises KeyError do
      Rugged::Commit.create(@repo,
      :message => "This is the commit message\n\nThis commit is created from Rugged",
      :committer => person,
      :author => person,
      :parents => [@repo.head.target],
      :tree => "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")
    end
  end

  def test_create_commit_to_s
    person = {:name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }

    id = Rugged::Commit.create(@repo,
      :message => "This is the commit message\n\nThis commit is created from Rugged",
      :committer => person,
      :author => person,
      :parents => [@repo.head.target],
      :tree => "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")

    buffer = Rugged::Commit.create_to_s(@repo,
      :message => "This is the commit message\n\nThis commit is created from Rugged",
      :committer => person,
      :author => person,
      :parents => [@repo.head.target],
      :tree => "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")

    commit = @repo.lookup(id)
    assert_equal buffer, commit.read_raw.data
  end
end

class CommitToMboxTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2 "diff_format_email"
    @repo.config['core.abbrev'] = 7

    prerelease = Rugged.libgit2_prerelease
    prerelease = "-#{prerelease}" unless prerelease.empty?
    @version_string = "#{Rugged.libgit2_version.join('.')}#{prerelease unless prerelease.empty?}"
  end

  def test_format_to_mbox
    assert_equal <<-EOS, @repo.lookup("9264b96c6d104d0e07ae33d3007b6a48246c6f92").to_mbox
From 9264b96c6d104d0e07ae33d3007b6a48246c6f92 Mon Sep 17 00:00:00 2001
From: Jacques Germishuys <jacquesg@striata.com>
Date: Wed, 9 Apr 2014 20:57:01 +0200
Subject: [PATCH] Modify some content

---
 file1.txt | 8 +++++---
 1 file changed, 5 insertions(+), 3 deletions(-)

diff --git a/file1.txt b/file1.txt
index 94aaae8..af8f41d 100644
--- a/file1.txt
+++ b/file1.txt
@@ -1,15 +1,17 @@
 file1.txt
 file1.txt
+_file1.txt_
 file1.txt
 file1.txt
 file1.txt
 file1.txt
+
+
 file1.txt
 file1.txt
 file1.txt
 file1.txt
 file1.txt
-file1.txt
-file1.txt
-file1.txt
+_file1.txt_
+_file1.txt_
 file1.txt
--
libgit2 #{@version_string}

EOS
  end

  def test_format_to_mbox_multiple
    commit = @repo.lookup("10808fe9c9be5a190c0ba68d1a002233fb363508")
    assert_equal <<-EOS, commit.to_mbox(patch_no: 1, total_patches: 2)
From 10808fe9c9be5a190c0ba68d1a002233fb363508 Mon Sep 17 00:00:00 2001
From: Jacques Germishuys <jacquesg@striata.com>
Date: Thu, 10 Apr 2014 19:37:05 +0200
Subject: [PATCH 1/2] Added file2.txt file3.txt

---
 file2.txt | 5 +++++
 file3.txt | 5 +++++
 2 files changed, 10 insertions(+)
 create mode 100644 file2.txt
 create mode 100644 file3.txt

diff --git a/file2.txt b/file2.txt
new file mode 100644
index 0000000..e909123
--- /dev/null
+++ b/file2.txt
@@ -0,0 +1,5 @@
+file2
+file2
+file2
+file2
+file2
diff --git a/file3.txt b/file3.txt
new file mode 100644
index 0000000..9435022
--- /dev/null
+++ b/file3.txt
@@ -0,0 +1,5 @@
+file3
+file3
+file3
+file3
+file3
--
libgit2 #{@version_string}

EOS

    commit = @repo.lookup("873806f6f27e631eb0b23e4b56bea2bfac14a373")
    assert_equal <<-EOS, commit.to_mbox(patch_no: 2, total_patches: 2)
From 873806f6f27e631eb0b23e4b56bea2bfac14a373 Mon Sep 17 00:00:00 2001
From: Jacques Germishuys <jacquesg@striata.com>
Date: Thu, 10 Apr 2014 19:37:36 +0200
Subject: [PATCH 2/2] Modified file2.txt, file3.txt

---
 file2.txt | 2 +-
 file3.txt | 2 +-
 2 files changed, 2 insertions(+), 2 deletions(-)

diff --git a/file2.txt b/file2.txt
index e909123..7aff11d 100644
--- a/file2.txt
+++ b/file2.txt
@@ -1,5 +1,5 @@
 file2
 file2
 file2
-file2
+file2!
 file2
diff --git a/file3.txt b/file3.txt
index 9435022..9a2d780 100644
--- a/file3.txt
+++ b/file3.txt
@@ -1,5 +1,5 @@
 file3
-file3
+file3!
 file3
 file3
 file3
--
libgit2 #{@version_string}

EOS

  end

  def test_format_to_mbox_exclude_marker
    commit = @repo.lookup("9264b96c6d104d0e07ae33d3007b6a48246c6f92")
    assert_equal <<-EOS, commit.to_mbox(exclude_subject_patch_marker: true)
From 9264b96c6d104d0e07ae33d3007b6a48246c6f92 Mon Sep 17 00:00:00 2001
From: Jacques Germishuys <jacquesg@striata.com>
Date: Wed, 9 Apr 2014 20:57:01 +0200
Subject: Modify some content

---
 file1.txt | 8 +++++---
 1 file changed, 5 insertions(+), 3 deletions(-)

diff --git a/file1.txt b/file1.txt
index 94aaae8..af8f41d 100644
--- a/file1.txt
+++ b/file1.txt
@@ -1,15 +1,17 @@
 file1.txt
 file1.txt
+_file1.txt_
 file1.txt
 file1.txt
 file1.txt
 file1.txt
+
+
 file1.txt
 file1.txt
 file1.txt
 file1.txt
 file1.txt
-file1.txt
-file1.txt
-file1.txt
+_file1.txt_
+_file1.txt_
 file1.txt
--
libgit2 #{@version_string}

EOS
  end

  def test_format_to_mbox_diff_options
    commit = @repo.lookup("9264b96c6d104d0e07ae33d3007b6a48246c6f92")
    assert_equal <<-EOS, commit.to_mbox(context_lines: 1, interhunk_lines: 1)
From 9264b96c6d104d0e07ae33d3007b6a48246c6f92 Mon Sep 17 00:00:00 2001
From: Jacques Germishuys <jacquesg@striata.com>
Date: Wed, 9 Apr 2014 20:57:01 +0200
Subject: [PATCH] Modify some content

---
 file1.txt | 8 +++++---
 1 file changed, 5 insertions(+), 3 deletions(-)

diff --git a/file1.txt b/file1.txt
index 94aaae8..af8f41d 100644
--- a/file1.txt
+++ b/file1.txt
@@ -2,2 +2,3 @@ file1.txt
 file1.txt
+_file1.txt_
 file1.txt
@@ -6,2 +7,4 @@ file1.txt
 file1.txt
+
+
 file1.txt
@@ -11,5 +14,4 @@ file1.txt
 file1.txt
-file1.txt
-file1.txt
-file1.txt
+_file1.txt_
+_file1.txt_
 file1.txt
--
libgit2 #{@version_string}

EOS
  end

  class TrailersTest < Rugged::TestCase
    def setup
      @source_repo = FixtureRepo.from_rugged("testrepo.git")
      @repo = FixtureRepo.clone(@source_repo)
      @repo.config['core.abbrev'] = 7
    end

    def test_can_parse_trailers
      person = {:name => 'Brian', :email => 'brian@gmail.com', :time => Time.now }

      commit_oid = Rugged::Commit.create(@repo,
        :message => "This is the commit message\n\nCo-authored-by: Charles <charliesome@github.com>\nSigned-off-by: Arthur Schreiber <arthurschreiber@github.com>",
        :committer => person,
        :author => person,
        :parents => [@repo.head.target],
        :tree => @repo.head.target.tree_oid)

      commit = @repo.lookup(commit_oid)

      expected = [
        ["Co-authored-by", "Charles <charliesome@github.com>"],
        ["Signed-off-by", "Arthur Schreiber <arthurschreiber@github.com>"]
      ]

      assert_equal expected, commit.trailers
    end
  end
end
