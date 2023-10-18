require "test_helper"

class TagTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2("testrepo.git")
  end

  def test_lookup_raises_error_if_object_type_does_not_match
    assert_raises Rugged::InvalidError do
      # blob
      Rugged::Tag::Annotation.lookup(@repo, "fa49b077972391ad58037050f2a75f74e3671e92")
    end

    assert_raises Rugged::InvalidError do
      # commit
      Rugged::Tag::Annotation.lookup(@repo, "8496071c1b46c854b31185ea97743be6a8774479")
    end

    assert_raises Rugged::InvalidError do
      # tree
      Rugged::Tag::Annotation.lookup(@repo, "181037049a54a1eb5fab404658a3a250b44335d7")
    end

    subclass = Class.new(Rugged::Tag::Annotation)

    assert_raises Rugged::InvalidError do
      # blob
      subclass.lookup(@repo, "fa49b077972391ad58037050f2a75f74e3671e92")
    end

    assert_raises Rugged::InvalidError do
      # commit
      subclass.lookup(@repo, "8496071c1b46c854b31185ea97743be6a8774479")
    end

    assert_raises Rugged::InvalidError do
      # tree
      subclass.lookup(@repo, "181037049a54a1eb5fab404658a3a250b44335d7")
    end
  end

  def test_reading_a_tag
    oid = "7b4384978d2493e851f9cca7858815fac9b10980"
    obj = @repo.lookup(oid)

    assert_equal oid, obj.oid
    assert_equal :tag, obj.type
    assert_equal "This is a very simple tag.\n", obj.message
    assert_equal "e90810b", obj.name
    assert_equal "e90810b8df3e80c413d903f631643c716887138d", obj.target.oid
    assert_equal :commit, obj.target_type
    c = obj.tagger
    assert_equal "Vicent Marti", c[:name]
    assert_equal 1281578357, c[:time].to_i
    assert_equal "tanoku@gmail.com", c[:email]
  end

  def test_reading_the_oid_of_a_tag
    oid = "7b4384978d2493e851f9cca7858815fac9b10980"
    obj = @repo.lookup(oid)

    assert_equal "e90810b8df3e80c413d903f631643c716887138d", obj.target_oid
  end

  def test_lookup
    tag = @repo.tags["e90810b"]

    assert_equal tag.name, "e90810b"
    assert_equal tag.canonical_name, "refs/tags/e90810b"
  end

  def test_lookup_git_compliance
    @repo.tags.create("refs/tags/v2.0", "e90810b8df3e80c413d903f631643c716887138d")

    assert_nil @repo.tags["v2.0"]
    assert_equal "refs/tags/refs/tags/v2.0", @repo.tags["refs/tags/v2.0"].canonical_name
    assert_equal "refs/tags/refs/tags/v2.0", @repo.tags["refs/tags/refs/tags/v2.0"].canonical_name

    @repo.tags.create("v2.0", "e90810b8df3e80c413d903f631643c716887138d")

    assert_equal "refs/tags/v2.0", @repo.tags["v2.0"].canonical_name
    assert_equal "refs/tags/v2.0", @repo.tags["refs/tags/v2.0"].canonical_name
    assert_equal "refs/tags/refs/tags/v2.0", @repo.tags["refs/tags/refs/tags/v2.0"].canonical_name
  end

  def test_each
    tags = @repo.tags.each.sort_by(&:name)

    assert_equal tags.count, 7
    assert_equal tags[0].name, "annotated_tag_to_blob"
    assert_equal tags[1].name, "e90810b"
  end

  def test_each_name
    tag_names = @repo.tags.each_name.sort

    assert_equal tag_names.count, 7
    assert_equal tag_names[0], "annotated_tag_to_blob"
    assert_equal tag_names[1], "e90810b"
  end

  def test_extract_signature
    raw_tag = <<-TAG
object 36060c58702ed4c2a40832c51758d5344201d89a
type commit
tag signed
tagger Ben Toews <mastahyeti@users.noreply.github.com> 1455121460 -0700

my signed tag
-----BEGIN PGP SIGNATURE-----
Version: GnuPG v1

iQEcBAABAgAGBQJWu2Q0AAoJEDJi7/JboNJwYNgH/0NQAgNthQi5kiZL5LdAKNE9
HRHj1Gumcqf04pPbuNXBTt4jdsWka8cTLu8q4CVE2EBtJRJfYhoKXt+bhDSa4WRc
W/M02GASaqBbY3BVAI0+jd/nmoUqz97jsYotOjVOhUbIGJjo2V2nKi1x6lAN3Nr2
bq9zJBbYWkIExZU29ycWWk+yTLPGcmcnr0itoUyywgg9NKivH2e8EmyQgAjLUp+M
NL9Fgg5UMTDoRvJs98i+SIJAkNzxN0mypr51G0kmZbWkSEshHvBoZlV731QqmT+j
1+eI1odAMHSNHqkmel8+1yooQQzPFc9ZmxGi8DdyuSv6AD7pzxX5vmU0AuBU9C4=
=Tmm9
-----END PGP SIGNATURE-----
TAG

    exp_signature = <<-SIGNATURE
-----BEGIN PGP SIGNATURE-----
Version: GnuPG v1

iQEcBAABAgAGBQJWu2Q0AAoJEDJi7/JboNJwYNgH/0NQAgNthQi5kiZL5LdAKNE9
HRHj1Gumcqf04pPbuNXBTt4jdsWka8cTLu8q4CVE2EBtJRJfYhoKXt+bhDSa4WRc
W/M02GASaqBbY3BVAI0+jd/nmoUqz97jsYotOjVOhUbIGJjo2V2nKi1x6lAN3Nr2
bq9zJBbYWkIExZU29ycWWk+yTLPGcmcnr0itoUyywgg9NKivH2e8EmyQgAjLUp+M
NL9Fgg5UMTDoRvJs98i+SIJAkNzxN0mypr51G0kmZbWkSEshHvBoZlV731QqmT+j
1+eI1odAMHSNHqkmel8+1yooQQzPFc9ZmxGi8DdyuSv6AD7pzxX5vmU0AuBU9C4=
=Tmm9
-----END PGP SIGNATURE-----
SIGNATURE

    exp_signed_data = <<-SIGNEDDATA
object 36060c58702ed4c2a40832c51758d5344201d89a
type commit
tag signed
tagger Ben Toews <mastahyeti@users.noreply.github.com> 1455121460 -0700

my signed tag
SIGNEDDATA

    tag_oid = @repo.write(raw_tag, :tag)

    signature, signed_data = Rugged::Tag.extract_signature(@repo, tag_oid)
    assert_equal exp_signature, signature
    assert_equal exp_signed_data, signed_data

    signature, signed_data = Rugged::Tag.extract_signature(@repo, tag_oid, Rugged::Tag::GPG_SIGNATURE_PREFIX)
    assert_equal exp_signature, signature
    assert_equal exp_signed_data, signed_data
  end
end

class AnnotatedTagTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2("testrepo.git")
    @tag = @repo.tags.create(
      'annotated_tag',
      "5b5b025afb0b4c913b4c338a42934a3863bf3644",
      :message => "test tag message\n",
      :tagger  => { :name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }
    )
  end

  def test_is_annotated
    assert_equal true, @tag.annotated?
  end

  def test_annotation
    annotation = @tag.annotation

    assert_kind_of Rugged::Tag::Annotation, annotation
    assert_equal "test tag message\n", annotation.message
    assert_equal 'Scott', annotation.tagger[:name]
    assert_equal 'schacon@gmail.com', annotation.tagger[:email]
    assert_kind_of Time, annotation.tagger[:time]
  end

  def test_target
    target = @tag.target

    assert_kind_of Rugged::Commit, target
    assert_equal "5b5b025afb0b4c913b4c338a42934a3863bf3644", target.oid
  end
end

class AnnotatedTagObjectTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2("testrepo.git")
    @annotation = @repo.tags.create_annotation("my-tag", "5b5b025afb0b4c913b4c338a42934a3863bf3644", {
      :message => "test tag message\n",
      :tagger  => { :name => "Scott", :email => "schacon@gmail.com", :time => Time.now },
    })
  end

  def test_annotation
    assert_kind_of Rugged::Tag::Annotation, @annotation
    assert_equal "test tag message\n", @annotation.message
    assert_equal 'Scott', @annotation.tagger[:name]
    assert_equal 'schacon@gmail.com', @annotation.tagger[:email]
    assert_kind_of Time, @annotation.tagger[:time]
  end

  def test_does_not_create_ref
    assert_nil @repo.tags["my-tag"]
  end

  def test_target
    assert_kind_of Rugged::Commit, @annotation.target
    assert_equal "5b5b025afb0b4c913b4c338a42934a3863bf3644", @annotation.target.oid
  end
end

class LightweightTagTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2("testrepo.git")
    @tag = @repo.tags.create('lightweight_tag', "5b5b025afb0b4c913b4c338a42934a3863bf3644")
  end

  def test_is_not_annotated
    assert_equal false, @tag.annotated?
  end

  def test_has_no_annotation
    assert_nil @tag.annotation
  end

  def test_target
    target = @tag.target

    assert_kind_of Rugged::Commit, target
    assert_equal "5b5b025afb0b4c913b4c338a42934a3863bf3644", target.oid
  end
end

class TagWriteTest < Rugged::TestCase
  def setup
    @source_repo = FixtureRepo.from_rugged("testrepo.git")
    @repo = FixtureRepo.clone(@source_repo)
  end

  def test_writing_a_tag
    tag = @repo.tags.create(
      'tag',
      "5b5b025afb0b4c913b4c338a42934a3863bf3644",
      :message => "test tag message\n",
      :tagger  => { :name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }
    )

    annotation = tag.annotation
    assert_equal :tag, annotation.type
    assert_equal "5b5b025afb0b4c913b4c338a42934a3863bf3644", annotation.target.oid
    assert_equal "test tag message\n", annotation.message
    assert_equal "Scott", annotation.tagger[:name]
    assert_equal "schacon@gmail.com", annotation.tagger[:email]
  end

  def test_writing_a_tag_without_signature
    name = 'Rugged User'
    email = 'rugged@example.com'
    @repo.config['user.name'] = name
    @repo.config['user.email'] = email

    tag = @repo.tags.create(
      'tag',
      "5b5b025afb0b4c913b4c338a42934a3863bf3644",
      :message => "test tag message\n"
    )

    assert_equal name, tag.annotation.tagger[:name]
    assert_equal email, tag.annotation.tagger[:email]
  end

  def test_tag_invalid_message_type
    assert_raises TypeError do
      @repo.tags.create(
        'tag',
        "5b5b025afb0b4c913b4c338a42934a3863bf3644",
        :message => :invalid_message,
        :tagger  => {:name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }
      )
    end
  end

  def test_writing_light_tags
    tag = @repo.tags.create('tag', "5b5b025afb0b4c913b4c338a42934a3863bf3644")
    assert_equal @repo.lookup("5b5b025afb0b4c913b4c338a42934a3863bf3644"), tag.target
  end
end
