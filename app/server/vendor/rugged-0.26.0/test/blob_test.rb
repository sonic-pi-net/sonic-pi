require "test_helper"

class BlobTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_rugged("testrepo.git")
  end

  def test_lookup_raises_error_if_object_type_does_not_match
    assert_raises Rugged::InvalidError do
      # commit
      Rugged::Blob.lookup(@repo, "8496071c1b46c854b31185ea97743be6a8774479")
    end

    assert_raises Rugged::InvalidError do
      # tag
      Rugged::Blob.lookup(@repo, "0c37a5391bbff43c37f0d0371823a5509eed5b1d")
    end

    assert_raises Rugged::InvalidError do
      # tree
      Rugged::Blob.lookup(@repo, "c4dc1555e4d4fa0e0c9c3fc46734c7c35b3ce90b")
    end

    subclass = Class.new(Rugged::Blob)

    assert_raises Rugged::InvalidError do
      # commit
      subclass.lookup(@repo, "8496071c1b46c854b31185ea97743be6a8774479")
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

  def test_read_blob_data
    oid = "fa49b077972391ad58037050f2a75f74e3671e92"
    blob = @repo.lookup(oid)
    assert_equal 9, blob.size
    assert_equal "new file\n", blob.content
    assert_equal :blob, blob.type
    assert_equal oid, blob.oid
    assert_equal "new file\n", blob.text
  end

  def test_blob_sloc
    oid = "7771329dfa3002caf8c61a0ceb62a31d09023f37"
    blob = @repo.lookup(oid)
    assert_equal 328, blob.sloc
  end

  def test_blob_content_with_size
    oid = "7771329dfa3002caf8c61a0ceb62a31d09023f37"
    blob = @repo.lookup(oid)
    content =  blob.content(10)
    assert_equal "# Rugged\n*", content
    assert_equal 10, content.size
  end

  def test_blob_content_with_size_gt_file_size
    oid = "7771329dfa3002caf8c61a0ceb62a31d09023f37"
    blob = @repo.lookup(oid)
    content =  blob.content(1000000)
    assert_equal blob.size, content.size
  end

  def test_blob_content_with_zero_size
    oid = "7771329dfa3002caf8c61a0ceb62a31d09023f37"
    blob = @repo.lookup(oid)
    content =  blob.content(0)
    assert_equal '', content
  end

  def test_blob_content_with_negative_size
    oid = "7771329dfa3002caf8c61a0ceb62a31d09023f37"
    blob = @repo.lookup(oid)
    content =  blob.content(-100)
    assert_equal blob.size, content.size
  end

  def test_blob_text_with_max_lines
    oid = "7771329dfa3002caf8c61a0ceb62a31d09023f37"
    blob = @repo.lookup(oid)
    assert_equal "# Rugged\n", blob.text(1)
  end

  def test_blob_text_with_lines_gt_file_lines
    oid = "7771329dfa3002caf8c61a0ceb62a31d09023f37"
    blob = @repo.lookup(oid)
    text = blob.text(1000000)
    assert_equal 464, text.lines.count
  end

  def test_blob_text_with_zero_lines
    oid = "7771329dfa3002caf8c61a0ceb62a31d09023f37"
    blob = @repo.lookup(oid)
    text = blob.text(0)
    assert_equal '', text
  end

  def test_blob_text_with_negative_lines
    oid = "7771329dfa3002caf8c61a0ceb62a31d09023f37"
    blob = @repo.lookup(oid)
    text = blob.text(-100)
    assert_equal 464, text.lines.count
  end

  def test_blob_text_default_encoding
    oid = "7771329dfa3002caf8c61a0ceb62a31d09023f37"
    blob = @repo.lookup(oid)
    assert_equal Encoding.default_external, blob.text.encoding
  end

  def test_blob_text_set_encoding
    oid = "7771329dfa3002caf8c61a0ceb62a31d09023f37"
    blob = @repo.lookup(oid)
    assert_equal Encoding::ASCII_8BIT, blob.text(0, Encoding::ASCII_8BIT).encoding
  end
end

class BlobMergeTest < Rugged::TestCase
  def setup
    @source_repo = FixtureRepo.from_rugged("testrepo.git")
    @repo = FixtureRepo.clone(@source_repo)

    @ancestor_content = "Common ancestor"
    @our_content = "Ours side"
    @their_content = "Theirs side"

    @expected_data = "<<<<<<< OURS\nOurs side\n" +
      "||||||| ANCESTOR\nCommon ancestor\n" +
      "=======\n" +
      "Theirs side\n>>>>>>> THEIRS\n"

    @opts = {
      :ancestor_label => "ANCESTOR",
      :our_label => "OURS",
      :their_label => "THEIRS",
      :style => :diff3 }
  end

  def test_blob_merge_files
    ancestor = { :content => @ancestor_content, :path => "file.txt",    :filemode => 0100644 }
    ours =     { :content => @our_content,      :path => "newfile.txt", :filemode => 0100644 }
    theirs =   { :content => @their_content,    :path => "file.txt",    :filemode => 0100755 }

    result = Rugged::Blob.merge_files(nil, ancestor, ours, theirs, @opts)

    assert_equal false, result[:automergeable]
    assert_equal "newfile.txt", result[:path]
    assert_equal 0100755, result[:filemode]
    assert_equal @expected_data, result[:data]
  end

  def test_blob_merge_files_by_oid
    ancestor_oid = Rugged::Blob.from_buffer(@repo, @ancestor_content)
    our_oid = Rugged::Blob.from_buffer(@repo, @our_content)
    their_oid = Rugged::Blob.from_buffer(@repo, @their_content)

    ancestor = { :oid => ancestor_oid, :path => "file.txt",    :filemode => 0100644 }
    ours =     { :oid => our_oid,      :path => "newfile.txt", :filemode => 0100644 }
    theirs =   { :oid => their_oid,    :path => "file.txt",    :filemode => 0100755 }

    result = Rugged::Blob.merge_files(@repo, ancestor, ours, theirs, @opts)

    assert_equal false, result[:automergeable]
    assert_equal "newfile.txt", result[:path]
    assert_equal 0100755, result[:filemode]
    assert_equal @expected_data, result[:data]
  end
end

class BlobWriteTest < Rugged::TestCase
  def setup
    @source_repo = FixtureRepo.from_rugged("testrepo.git")
    @repo = FixtureRepo.clone(@source_repo)
  end

  def test_fetch_blob_content_with_nulls
    content = "100644 example_helper.rb\x00\xD3\xD5\xED\x9DA4_"+
               "\xE3\xC3\nK\xCD<!\xEA-_\x9E\xDC=40000 examples\x00"+
               "\xAE\xCB\xE9d!|\xB9\xA6\x96\x024],U\xEE\x99\xA2\xEE\xD4\x92"

    content.force_encoding('binary') if content.respond_to?(:force_encoding)

    oid = @repo.write(content, 'tree')
    blob = @repo.lookup(oid)
    assert_equal content, blob.read_raw.data
  end

  def test_write_blob_data
    assert_equal '1d83f106355e4309a293e42ad2a2c4b8bdbe77ae',
      Rugged::Blob.from_buffer(@repo, "a new blob content")
  end

  def test_write_blob_from_workdir
    assert_equal '1385f264afb75a56a5bec74243be9b367ba4ca08',
      Rugged::Blob.from_workdir(@repo, "README")
  end

  def test_write_blob_from_disk
    file_path = File.join(TEST_DIR, (File.join('fixtures', 'archive.tar.gz')))
    File.open(file_path, 'rb') do |file|
      oid = Rugged::Blob.from_disk(@repo, file.path)
      assert oid

      blob = @repo.lookup(oid)
      file.rewind
      assert_equal file.read, blob.content
    end
  end

  def test_blob_is_binary
    binary_file_path = File.join(TEST_DIR, (File.join('fixtures', 'archive.tar.gz')))
    binary_blob = @repo.lookup(Rugged::Blob.from_disk(@repo, binary_file_path))
    assert binary_blob.binary?

    text_file_path = File.join(TEST_DIR, (File.join('fixtures', 'text_file.md')))
    text_blob = @repo.lookup(Rugged::Blob.from_disk(@repo, text_file_path))
    refute text_blob.binary?
  end
end

class BlobLOCTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_rugged("testrepo.git")
  end

  def write_blob(data)
    sha = Rugged::Blob.from_buffer(@repo, data)
    Rugged::Blob.lookup(@repo, sha)
  end

  def test_loc_end_nl
    blob = write_blob("hello\nworld\nwhat\n")
    assert_equal 3, blob.loc
  end

  def test_loc_no_end_nl
    blob = write_blob("hello\nworld\nwhat")
    assert_equal 3, blob.loc
  end

  def test_loc_carriages
    blob = write_blob("hello\r\nworld\r\nwhat\r\n")
    assert_equal 3, blob.loc
  end

  def test_loc_carriages_no_end_nl
    blob = write_blob("hello\r\nworld\r\nwhat")
    assert_equal 3, blob.loc
  end

  def test_loc_mixed
    blob = write_blob("hello\nworld\rwhat\r")
    assert_equal 3, blob.loc
  end

  def test_loc_mixed_no_end_nl
    blob = write_blob("hello\nworld\rwhat")
    assert_equal 3, blob.loc
  end
end

class BlobDiffTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2("diff")
  end

  def test_diff_blob
    a = @repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = @repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    blob  = @repo.lookup(a.tree["readme.txt"][:oid])
    other = @repo.lookup(b.tree["readme.txt"][:oid])

    patch = blob.diff(other)

    assert_equal :modified, patch.delta.status

    hunks = []
    patch.each_hunk do |hunk|
      assert_instance_of Rugged::Diff::Hunk, hunk
      hunks << hunk
    end
    assert_equal 3, hunks.size

    assert hunks[0].header.start_with? "@@ -1,4 +1,4 @@"
    assert hunks[1].header.start_with? "@@ -7,10 +7,6 @@"
    assert hunks[2].header.start_with? "@@ -24,12 +20,9 @@"

    lines = []
    hunks[0].each_line do |line|
      lines << line
    end
    assert_equal 5, lines.size

    assert_equal :deletion, lines[0].line_origin
    assert_equal "The Git feature that really makes it stand apart from nearly every other SCM\n", lines[0].content

    assert_equal :addition, lines[1].line_origin
    assert_equal "The Git feature that r3ally mak3s it stand apart from n3arly 3v3ry other SCM\n", lines[1].content

    assert_equal :context, lines[2].line_origin
    assert_equal "out there is its branching model.\n", lines[2].content

    assert_equal :context, lines[3].line_origin
    assert_equal "\n", lines[3].content

    assert_equal :context, lines[4].line_origin
    assert_equal "Git allows and encourages you to have multiple local branches that can be\n", lines[4].content
  end

  def test_diff_string
    a = @repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = @repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    blob  = @repo.lookup(a.tree["readme.txt"][:oid])
    other = @repo.lookup(b.tree["readme.txt"][:oid]).content

    patch = blob.diff(other)

    assert_equal :modified, patch.delta.status

    hunks = []
    patch.each_hunk do |hunk|
      assert_instance_of Rugged::Diff::Hunk, hunk
      hunks << hunk
    end
    assert_equal 3, hunks.size

    assert hunks[0].header.start_with? "@@ -1,4 +1,4 @@"
    assert hunks[1].header.start_with? "@@ -7,10 +7,6 @@"
    assert hunks[2].header.start_with? "@@ -24,12 +20,9 @@"

    lines = []
    hunks[0].each_line do |line|
      lines << line
    end
    assert_equal 5, lines.size

    assert_equal :deletion, lines[0].line_origin
    assert_equal "The Git feature that really makes it stand apart from nearly every other SCM\n", lines[0].content

    assert_equal :addition, lines[1].line_origin
    assert_equal "The Git feature that r3ally mak3s it stand apart from n3arly 3v3ry other SCM\n", lines[1].content

    assert_equal :context, lines[2].line_origin
    assert_equal "out there is its branching model.\n", lines[2].content

    assert_equal :context, lines[3].line_origin
    assert_equal "\n", lines[3].content

    assert_equal :context, lines[4].line_origin
    assert_equal "Git allows and encourages you to have multiple local branches that can be\n", lines[4].content
  end

  def test_diff_nil
    a = @repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")

    blob  = @repo.lookup(a.tree["readme.txt"][:oid])

    patch = blob.diff(nil)

    assert_equal :deleted, patch.delta.status

    hunks = []
    patch.each_hunk do |hunk|
      assert_instance_of Rugged::Diff::Hunk, hunk
      hunks << hunk
    end
    assert_equal 1, hunks.size

    assert hunks[0].header.start_with? "@@ -1,35 +0,0 @@"

    lines = []
    hunks[0].each_line do |line|
      lines << line
    end
    assert_equal 35, lines.size

    lines.each do |line|
      assert_equal :deletion, line.line_origin
    end
  end

  def test_diff_with_paths
    a = @repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = @repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    blob  = @repo.lookup(a.tree["readme.txt"][:oid])
    other = @repo.lookup(b.tree["readme.txt"][:oid])

    patch = blob.diff(other, :old_path => "old_readme.txt", :new_path => "new_readme.txt")
    assert_equal "old_readme.txt", patch.delta.old_file[:path]
    assert_equal "new_readme.txt", patch.delta.new_file[:path]
  end
end

class BlobCreateFromIOTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_rugged("testrepo.git")
  end

  def test_write_blob_from_io_with_hintpath
    file_path= File.join(TEST_DIR, (File.join('fixtures', 'archive.tar.gz')))
    File.open(file_path, 'rb') do |io|
      oid = Rugged::Blob.from_io(@repo, io, 'archive.tar.gz2')
      io.rewind
      blob = @repo.lookup(oid)
      assert_equal io.read, blob.content
    end
  end

  def test_write_blob_from_io_without_hintpath
    file_path= File.join(TEST_DIR, (File.join('fixtures', 'archive.tar.gz')))
    File.open(file_path, 'rb') do |io|
      oid = Rugged::Blob.from_io(@repo, io)
      io.rewind
      blob = @repo.lookup(oid)
      assert_equal io.read, blob.content
    end
  end

  class BrokenIO
    def read(length)
      raise IOError
    end
  end

  def test_write_blob_from_io_broken_io_raises_error
    assert_raises IOError do
      Rugged::Blob.from_io(@repo, BrokenIO.new)
    end
  end

  class OverflowIO
    def initialize()
      @called = false
    end

    def read(size)
      res = @called ? nil : 'a' * size * 4
      @called = true
      res
    end
  end

  def test_write_blob_from_io_overflow_io
    assert Rugged::Blob.from_io(@repo, OverflowIO.new)
  end

  class BadIO
    def read(length)
      :invalid_data
    end
  end

  def test_write_blob_from_io_bad_io
    assert_raises TypeError do
      Rugged::Blob.from_io(@repo, BadIO.new)
    end
  end
end

class BlobHashSignatureTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_rugged("testrepo.git")
  end

  LOREM = <<-LOREM
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Sed quis volutpat nunc. Vivamus consectetur pretium lacus, hendrerit dapibus justo rutrum at.
Fusce imperdiet volutpat ex, vel volutpat velit posuere ac.
Aenean ante neque, eleifend eu ligula sed, porttitor sagittis eros.
Vestibulum tincidunt pulvinar ante sit amet tristique.
Phasellus eu lacus in nunc pulvinar fringilla eu at felis.
Vestibulum lobortis ipsum eleifend tellus eleifend ultricies. Sed maximus ornare nunc vel consequat.
Praesent pharetra urna orci, nec pellentesque nulla viverra condimentum.
Donec ipsum sapien, eleifend gravida lectus vel, congue pulvinar enim.
Vestibulum pretium gravida velit sit amet consequat.
Sed sit amet est eu sapien lacinia porttitor eu eu lacus.
Fusce tempus est a nisi dignissim pulvinar.
Quisque maximus eleifend massa, non elementum massa convallis a.
LOREM

  def test_signature_from_blob
    blob = @repo.lookup("7771329dfa3002caf8c61a0ceb62a31d09023f37")

    sig1 = Rugged::Blob::HashSignature.new(blob)
    assert blob.similarity(sig1) == 100
    assert blob.similarity(LOREM) < 20

    sig1 = Rugged::Blob::HashSignature.new(LOREM)
    sig2 = Rugged::Blob::HashSignature.new(LOREM.gsub('ipsum', 'epsen'))
    assert Rugged::Blob::HashSignature.compare(sig1, sig2) > 75
  end
end
