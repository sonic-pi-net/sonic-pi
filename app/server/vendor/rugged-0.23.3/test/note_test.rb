require 'test_helper'

class NoteTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_rugged("testrepo.git")
  end

  def test_read_note_for_object
    oid = "36060c58702ed4c2a40832c51758d5344201d89a"
    obj = @repo.lookup(oid)
    notes = obj.notes
    assert_equal "note text\n", notes[:message]
    assert_equal "94eca2de348d5f672faf56b0decafa5937e3235e", notes[:oid]
  end

  def test_read_note_for_object_from_ref
    oid = "36060c58702ed4c2a40832c51758d5344201d89a"
    obj = @repo.lookup(oid)
    notes = obj.notes('refs/notes/commits')
    assert_equal "note text\n", notes[:message]
    assert_equal "94eca2de348d5f672faf56b0decafa5937e3235e", notes[:oid]
  end

  def test_object_without_note
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    obj = @repo.lookup(oid)
    assert_nil obj.notes
  end

  def test_nil_ref_lookup
    oid = "36060c58702ed4c2a40832c51758d5344201d89a"
    obj = @repo.lookup(oid)
    assert_nil obj.notes('refs/notes/missing')
  end

  def test_iterate_over_notes
    @repo.each_note('refs/notes/commits') do |note_blob, annotated_object|
      assert_equal "note text\n", note_blob.content
      assert_equal "36060c58702ed4c2a40832c51758d5344201d89a", annotated_object.oid
    end
  end

  def test_each_note_enumerable
    enum = @repo.each_note('refs/notes/commits')
    assert enum.kind_of? Enumerable
  end

  def test_default_ref
    assert_equal 'refs/notes/commits', @repo.default_notes_ref
  end
end

class NoteWriteTest < Rugged::TestCase
  def setup
    @source_repo = FixtureRepo.from_rugged("testrepo.git")
    @repo = FixtureRepo.clone(@source_repo)
  end

  def test_create_note
    person = {:name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }

    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    message ="This is the note message\n\nThis note is created from Rugged"
    obj = @repo.lookup(oid)

    note_oid = obj.create_note(
      :message => message,
      :committer => person,
      :author => person,
      :ref => 'refs/notes/test'
    )

    assert_equal '38c3a690c474d8dcdb13088205a464a60312eec4', note_oid
    # note is actually a blob
    blob = @repo.lookup(note_oid)
    assert_equal blob.oid, note_oid
    assert_equal blob.content, message
    assert_equal blob.type, :blob

    note = obj.notes('refs/notes/test')
    assert_equal note[:oid], note_oid
    assert_equal note[:message], message
  end

  def test_create_note_without_signature
    name = 'Rugged User'
    email = 'rugged@example.com'
    @repo.config['user.name'] = name
    @repo.config['user.email'] = email

    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    message ="This is the note message\n\nThis note is created from Rugged"
    obj = @repo.lookup(oid)

    note_oid = obj.create_note(
      :message => message,
      :ref => 'refs/notes/test'
    )
    assert_equal '38c3a690c474d8dcdb13088205a464a60312eec4', note_oid
    note_commit = @repo.references['refs/notes/test'].target
    assert_equal name, note_commit.committer[:name]
    assert_equal email, note_commit.committer[:email]
    assert_equal name, note_commit.author[:name]
    assert_equal email, note_commit.author[:email]
  end

  def test_create_note_on_object_with_notes_raises
    person = {:name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    message ="This is the note message\n\nThis note is created from Rugged"
    obj = @repo.lookup(oid)
    obj.create_note(
      :message => message,
      :committer => person,
      :author => person,
      :ref => 'refs/notes/test'
    )

    assert_raises Rugged::RepositoryError do
      obj.create_note(
        :message => message,
        :committer => person,
        :author => person,
        :ref => 'refs/notes/test'
      )
    end
  end

  def test_overwrite_object_note
    person = {:name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }
    oid = "8496071c1b46c854b31185ea97743be6a8774479"
    message ="This is the note message\n\nThis note is created from Rugged"
    obj = @repo.lookup(oid)
    obj.create_note(
      :message => message,
      :committer => person,
      :author => person,
      :ref => 'refs/notes/test'
    )

    obj.create_note(
      :message => 'new message',
      :committer => person,
      :author => person,
      :ref => 'refs/notes/test',
      :force => true
    )

    note = obj.notes('refs/notes/test')
    assert_equal note[:message], 'new message'
  end

  def test_remove_note
    oid = "36060c58702ed4c2a40832c51758d5344201d89a"
    person = {:name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }
    message ="This is the note message\n\nThis note is created from Rugged"
    obj = @repo.lookup(oid)

    obj.create_note(
      :message => message,
      :committer => person,
      :author => person,
      :ref => 'refs/notes/test'
    )

    assert obj.remove_note(
      :committer => person,
      :author => person,
      :ref => 'refs/notes/test'
    )

    assert_nil obj.notes('refs/notes/test')
  end

  def test_remote_without_signature
    name = 'Rugged User'
    email = 'rugged@example.com'
    @repo.config['user.name'] = name
    @repo.config['user.email'] = email
    oid = "36060c58702ed4c2a40832c51758d5344201d89a"

    message ="This is the note message\n\nThis note is created from Rugged"
    obj = @repo.lookup(oid)

    obj.create_note(
      :message => message,
      :ref => 'refs/notes/test'
    )

    obj.create_note(
      :message => message,
    )

    assert obj.remove_note(:ref => 'refs/notes/test')
    assert obj.remove_note

    assert_nil obj.notes('refs/notes/test')
    assert_nil obj.notes
  end

  def test_remove_missing_note
    person = {:name => 'Scott', :email => 'schacon@gmail.com', :time => Time.now }
    oid = "36060c58702ed4c2a40832c51758d5344201d89a"
    obj = @repo.lookup(oid)
    refute obj.remove_note(
      :committer => person,
      :author => person,
      :ref => 'refs/notes/test'
    )
  end
end
