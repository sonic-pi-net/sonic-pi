require "test_helper"
require 'net/http'

class RemoteNetworkTest < Rugged::TestCase
  include Rugged::RepositoryAccess

  def test_remote_network_connect
    begin
      Net::HTTP.new('github.com').head('/')
    rescue SocketError => msg
      skip "github is not reachable: #{msg}"
    end

    remote = Rugged::Remote.new(@repo, 'git://github.com/libgit2/libgit2.git')
    assert remote.ls.any?
  end
end

class RemoteTest < Rugged::TestCase
  include Rugged::RepositoryAccess

  class TestException < StandardError
  end

  def test_list_remote_names
    remote_names = Rugged::Remote.names(@repo)
    assert_equal ["test_remote", "libgit2"].sort, remote_names.sort
  end

  def test_list_remotes
    remotes = @repo.remotes
    assert remotes.kind_of? Enumerable
    assert_equal ["test_remote", "libgit2"].sort, remotes.map(&:name).sort
  end

  def test_remotes_each_protect
    assert_raises TestException do
      @repo.remotes.each do |remote|
        raise TestException
      end
    end
  end

  def test_remote_new_name
    remote = Rugged::Remote.new(@repo, 'git://github.com/libgit2/libgit2.git')
    assert_nil remote.name
    assert_equal 'git://github.com/libgit2/libgit2.git', remote.url
  end

  def test_remote_new_invalid_url
    assert_raises ArgumentError do
      Rugged::Remote.new(@repo, 'libgit2')
    end
  end

  def test_url_set
    new_url = 'git://github.com/libgit2/TestGitRepository.git'
    remote = Rugged::Remote.new(@repo, 'git://github.com/libgit2/libgit2.git')
    remote.url = new_url
    assert_equal new_url, remote.url
  end

  def test_url_set_invalid
    url = 'upstream'
    remote = Rugged::Remote.new(@repo, 'git://github.com/libgit2/libgit2.git')
    assert_raises ArgumentError do
      remote.url = url
    end
  end

  def test_push_url
    assert_equal 'git://github.com/libgit2/TestEmptyRepository.git',
      Rugged::Remote.lookup(@repo, 'test_remote').push_url

    assert_nil Rugged::Remote.lookup(@repo, 'libgit2').push_url
  end

  def test_push_url_set
    new_url = 'git://github.com/libgit2/TestGitRepository.git'
    remote = Rugged::Remote.new(@repo, 'git://github.com/libgit2/libgit2.git')

    assert_nil remote.push_url
    remote.push_url = new_url
    assert_equal new_url, remote.push_url
  end

  def test_push_url_set_invalid
    new_url = 'upstream'
    remote = Rugged::Remote.new(@repo, 'git://github.com/libgit2/libgit2.git')
    assert_raises ArgumentError do
      remote.push_url = new_url
    end
  end

  def test_fetch_refspecs
    remote = Rugged::Remote.lookup(@repo, 'test_remote')
    assert_equal ['+refs/heads/*:refs/remotes/test_remote/*'], remote.fetch_refspecs

    assert_empty Rugged::Remote.lookup(@repo, 'libgit2').fetch_refspecs
  end

  def test_push_refspecs
    remote = Rugged::Remote.lookup(@repo, 'test_remote')
    assert_equal ['refs/heads/*:refs/heads/testing/*'], remote.push_refspecs

    assert_empty Rugged::Remote.lookup(@repo, 'libgit2').push_refspecs
  end

  def test_add_fetch
    remote = Rugged::Remote.new(@repo, 'git://github.com/libgit2/libgit2.git')
    assert_nil remote.add_fetch('+refs/heads/*:refs/remotes/test/*')
    assert_equal ['+refs/heads/*:refs/remotes/test/*'], remote.fetch_refspecs
  end

  def test_add_push
    remote = Rugged::Remote.new(@repo, 'git://github.com/libgit2/libgit2.git')
    assert_nil remote.add_push('refs/heads/*:refs/heads/test/*')
    assert_equal ['refs/heads/*:refs/heads/test/*'], remote.push_refspecs
  end

  def test_clear_refspecs
    remote = Rugged::Remote.lookup(@repo, 'test_remote')

    remote.clear_refspecs

    assert_empty remote.push_refspecs
    assert_empty remote.fetch_refspecs
  end

  def test_remote_lookup
    remote = Rugged::Remote.lookup(@repo, 'libgit2')
    assert_equal 'git://github.com/libgit2/libgit2.git', remote.url
    assert_equal 'libgit2', remote.name
  end

  def test_remote_lookup_missing
    assert_nil Rugged::Remote.lookup(@repo, 'missing_remote')
  end

  def test_remote_lookup_invalid
    assert_raises Rugged::ConfigError do
      Rugged::Remote.lookup(@repo, "*\?")
    end
  end
end

class RemotePushTest < Rugged::SandboxedTestCase
  def setup
    super
    @remote_repo = sandbox_init("testrepo.git")
    # We can only push to bare repos
    @remote_repo.config['core.bare'] = 'true'

    @repo = sandbox_clone("testrepo.git", "testrepo")
    @repo.references.create("refs/heads/unit_test",
      "8496071c1b46c854b31185ea97743be6a8774479")

    @remote = Rugged::Remote.lookup(@repo, 'origin')
  end

  def teardown
    @repo.close
    @remote_repo.close

    super
  end

  def test_push_single_ref
    result = @remote.push(["refs/heads/master", "refs/heads/master:refs/heads/foobar", "refs/heads/unit_test"])
    assert_equal({}, result)

    assert_equal "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", @remote_repo.ref("refs/heads/foobar").target_id
    assert_equal "8496071c1b46c854b31185ea97743be6a8774479", @remote_repo.ref("refs/heads/unit_test").target_id
  end

  def test_push_to_non_bare_raise_error
    @remote_repo.config['core.bare'] = 'false'

    exception = assert_raises Rugged::InvalidError do
      @remote.push(["refs/heads/master"])
    end

    assert_equal "Local push doesn't (yet) support pushing to non-bare repos.", exception.message
  end

  def test_push_non_forward_raise_error
    exception = assert_raises Rugged::ReferenceError do
      @remote.push(["refs/heads/unit_test:refs/heads/master"])
    end

    assert_equal "Cannot push non-fastforwardable reference", exception.message
    assert_equal "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", @remote_repo.ref("refs/heads/master").target_id
  end

  def test_push_non_forward_forced_raise_no_error
    result = @remote.push(["+refs/heads/unit_test:refs/heads/master"])
    assert_equal({}, result)

    assert_equal "8496071c1b46c854b31185ea97743be6a8774479", @remote_repo.ref("refs/heads/master").target_id
  end
end

class RemoteWriteTest < Rugged::TestCase
  include Rugged::TempRepositoryAccess

  def test_remote_add
    Rugged::Remote.add(@repo, 'upstream', 'git://github.com/libgit2/libgit2.git')
    remote = Rugged::Remote.lookup(@repo, 'upstream')
    assert_equal 'upstream', remote.name
    assert_equal 'git://github.com/libgit2/libgit2.git', remote.url
  end

  def test_remote_add_with_invalid_url
    assert_raises ArgumentError do
      Rugged::Remote.add(@repo, 'upstream', 'libgit2')
    end
  end

  def test_url_set
    new_url = 'git://github.com/l?#!@#$ibgit2/TestGitRepository.git'
    remote = Rugged::Remote.lookup(@repo, 'origin')
    remote.url = new_url
    assert remote.save
    assert_equal new_url, Rugged::Remote.lookup(@repo, 'origin').url
  end

  def test_rename
    remote = Rugged::Remote.lookup(@repo, 'origin')
    assert_nil remote.rename!('new_remote_name')
    assert Rugged::Remote.lookup(@repo, 'new_remote_name')
  end

  def test_rename_invalid_name
    remote = Rugged::Remote.lookup(@repo, 'origin')
    assert_raises Rugged::ConfigError do
      remote.rename!('/?')
    end
  end

  def test_rename_exists
    remote = Rugged::Remote.lookup(@repo, 'origin')
    assert_raises Rugged::ConfigError do
      remote.rename!('origin')
    end
  end

  def test_rename_error_callback
    @repo.config['remote.origin.fetch']  = '+refs/*:refs/*'
    remote = Rugged::Remote.lookup(@repo, 'origin')
    assert_equal ["+refs/*:refs/*"], remote.rename!('test_remote')
  end
end

class RemoteTransportTest < Rugged::TestCase
  class TestException < StandardError
  end

  def setup
    @path = Dir.mktmpdir 'dir'
    @repo = Rugged::Repository.init_at(@path, false)
    repo_dir = File.join(TEST_DIR, (File.join('fixtures', 'testrepo.git', '.')))
    @remote = Rugged::Remote.add(@repo, 'origin', repo_dir)
  end

  def teardown
    @repo.close
    FileUtils.remove_entry_secure(@path)
  end

  def test_remote_ls
    assert @remote.ls.kind_of? Enumerable
    rheads = @remote.ls.to_a

    assert_equal 7, rheads.count

    rhead = rheads.first
    assert_equal false, rhead[:local?]
    assert rhead[:oid]
    assert_nil rhead[:loid]
  end

  def test_remote_fetch
    assert_equal({
      total_objects: 19,
      indexed_objects: 19,
      received_objects: 19,
      local_objects: 0,
      total_deltas: 2,
      indexed_deltas: 2,
      received_bytes: 1563
    }, @remote.fetch)

    assert_equal '36060c58702ed4c2a40832c51758d5344201d89a', @repo.branches['origin/master'].target_id
    assert_equal '41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9', @repo.branches["origin/packed"].target_id
    assert_equal '5b5b025afb0b4c913b4c338a42934a3863bf3644', @repo.tags["v0.9"].target_id
    assert_equal '0c37a5391bbff43c37f0d0371823a5509eed5b1d', @repo.tags["v1.0"].target_id
  end

  def test_update_tips_callback
    @remote.fetch update_tips: lambda { |ref, source, destination|
      assert @repo.references[ref]
      assert_nil source
      assert destination
    }

    assert_equal '36060c58702ed4c2a40832c51758d5344201d89a', @repo.branches['origin/master'].target_id
    assert_equal '41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9', @repo.branches["origin/packed"].target_id
    assert_equal '5b5b025afb0b4c913b4c338a42934a3863bf3644', @repo.tags["v0.9"].target_id
    assert_equal '0c37a5391bbff43c37f0d0371823a5509eed5b1d', @repo.tags["v1.0"].target_id
  end

  def test_update_tips_callback_error
    assert_raises TestException do
      @remote.fetch update_tips: lambda { |*args|
        raise TestException
      }
    end

    # In case of an error inside the callback, all further tip updates get cancelled
    assert_equal '36060c58702ed4c2a40832c51758d5344201d89a', @repo.branches["origin/master"].target_id
    refute @repo.branches["origin/packed"]
    refute @repo.tags["v0.9"]
    refute @repo.tags["v1.0"]
  end

  def test_transfer_progress_callback
    total_objects = indexed_objects = received_objects = local_objects = total_deltas = indexed_deltas = received_bytes = nil
    callsback = 0

    @remote.fetch transfer_progress: lambda { |*args|
      total_objects, indexed_objects, received_objects, local_objects, total_deltas, indexed_deltas, received_bytes = args
      callsback += 1
    }

    assert_equal 22, callsback
    assert_equal 19, total_objects
    assert_equal 19, indexed_objects
    assert_equal 19, received_objects
    assert_equal 0, local_objects
    assert_equal 2, total_deltas
    assert_equal 2, indexed_deltas
    assert_equal 1563, received_bytes
  end
end
