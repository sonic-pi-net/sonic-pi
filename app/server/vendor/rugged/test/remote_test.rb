require "test_helper"
require 'net/http'

class RemoteNetworkTest < Rugged::TestCase
  include Rugged::RepositoryAccess

  def skip_if_unreachable
    begin
      Net::HTTP.new('github.com').head('/')
    rescue SocketError => msg
      skip "github is not reachable: #{msg}"
    end
  end

  def test_remote_network_connect
    skip_if_unreachable
    remote = @repo.remotes.create_anonymous('git://github.com/libgit2/libgit2.git')
    assert remote.ls.any?
  end

  def test_remote_check_connection_fetch
    skip_if_unreachable
    remote = @repo.remotes.create_anonymous('git://github.com/libgit2/libgit2.git')
    assert remote.check_connection(:fetch)
  end

  def test_remote_check_connection_push
    skip_if_unreachable
    remote = @repo.remotes.create_anonymous('git://github.com/libgit2/libgit2.git')
    assert !remote.check_connection(:push)
  end

  def test_remote_check_connection_push_credentials
    skip_if_unreachable
    remote = @repo.remotes.create_anonymous('https://github.com/libgit2-push-test/libgit2-push-test.git')
    credentials = Rugged::Credentials::UserPassword.new(username: "libgit2-push-test", password: "123qwe123")
    assert remote.check_connection(:push, credentials: credentials)
  end

  def test_remote_check_connection_invalid
    skip_if_unreachable
    remote = @repo.remotes.create_anonymous('git://github.com/libgit2/libgit2.git')
    assert_raises(TypeError) { remote.check_connection(:pull) }
  end
end

class RemoteTest < Rugged::SandboxedTestCase
  def setup
    super
    @repo = sandbox_init("testrepo.git")
  end

  def teardown
    @repo.close
    super
  end

  class TestException < StandardError
  end

  def test_list_remote_names
    assert_equal ["empty-remote-pushurl", "empty-remote-url", "joshaber", "test", "test_with_pushurl"], @repo.remotes.each_name.sort
  end

  def test_list_remotes
    assert @repo.remotes.kind_of? Enumerable
    assert_equal ["empty-remote-pushurl", "empty-remote-url", "joshaber", "test", "test_with_pushurl"], @repo.remotes.map(&:name).sort
  end

  def test_remotes_each_protect
    assert_raises TestException do
      @repo.remotes.each do |remote|
        raise TestException
      end
    end
  end

  def test_remote_new_name
    remote = @repo.remotes.create_anonymous('git://github.com/libgit2/libgit2.git')
    assert_nil remote.name
    assert_equal 'git://github.com/libgit2/libgit2.git', remote.url
  end

  def test_remote_new_invalid_url
    @repo.remotes.create_anonymous('libgit2')
  end

  def test_remote_delete
    @repo.remotes.delete("test")
    assert_nil @repo.remotes["test"]
  end

  def test_url_set
    new_url = 'git://github.com/libgit2/TestGitRepository.git'
    remote = @repo.remotes.create_anonymous('git://github.com/libgit2/libgit2.git')
    remote.url = new_url
    assert_equal new_url, remote.url
  end

  def test_url_set_invalid
    remote = @repo.remotes.create_anonymous('git://github.com/libgit2/libgit2.git')
    remote.url = 'upstream'
  end

  def test_push_url
    assert_equal 'git://github.com/libgit2/pushlibgit2',
      @repo.remotes['test_with_pushurl'].push_url

    assert_nil @repo.remotes['joshaber'].push_url
  end

  def test_push_url_set
    new_url = 'git://github.com/libgit2/TestGitRepository.git'
    remote = @repo.remotes.create_anonymous('git://github.com/libgit2/libgit2.git')

    assert_nil remote.push_url
    remote.push_url = new_url
    assert_equal new_url, remote.push_url
  end

  def test_push_url_set_invalid
    remote = @repo.remotes.create_anonymous('git://github.com/libgit2/libgit2.git')
    remote.push_url = 'upstream'
  end

  def test_fetch_refspecs
    remote = @repo.remotes['test']
    assert_equal ['+refs/heads/*:refs/remotes/test/*'], remote.fetch_refspecs

    assert_empty @repo.remotes['joshaber'].fetch_refspecs
  end

  def test_push_refspecs
    remote = @repo.remotes['test']
    assert_empty remote.push_refspecs

    remote.add_push('refs/heads/*:refs/heads/testing/*')
    assert_equal ['refs/heads/*:refs/heads/testing/*'], remote.push_refspecs

    assert_empty @repo.remotes['joshaber'].push_refspecs
  end

  def test_add_fetch
    remote = @repo.remotes.create_anonymous('git://github.com/libgit2/libgit2.git')
    assert_nil remote.add_fetch('+refs/heads/*:refs/remotes/test/*')
    assert_equal ['+refs/heads/*:refs/remotes/test/*'], remote.fetch_refspecs
  end

  def test_add_push
    remote = @repo.remotes.create_anonymous('git://github.com/libgit2/libgit2.git')
    assert_nil remote.add_push('refs/heads/*:refs/heads/test/*')
    assert_equal ['refs/heads/*:refs/heads/test/*'], remote.push_refspecs
  end

  def test_clear_refspecs
    remote = @repo.remotes['test']

    remote.clear_refspecs

    assert_empty remote.push_refspecs
    assert_empty remote.fetch_refspecs
  end

  def test_remote_lookup
    remote = @repo.remotes['test']
    assert_equal 'git://github.com/libgit2/libgit2', remote.url
    assert_equal 'test', remote.name
  end

  def test_remote_lookup_missing
    assert_nil @repo.remotes['missing_remote']
  end

  def test_remote_lookup_invalid
    assert_raises Rugged::ConfigError do
      @repo.remotes["*\?"]
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

    @remote = @repo.remotes['origin']
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
    @repo.remotes.create('upstream', 'git://github.com/libgit2/libgit2.git')
    remote = @repo.remotes['upstream']
    assert_equal 'upstream', remote.name
    assert_equal 'git://github.com/libgit2/libgit2.git', remote.url
  end

  def test_remote_add_with_invalid_url
    @repo.remotes.create('upstream', 'libgit2')
  end

  def test_url_set
    new_url = 'git://github.com/l?#!@#$ibgit2/TestGitRepository.git'
    remote = @repo.remotes['origin']
    remote.url = new_url
    assert remote.save
    assert_equal new_url, @repo.remotes['origin'].url
  end

  def test_rename
    new_remote = @repo.remotes.rename('origin', 'new_remote_name') { }
    assert_equal new_remote.name, 'new_remote_name'
  end

  def test_rename_with_remote
    old_remote = @repo.remotes['origin']
    new_remote = @repo.remotes.rename(old_remote, 'new_remote_name') { }

    assert_equal new_remote.name, 'new_remote_name'
    assert_equal old_remote.name, 'origin'
  end

  def test_rename_invalid_name
    assert_raises Rugged::ConfigError do
      @repo.remotes.rename('origin', '/?') { }
    end
  end

  def test_rename_to_existing
    assert_raises Rugged::ConfigError do
      @repo.remotes.rename('origin', 'origin') { }
    end
  end

  def test_rename_error_callback
    @repo.config['remote.origin.fetch'] = '+refs/*:refs/*'

    problems = []
    @repo.remotes.rename('origin', 'test_remote') { |problem| problems << problem }
    assert_equal ["+refs/*:refs/*"], problems
  end
end

class RemoteTransportTest < Rugged::TestCase
  class TestException < StandardError
  end

  def setup
    @path = Dir.mktmpdir 'dir'
    @repo = Rugged::Repository.init_at(@path, false)
    repo_dir = File.join(TEST_DIR, (File.join('fixtures', 'testrepo.git', '.')))
    @remote = @repo.remotes.create('origin', repo_dir)
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
