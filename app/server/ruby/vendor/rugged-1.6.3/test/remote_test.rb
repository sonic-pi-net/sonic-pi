require "test_helper"
require 'net/http'

class RemoteNetworkTest < Rugged::OnlineTestCase
  def setup
    @repo = FixtureRepo.from_rugged("testrepo.git")
  end

  def skip_if_no_test_url
    skip "no local git-daemon" unless ENV["GITTEST_REMOTE_GIT_RO_URL"]
  end

  def test_remote_network_connect
    skip_if_no_test_url
    remote = @repo.remotes.create_anonymous(ENV["GITTEST_REMOTE_GIT_RO_URL"])
    assert remote.ls.any?
  end

  def test_remote_check_connection_fetch
    skip_if_no_test_url
    remote = @repo.remotes.create_anonymous(ENV["GITTEST_REMOTE_GIT_RO_URL"])
    assert remote.check_connection(:fetch)
  end

  def test_remote_check_connection_push
    skip_if_no_test_url
    remote = @repo.remotes.create_anonymous(ENV["GITTEST_REMOTE_GIT_RO_URL"])
    assert !remote.check_connection(:push)
  end

  def test_remote_check_connection_push_credentials
    skip unless Rugged.features.include?(:ssh) && ssh_creds?

    url = ENV['GITTEST_REMOTE_SSH_URL']
    remote = @repo.remotes.create_anonymous(url)
    assert remote.check_connection(:push, credentials: ssh_key_credential)
  end

  def test_remote_check_connection_invalid
    skip_if_no_test_url
    remote = @repo.remotes.create_anonymous('git://github.com/libgit2/libgit2.git')
    assert_raises(TypeError) { remote.check_connection(:pull) }
  end
end

class RemoteTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2("testrepo.git")
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

  def test_push_url
    assert_equal 'git://github.com/libgit2/pushlibgit2',
      @repo.remotes['test_with_pushurl'].push_url

    assert_nil @repo.remotes['joshaber'].push_url
  end

  def test_fetch_refspecs
    remote = @repo.remotes['test']
    assert_equal ['+refs/heads/*:refs/remotes/test/*'], remote.fetch_refspecs

    assert_empty @repo.remotes['joshaber'].fetch_refspecs
  end

  def test_push_refspecs
    remote = @repo.remotes['test']
    assert_empty remote.push_refspecs
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

class RemotePushTest < Rugged::TestCase
  def setup
    @remote_repo = FixtureRepo.from_libgit2("testrepo.git")
    # We can only push to bare repos
    @remote_repo.config['core.bare'] = 'true'

    @repo = FixtureRepo.clone(@remote_repo)
    @repo.references.create("refs/heads/unit_test",
      "8496071c1b46c854b31185ea97743be6a8774479")

    @remote = @repo.remotes['origin']
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

    assert_equal "local push doesn't (yet) support pushing to non-bare repos.", exception.message
  end

  def test_push_non_forward_raise_error
    exception = assert_raises Rugged::ReferenceError do
      @remote.push(["refs/heads/unit_test:refs/heads/master"])
    end

    assert_equal "cannot push non-fastforwardable reference", exception.message
    assert_equal "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", @remote_repo.ref("refs/heads/master").target_id
  end

  def test_push_non_forward_forced_raise_no_error
    result = @remote.push(["+refs/heads/unit_test:refs/heads/master"])
    assert_equal({}, result)

    assert_equal "8496071c1b46c854b31185ea97743be6a8774479", @remote_repo.ref("refs/heads/master").target_id
  end

end

class RemotePruneTest < Rugged::TestCase
  def setup
    @remote_repo = FixtureRepo.from_libgit2("testrepo.git")
    # We can only push to bare repos
    @remote_repo.config['core.bare'] = 'true'

    @repo = FixtureRepo.clone(@remote_repo)
    @repo.references.create("refs/heads/unit_test", "8496071c1b46c854b31185ea97743be6a8774479")

    @remote = @repo.remotes['origin']

    @remote.push(["refs/heads/unit_test"])
    @remote_repo.references.delete("refs/heads/unit_test")
  end

  def test_fetch_prune_is_forced
    assert_equal "8496071c1b46c854b31185ea97743be6a8774479", @repo.ref("refs/remotes/origin/unit_test").target_id
    @remote.fetch(prune: true)
    assert_nil @repo.ref("refs/remotes/origin/unit_test")
  end

  def test_fetch_prune_is_not_forced
    @remote.fetch(prune: false)
    assert_equal "8496071c1b46c854b31185ea97743be6a8774479", @repo.ref("refs/remotes/origin/unit_test").target_id
  end

  def test_fetch_prune_nil
    @remote.fetch(prune: nil)
    assert_equal "8496071c1b46c854b31185ea97743be6a8774479", @repo.ref("refs/remotes/origin/unit_test").target_id
  end

  def test_fetch_prune_with_invalid_argument_raises
    assert_raises TypeError do
      @remote.fetch(prune: 'INVALID')
    end
  end
end

class RemoteWriteTest < Rugged::TestCase
  def setup
    @source_repo = FixtureRepo.from_rugged("testrepo.git")
    @repo = FixtureRepo.clone(@source_repo)
  end

  def test_remote_add
    @repo.remotes.create('upstream', 'git://github.com/libgit2/libgit2.git')
    remote = @repo.remotes['upstream']
    assert_equal 'upstream', remote.name
    assert_equal 'git://github.com/libgit2/libgit2.git', remote.url
  end

  def test_remote_add_with_invalid_url
    @repo.remotes.create('upstream', 'libgit2')
  end

  def test_remote_set_url
    remote = @repo.remotes['origin']

    old_url = remote.url
    new_url = 'git://github.com/l?#!@#$ibgit2/TestGitRepository.git'

    @repo.remotes.set_url(remote, new_url)

    assert_equal old_url, remote.url
    assert_equal new_url, @repo.remotes['origin'].url
  end

  def test_remote_set_push_url
    remote = @repo.remotes['origin']

    new_url = 'git://github.com/l?#!@#$ibgit2/TestGitRepository.git'

    @repo.remotes.set_push_url(remote, new_url)

    assert_nil   remote.push_url
    assert_equal new_url, @repo.remotes['origin'].push_url
  end

  def test_remote_add_fetch_refspech
    remote = @repo.remotes['origin']
    assert_nil @repo.remotes.add_fetch_refspec('origin', '+refs/pull/*/head:refs/remotes/origin/pr/*')

    assert_equal ['+refs/heads/*:refs/remotes/origin/*'], remote.fetch_refspecs
    assert_equal [
      '+refs/heads/*:refs/remotes/origin/*', '+refs/pull/*/head:refs/remotes/origin/pr/*'
    ], @repo.remotes['origin'].fetch_refspecs
  end

  def test_remote_add_push_refspec
    remote = @repo.remotes['origin']
    assert_nil @repo.remotes.add_push_refspec('origin', 'refs/heads/*:refs/heads/test/*')

    assert_equal [], remote.push_refspecs
    assert_equal [
      'refs/heads/*:refs/heads/test/*'
    ], @repo.remotes['origin'].push_refspecs
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
    @repo = FixtureRepo.empty
    repo_dir = File.join(TEST_DIR, (File.join('fixtures', 'testrepo.git', '.')))
    @remote = @repo.remotes.create('origin', repo_dir)
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
