require 'test_helper'

class OnlineLsTest < Rugged::OnlineTestCase
  def setup
    @repo = FixtureRepo.from_libgit2("push_src")
  end

  def test_ls_over_https
    skip unless Rugged.features.include?(:https)

    remote = @repo.remotes.create("origin", "https://github.com/libgit2/TestGitRepository.git")

    assert_equal [
      { :local? => false, :oid => "49322bb17d3acc9146f98c97d078513228bbf3c0", :loid => nil, :name => "HEAD" },
      { :local? => false, :oid => "0966a434eb1a025db6b71485ab63a3bfbea520b6", :loid => nil, :name => "refs/heads/first-merge" },
      { :local? => false, :oid => "49322bb17d3acc9146f98c97d078513228bbf3c0", :loid => nil, :name => "refs/heads/master" },
      { :local? => false, :oid => "42e4e7c5e507e113ebbb7801b16b52cf867b7ce1", :loid => nil, :name => "refs/heads/no-parent" },
      { :local? => false, :oid => "d96c4e80345534eccee5ac7b07fc7603b56124cb", :loid => nil, :name => "refs/tags/annotated_tag" },
      { :local? => false, :oid => "c070ad8c08840c8116da865b2d65593a6bb9cd2a", :loid => nil, :name => "refs/tags/annotated_tag^{}" },
      { :local? => false, :oid => "55a1a760df4b86a02094a904dfa511deb5655905", :loid => nil, :name => "refs/tags/blob" },
      { :local? => false, :oid => "8f50ba15d49353813cc6e20298002c0d17b0a9ee", :loid => nil, :name => "refs/tags/commit_tree" },
      { :local? => false, :oid => "6e0c7bdb9b4ed93212491ee778ca1c65047cab4e", :loid => nil, :name => "refs/tags/nearly-dangling"}
    ], remote.ls.to_a
  end

  def test_ls_over_git
    skip unless git_creds?

    remote = @repo.remotes.create("origin", ENV['GITTEST_REMOTE_GIT_URL'])
    remote.push(["refs/heads/b1:refs/heads/b1"])

    assert_equal [
      { :local? => false, :oid => "a78705c3b2725f931d3ee05348d83cc26700f247", :loid => nil, :name => "refs/heads/b1" }
    ], remote.ls.to_a
  end

  def test_ls_over_ssh_with_credentials
    skip unless Rugged.features.include?(:ssh) && ssh_creds?

    remote = @repo.remotes.create("origin", ENV['GITTEST_REMOTE_SSH_URL'])
    remote.push(["refs/heads/b1:refs/heads/b1"], credentials: ssh_key_credential)

    assert_equal [
      { :local? => false, :oid => "a78705c3b2725f931d3ee05348d83cc26700f247", :loid => nil, :name => "refs/heads/b1" }
    ], remote.ls(credentials: ssh_key_credential).to_a
  end
end
