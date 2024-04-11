require 'test_helper'

class OnlineGitPushTest < Rugged::OnlineTestCase
  def setup
    skip unless git_creds?
    @repo = FixtureRepo.from_libgit2("push_src")
    @remote = @repo.remotes.create("test", ENV['GITTEST_REMOTE_GIT_URL'])
    @target_repo = Rugged::Repository.new(ENV['GITTEST_REMOTE_REPO_PATH'])
  end

  def test_push_branches
    skip unless git_creds?

    @remote.push([
                   "refs/heads/b1:refs/heads/b1",
                   "refs/heads/b2:refs/heads/b2",
                   "refs/heads/b3:refs/heads/b3",
                   "refs/heads/b4:refs/heads/b4",
                   "refs/heads/b5:refs/heads/b5"
                 ])

    assert_equal @repo.references["refs/heads/b1"].target_id, @target_repo.references["refs/heads/b1"].target_id
    assert_equal @repo.references["refs/heads/b2"].target_id, @target_repo.references["refs/heads/b2"].target_id
    assert_equal @repo.references["refs/heads/b3"].target_id, @target_repo.references["refs/heads/b3"].target_id
    assert_equal @repo.references["refs/heads/b4"].target_id, @target_repo.references["refs/heads/b4"].target_id
    assert_equal @repo.references["refs/heads/b5"].target_id, @target_repo.references["refs/heads/b5"].target_id
  end
end

class OnlineSshPushTest < Rugged::OnlineTestCase
  def setup
    skip unless Rugged.features.include?(:ssh) && ssh_creds?

    @repo = FixtureRepo.from_libgit2("push_src")
    @remote = @repo.remotes.create("test", ENV['GITTEST_REMOTE_SSH_URL'])
    @target_repo = Rugged::Repository.new(ENV['GITTEST_REMOTE_REPO_PATH'])
  end

  def test_push_branches
    skip unless ssh_creds?

    @remote.push([
                   "refs/heads/b1:refs/heads/b1",
                   "refs/heads/b2:refs/heads/b2",
                   "refs/heads/b3:refs/heads/b3",
                   "refs/heads/b4:refs/heads/b4",
                   "refs/heads/b5:refs/heads/b5"
                 ], **{
                   credentials: ssh_key_credential
                 })

    assert_equal @repo.references["refs/heads/b1"].target_id, @target_repo.references["refs/heads/b1"].target_id
    assert_equal @repo.references["refs/heads/b2"].target_id, @target_repo.references["refs/heads/b2"].target_id
    assert_equal @repo.references["refs/heads/b3"].target_id, @target_repo.references["refs/heads/b3"].target_id
    assert_equal @repo.references["refs/heads/b4"].target_id, @target_repo.references["refs/heads/b4"].target_id
    assert_equal @repo.references["refs/heads/b5"].target_id, @target_repo.references["refs/heads/b5"].target_id
  end
end
