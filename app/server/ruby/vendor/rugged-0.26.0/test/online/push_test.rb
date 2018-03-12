require 'test_helper'

class OnlineGitPushTest < Rugged::OnlineTestCase
  def setup
    @repo = FixtureRepo.from_libgit2("push_src")
    @remote = @repo.remotes.create("test", ENV['GITTEST_REMOTE_GIT_URL'])
    @target_repo = Rugged::Repository.new(ENV['GITTEST_REMOTE_REPO_PATH'])
  end

  if git_creds?
    def test_push_branches
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
end

if Rugged.features.include?(:ssh)
  class OnlineSshPushTest < Rugged::OnlineTestCase
    def setup
      @repo = FixtureRepo.from_libgit2("push_src")
      @remote = @repo.remotes.create("test", ENV['GITTEST_REMOTE_SSH_URL'])
      @target_repo = Rugged::Repository.new(ENV['GITTEST_REMOTE_REPO_PATH'])
    end

    if ssh_creds?
      def test_push_branches
        @remote.push([
          "refs/heads/b1:refs/heads/b1",
          "refs/heads/b2:refs/heads/b2",
          "refs/heads/b3:refs/heads/b3",
          "refs/heads/b4:refs/heads/b4",
          "refs/heads/b5:refs/heads/b5"
        ], {
          credentials: ssh_key_credential
        })

        assert_equal @repo.references["refs/heads/b1"].target_id, @target_repo.references["refs/heads/b1"].target_id
        assert_equal @repo.references["refs/heads/b2"].target_id, @target_repo.references["refs/heads/b2"].target_id
        assert_equal @repo.references["refs/heads/b3"].target_id, @target_repo.references["refs/heads/b3"].target_id
        assert_equal @repo.references["refs/heads/b4"].target_id, @target_repo.references["refs/heads/b4"].target_id
        assert_equal @repo.references["refs/heads/b5"].target_id, @target_repo.references["refs/heads/b5"].target_id
      end
    end
  end
end
