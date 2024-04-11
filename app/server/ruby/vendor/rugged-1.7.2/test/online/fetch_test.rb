require 'test_helper'

class OnlineFetchTest < Rugged::OnlineTestCase
  def setup
    @repo = FixtureRepo.empty
  end

  def test_fetch_over_git
    skip unless git_creds?
    @repo.remotes.create("origin", ENV['GITTEST_REMOTE_GIT_URL'])

    @repo.fetch("origin")
  end

  def test_fetch_over_https
    skip unless Rugged.features.include?(:https)

    @repo.remotes.create("origin", "https://github.com/libgit2/TestGitRepository.git")

    @repo.fetch("origin")

    assert_equal [
      "refs/remotes/origin/first-merge",
      "refs/remotes/origin/master",
      "refs/remotes/origin/no-parent",
      "refs/tags/annotated_tag",
      "refs/tags/blob",
      "refs/tags/commit_tree"
    ], @repo.refs.map(&:name).sort
  end

  def test_fetch_over_https_with_certificate_callback
    skip unless Rugged.features.include?(:https)

    @repo.remotes.create("origin", "https://github.com/libgit2/TestGitRepository.git")

    args = {}
    @repo.fetch(
      "origin",
      certificate_check: lambda { |valid, host|
        args[:valid] = valid
        args[:host] = host
        true
      }
    )

    assert_equal({ valid: true, host: "github.com" }, args)
  end

  def test_fetch_over_https_with_certificate_callback_fail
    skip unless Rugged.features.include?(:https)

    @repo.remotes.create("origin", "https://github.com/libgit2/TestGitRepository.git")

    exception = assert_raises Rugged::HTTPError do
      @repo.fetch(
        "origin",
        certificate_check: lambda { |valid, host| false }
      )
    end

    assert_equal "user rejected certificate for github.com", exception.message
  end

  def test_fetch_over_https_with_certificate_callback_exception
    skip unless Rugged.features.include?(:https)

    @repo.remotes.create("origin", "https://github.com/libgit2/TestGitRepository.git")

    exception = assert_raises RuntimeError do
      @repo.fetch(
        "origin",
        certificate_check: lambda { |valid, host|
          raise "Exception from callback"
        }
      )
    end

    assert_equal "Exception from callback", exception.message
  end

  def test_fetch_over_ssh_with_credentials
    skip unless Rugged.features.include?(:ssh) && ssh_creds?

    @repo.remotes.create("origin", ENV['GITTEST_REMOTE_SSH_URL'])

    @repo.fetch("origin", **{
                  credentials: ssh_key_credential
                })
  end

  def test_fetch_over_ssh_with_credentials_from_agent
    skip unless Rugged.features.include?(:ssh) && ssh_creds?

    @repo.remotes.create("origin", ENV['GITTEST_REMOTE_SSH_URL'])

    @repo.fetch("origin", **{
                  credentials: ssh_key_credential_from_agent
                })
  end

  def test_fetch_over_ssh_with_credentials_callback
    skip unless Rugged.features.include?(:ssh) && ssh_creds?

    @repo.remotes.create("origin", ENV['GITTEST_REMOTE_SSH_URL'])

    @repo.fetch("origin",
                credentials: lambda { |url, username, allowed_types|
                  return ssh_key_credential
                }
               )
  end
end
