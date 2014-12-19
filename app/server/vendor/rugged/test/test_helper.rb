require 'tempfile'
require 'tmpdir'
require 'minitest/autorun'
require 'rugged'
require 'pp'

module Rugged
  class TestCase < MiniTest::Unit::TestCase
    TEST_DIR = File.dirname(File.expand_path(__FILE__))
    LIBGIT2_FIXTURE_DIR = File.expand_path("../../vendor/libgit2/tests/resources", __FILE__)

    protected
    def with_default_encoding(encoding, &block)
      old_encoding = Encoding.default_internal

      new_encoding = Encoding.find(encoding)
      Encoding.default_internal = new_encoding

      yield new_encoding

      Encoding.default_internal = old_encoding
    end
  end

  class SandboxedTestCase < TestCase
    def setup
      super
      @_sandbox_path = Dir.mktmpdir("rugged_sandbox")
    end

    def teardown
      FileUtils.remove_entry_secure @_sandbox_path
      super
    end

    def sandbox_fixture(repository)
      FileUtils.cp_r(File.join(TestCase::LIBGIT2_FIXTURE_DIR, repository), @_sandbox_path)
    end

    # Fills the current sandbox folder with the files
    # found in the given repository
    def sandbox_init(repository)
      sandbox_fixture(repository)

      fixture_repo_path = File.join(@_sandbox_path, repository)
      Dir.chdir(fixture_repo_path) do
        File.rename(".gitted", ".git") if File.exist?(".gitted")
        File.rename("gitattributes", ".gitattributes") if File.exist?("gitattributes")
        File.rename("gitignore", ".gitignore") if File.exist?("gitignore")
      end

      Rugged::Repository.new(fixture_repo_path)
    end

    def sandbox_clone(repository, name)
      Dir.chdir(@_sandbox_path) do
        `git clone --quiet -- #{repository} #{name}`
      end

      Rugged::Repository.new(File.join(@_sandbox_path, name))
    end
  end

  class OnlineTestCase < SandboxedTestCase
    def reset_remote_repo
      remote_repo = Rugged::Repository.new(ENV['GITTEST_REMOTE_REPO_PATH'])
      remote_repo.references.each do |ref|
        remote_repo.references.delete(ref)
      end
      remote_repo.close
    end

    def self.ssh_creds?
      %w{URL USER KEY PUBKEY PASSPHRASE}.all? { |key| ENV["GITTEST_REMOTE_SSH_#{key}"] }
    end

    def self.git_creds?
      ENV['GITTEST_REMOTE_GIT_URL']
    end

    def ssh_key_credential
      Rugged::Credentials::SshKey.new({
        username:   ENV["GITTEST_REMOTE_SSH_USER"],
        publickey:  ENV["GITTEST_REMOTE_SSH_PUBKEY"],
        privatekey: ENV["GITTEST_REMOTE_SSH_KEY"],
        passphrase: ENV["GITTEST_REMOTE_SSH_PASSPHASE"],
      })
    end

    def ssh_key_credential_from_agent
      Rugged::Credentials::SshKeyFromAgent.new({
        username: ENV["GITTEST_REMOTE_SSH_USER"]
      })
    end
  end

  # Rugged reuses libgit2 fixtures and needs the same setup code.
  #
  # This should be the same as the libgit2 fixture
  # setup in vendor/libgit2/tests/submodule/submodule_helpers.c
  class SubmoduleTestCase < Rugged::SandboxedTestCase
    def setup
      super
    end

    def setup_submodule
      repository = sandbox_init('submod2')
      sandbox_fixture('submod2_target')

      Dir.chdir(@_sandbox_path) do
        File.rename(
          File.join('submod2_target', '.gitted'),
          File.join('submod2_target', '.git')
        )

        rewrite_gitmodules(repository.workdir)

        File.rename(
          File.join('submod2', 'not-submodule', '.gitted'),
          File.join('submod2', 'not-submodule', '.git')
        )

        File.rename(
          File.join('submod2', 'not', '.gitted'),
          File.join('submod2', 'not', '.git')
        )
      end

      repository
    end

    def rewrite_gitmodules(workdir)
      input_path = File.join(workdir, 'gitmodules')
      output_path = File.join(workdir, '.gitmodules')
      submodules = []

      File.open(input_path, 'r') do |input|
        File.open(output_path, 'w') do |output|
          input.each_line do |line|
            if %r{path = (?<submodule>.+$)} =~ line
              submodules << submodule.strip
            # convert relative URLs in "url =" lines
            elsif %r{url = (?<url>.+$)} =~ line
              line = "url = #{File.expand_path(File.join(workdir, url))}\n"
            end
            output.write(line)
          end
        end
        FileUtils.remove_entry_secure(input_path)

        # rename .gitted -> .git in submodule dirs
        submodules.each do |submodule|
          submodule_path = File.join(workdir, submodule)
          if File.exist?(File.join(submodule_path, '.gitted'))
            Dir.chdir(submodule_path) do
              File.rename('.gitted', '.git')
            end
          end
        end
      end
    end
  end

  module RepositoryAccess
    def setup
      @path = File.dirname(__FILE__) + '/fixtures/testrepo.git/'
      @repo = Rugged::Repository.new(@path)
    end
  end

  module TempRepositoryAccess
    def setup
      @path = temp_repo("testrepo.git")
      @repo = Rugged::Repository.new(@path)
    end

    def teardown
      @repo.close
      GC.start
      destroy_temp_repo(@path)
    end

    protected
    def temp_repo(repo)
      dir = Dir.mktmpdir 'dir'
      repo_dir = File.join(TestCase::TEST_DIR, (File.join('fixtures', repo, '.')))
      `git clone --quiet -- #{repo_dir} #{dir}`
      dir
    end

    def destroy_temp_repo(path)
      FileUtils.remove_entry_secure(path)
    end
  end
end
