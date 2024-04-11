require 'tempfile'
require 'tmpdir'
require 'minitest/autorun'
require 'rugged'
require 'pp'

module Rugged
  class TestCase < Minitest::Test
    # Set up some isolation for our tests so we don't try to touch any
    # configuration from the user running the tests
    def before_setup
      @configdir ||= begin
        path = Dir.mktmpdir("rugged-config")
        Rugged::Settings['search_path_global'] = path
        Rugged::Settings['search_path_xdg'] = path
        Rugged::Settings['search_path_system'] = path
      end
      super
    end

    # Automatically clean up created fixture repos after each test run
    def after_teardown
      Rugged::TestCase::FixtureRepo.teardown
      FileUtils.remove_entry_secure(@configdir)
      super
    end

    module FixtureRepo
      # Create a new, empty repository.
      def self.empty(*args)
        path = Dir.mktmpdir("rugged-empty")
        ensure_cleanup(path)
        Rugged::Repository.init_at(path, *args)
      end

      # Create a repository based on a rugged fixture repo.
      def self.from_rugged(name, *args)
        path = Dir.mktmpdir("rugged-#{name}")
        ensure_cleanup(path)

        copy_fixture(path, name, File.join(TestCase::TEST_DIR, "fixtures", name, "."), *args)
      end

      # Create a repository based on a libgit2 fixture repo.
      def self.from_libgit2(name, *args)
        path = Dir.mktmpdir("rugged-libgit2-#{name}")
        ensure_cleanup(path)

        copy_fixture(path, name, File.join(TestCase::LIBGIT2_FIXTURE_DIR, name, "."), *args)
      end

      def self.copy_fixture(path, name, fixture_prefix, *args)
        repo_path = File.join(path, name)
        FileUtils.cp_r(fixture_prefix, repo_path)

        prepare(repo_path)

        Rugged::Repository.new(repo_path, *args).tap do |repo|
          rewrite_gitmodules(repo) unless repo.bare?
        end
      end

      # Create a repository cloned from another Rugged::Repository instance.
      def self.clone(repository)
        path = Dir.mktmpdir("rugged")
        ensure_cleanup(path)

        `git clone --quiet -- #{repository.path} #{path}`

        Rugged::Repository.new(path)
      end

      def self.prepare(path)
        Dir.chdir(path) do
          File.rename(".gitted", ".git") if File.exist?(".gitted")
          File.rename("gitattributes", ".gitattributes") if File.exist?("gitattributes")
          File.rename("gitignore", ".gitignore") if File.exist?("gitignore")
        end
      end

      # Rugged reuses libgit2 fixtures and needs the same setup code.
      #
      # This should be the same as the libgit2 fixture
      # setup in vendor/libgit2/tests/submodule/submodule_helpers.c
      def self.rewrite_gitmodules(repo)
        workdir = repo.workdir

        return unless File.exist?(File.join(workdir, 'gitmodules'))

        input_path = File.join(workdir, 'gitmodules')
        output_path = File.join(workdir, '.gitmodules')
        submodules = []

        File.open(input_path, 'r') do |input|
          File.open(output_path, 'w') do |output|
            input.each_line do |line|
              if %r{path = (?<submodule>.+$)} =~ line
                submodules << submodule.strip
              elsif %r{url = \.\.\/(?<url>.+$)} =~ line
                # Copy repositories pointed to by relative urls
                # and replace the relative url by the absolute path to the
                # copied repo.
                url.strip!
                path = Dir.mktmpdir(url)
                ensure_cleanup(path)
                FileUtils.cp_r(File.join(TestCase::LIBGIT2_FIXTURE_DIR, url, "."), path)

                line = "url = #{path}\n"
              end
              output.write(line)
            end
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

      # Delete temp directories that got created
      def self.teardown
        self.directories.each { |path| FileUtils.remove_entry_secure(path) }
        self.directories.clear
      end

      def self.directories
        @directories ||= []
      end

      # Registers the given +path+ to be deleted when #teardown is called.
      def self.ensure_cleanup(path)
        self.directories << path
      end
    end

    TEST_DIR = File.dirname(File.expand_path(__FILE__))
    LIBGIT2_FIXTURE_DIR = File.expand_path("../../vendor/libgit2/tests/resources", __FILE__)
  end

  class OnlineTestCase < TestCase
    if ENV['GITTEST_REMOTE_REPO_PATH']
      def before_setup
        remote_repo = Rugged::Repository.new(ENV['GITTEST_REMOTE_REPO_PATH'])
        remote_repo.references.each do |ref|
          remote_repo.references.delete(ref)
        end

        super
      end
    end

    def ssh_creds?
      %w{URL USER KEY PUBKEY PASSPHRASE}.all? { |key| ENV["GITTEST_REMOTE_SSH_#{key}"] }
    end

    def git_creds?
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
end
