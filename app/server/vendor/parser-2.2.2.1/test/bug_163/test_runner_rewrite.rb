require 'pathname'
require 'fileutils'
require 'shellwords'

BASE_DIR = Pathname.new(__FILE__) + '../..'
require (BASE_DIR + 'helper').expand_path

class TestRunnerRewrite < Minitest::Test
  def setup
    @ruby_rewrite = BASE_DIR.expand_path + '../bin/ruby-rewrite'
    @test_dir     = BASE_DIR + 'bug_163'
    @fixtures_dir = @test_dir + 'fixtures'
  end

  def test_rewriter
    Dir.mktmpdir("parser", BASE_DIR.expand_path.to_s) do |tmp_dir|
      tmp_dir = Pathname.new(tmp_dir)
      sample_file = tmp_dir + 'bug_163.rb'
      sample_file_expanded = sample_file.expand_path
      expected_file = @fixtures_dir + 'output.rb'

      FileUtils.cp(@fixtures_dir + 'input.rb', tmp_dir + 'bug_163.rb')
      FileUtils.cd @test_dir do
        exit_code = system %Q{
          #{Shellwords.escape(@ruby_rewrite.to_s)} --modify \
            -l rewriter.rb \
            #{Shellwords.escape(sample_file_expanded.to_s)}
        }
      end

      assert File.read(expected_file.expand_path) == File.read(sample_file),
        "#{sample_file} should be identical to #{expected_file}"
    end
  end
end
