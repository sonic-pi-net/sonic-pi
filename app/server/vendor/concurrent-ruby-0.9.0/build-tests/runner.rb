#!/usr/bin/env ruby

if File.exist?('Gemfile')
  puts <<-WARNING
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! A Gemfile has been detected. This will likely cause some tests   !!
!! to erroneously fail (RSpec + Bundler shenanigans!). You may need !!
!! to run tests from a different directory.                         !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  WARNING
end

$:.push File.join(File.dirname(__FILE__), '..', 'lib')

require 'concurrent/version'
require_relative 'platform_helpers'


EXT_PLATFORMS = {
  'i686-linux' => 'x86-linux',
  'x86_64-linux' => 'x86_64-linux',
  'i386-mingw32' => 'x86-mingw32',
  'x64-mingw32' => 'x64-mingw32',
  'i386-solaris2.11' => 'x86-solaris-2.11',
  'x86_64-darwin14.0' => 'x86_64-darwin-14',
}

TEST_PATH = File.dirname(__FILE__)
PKG_PATH = File.join(File.dirname(__FILE__), '..', 'pkg')
TEST_FILES = Dir["#{TEST_PATH}/*_spec.rb"]

RSPEC = "rspec --default-path #{TEST_PATH} -fd #{windows? ? '' : '--color'} --seed 0"

INSTALL_RSPEC_COMMAND = 'gem install rspec'

UNINSTALL_GEMS_COMMAND = <<-CMD
gem uninstall -q -a -I concurrent-ruby-ext && \
gem uninstall -q -a -I concurrent-ruby && \
gem uninstall -q -a -I ref
CMD

PLATFORM_BREAK = "######################################################################\n"
SUITE_BREAK    = "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

def platform_specific_extensions?(platform = RUBY_PLATFORM)
  EXT_PLATFORMS.keys.include?(platform) &&
    File.exists?("#{PKG_PATH}/#{extension_gem_name(platform)}")
end

def extension_gem_name(platform = RUBY_PLATFORM)
  platform = EXT_PLATFORMS.fetch(platform, '')
  platform = '-' + platform unless platform.empty?
  "concurrent-ruby-ext-#{Concurrent::VERSION}#{platform}.gem"
end

def install_gems_command(ext, platform = '')
  cmd = "gem install #{PKG_PATH}/concurrent-ruby-#{Concurrent::VERSION}.gem"
  if ext
    cmd << " && gem install #{PKG_PATH}/#{extension_gem_name(platform)}"
  end
  cmd
end

def install_java_gem_command
  "gem install #{PKG_PATH}/concurrent-ruby-#{Concurrent::VERSION}-java.gem"
end

def run_test_suite(files, ext, platform = '')

  test_platform = if ext
                    'EXT'
                  elsif jruby?
                    'JRUBY'
                  else
                    'RUBY'
                  end

  cmd = if jruby?
          install_java_gem_command
        else
          install_gems_command(ext, platform)
        end
  ok = system(cmd)

  files.each do |file|
    if windows?
      cmd = "set TEST_PLATFORM=#{test_platform} && #{RSPEC} #{file}"
    else
      cmd = "TEST_PLATFORM='#{test_platform}' #{RSPEC} #{file}"
    end
    ok = system(cmd)
  end

  ok = system(UNINSTALL_GEMS_COMMAND)
end

ok = system(INSTALL_RSPEC_COMMAND)
ok = system(UNINSTALL_GEMS_COMMAND)

puts PLATFORM_BREAK
puts RUBY_PLATFORM
puts SUITE_BREAK

run_test_suite(TEST_FILES, false)
if mri?
  if ! windows?
    puts SUITE_BREAK
    run_test_suite(TEST_FILES, true)
  end
  if platform_specific_extensions?(RUBY_PLATFORM)
    puts SUITE_BREAK
    run_test_suite(TEST_FILES, true, RUBY_PLATFORM)
  end
end
