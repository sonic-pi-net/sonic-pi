def bundle_check
  `bundle check` == "Resolving dependencies...\nThe Gemfile's dependencies are satisfied\n"
end

def execute(command)
  puts command
  system command
end

execute 'bundle install' unless bundle_check
execute "bundle exec ruby -w -I'lib:test' #{ARGV[0]}"
