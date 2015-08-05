unless defined?(JRUBY_VERSION)

  desc 'Display LOC (lines of code) report'
  task :loc do
    puts `countloc -r lib`
  end

  desc 'Display code quality analysis report'
  task :critic do
    sh 'rubycritic lib --path critic'
  end
end
