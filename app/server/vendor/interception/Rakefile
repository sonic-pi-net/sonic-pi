
task :clean do
  sh 'rm -f ext/*.o ext/*.so ext/*.dylib'
  sh 'rm -f ext/org/pryrepl/*.class'
end

desc "Compile *.c files"
task :compile => [:clean] do
  cd 'ext/' do
    sh 'ruby extconf.rb'
    sh 'make'
  end
end

desc "Run example"
task :example do
  sh "ruby -I./lib/ ./examples/example.rb "
end

desc "Run example 2"
task :example2 do
  sh "ruby -I./lib/ ./examples/example2.rb "
end

desc "Run tests"
task :test do
  sh 'rspec spec -r ./spec/spec_helpers.rb'
end

task :default => [:compile, :test]


