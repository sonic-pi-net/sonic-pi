require 'bundler/gem_tasks'
require 'bundler/setup'

task :default => :test

desc "Run test suite"
task :test do
  sh "bacon -Itest -a"
end

PAGES_REPO = 'git@github.com:whitequark/ast'

desc "Build and deploy documentation to GitHub pages"
task :pages do
  system "git clone #{PAGES_REPO} gh-temp/ -b gh-pages; rm gh-temp/* -rf; touch gh-temp/.nojekyll" or abort
  system "yardoc -o gh-temp/;" or abort
  system "cd gh-temp/; git add -A; git commit -m 'Updated pages.'; git push -f origin gh-pages" or abort
  FileUtils.rm_rf 'gh-temp'
end
