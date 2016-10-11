#
# Enforce the alphabetical execution of specs because rspec 2+ executes them
# randomly with `rspec spec` while we need them in an accurate order for coverage
# reports that include the spec files.
#
# This is due to the fact that coverage will not include the first loaded spec/test file.
# To get predictable coverage results, we need to know which one that is...
#
Transform "bundle exec rspec spec" do |_|
  files = nil # Avoid shadowing
  in_current_directory { files = Dir["spec/**/*_spec.rb"] }
  "bundle exec rspec #{files.sort.join(' ')}"
end
