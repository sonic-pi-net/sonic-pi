require_relative 'null_finder_test'
require_relative 'name_error_extension_test'
require_relative 'similar_name_finder_test'

if defined?(RUBY_ENGINE) && %w(ruby jruby).include?(RUBY_ENGINE)
  require_relative 'similar_method_finder_test'
  require_relative 'no_method_error_extension_test'
else
  puts "didn't run the tests for NoMethodError extension."
  puts "running tests for #{RUBY_ENGINE}." if defined?(RUBY_ENGINE)
end

if defined?(ActiveRecord)
  require_relative 'similar_attribute_finder_test'
end

require_relative 'similar_class_finder_test'
