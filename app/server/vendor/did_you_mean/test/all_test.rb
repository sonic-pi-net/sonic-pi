require_relative 'null_finder_test'
require_relative 'name_error_extension_test'
require_relative 'similar_name_finder_test'

begin
  require 'did_you_mean/method_missing'

  # only run the tests below when method_missing extension exists
  require_relative 'similar_method_finder_test'
  require_relative 'no_method_error_extension_test'
rescue LoadError => e
  puts "didn't run the tests for NoMethodError extension: #{e.to_s}"
  puts "running tests for #{RUBY_ENGINE}"
end

if defined?(ActiveRecord)
  require_relative 'similar_attribute_finder_test'
end

require_relative 'similar_class_finder_test'
