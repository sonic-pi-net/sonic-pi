# encoding: UTF-8

# Top level module for TZInfo.
module TZInfo
  # Top level module for TZInfo::Data.
  module Data
    location = File.dirname(File.dirname(__FILE__))

    if location.respond_to?(:untaint) && RUBY_VERSION =~ /\A(\d+)\.(\d+)(?:\.|\z)/ && ($1 == '2' && $2.to_i < 7 || $1.to_i <= 1)
      location.untaint
    end

    # The directory containing the TZInfo::Data files.
    LOCATION = location.freeze
  end
end

require_relative 'data/version'
