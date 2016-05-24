=begin
  taglist.rb - Locale module

  Copyright (C) 2008  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby.

  $Id: taglist.rb 27 2008-12-03 15:06:50Z mutoh $
=end

module Locale
  # This provides the subclass of Array which behaves like 
  # the first(top priority) Locale::Tag object. 
  # "Locale.current.language" is same with "Locale.current[0].language".
  #
  # Locale.current returns an Array of Tag(s) now.
  # But the old Locale.current(Ruby-GetText) and Locale.get 
  # returns Locale::Object (similier with Locale::Tag::Posix). 
  # This is the class for backward compatibility.
  #
  # It is recommanded to use Locale.current[0] or 
  # Locale.candidates to find the current locale instead
  # of this function.
  #
  class TagList < Array 
    # Returns the top priority language. (simple)
    def language
      self[0].language
    end
    # Returns the top priority region/country. (simple)
    def country
      self[0].region
    end
    # Returns the top priority region/country. (simple)
    def region
      self[0].region
    end
    # Returns the top priority script. (common)
    def script
      self[0].script
    end
    # Returns the top priority charset. (posix)
    def charset
      top_priority_charset = nil
      first_tag = self[0]
      if first_tag.respond_to?(:charset)
        top_priority_charset = first_tag.charset
      end
      top_priority_charset ||= ::Locale.driver_module.charset
      top_priority_charset
    end

    # Returns the top priority modifier. (posix)
    def modifier
      (self[0].respond_to? :modifier) ? self[0].modifier : nil
    end

    # Returns the top priority variants.(common, rfc, cldr)
    def variants
      (self[0].respond_to? :variants) ? self[0].variants : nil
    end

    # Returns the top priority extensions.(common, rfc, cldr)
    def extensions
      (self[0].respond_to? :extensions) ? self[0].extensions : nil
    end

    # Returns the top priority privateuse(rfc)
    def privateuse
      (self[0].respond_to? :privateuse) ? self[0].privateuse : nil
    end

    def to_str
      self[0].to_str
    end

    def to_s
      self[0].to_s
    end
    
    def to_common
      self[0].to_common
    end

    def to_simple
      self[0].to_simple
    end

    def to_rfc
      self[0].to_rfc
    end

    def to_cldr
      self[0].to_cldr
    end

    def to_posix
      self[0].to_posix
    end
  end
end
