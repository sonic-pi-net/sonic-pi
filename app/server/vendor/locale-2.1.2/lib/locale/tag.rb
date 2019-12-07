=begin
  tag.rb - Locale::Tag module

  Copyright (C) 2008,2009  Masao Mutoh
 
  You may redistribute it and/or modify it under the same
  license terms as Ruby.
=end

require 'locale/tag/simple'
require 'locale/tag/irregular'
require 'locale/tag/common'
require 'locale/tag/rfc'
require 'locale/tag/cldr'
require 'locale/tag/posix'

module Locale

  # Language tag / locale identifiers.
  module Tag
    module_function
    # Parse a language tag/locale name and return Locale::Tag
    # object.
    # * tag: a tag as a String. e.g.) ja-Hira-JP
    # * Returns: a Locale::Tag subclass.
    def parse(tag)
      # Common is not used here.
      [Simple, Common, Rfc, Cldr, Posix].each do |parser|
        ret = parser.parse(tag)
        return ret if ret
      end
      Locale::Tag::Irregular.new(tag)
    end
  end
end

