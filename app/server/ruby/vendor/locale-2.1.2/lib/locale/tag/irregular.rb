=begin
  locale/tag/irregular.rb - Locale::Tag::Irregular

  Copyright (C) 2008  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby.

  $Id: irregular.rb 27 2008-12-03 15:06:50Z mutoh $
=end

require 'locale/tag/simple'

module Locale

  module Tag
    # Broken tag class.
    class Irregular < Simple

      def initialize(tag)
        tag = "en" if tag == nil or tag == ""
        super(tag.to_s)
        @tag = tag
      end

      # Returns an Array of tag-candidates order by priority.
      def candidates
        [Irregular.new(tag)]
      end
      
      # Conver to the klass(the class of Language::Tag)
      private
      def convert_to(klass)
        klass.new(tag)
      end
    end
  end
end
