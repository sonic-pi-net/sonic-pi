=begin
  locale/tag/posix.rb - Locale::Tag::Posix

  Copyright (C) 2008  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby.

  $Id: posix.rb 27 2008-12-03 15:06:50Z mutoh $
=end

module Locale 
  module Tag 

    # Locale tag class for POSIX locale
    # * ja
    # * ja_JP
    # * ja_JP.UTF-8
    # * ja_JP.UTF-8@Osaka
    # * C/POSIX (-> en_US)
    class Posix < Simple
      LANGUAGE = "([a-z]{2,})"
      TAG_RE = /\A#{LANGUAGE}(?:_#{REGION})?(?:\.([^@]+))?(?:@(.*))?\Z/i

      attr_reader :charset, :modifier

      def initialize(language, region = nil, charset = nil, modifier = nil)
        @charset, @modifier = charset, modifier
        super(language, region)
      end

      def self.parse(tag)
        if tag =~ /^(C|POSIX)$/
          ret = self.new("en", "US")
          ret.tag = tag
          ret
        elsif tag =~ TAG_RE
          ret = self.new($1, $2, $3, $4)
          ret.tag = tag
          ret
        else
          nil
        end
      end

      # Returns the language tag. 
      #   <language>_<COUNTRY>.<CHARSET>@<MODIFIER>
      #   (e.g.) "ja_JP.EUC-JP@Modifier"
      def to_s
        s = @language.dup
        s << "_#{@region}" if @region
        s << ".#{@charset}" if @charset
        s << "@#{@modifier}" if @modifier
        s
      end

      # Set the charset.
      def charset=(val)
        @charset = val
      end

      # Set the modifier as a String
      def modifier=(val)
        @modifier = val
      end

      # Returns an Array of tag-candidates order by priority.
      # Use Locale.candidates instead of this method.
      def candidates
        [self.class.new(language, region, charset, modifier),  #ja_JP.UTF-8@Modifier
         self.class.new(language, region, charset),            #ja_JP.UTF-8
         self.class.new(language, region, nil, modifier),      #ja_JP@Modifier
         self.class.new(language, region, nil, nil),           #ja_JP@Modifier
         self.class.new(language, nil, charset, modifier),     #ja.UTF-8@Modifier
         self.class.new(language, nil, charset),               #ja.UTF-8
         self.class.new(language, nil, nil, modifier),         #ja@Modifier
         self.class.new(language)]                             #ja
      end

      # A modifier is converted to a variant.
      # If the modifier is less than 5 characters, it is not canonical value.
      private
      def convert_to(klass)
        if klass == Simple
          super
        elsif klass == Posix
          klass.new(language, region, charset, modifier)
        else
          klass.new(language, nil, region, modifier ? [modifier] : [])
        end
      end

    end
  end
end
