=begin
  locale/tag/common.rb - Locale::Tag::Common

  Copyright (C) 2008,2009  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby.
=end

require 'locale/tag/simple'

module Locale
  module Tag
    # Common Language tag class for Ruby.
    # Java and MS Windows use this format. 
    #
    # * ja (language: RFC4646)
    # * ja_JP (country: RFC4646(2 alpha or 3 digit))
    # * ja-JP
    # * ja_Hira_JP (script: 4 characters)
    # * ja-Hira-JP
    # * ja_Hira_JP_MOBILE (variants: more than 2 characters or 3 digit)
    # * ja_Hira_JP_MOBILE_IPHONE (2 variants example)
    #
    class Common < Simple
      LANGUAGE = "(#{ALPHA}{2,3}|#{ALPHA}{4}|#{ALPHA}{5,8})" #RFC4646 (ISO639/reserved/registered)
      SCRIPT = "(#{ALPHA}{4})"
      VARIANT = "(#{ALPHANUM}{3,}|#{DIGIT}#{ALPHANUM}{3})"   #RFC3066 compatible

      TAG_RE = /\A#{LANGUAGE}(?:[-_]#{SCRIPT})?
                  (?:[-_]#{REGION})?((?:[-_]#{VARIANT})*)\Z/ix

      attr_reader :script, :variants

      class << self
        # Parse the language tag and return the new Locale::Tag::Common. 
        def parse(tag)
          if tag =~ /\APOSIX\Z/  # This is the special case of POSIX locale but match this regexp.
            nil
          elsif tag =~ TAG_RE
            lang, script, region, subtag = $1, $2, $3, $4
            variants = subtag.scan(/(^|[-_])#{VARIANT}(?=([-_]|$))/i).collect{|v| v[1]}
            
            ret = self.new(lang, script, region, variants)
            ret.tag = tag
            ret
          else
            nil
          end
        end
      end

      # Create a Locale::Tag::Common.
      def initialize(language, script = nil, region = nil, variants = [])
        @script, @variants = script, variants
        @script = @script.capitalize  if @script
        super(language, region)
      end
      
      # Set the script (with capitalize)
      def script=(val)
        @script = val
        @script = @script.capitalize if @script
        @script
      end

      # Set the variants as an Array.
      def variants=(val)
        @variants = val
      end

      # Returns an Array of tag-candidates order by priority.
      # Use Locale.candidates instead of this method.
      #
      # Locale::Tag::Rfc, Cldr don't have their own candidates,
      # because it's meaningless to compare the extensions, privateuse, etc.
      def candidates
        [self.class.new(language, script, region, variants),   #ja-Kana-JP-FOO
         self.class.new(language, script, region),             #ja-Kana-JP
         self.class.new(language, nil, region, variants),      #ja-JP-FOO
         self.class.new(language, nil, region),                #ja-JP
         self.class.new(language, script, nil, variants),      #ja-Kana-FOO
         self.class.new(language, script),                     #ja-Kana
         self.class.new(language, nil, nil, variants),         #ja-FOO
         self.class.new(language)]                             #ja
      end

      private
      def convert_to(klass)  #:nodoc:
        if klass == Simple
          super
        elsif klass == Posix
          if variants.size > 0 
            var = variants.join("-") 
          else
            var = nil
          end
          klass.new(language, region, nil, var)
        elsif klass == Cldr
          klass.new(language, script, region, variants.map{|v| v.upcase})
        else
          klass.new(language, script, region, variants)
        end
      end

      # Returns the common language tag with "_". 
      #   <language>_<Script>_<REGION>_VARIANTS1_VARIANTS2
      #   (e.g.) "ja_Hira_JP_VARIANTS1_VARIANTS2"
      #
      # This is used in internal only. Use to_s instead.
      def to_string
        s = @language.dup
    
        s << "_" << @script if @script
        s << "_" << @region if @region

        @variants.each do |v|
          s << "_#{v}"
        end
        s
      end
    end
  end
end
