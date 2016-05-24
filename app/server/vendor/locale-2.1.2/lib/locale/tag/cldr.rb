=begin
  locale/tag/cldr.rb - Locale::Tag::CLDR

  Copyright (C) 2008,2009  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby.
=end

require 'locale/tag/common'

module Locale 
  module Tag 

    # Unicode locale identifier class for CLDR-1.6.1.
    # (Unicode Common Locale Data Repository).
    class Cldr < Common

      VARIANT = "(#{ALPHANUM}{5,8}|#{DIGIT}#{ALPHANUM}{3})" 
      EXTENSION = "#{ALPHANUM}+=[a-z0-9\-]+"

      TAG_RE = /\A#{LANGUAGE}(?:[-_]#{SCRIPT})?
                  (?:[-_]#{REGION})?((?:[-_]#{VARIANT})*
                  (?:@(#{EXTENSION};?)+)*)\Z/ix

      attr_reader :extensions

      class << self
        # Parse the language tag and return the new Locale::Tag::CLDR. 
        def parse(tag)
          if tag =~ /\APOSIX\Z/  # This is the special case of POSIX locale but match this regexp.
            nil
          elsif tag =~ TAG_RE
            lang, script, region, subtag = $1, $2, $3, $4
            
            extensions = {}
            subtag.scan(/#{EXTENSION}/i).each{|v| 
              subtag.sub!(v, "")
              key, type = v.split("=")
              extensions[key] = type
            }
            variants = subtag.scan(/#{VARIANT}/i).collect{|v| v[0].upcase}
            
            ret = self.new(lang, script, region, variants, extensions)
            ret.tag = tag
            ret
          else
            nil
          end
        end
      end

      # Create Locale::Tag::Cldr.
      #
      # variants should be upcase.
      def initialize(language, script = nil, region = nil, 
                     variants = [], extensions = {})
        @extensions = extensions
        super(language, script, region, variants.map{|v| v.upcase})
      end

      # Sets the extensions as an Hash.
      def extensions=(val)
        @extensions = val
      end

      private
      def convert_to(klass) # :nodoc:
        if klass == Cldr
          klass.new(language, script, region, variants, extensions)
        elsif klass == Rfc
          exts = []
          @extensions.to_a.sort.each do |k, v|
            exts << "k-#{k[0,8]}-#{v[0,8]}"
          end

          klass.new(language, script, region, variants, exts)
        else
          super
        end
      end

      # Returns the language tag. 
      # (e.g.) "ja_Hira_JP_VARIANT1_VARIANT2@foo1=var1;foo2=var2"
      #
      # This is used in internal only. Use to_s instead.
      def to_string
        s = super
        if @extensions.size > 0
          s << "@" << @extensions.to_a.sort.map{|k, v| "#{k}=#{v}"}.join(";")
        end
        s 
      end
    end
  end
end
