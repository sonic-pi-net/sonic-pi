=begin
  locale/tag/rfc.rb - Locale::Tag::Rfc

  Copyright (C) 2008,2009  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby.
=end

require 'locale/tag/common'

module Locale 
  module Tag 

    # Language tag class for RFC4646(BCP47).
    class Rfc < Common
      SINGLETON = '[a-wyz0-9]'
      VARIANT = "(#{ALPHANUM}{5,8}|#{DIGIT}#{ALPHANUM}{3})" 
      EXTENSION = "(#{SINGLETON}(?:-#{ALPHANUM}{2,8})+)"
      PRIVATEUSE = "(x(?:-#{ALPHANUM}{1,8})+)"
      GRANDFATHERED = "#{ALPHA}{1,3}(?:-#{ALPHANUM}{2,8}){1,2}"
      
      TAG_RE = /\A#{LANGUAGE}(?:-#{SCRIPT})?
                  (?:-#{REGION})?((?:-#{VARIANT})*
                  (?:-#{EXTENSION})*(?:-#{PRIVATEUSE})?)\Z/ix

      attr_reader :extensions, :privateuse

      class << self
        # Parse the language tag and return the new Locale::Tag::Rfc. 
        def parse(tag)
          if tag =~ /\APOSIX\Z/  # This is the special case of POSIX locale but match this regexp.
            nil
          elsif tag =~ TAG_RE
            lang, script, region, subtag = $1, $2, $3, $4
            extensions = []
            variants = []
            if subtag =~ /#{PRIVATEUSE}/
                subtag, privateuse = $`, $1
              # Private use for CLDR.
              if /x-ldml(.*)/ =~ privateuse
                p_subtag = $1 
                extensions = p_subtag.scan(/(^|-)#{EXTENSION}/i).collect{|v| p_subtag.sub!(v[1], ""); v[1]}
                variants = p_subtag.scan(/(^|-)#{VARIANT}(?=(-|$))/i).collect{|v| v[1]}
              end
            end
            extensions += subtag.scan(/(^|-)#{EXTENSION}/i).collect{|v| subtag.sub!(v[1], ""); v[1]}
            variants += subtag.scan(/(^|-)#{VARIANT}(?=(-|$))/i).collect{|v| v[1]}
            
            ret = self.new(lang, script, region, variants, extensions, privateuse)
            ret.tag = tag
            ret
          else
            nil
          end
        end
      end

      def initialize(language, script = nil, region = nil, variants = [],
                   extensions = [], privateuse = nil)
        @extensions, @privateuse = extensions, privateuse
        super(language, script, region, variants)
      end

      # Sets the extensions as an Array.
      def extensions=(val)
        @extensions = val
      end

      # Sets the privateuse as a String
      def privateuse=(val)
        @privateuse = val
      end

      private
      def convert_to(klass)
        if klass == Rfc
          klass.new(language, script, region, variants, extensions, privateuse)
        elsif klass == Cldr
          exts = {}
          extensions.sort.each do |v|
            if v =~ /^k-(#{ALPHANUM}{2,})-(.*)$/i
              exts[$1] = $2
            end
          end
          klass.new(language, script, region, variants, exts)
        else
          super
        end
      end

      # Returns the language tag 
      #   <language>-<Script>-<REGION>-<variants>-<extensions>-<PRIVATEUSE>
      #   (e.g.) "ja-Hira-JP-variant"
      #
      # This is used in internal only. Use to_s instead.
      def to_string
        s = super.gsub(/_/, "-")
        @extensions.sort.each do |v|
          s << "-#{v}"
        end
        s << "-#{@privateuse}" if @privateuse
        s
      end

    end
  end
end
