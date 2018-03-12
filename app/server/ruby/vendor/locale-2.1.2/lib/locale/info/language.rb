# encoding: UTF-8
=begin

 language.rb - Locale::Info::Language class

 Copyright (C) 2008  Masao Mutoh
 
 Original Author:: Brian Pontarelli

 $Id: language.rb 27 2008-12-03 15:06:50Z mutoh $
=end

require 'zlib'

module Locale

  module Info
    # This class contains all the of the ISO information for the ISO 639-3
    # languages. This class is immutable once constructed.
    class Language
      attr_reader :two_code, :three_code, :scope, :type, :name

      #
      # Constructs a new Language instance.
      #
      # * code  The 2 or 3 digit ISO 639-3 language code.
      # * scope A single character that defines the ISO scope of the language - <tt>(I)ndividual</tt>,
      #   <tt>(M)acrolanguage</tt>, or <tt>(S)pecial</tt>.
      # * type   A single character that defines the ISO type of the language - <tt>(A)ncient</tt>,
      #   <tt>(C)onstructed</tt>, <tt>(E)xtinct</tt>, <tt>(H)istorical</tt>, <tt>(L)iving</tt>,
      #   or <tt>(S)pecial</tt>.
      # * name  The name of the language.
      #
      def initialize(two_code, three_code, scope, type, name)
        @two_code, @three_code, @scope, @type, @name = two_code, three_code, scope, type, name

        @individual   = (scope == "I")
        @macro        = (scope == "M")
        @special      = (scope == "S")
        @constructed  = (type == "C")
        @living       = (type == "L")
        @extinct      = (type == "E")
        @ancient      = (type == "A")
        @historical   = (type == "H")
        @special_type = (type == "S")
      end

      # Returns true if the language is an individual language according to the ISO 639-3 data.
      def individual?; @individual; end

      # Returns true if the language is a macro language according to the ISO 639-3 data.
      def macro?; @macro; end

      # Returns true if the language is a special language according to the ISO 639-3 data.
      def special?; @special; end

      # Returns true if the language is a constructed language according to the ISO 639-3 data.
      def constructed?; @constructed; end

      # Returns true if the language is a living language according to the ISO 639-3 data.
      def living?; @living; end

      # Returns true if the language is an extinct language according to the ISO 639-3 data.
      def extinct?; @extinct; end

      # Returns true if the language is an ancient language according to the ISO 639-3 data.
      def ancient?; @ancient; end

      # Returns true if the language is an historical language according to the ISO 639-3 data.
      def historical?; @historical; end

      # Returns true if the language is a special type language according to the ISO 639-3 data.
      def special_type?; @special_type; end

      # Returns the two or three code.
      def to_s
        if two_code and two_code.size > 0
          two_code
        else
          three_code
        end
      end

      # Returns this object is valid as ISO 639 data.
      def iso_language?
        @@lang_two_codes[two_code] != nil || @@lang_three_codes[three_code] != nil
      end
    end

    @@lang_two_codes = Hash.new
    @@lang_three_codes = Hash.new

    Zlib::GzipReader.open(File.dirname(__FILE__) + "/../data/languages.tab.gz") do |gz|
      gz.readlines.each do |l|
        l.force_encoding('UTF-8') if l.respond_to?(:force_encoding)
        unless l =~ /^\s*$/
          parts = l.split(/\t/)
          lang = Language.new(parts[2], parts[0], parts[3], parts[4], parts[5].strip)
          @@lang_three_codes[parts[0]] = lang
          @@lang_two_codes[parts[2]] = lang if parts[2].length > 0
        end
      end
    end

    module_function

    # Returns a hash of all the ISO languages. The hash is {String, language} where
    # the string is the 3 digit language code from the ISO 639 data. This contains
    # all of the data from the ISO 639-3 data (7600 Languages).
    #
    # Need to require 'locale/info' or 'locale/language'.
    def three_languages
      @@lang_three_codes
    end

    # Returns a hash of all the ISO languages. The hash is {String, language} where
    # the string is the 2 digit language code from the ISO 639-1 data. This contains
    # all of the data from the ISO 639-1 data (186 Languages).
    #
    # Need to require 'locale/info' or 'locale/language'.
    def two_languages
      @@lang_two_codes
    end

    # Returns the language for the given 2 or 3 digit code.
    #
    # Need to require 'locale/info' or 'locale/language'.
    def get_language(code)
      @@lang_three_codes[code] || @@lang_two_codes[code]
    end

    # Returns the language code is valid.
    #
    # Need to require 'locale/info' or 'locale/language'.
    def language_code?(code)
      get_language(code) != nil
    end
  end
end
