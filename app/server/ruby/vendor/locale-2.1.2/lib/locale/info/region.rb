# encoding: UTF-8
=begin

 region.rb - Locale::Info::Region class

 Copyright (C) 2008  Masao Mutoh
 
 First Author:: Brian Pontarelli

 $Id: region.rb 27 2008-12-03 15:06:50Z mutoh $
=end

require 'zlib'

module Locale

  module Info
    # This class models out a region/country from the ISO 3166 standard for region codes.
    # In ISO3166, it's called "Country" but Ruby/Locale the word "Region" instead.
    class Region
      attr_reader :code, :name

      # code::  The 2 or 3 digit ISO 3166 region code.
      # name::  The name of the region.
      def initialize(code, name)
        @code = code
        @name = name
      end

      def iso_region?
        @@regions[code] != nil
      end

      def to_s
        "#{code}"
      end
    end

    @@regions = Hash.new
    Zlib::GzipReader.open(File.dirname(__FILE__) + "/../data/regions.tab.gz") do |gz|
      gz.readlines.each do |l|
        l.force_encoding('UTF-8') if l.respond_to?(:force_encoding)
        unless l =~ /^\s*$/
          parts = l.split(/\t/)
          region = Region.new(parts[0], parts[1].strip)
          @@regions[parts[0]] = region
        end
      end
    end

    module_function

    # Returns a hash of all the ISO regions. The hash is {String, Region} where
    # the string is the 2 digit region code from the ISO 3166 data.
    #
    # You need to require 'locale/info' or 'locale/region'.
    def regions
      @@regions
    end

    # Returns the region for the given code.
    #
    # You need to require 'locale/info' or 'locale/info/region'.
    def get_region(code)
      @@regions[code]
    end

    # Returns the region code is valid.
    #
    # You need to require 'locale/info' or 'locale/info/region'.
    def valid_region_code?(code)
      @@regions[code] != nil
    end
  end
end
