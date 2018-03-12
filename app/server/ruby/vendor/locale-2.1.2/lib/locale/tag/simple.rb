=begin
  locale/tag/simple.rb - Locale::Tag::Simple

  Copyright (C) 2008,2009  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby.
=end

module Locale
  module Tag
    # Abstract language tag class.
    # This class has <language>, <region> which
    # all of language tag specifications have.
    #
    # * ja (language: ISO 639 (2 or 3 alpha))
    # * ja_JP (country: RFC4646 (ISO3166/UN M.49) (2 alpha or 3 digit)
    # * ja-JP
    # * ja-392
    class Simple
      ALPHA = '[a-z]'
      DIGIT = '[0-9]'
      ALPHANUM = "[a-zA-Z0-9]"

      LANGUAGE = "(#{ALPHA}{2,3})" # ISO 639
      REGION = "(#{ALPHA}{2}|#{DIGIT}{3})"   # RFC4646 (ISO3166/UN M.49)

      TAG_RE = /\A#{LANGUAGE}(?:[_-]#{REGION})?\Z/i

      attr_reader :language, :region

      # tag is set when .parse method is called.
      # This value is used when the program want to know the original
      # String.
      attr_accessor :tag

      # call-seq:
      # to_common
      # to_posix
      # to_rfc
      # to_cldr
      #
      # Convert to each tag classes.
      [:simple, :common, :posix, :rfc, :cldr].each do |name|
        class_eval <<-EOS
          def to_#{name}
            convert_to(#{name.to_s.capitalize})
          end
        EOS
      end
        
      class << self
        # Parse the language tag and return the new Locale::Tag::Simple. 
        def parse(tag)
          if tag =~ TAG_RE
            ret = self.new($1, $2)
            ret.tag = tag
            ret
          else
            nil
          end
        end
      end

      # Create a Locale::Tag::Simple
      def initialize(language, region = nil)
        raise "language can't be nil." unless language
        @language, @region = language, region
        @language = @language.downcase if @language
        @region = @region.upcase if @region
      end

      # Returns the language tag as the String. 
      #   <language>_<REGION>
      #   (e.g.) "ja_JP"
      def to_s
        to_string
      end

      def to_str  #:nodoc:
        to_s
      end

      def <=>(other)
        self.to_s <=> other.to_s
      end

      def ==(other)  #:nodoc:
        other != nil and hash == other.hash
      end
      
      def eql?(other) #:nodoc:
        self.==(other)
      end
      
      def hash #:nodoc:
        "#{self.class}:#{to_s}".hash
      end

      def inspect  #:nodoc:
        %Q[#<#{self.class}: #{to_s}>]
      end

      # For backward compatibility.
      def country; region end

      # Set the language (with downcase)
      def language=(val)
        @language = val
        @language = @language.downcase if @language
        @language
      end

      # Set the region (with upcase)
      def region=(val)
        @region = val
        @region = @region.upcase if @region
        @region
      end

      # Returns an Array of tag-candidates order by priority.
      # Use Locale.candidates instead of this method.
      def candidates
        [self.class.new(language, region), self.class.new(language)]
      end

      # Convert to the klass(the class of Language::Tag)
      private
      def convert_to(klass)  #:nodoc:
        if klass == Simple || klass == Posix
          klass.new(language, region)
        else
          klass.new(language, nil, region)
        end
      end

      # Return simple language tag which format is"<lanuguage>_<REGION>". 
      # This is to use internal only. Use to_s instead.
      def to_string  
        s = @language.dup
        s << "_" << @region if @region
        s      
      end
    end
  end
end
