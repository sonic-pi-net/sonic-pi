# -*- coding: utf-8 -*-

=begin
  text_domain.rb - GetText::TextDomain

  Copyright (C) 2001-2009  Masao Mutoh
  Copyright (C) 2001-2003  Masahiro Sakai

      Masahiro Sakai    <s01397ms@sfc.keio.ac.jp>
      Masao Mutoh       <mutomasa at gmail.com>

  You may redistribute it and/or modify it under the same
  license terms as Ruby or LGPL.
=end

require 'gettext/mo'
require 'gettext/locale_path'

module GetText
  # GetText::TextDomain class manages mo-files of a text domain.
  #
  # Usually, you don't need to use this class directly.
  #
  # Notice: This class is unstable. APIs will be changed.
  class TextDomain

    attr_reader :output_charset
    attr_reader :mofiles
    attr_reader :name

    @@cached = ! $DEBUG
    # Cache the mo-file or not.
    # Default is true. If $DEBUG is set then false.
    def self.cached?
      @@cached
    end

    # Set to cache the mo-file or not.
    # * val: true if cached, otherwise false.
    def self.cached=(val)
      @@cached = val
    end

    # Creates a new GetText::TextDomain.
    # * name: the text domain name.
    # * topdir: the locale path ("%{topdir}/%{lang}/LC_MESSAGES/%{name}.mo") or nil.
    # * output_charset: output charset.
    # * Returns: a newly created GetText::TextDomain object.
    def initialize(name, topdir = nil, output_charset = nil)
      @name, @output_charset = name, output_charset

      @locale_path = LocalePath.new(@name, topdir)
      @mofiles = {}
    end

    # Translates the translated string.
    # * lang: Locale::Tag::Simple's subclass.
    # * msgid: the original message.
    # * Returns: the translated string or nil.
    def translate_singular_message(lang, msgid)
      return "" if msgid.nil?

      lang_key = lang.to_s

      mo = nil
      if self.class.cached?
        mo = @mofiles[lang_key]
      end
      unless mo
        mo = load_mo(lang)
      end

      if (! mo) or (mo ==:empty)
        return nil
      end

      return mo[msgid] if mo.has_key?(msgid)

      ret = nil
      if msgid.include?("\000")
        # Check "aaa\000bbb" and show warning but return the singular part.
        msgid_single = msgid.split("\000")[0]
        msgid_single_prefix_re = /^#{Regexp.quote(msgid_single)}\000/
        mo.each do |key, val|
          if msgid_single_prefix_re =~ key
            # Usually, this is not caused to make po-files from rgettext.
            separated_msgid = msgid.gsub(/\000/, '", "')
            duplicated_msgid = key.gsub(/\000/, '", "')
            warn("Warning: " +
                  "n_(\"#{separated_msgid}\") and " +
                  "n_(\"#{duplicated_msgid}\") " +
                  "are duplicated.")
            ret = val
            break
          end
        end
      else
        plural_msgid_prefix = "#{msgid}\000"
        mo.each do |key, val|
          next unless Encoding.compatible?(key, plural_msgid_prefix)
          next unless key.start_with?(plural_msgid_prefix)
          ret = val.split("\000")[0]
          break
        end
      end
      ret
    end

    DEFAULT_PLURAL_CALC = Proc.new {|n| n != 1}
    DEFAULT_SINGLE_CALC = Proc.new {|n| 0}

    # Translates the translated string.
    # * lang: Locale::Tag::Simple's subclass.
    # * msgid: the original message.
    # * msgid_plural: the original message(plural).
    # * Returns: the translated string as an Array ([[msgstr1, msgstr2, ...], cond]) or nil.
    def translate_plural_message(lang, msgid, msgid_plural)   #:nodoc:
      key = msgid + "\000" + msgid_plural
      msg = translate_singular_message(lang, key)
      ret = nil
      if ! msg
        ret = nil
      elsif msg.include?("\000")
        # [[msgstr[0], msgstr[1], msgstr[2],...], cond]
        mo = @mofiles[lang.to_s]
        cond = (mo and mo != :empty) ? mo.plural_as_proc : DEFAULT_PLURAL_CALC
        ret = [msg.split("\000"), cond]
      else
        ret = [[msg], DEFAULT_SINGLE_CALC]
      end
      ret
    end

    # Clear cached mofiles.
    def clear
      @mofiles = {}
    end

    # Set output_charset.
    # * charset: output charset.
    def output_charset=(charset)
      @output_charset = charset
      clear
    end

    private
    # Load a mo-file from the file.
    # lang is the subclass of Locale::Tag::Simple.
    def load_mo(lang)
      lang_key = lang.to_s

      mo = @mofiles[lang_key]
      if mo
        if mo == :empty
          return :empty
        elsif ! self.class.cached?
          mo.update!
        end
        return mo
      end

      path = @locale_path.current_path(lang)

      if path
        charset = @output_charset || lang.to_posix.charset || Locale.charset || "UTF-8"
        charset = normalize_charset(charset)
        @mofiles[lang_key] = MO.open(path, charset)
      else
        @mofiles[lang_key] = :empty
      end
    end

    def normalize_charset(charset)
      case charset
      when /\Autf8\z/i
        "UTF-8"
      else
        charset
      end
    end
  end
end
