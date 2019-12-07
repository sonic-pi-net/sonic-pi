=begin
  locale.rb - Locale module

  Copyright (C) 2012  Kouhei Sutou <kou@clear-code.com>
  Copyright (C) 2002-2009  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby.

  Original: Ruby-GetText-Package-1.92.0.

  $Id: locale.rb 27 2008-12-03 15:06:50Z mutoh $
=end

require 'locale/tag'
require 'locale/taglist'
require 'locale/driver'
require 'locale/version'

# Locale module manages the locale informations of the application.
# These functions are the most important APIs in this library.
# Almost of all i18n/l10n programs use this APIs only.
module Locale
  @@default_tag = nil
  @@driver_name = nil

  module_function
  def require_driver(name)  #:nodoc:
    require "locale/driver/#{name}"
    @@driver_name = name.to_sym
  end

  def create_language_tag(tag)  #:nodoc:
    case tag
    when nil
    when Locale::Tag::Simple
      tag
    when Locale::TagList
      tag[0]
    else
      Locale::Tag.parse(tag)
    end
  end

  # Initialize Locale library. 
  # Usually, you don't need to call this directly, because
  # this is called when Locale's methods are called.
  #
  # If you use this library with CGI or the kind of CGI.
  # You need to call Locale.init(:driver => :cgi).
  #
  # ==== For Framework designers/programers:
  # If your framework is for WWW, call this once like: Locale.init(:driver => :cgi).
  #
  # ==== To Application programers:
  # If your framework doesn't use ruby-locale and the application is for WWW,
  # call this once like: Locale.init(:driver => :cgi).
  #
  # ==== To Library authors:
  # Don't call this, even if your application is only for WWW.
  #
  # * opts: Options as a Hash.
  #   * :driver - The driver. :cgi if you use Locale module with CGI,
  #     nil if you use system locale.
  #       (ex) Locale.init(:driver => :cgi)
  #
  def init(opts = {})
    if opts[:driver]
      require_driver opts[:driver]
    else
      if /cygwin|mingw|win32/ =~ RUBY_PLATFORM
        require_driver 'win32' 
      elsif /java/ =~ RUBY_PLATFORM
        require_driver 'jruby' 
      else
        require_driver 'posix' 
      end
    end
  end

  # Gets the driver module.
  #
  # Usually you don't need to call this method.
  #
  # * Returns: the driver module.
  def driver_module 
    Locale.init if @@driver_name.nil?
    Driver::MODULES[@@driver_name]
  end

  DEFAULT_LANGUAGE_TAG = Locale::Tag::Simple.new("en") #:nodoc:

  # Sets the default locale as the language tag 
  # (Locale::Tag's class or String(such as "ja_JP")).
  # 
  # * tag: the default language_tag
  # * Returns: self.
  def set_default(tag)
    Thread.list.each do |thread|
      thread[:current_languages] = nil
      thread[:candidates_caches] = nil
    end
    @@default_tag = create_language_tag(tag)
    self
  end

  # Same as Locale.set_default.
  #
  # * locale: the default locale (Locale::Tag's class) or a String such as "ja-JP".
  # * Returns: locale.
  def default=(tag)
    set_default(tag)
    @@default_tag
  end

  # Gets the default locale(language tag).
  #
  # If the default language tag is not set, this returns nil.
  #
  # * Returns: the default locale (Locale::Tag's class).
  def default
    @@default_tag || DEFAULT_LANGUAGE_TAG
  end

  # Sets the locales of the current thread order by the priority. 
  # Each thread has a current locales.
  # The system locale/default locale is used if the thread doesn't have current locales.
  #
  # * tag: Locale::Language::Tag's class or the language tag as a String. nil if you need to
  #   clear current locales.
  # * charset: the charset (override the charset even if the locale name has charset) or nil.
  # * Returns: self
  #
  # (e.g.)
  #    Locale.set_current("ja_JP.eucJP")
  #    Locale.set_current("ja-JP")
  #    Locale.set_current("en_AU", "en_US", ...)
  #    Locale.set_current(Locale::Tag::Simple.new("ja", "JP"), ...)
  def set_current(*tags)
    languages = nil
    if tags[0]
      languages = Locale::TagList.new
      tags.each do |tag|
        case tag
        when Locale::TagList
          languages.concat(tag)
        else
          languages << create_language_tag(tag)
        end
      end
    end
    Thread.current[:current_languages] = languages
    Thread.current[:candidates_caches] = nil
    self
  end

  # Sets a current locale. This is a single argument version of Locale.set_current.
  #
  # * tag: the language tag such as "ja-JP"
  # * Returns: an Array of the current locale (Locale::Tag's class).
  #
  #    Locale.current = "ja-JP"
  #    Locale.current = "ja_JP.eucJP"
  def current=(tag)
    set_current(tag)
    Thread.current[:current_languages]
  end

  # Gets the current locales (Locale::Tag's class).
  # If the current locale is not set, this returns system/default locale.
  #
  # This method returns the current language tags even if it isn't included in app_language_tags.
  #
  # Usually, the programs should use Locale.candidates to find the correct locale, not this method.
  #
  # * Returns: an Array of the current locales (Locale::Tag's class).
  def current
    unless Thread.current[:current_languages]
      loc = driver_module.locales
      Thread.current[:current_languages] = loc ? loc : Locale::TagList.new([default])
    end
    Thread.current[:current_languages]
  end

  # Deprecated.
  def get #:nodoc: 
    current
  end

  # Deprecated.
  def set(tag)  #:nodoc:
    set_current(tag)
  end

  # Returns the language tags which are variations of the current locales order by priority.
  #
  # For example, if the current locales are ["fr", "ja_JP", "en_US", "en-Latn-GB-VARIANT"], 
  # then returns ["fr", "ja_JP", "en_US", "en-Latn-GB-VARIANT", "en_Latn_GB", "en_GB", "ja", "en"].
  # "en" is the default locale(You can change it using set_default). 
  # The default locale is added at the end of the list even if it isn't exist.
  #
  # Usually, this method is used to find the locale data as the path(or a kind of IDs).
  # * options: options as a Hash or nil.
  #   * :supported_language_tags - 
  #     An Array of the language tags order by the priority. This option 
  #     restricts the locales which are supported by the library/application.
  #     Default is nil if you don't need to restrict the locales.
  #      (e.g.1) ["fr_FR", "en_GB", "en_US", ...]
  #   * :type - 
  #     The type of language tag. :common, :rfc, :cldr, :posix and 
  #     :simple are available. Default value is :common
  def candidates(options = {})
    opts = {
      :supported_language_tags => nil,
      :current                 => current,
      :type                    => :common,
    }.merge(options)

    Thread.current[:candidates_caches] ||= {}
    Thread.current[:candidates_caches][opts] ||=
      collect_candidates(opts[:type], opts[:current],
                         opts[:supported_language_tags])
  end

  # collect tag candidates.
  # The result is shared from all threads.
  def collect_candidates(type, tags, supported_tags) # :nodoc:
    candidate_tags = tags.collect{|v| v.send("to_#{type}").candidates}
    default_tags = default.send("to_#{type}").candidates
    if app_language_tags
      app_tags = app_language_tags.collect{|v| v.send("to_#{type}")}.flatten.uniq
    end
    if supported_tags
      supported_tags = supported_tags.collect{|v| Locale::Tag.parse(v).send("to_#{type}")}.flatten.uniq
    end

    tags = []
    unless candidate_tags.empty?
      (0...candidate_tags[0].size).each {|i|
        tags += candidate_tags.collect{|v| v[i]}
      }
    end
    tags += default_tags
    tags.uniq!

    all_tags = nil
    if app_tags
      if supported_tags
        all_tags = app_tags & supported_tags
      else
        all_tags = app_tags
      end
    elsif supported_tags
      all_tags = supported_tags
    end
    if all_tags
      tags &= all_tags
      tags = default_tags.uniq if tags.size == 0
    end

    Locale::TagList.new(tags)
  end

  # Gets the current charset.
  #
  # This returns the current user/system charset. This value is
  # read only, so you can't set it by yourself.
  #
  # * Returns: the current charset.
  def charset
    driver_module.charset || "UTF-8"
  end

  # Clear current locale.
  # * Returns: self
  def clear
    Thread.current[:current_languages] = nil
    Thread.current[:candidates_caches] = nil
    self
  end

  # Clear all locales and charsets of all threads. 
  # This doesn't clear the default and app_language_tags.
  # Use Locale.default = nil to unset the default locale.
  # * Returns: self
  def clear_all
    Thread.list.each do |thread|
      thread[:current_languages] = nil
      thread[:candidates_caches] = nil
    end
    self
  end

  @@app_language_tags = nil
  # Set the language tags which is supported by the Application.
  # This value is same with supported_language_tags in Locale.candidates
  # to restrict the result but is the global setting.
  # If you set a language tag, the application works as the single locale 
  # application.
  #
  # If the current locale is not included in app_language_tags,
  # Locale.default value is used.
  # Use Locale.set_default() to set correct language 
  # if "en" is not included in the language tags.
  #
  # Set nil if clear the value.
  #
  # Note that the libraries/plugins shouldn't set this value.
  #
  #  (e.g.) Locale.set_app_language_tags("fr_FR", "en-GB", "en_US", ...)
  def set_app_language_tags(*tags)
    if tags[0]
      @@app_language_tags = tags.collect{|v| Locale::Tag.parse(v)}
    else
      @@app_language_tags = nil
    end
    
    clear_all
    self
  end

  # Returns the app_language_tags. Default is nil. See set_app_language_tags for more details.
  def app_language_tags
    @@app_language_tags
  end

end
