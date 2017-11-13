# -*- coding: utf-8 -*-

=begin
  gettext.rb - GetText module

  Copyright (C) 2001-2010  Masao Mutoh
  Copyright (C) 2001-2003  Masahiro Sakai

      Masao Mutoh       <mutomasa at gmail.com>
      Masahiro Sakai    <s01397ms@sfc.keio.ac.jp>

  You may redistribute it and/or modify it under the same
  license terms as Ruby or LGPL.
=end

require 'locale'

require 'gettext/version'
require 'gettext/text_domain_manager'

module GetText
  # If the text domain isn't bound when calling GetText.textdomain, this error is raised.
  class NoboundTextDomainError < RuntimeError
    def initialize(domainname)
      @domainname = domainname
    end
    def message
      "#{@domainname} is not bound."
    end
  end

  extend self

  def self.included(mod)  #:nodoc:
    mod.extend self
  end

  # bindtextdomain(domainname, options = {})
  #
  # Bind a text domain(%{path}/%{locale}/LC_MESSAGES/%{domainname}.mo) to
  # your program.
  # Normally, the texdomain scope becomes the class/module(and parent
  # classes/included modules).
  #
  # * domainname: the text domain name.
  # * options: options as an Hash.
  #   * :path - the path to the mo-files. When the value is nil, it will search default paths such as
  #     /usr/share/locale, /usr/local/share/locale)
  #   * :output_charset - The output charset. Same with GetText.set_output_charset. Usually, L10n
  #     library doesn't use this option. Application may use this once.
  # * Returns: the GetText::TextDomainManager.
  #
  def bindtextdomain(domainname, *options)
    bindtextdomain_to(self, domainname, *options)
  end

  # Includes GetText module and bind a text domain to a class.
  # * klass: the target ruby class.
  # * domainname: the text domain name.
  # * options: options as an Hash. See GetText.bindtextdomain.
  def bindtextdomain_to(klass, domainname, *options)
    if options[0].kind_of? Hash
      opts = options[0]
    else
      # for backward compatibility.
      opts = {}
      opts[:path] = options[0] if options[0]
      opts[:output_charset] = options[2] if options[2]
    end
    unless (klass.kind_of? GetText or klass.include? GetText)
      klass.__send__(:include, GetText)
    end
    TextDomainManager.bind_to(klass, domainname, opts)
  end

  # Binds a existed text domain to your program.
  # This is the same function with GetText.bindtextdomain but simpler(and faster) than bindtextdomain.
  # Note that you need to call GetText.bindtextdomain first. If the domainname hasn't bound yet,
  # raises GetText::NoboundTextDomainError.
  # * domainname: a text domain name.
  # * Returns: the GetText::TextDomainManager.
  def textdomain(domainname) #:nodoc:
    textdomain_to(self, domainname)
  end

  # Includes GetText module and bind an exsited text domain to a class.
  # See text domain for more detail.
  # * klass: the target ruby class.
  # * domainname: the text domain name.

  def textdomain_to(klass, domainname)  #:nodoc:
    domain = TextDomainManager.text_domain_pool(domainname)
    raise NoboundTextDomainError.new(domainname) unless domain
    bindtextdomain_to(klass, domainname)
  end

  # call-seq:
  #   gettext(msgid)
  #   _(msgid)
  #
  # Translates msgid and return the message.
  # This doesn't make a copy of the message.
  #
  # You need to use String#dup if you want to modify the return value
  # with destructive functions.
  #
  # (e.g.1) _("Hello ").dup << "world"
  #
  # But e.g.1 should be rewrite to:
  #
  # (e.g.2) _("Hello %{val}") % {:val => "world"}
  #
  # Because the translator may want to change the position of "world".
  #
  # * msgid: the message id.
  # * Returns: localized text by msgid. If there are not binded mo-file, it will return msgid.
  def gettext(msgid)
    TextDomainManager.translate_singular_message(self, msgid)
  end

  # call-seq:
  #   sgettext(msgid, div = '|')
  #   s_(msgid, div = '|')
  #
  # Translates msgid, but if there are no localized text,
  # it returns a last part of msgid separeted "div".
  #
  # * msgid: the message id.
  # * separator: separator or nil for no seperation.
  # * Returns: the localized text by msgid. If there are no localized text,
  #   it returns a last part of the msgid separeted by "seperator".
  #   <tt>Movie|Location -> Location</tt>
  # See: http://www.gnu.org/software/gettext/manual/html_mono/gettext.html#SEC151
  def sgettext(msgid, seperator = "|")
    TextDomainManager.translate_singular_message(self, msgid, seperator)
  end

  # call-seq:
  #   pgettext(msgctxt, msgid)
  #   p_(msgctxt, msgid)
  #
  # Translates msgid with msgctxt. This methods is similer with s_().
  #  e.g.) p_("File", "New")   == s_("File|New")
  #        p_("File", "Open")  == s_("File|Open")
  #
  # * msgctxt: the message context.
  # * msgid: the message id.
  # * Returns: the localized text by msgid. If there are no localized text,
  #   it returns msgid.
  # See: http://www.gnu.org/software/autoconf/manual/gettext/Contexts.html
  def pgettext(msgctxt, msgid)
    TextDomainManager.translate_singular_message(self, "#{msgctxt}\004#{msgid}", "\004")
  end

  # call-seq:
  #   ngettext(msgid, msgid_plural, n)
  #   ngettext(msgids, n)  # msgids = [msgid, msgid_plural]
  #   n_(msgid, msgid_plural, n)
  #   n_(msgids, n)  # msgids = [msgid, msgid_plural]
  #
  # The ngettext is similar to the gettext function as it finds the message catalogs in the same way.
  # But it takes two extra arguments for plural form.
  #
  # * msgid: the singular form.
  # * msgid_plural: the plural form.
  # * n: a number used to determine the plural form.
  # * Returns: the localized text which key is msgid_plural if n is plural(follow plural-rule) or msgid.
  #   "plural-rule" is defined in po-file.
  def ngettext(msgid, msgid_plural, n = nil)
    TextDomainManager.translate_plural_message(self, msgid, msgid_plural, n)
  end

  # call-seq:
  #   nsgettext(msgid, msgid_plural, n, div = "|")
  #   nsgettext(msgids, n, div = "|")  # msgids = [msgid, msgid_plural]
  #   ns_(msgid, msgid_plural, n, div = "|")
  #   ns_(msgids, n, div = "|")  # msgids = [msgid, msgid_plural]
  #
  # The nsgettext is similar to the ngettext.
  # But if there are no localized text,
  # it returns a last part of msgid separeted "div".
  #
  # * msgid: the singular form with "div". (e.g. "Special|An apple")
  # * msgid_plural: the plural form. (e.g. "%{num} Apples")
  # * n: a number used to determine the plural form.
  # * Returns: the localized text which key is msgid_plural if n is plural(follow plural-rule) or msgid.
  #   "plural-rule" is defined in po-file.
  def nsgettext(msgid, msgid_plural, n="|", seperator = "|")
    TextDomainManager.translate_plural_message(self, msgid, msgid_plural, n, seperator)
  end

  # call-seq:
  #   npgettext(msgctxt, msgid, msgid_plural, n)
  #   npgettext(msgctxt, msgids, n)  # msgids = [msgid, msgid_plural]
  #   np_(msgctxt, msgid, msgid_plural, n)
  #   np_(msgctxt, msgids, n)  # msgids = [msgid, msgid_plural]
  #
  # The npgettext is similar to the nsgettext function.
  #   e.g.) np_("Special", "An apple", "%{num} Apples", num) == ns_("Special|An apple", "%{num} Apples", num)
  # * msgctxt: the message context.
  # * msgid: the singular form.
  # * msgid_plural: the plural form.
  # * n: a number used to determine the plural form.
  # * Returns: the localized text which key is msgid_plural if n is plural(follow plural-rule) or msgid.
  #   "plural-rule" is defined in po-file.
  def npgettext(msgctxt, msgids, arg2 = nil, arg3 = nil)
    if msgids.kind_of?(Array)
      msgid = msgids[0]
      msgid_ctxt = "#{msgctxt}\004#{msgid}"
      msgid_plural = msgids[1]
      opt1 = arg2
      opt2 = arg3
    else
      msgid = msgids
      msgid_ctxt = "#{msgctxt}\004#{msgid}"
      msgid_plural = arg2
      opt1 = arg3
      opt2 = nil
    end

    msgstr = TextDomainManager.translate_plural_message(self, msgid_ctxt, msgid_plural, opt1, opt2)
    if msgstr == msgid_ctxt
      msgid
    else
      msgstr
    end
  end

  # makes dynamic translation messages readable for the gettext parser.
  # <tt>_(fruit)</tt> cannot be understood by the gettext parser. To help the parser find all your translations,
  # you can add <tt>fruit = N_("Apple")</tt> which does not translate, but tells the parser: "Apple" needs translation.
  # * msgid: the message id.
  # * Returns: msgid.
  def N_(msgid)
    msgid
  end

  # This is same function as N_ but for ngettext.
  # * msgid: the message id.
  # * msgid_plural: the plural message id.
  # * Returns: msgid.
  def Nn_(msgid, msgid_plural)
    [msgid, msgid_plural]
  end

  # Sets charset(String) such as "euc-jp", "sjis", "CP932", "utf-8", ...
  # You shouldn't use this in your own Libraries.
  # * charset: an output_charset
  # * Returns: self
  def set_output_charset(charset)
    TextDomainManager.output_charset = charset
    self
  end

  # Gets the current output_charset which is set using GetText.set_output_charset.
  # * Returns: output_charset.
  def output_charset
    TextDomainManager.output_charset
  end

  # Set the locale. This value forces the locale whole the programs.
  # This method calls Locale.set_app_language_tags, Locale.default, Locale.current.
  # Use Locale methods if you need to handle locales more flexible.
  def set_locale(lang)
    Locale.set_app_language_tags(lang)
    Locale.default = lang
    Locale.current = lang
  end

  # Set the locale to the current thread.
  # Note that if #set_locale is set, this value is ignored.
  # If you need, set_locale(nil); set_current_locale(lang)
  def set_current_locale(lang)
    Locale.current = lang
  end

  def locale
    Locale.current[0]
  end

  alias :locale= :set_locale #:nodoc:
  alias :current_locale= :set_current_locale #:nodoc:
  alias :_ :gettext   #:nodoc:
  alias :n_ :ngettext #:nodoc:
  alias :s_ :sgettext #:nodoc:
  alias :ns_ :nsgettext #:nodoc:
  alias :np_ :npgettext #:nodoc:

  alias :output_charset= :set_output_charset #:nodoc:

unless defined? XX
  # This is the workaround to conflict p_ methods with the xx("double x") library.
  # http://rubyforge.org/projects/codeforpeople/
  alias :p_ :pgettext #:nodoc:
end
end
