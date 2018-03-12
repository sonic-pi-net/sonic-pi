# -*- coding: utf-8 -*-
#
# Copyright (C) 2012  Haruka Yoshihara <yoshihara@clear-code.com>
# Copyright (C) 2012-2014  Kouhei Sutou <kou@clear-code.com>
#
# License: Ruby's or LGPL
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

require "etc"
require "gettext"
require "gettext/po_parser"
require "gettext/tools/msgmerge"
require "locale/info"
require "optparse"

module GetText
  module Tools
    class MsgInit
      class Error < StandardError
      end

      class ArgumentError < Error
      end

      class ValidationError < Error
      end

      class << self
        # Create a new .po file from initializing .pot file with user's
        # environment and input.
        # @param [Array<String>] arguments arguments for rmsginit.
        # @return [void]
        def run(*arguments)
          new.run(*arguments)
        end
      end

      include GetText

      bindtextdomain("gettext")

      def initialize
        @input_file = nil
        @output_file = nil
        @locale = nil
        @language = nil
        @entry = nil
        @comment = nil
        @translator = nil
        @set_translator = true
        @translator_name = nil
        @translator_eamil = nil
      end

      # Create .po file from .pot file, user's inputs and metadata.
      # @param [Array] arguments the list of arguments for rmsginit
      def run(*arguments)
        parse_arguments(*arguments)
        validate

        parser = POParser.new
        parser.ignore_fuzzy = false
        pot = parser.parse_file(@input_file, GetText::PO.new)
        po = replace_pot_header(pot)

        File.open(@output_file, "w") do |f|
          f.puts(po.to_s)
        end
      end

      private
      VERSION = GetText::VERSION

      def parse_arguments(*arguments)
        parser = OptionParser.new
        description = _("Create a new .po file from initializing .pot " +
                          "file with user's environment and input.")
        parser.separator(description)
        parser.separator("")
        parser.separator(_("Specific options:"))

        input_description = _("Use INPUT as a .pot file. If INPUT is not " +
                                "specified, INPUT is a .pot file existing " +
                                "the current directory.")
        parser.on("-i", "--input=FILE", input_description) do |input|
          @input_file = input
        end

        output_description = _("Use OUTPUT as a created .po file. If OUTPUT " +
                                 "is not specified, OUTPUT depend on LOCALE " +
                                 "or the current locale on your environment.")
        parser.on("-o", "--output=OUTPUT", output_description) do |output|
          @output_file = output
        end

        locale_description = _("Use LOCALE as target locale. If LOCALE is " +
                                 "not specified, LOCALE is the current " +
                                 "locale on your environment.")
        parser.on("-l", "--locale=LOCALE", locale_description) do |loc|
          @locale = loc
        end

        parser.on("--[no-]translator",
                  _("Whether set translator information or not"),
                  _("(set)")) do |boolean|
          @set_translator = boolean
        end

        parser.on("--translator-name=NAME",
                  _("Use NAME as translator name")) do |name|
          @translator_name = name
        end

        parser.on("--translator-email=EMAIL",
                  _("Use EMAIL as translator email address")) do |email|
          @translator_email = email
        end

        parser.on("-h", "--help", _("Display this help and exit")) do
          puts(parser.help)
          exit(true)
        end

        version_description = _("Display version and exit")
        parser.on_tail("-v", "--version", version_description) do
          puts(VERSION)
          exit(true)
        end

        begin
          parser.parse!(arguments)
        rescue OptionParser::ParseError
          raise(ArgumentError, $!.message)
        end
      end

      def validate
        if @input_file.nil?
          @input_file = Dir.glob("./*.pot").first
          if @input_file.nil?
            raise(ValidationError,
                  _(".pot file does not exist in the current directory."))
          end
        else
          unless File.exist?(@input_file)
            raise(ValidationError,
                  _("file '%s' does not exist." % @input_file))
          end
        end

        if @locale.nil?
          language_tag = Locale.current
        else
          language_tag = Locale::Tag.parse(@locale)
        end

        unless valid_locale?(language_tag)
          raise(ValidationError,
                _("Locale '#{language_tag}' is invalid. " +
                    "Please check if your specified locale is usable."))
        end
        @locale = language_tag.to_simple.to_s
        @language = language_tag.language

        @output_file ||= "#{@locale}.po"
        if File.exist?(@output_file)
          raise(ValidationError,
                _("file '%s' has already existed." % @output_file))
        end
      end

      def valid_locale?(language_tag)
        return false if language_tag.instance_of?(Locale::Tag::Irregular)

        Locale::Info.language_code?(language_tag.language)
      end

      def replace_pot_header(pot)
        @entry = pot[""].msgstr
        @comment = pot[""].translator_comment
        @translator = translator_info

        replace_entry
        replace_comment

        pot[""] = @entry
        pot[""].translator_comment = @comment
        pot[""].flags = pot[""].flags.reject do |flag|
          flag == "fuzzy"
        end
        pot
      end

      def translator_info
        return nil unless @set_translator
        name = translator_name
        email = translator_email
        if name and email
          "#{name} <#{email}>"
        else
          nil
        end
      end

      def translator_name
        @translator_name ||= read_translator_name
      end

      def read_translator_name
        prompt(_("Please enter your full name"), guess_translator_name)
      end

      def guess_translator_name
        name = guess_translator_name_from_password_entry
        name ||= ENV["USERNAME"]
        name
      end

      def guess_translator_name_from_password_entry
        password_entry = find_password_entry
        return nil if password_entry.nil?

        name = password_entry.gecos.split(/,/).first.strip
        name = nil if name.empty?
        name
      end

      def find_password_entry
        Etc.getpwuid
      rescue ArgumentError
        nil
      end

      def translator_email
        @translator_email ||= read_translator_email
      end

      def read_translator_email
        prompt(_("Please enter your email address"), guess_translator_email)
      end

      def guess_translator_email
        ENV["EMAIL"]
      end

      def prompt(message, default)
        print(message)
        print(" [#{default}]") if default
        print(": ")

        user_input = $stdin.gets.chomp
        if user_input.empty?
          default
        else
          user_input
        end
      end

      def replace_entry
        replace_last_translator
        replace_pot_revision_date
        replace_language
        replace_plural_forms
      end

      def replace_comment
        replace_description
        replace_first_author
        replace_copyright_year
        @comment = @comment.gsub(/^fuzzy$/, "")
      end

      EMAIL = "EMAIL@ADDRESS"
      FIRST_AUTHOR_KEY = /^FIRST AUTHOR <#{EMAIL}>, (\d+\.)$/

      def replace_last_translator
        unless @translator.nil?
          @entry = @entry.gsub(LAST_TRANSLATOR_KEY, "\\1 #{@translator}")
        end
      end

      POT_REVISION_DATE_KEY = /^(PO-Revision-Date:).+/

      def replace_pot_revision_date
        @entry = @entry.gsub(POT_REVISION_DATE_KEY, "\\1 #{revision_date}")
      end

      LANGUAGE_KEY = /^(Language:).+/
      LANGUAGE_TEAM_KEY = /^(Language-Team:).+/

      def replace_language
        language_name = Locale::Info.get_language(@language).name
        @entry = @entry.gsub(LANGUAGE_KEY, "\\1 #{@locale}")
        @entry = @entry.gsub(LANGUAGE_TEAM_KEY, "\\1 #{language_name}")
      end

      PLURAL_FORMS =
        /^(Plural-Forms:) nplurals=INTEGER; plural=EXPRESSION;$/

      def replace_plural_forms
        plural_entry = plural_forms(@language)
        if PLURAL_FORMS =~ @entry
          @entry = @entry.gsub(PLURAL_FORMS, "\\1 #{plural_entry}\n")
        else
          @entry << "Plural-Forms: #{plural_entry}\n"
        end
      end

      def plural_forms(language)
        case language
        when "ja", "vi", "ko", /\Azh.*\z/
          nplural = "1"
          plural_expression = "0"
        when "en", "de", "nl", "sv", "da", "no", "fo", "es", "pt",
             "it", "bg", "el", "fi", "et", "he", "eo", "hu", "tr",
             "ca", "nb"
          nplural = "2"
          plural_expression = "n != 1"
        when "pt_BR", "fr"
          nplural = "2"
          plural_expression = "n>1"
        when "lv"
          nplural = "3"
          plural_expression = "n%10==1 && n%100!=11 ? 0 : n != 0 ? 1 : 2"
        when "ga"
          nplural = "3"
          plural_expression = "n==1 ? 0 : n==2 ? 1 : 2"
        when "ro"
          nplural = "3"
          plural_expression = "n==1 ? 0 : " +
                                "(n==0 || (n%100 > 0 && n%100 < 20)) ? 1 : 2"
        when "lt", "bs"
          nplural = "3"
          plural_expression = "n%10==1 && n%100!=11 ? 0 : " +
                                "n%10>=2 && (n%100<10 || n%100>=20) ? 1 : 2"
        when "ru", "uk", "sr", "hr"
          nplural = "3"
          plural_expression = "n%10==1 && n%100!=11 ? 0 : n%10>=2 && " +
                                "n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2"
        when "cs", "sk"
          nplural = "3"
          plural_expression = "(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2"
        when "pl"
          nplural = "3"
          plural_expression = "n==1 ? 0 : n%10>=2 && " +
                                "n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2"
        when "sl"
          nplural = "4"
          plural_expression = "n%100==1 ? 0 : n%100==2 ? 1 : n%100==3 " +
                                "|| n%100==4 ? 2 : 3"
        else
          nplural = nil
          plural_expression = nil
        end

        "nplurals=#{nplural}; plural=#{plural_expression};"
      end

      DESCRIPTION_TITLE = /^SOME DESCRIPTIVE TITLE\.$/

      def replace_description
        language_name = Locale::Info.get_language(@language).name
        package_name = ""
        @entry.gsub(/Project-Id-Version: (.+?) .+/) do
          package_name = $1
        end
        description = "#{language_name} translations " +
                        "for #{package_name} package."
        @comment = @comment.gsub(DESCRIPTION_TITLE, "\\1 #{description}")
      end

      YEAR_KEY = /^(FIRST AUTHOR <#{EMAIL}>,) YEAR\.$/
      LAST_TRANSLATOR_KEY = /^(Last-Translator:) FULL NAME <#{EMAIL}>$/

      def replace_first_author
        @comment = @comment.gsub(YEAR_KEY, "\\1 #{year}.")
        unless @translator.nil?
          @comment = @comment.gsub(FIRST_AUTHOR_KEY, "#{@translator}, \\1")
        end
      end

      COPYRIGHT_KEY = /^(Copyright \(C\)) YEAR (THE PACKAGE'S COPYRIGHT HOLDER)$/
      def replace_copyright_year
        @comment = @comment.gsub(COPYRIGHT_KEY, "\\1 #{year} \\2")
      end

      def now
        @now ||= Time.now
      end

      def revision_date
        now.strftime("%Y-%m-%d %H:%M%z")
      end

      def year
        now.year
      end
    end
  end
end
