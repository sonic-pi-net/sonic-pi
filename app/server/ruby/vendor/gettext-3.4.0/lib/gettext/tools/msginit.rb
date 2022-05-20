# -*- coding: utf-8 -*-
#
# Copyright (C) 2012  Haruka Yoshihara <yoshihara@clear-code.com>
# Copyright (C) 2012-2021  Sutou Kouhei <kou@clear-code.com>
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
require "optparse"

begin
  require "datasets"
rescue LoadError
end
require "locale/info"

require "gettext"
require "gettext/po_parser"
require "gettext/tools/msgmerge"

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
                  _("file '%s' does not exist.") % @input_file)
          end
        end

        if @locale.nil?
          language_tag = Locale.current
        else
          language_tag = Locale::Tag.parse(@locale)
        end

        unless valid_locale?(language_tag)
          raise(ValidationError,
                _("Locale '%s' is invalid. " +
                  "Please check if your specified locale is usable.") %
                language_tag)
        end
        @locale = language_tag.to_simple.to_s
        @language = language_tag.language

        @output_file ||= "#{@locale}.po"
        if File.exist?(@output_file)
          raise(ValidationError,
                _("file '%s' has already existed.") % @output_file)
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
        converter = CLDRPluralsConverter.new(language)
        converter.convert
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

      class CLDRPluralsConverter
        def initialize(language)
          @language = language

        end

        def convert
          n_plurals = nil
          expression = nil
          if defined?(Datasets::CLDRPlurals)
            plurals = Datasets::CLDRPlurals.new
            plurals.each do |locale|
              next unless locale.name == @language
              n_plurals, expression = convert_plural_rules(locale.rules)
              break
            end
          end
          "nplurals=#{n_plurals}; plural=#{expression};"
        end

        private
        def convert_plural_rules(rules)
          n_plurals = 1
          conditions = []
          order = [
            "one",
            "zero",
            "two",
            "few",
            "many",
            "other",
          ]
          rules = rules.reject do |rule|
            rule.integer_samples.nil?
          end
          rules = rules.sort_by do |rule|
            order.index(rule.count)
          end
          rules[0..-2].each do |rule|
            next if rule.condition.nil?
            condition = convert_plural_condition(rule.condition)
            next if condition.nil?
            next if condition == false
            n_plurals += 1
            conditions << condition
          end
          expression = ""
          case conditions.size
          when 0
            expression << "0"
          when 1
            condition = conditions[0]
            case condition
            when "(n == 1)"
              expression << "n != 1"
            when "(n <= 1)"
              expression << "n > 1"
            else
              expression << "#{condition} ? 1 : 0"
            end
          else
            (conditions.size + 1).times do |i|
              if i == conditions.size
                expression << i.to_s
              else
                condition = conditions[i]
                expression << "#{condition} ? #{i} : "
              end
            end
          end
          [n_plurals, expression]
        end

        def convert_plural_condition(condition)
          case condition[0]
          when :and
            gettext_condition = nil
            condition[1..-1].each do |sub_condition|
              sub_gettext_condition = convert_plural_condition(sub_condition)
              case sub_gettext_condition
              when String
                if gettext_condition.is_a?(String)
                  gettext_condition << " && #{sub_gettext_condition}"
                else
                  gettext_condition = sub_gettext_condition
                end
              when TrueClass
                unless gettext_condition.is_a?(String)
                  gettext_condition = true
                end
              when FalseClass
                return false
              else
                raise "unknown value #{sub_gettext_condition.inspect}"
              end
            end
            gettext_condition
          when :or
            gettext_condition = false
            condition[1..-1].each do |sub_condition|
              sub_gettext_condition = convert_plural_condition(sub_condition)
              case sub_gettext_condition
              when String
                if gettext_condition.is_a?(String)
                  gettext_condition << " || #{sub_gettext_condition}"
                else
                  gettext_condition = sub_gettext_condition
                end
              when TrueClass
                return true
              when FalseClass
              else
                raise "unknown value #{sub_gettext_condition.inspect}"
              end
            end
            gettext_condition
          when :equal
            left = convert_plural_condition(condition[1])
            right = condition[2]
            case left
            when String
              right = compact_equal_values(right)
              gettext_conditions = right.collect do |right_value|
                case right_value
                when Range
                  if right_value.begin.zero?
                    "(#{left} <= #{right_value.end})"
                  else
                    "(#{left} >= #{right_value.begin} && " +
                      "#{left} <= #{right_value.end})"
                  end
                else
                  "(#{left} == #{right_value})"
                end
              end
              if gettext_conditions.size == 1
                gettext_conditions[0]
              else
                gettext_conditions.join(" || ")
              end
            when 0
              if right.include?(0)
                true
              else
                false
              end
            else
              false
            end
          when :not_equal
            left = convert_plural_condition(condition[1])
            right = condition[2]
            case left
            when String
              right = compact_equal_values(right)
              gettext_conditions = right.collect do |right_value|
                case right_value
                when Range
                  "(#{left} < #{right_value.begin} || " +
                    "#{left} > #{right_value.end})"
                else
                  "(#{left} != #{right_value})"
                end
              end
              if gettext_conditions.size == 1
                gettext_conditions[0]
              else
                gettext_conditions = gettext_conditions.collect do |gettext_condition|
                  "(#{gettext_condition})"
                end
                gettext_conditions.join(" && ")
              end
            when 0
              if right.include?(0)
                false
              else
                true
              end
            else
              false
            end
          when :mod
            left = convert_plural_condition(condition[1])
            right = condition[2]
            case left
            when "n"
              "(n % #{right})"
            else
              false
            end
          when "n", "i"
            "n"
          when "v", "w"
            0
          when "f", "t", "c", "e"
            false
          else
            raise "unknown operator: #{condition[0].inspect}"
          end
        end

        def compact_equal_values(values)
          if values == [0, 1]
            [0..1]
          else
            values
          end
        end
      end
    end
  end
end
