# -*- coding: utf-8 -*-
#
# Copyright (C) 2012-2020  Sutou Kouhei <kou@clear-code.com>
# Copyright (C) 2012  Haruka Yoshihara <yoshihara@clear-code.com>
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

require "locale"
require "gettext/tools/xgettext"

class TestToolsXGetText < Test::Unit::TestCase
  include Helper::Tmpdir

  def setup
    @xgettext = GetText::Tools::XGetText.new
    @now = Time.parse("2012-08-19 18:10+0900")
    stub(@xgettext).now {@now}
  end

  setup :setup_tmpdir
  teardown :teardown_tmpdir

  setup
  def setup_paths
    @rb_file_path = File.join(@tmpdir, "lib", "xgettext.rb")
    @pot_file_path = File.join(@tmpdir, "po", "xgettext.pot")
    @rhtml_file_path = File.join(@tmpdir, "templates", "xgettext.rhtml")
    FileUtils.mkdir_p(File.dirname(@rb_file_path))
    FileUtils.mkdir_p(File.dirname(@pot_file_path))
    FileUtils.mkdir_p(File.dirname(@rhtml_file_path))
  end

  private
  def generate(ruby_source, *command_line_options)
    File.open(@rb_file_path, "w") do |rb_file|
      rb_file.puts(ruby_source)
    end

    command_line = ["--output", @pot_file_path]
    command_line += command_line_options
    command_line += [@rb_file_path]
    @xgettext.run(*command_line)

    File.read(@pot_file_path)
  end

  def header(options=nil)
    options ||= {}
    package_name = options[:package_name] || "PACKAGE"
    package_version = options[:package_version] || "VERSION"
    msgid_bugs_address = options[:msgid_bugs_address] || ""
    copyright_year = options[:copyright_year] || "YEAR"
    copyright_holder = options[:copyright_holder] ||
                         "THE PACKAGE'S COPYRIGHT HOLDER"
    output_encoding = options[:to_code] || "UTF-8"

    time = @now.strftime("%Y-%m-%d %H:%M%z")
    <<-"EOH"
# SOME DESCRIPTIVE TITLE.
# Copyright (C) #{copyright_year} #{copyright_holder}
# This file is distributed under the same license as the #{package_name} package.
# FIRST AUTHOR <EMAIL@ADDRESS>, #{copyright_year}.
#
#, fuzzy
msgid ""
msgstr ""
"Project-Id-Version: #{package_name} #{package_version}\\n"
"Report-Msgid-Bugs-To: #{msgid_bugs_address}\\n"
"POT-Creation-Date: #{time}\\n"
"PO-Revision-Date: #{time}\\n"
"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n"
"Language-Team: LANGUAGE <LL@li.org>\\n"
"Language: \\n"
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=#{output_encoding}\\n"
"Content-Transfer-Encoding: 8bit\\n"
"Plural-Forms: nplurals=INTEGER; plural=EXPRESSION;\\n"
EOH
  end

  class TestReference < self
    def test_relative
      pot_content = generate(<<-EOR)
_("Hello")
EOR
      assert_equal(<<-EOP, pot_content)
#{header}
#: ../lib/xgettext.rb:1
msgid "Hello"
msgstr ""
EOP
    end

    def test_same_message
      pot_content = generate(<<-EOR)
_("Hello")
_("Hello")
EOR
      assert_equal(<<-EOP, pot_content)
#{header}
#: ../lib/xgettext.rb:1 ../lib/xgettext.rb:2
msgid "Hello"
msgstr ""
EOP
    end
  end

  class TestEncoding < self
    def test_different_encoding_from_current_locale
      rhtml = <<-EOR
<%#-*- coding: sjis -*-%>
<html>
<head>
<title></title>
</head>
<body>
<h1><%= _("わたし") %></h1>
</body>
</html>
EOR
      File.open(@rhtml_file_path, "w") do |rhtml_file|
        rhtml_file.puts(rhtml.encode("sjis"))
      end

      @xgettext.run("--output", @pot_file_path, @rhtml_file_path)

      encoding = "UTF-8"
      pot_content = File.read(@pot_file_path)
      pot_content.force_encoding(encoding)
      expected_content = <<-EOP
#{header}
#: ../templates/xgettext.rhtml:7
msgid "わたし"
msgstr ""
EOP
      expected_content = expected_content.encode(encoding)
      assert_equal(expected_content, pot_content)
    end

    def test_multiple_encodings
      File.open(@rb_file_path, "w") do |rb_file|
        rb_file.puts(<<-EOR.encode("euc-jp"))
# -*- coding: euc-jp -*-
_("こんにちは")
EOR
      end

      File.open(@rhtml_file_path, "w") do |rhtml_file|
        rhtml_file.puts(<<-EOR.encode("cp932"))
<%# -*- coding: cp932 -*-%>
<h1><%= _("わたし") %></h1>
EOR
      end

      @xgettext.run("--output", @pot_file_path, @rb_file_path, @rhtml_file_path)

      encoding = "UTF-8"
      pot_content = File.read(@pot_file_path)
      pot_content.force_encoding(encoding)
      expected_content = <<-EOP
#{header}
#: ../lib/xgettext.rb:2
msgid "こんにちは"
msgstr ""

#: ../templates/xgettext.rhtml:2
msgid "わたし"
msgstr ""
EOP
      expected_content = expected_content.encode(encoding)
      assert_equal(expected_content, pot_content)
    end
  end

  class TestTranslatorComment < self
    def test_n_
      pot_content = generate(<<-RB, "--add-comments=TRANSLATORS:")
n_members = 1
# TRANSLATORS: Use this message as test
n_("I will go!",
   "We will go!",
   n_members)
RB
      expected_content = <<-POT
#{header}
#. TRANSLATORS: Use this message as test
#: ../lib/xgettext.rb:3
msgid "I will go!"
msgid_plural "We will go!"
msgstr[0] ""
msgstr[1] ""
POT
      assert_equal(expected_content, pot_content)
    end

    def test_no_add_comments
      pot_content = generate(<<-RUBY)
# TRNALSTORS: There is no translator's tag
_("Message")
      RUBY

      assert_equal(<<-POT, pot_content)
#{header}
#: ../lib/xgettext.rb:2
msgid "Message"
msgstr ""
      POT
    end

    def test_add_comments_without_tag
      pot_content = generate(<<-RUBY, "--add-comments")
# There is no translator's tag but all
# comments are included.
_("Message")
      RUBY

      assert_equal(<<-POT, pot_content)
#{header}
#. There is no translator's tag but all
#. comments are included.
#: ../lib/xgettext.rb:3
msgid "Message"
msgstr ""
      POT
    end

    def test_not_started_with_tag
      pot_content = generate(<<-RUBY, "--add-comments=TRANSLATORS:")
# This comment isn't started with "TRANSLATORS:" tag
_("Message")
      RUBY

      assert_equal(<<-POT, pot_content)
#{header}
#: ../lib/xgettext.rb:2
msgid "Message"
msgstr ""
      POT
    end

    def test_not_first_line_is_started_with_tag
      pot_content = generate(<<-RUBY, "--add-comments=TRANSLATORS:")
# The first line.
# TRANSLATORS: The second line.
# The third line.
_("Message")
      RUBY

      assert_equal(<<-POT, pot_content)
#{header}
#. TRANSLATORS: The second line.
#. The third line.
#: ../lib/xgettext.rb:4
msgid "Message"
msgstr ""
      POT
    end

    def test_indented
      pot_content = generate(<<-RUBY, "--add-comments")
# The following lines are indented
#   * indented1
#   * indented2
_("Message")
      RUBY

      assert_equal(<<-POT, pot_content)
#{header}
#. The following lines are indented
#. * indented1
#. * indented2
#: ../lib/xgettext.rb:4
msgid "Message"
msgstr ""
      POT
    end

    def test_multiple
      pot_content = generate(<<-RUBY, "--add-comments=TRANSLATORS:")
# TRANSLATORS: The first comment
_("Message")

# TRANSLATORS: The second comment
_("Message")
      RUBY

      assert_equal(<<-POT, pot_content)
#{header}
#. TRANSLATORS: The first comment
#. TRANSLATORS: The second comment
#: ../lib/xgettext.rb:2 ../lib/xgettext.rb:5
msgid "Message"
msgstr ""
      POT
    end
  end

  class TestCommandLineOption < self
    def test_package_name
      package_name = "test-package"
      pot_content = generate(":hello", "--package-name", package_name)

      options = {:package_name => package_name}
      assert_equal(header(options), pot_content)
    end

    def test_package_version
      package_version = "1.2.3"
      pot_content = generate(":hello", "--package-version", package_version)

      options = {:package_version => package_version}
      assert_equal(header(options), pot_content)
    end

    def test_report_msgid_bugs_to
      msgid_bugs_address = "me@example.com"
      pot_content = generate(":hello",
                             "--msgid-bugs-address", msgid_bugs_address)

      options = {:msgid_bugs_address => msgid_bugs_address}
      assert_equal(header(options), pot_content)
    end

    def test_copyright_year
      copyright_year = "2013"
      pot_content = generate(":hello", "--copyright-year", copyright_year)

      options = {:copyright_year => copyright_year}
      assert_equal(header(options), pot_content)
    end

    def test_copyright_holder
      copyright_holder = "me"
      pot_content = generate(":hello", "--copyright-holder", copyright_holder)

      options = {:copyright_holder => copyright_holder}
      assert_equal(header(options), pot_content)
    end

    def test_to_code
      output_encoding = "EUC-JP"
      pot_content = generate(<<-EOR, "--output-encoding", output_encoding)
# -*- coding: utf-8 -*-

_("わたし")
EOR
      pot_content.force_encoding(output_encoding)

      options = {:to_code => output_encoding}
      expected_pot = <<-EOP
#{header(options)}
#: ../lib/xgettext.rb:3
msgid "わたし"
msgstr ""
EOP
      expected_pot = expected_pot.encode(output_encoding)

      assert_equal(expected_pot, pot_content)
    end

    class TestLocation < self
      def test_default
        assert_equal(<<-POT, generate("_('hello')"))
#{header}
#: ../lib/xgettext.rb:1
msgid "hello"
msgstr ""
        POT
      end

      def test_location
        assert_equal(<<-POT, generate("_('hello')", "--location"))
#{header}
#: ../lib/xgettext.rb:1
msgid "hello"
msgstr ""
        POT
      end

      def test_no_location
        assert_equal(<<-POT, generate("_('hello')", "--no-location"))
#{header}
msgid "hello"
msgstr ""
        POT
      end
    end

    class TestSort < self
      def setup
        super
        @code = <<-RUBY
        RUBY
      end

      def test_default
        assert_equal(<<-POT, generate)
#{header}
#: ../lib/xgettext.rb:1
msgid "World"
msgstr ""

#: ../lib/xgettext.rb:2
msgctxt "context"
msgid "Hello"
msgstr ""

#: ../templates/xgettext.rhtml:1
msgid "ABC"
msgstr ""

#: ../templates/xgettext.rhtml:2
msgid "123"
msgstr ""
        POT
      end

      def test_no_sort_output
        assert_equal(<<-POT, generate("--no-sort-output"))
#{header}
#: ../templates/xgettext.rhtml:1
msgid "ABC"
msgstr ""

#: ../templates/xgettext.rhtml:2
msgid "123"
msgstr ""

#: ../lib/xgettext.rb:1
msgid "World"
msgstr ""

#: ../lib/xgettext.rb:2
msgctxt "context"
msgid "Hello"
msgstr ""
        POT
      end

      def test_sort_by_file
        assert_equal(<<-POT, generate("--sort-by-file"))
#{header}
#: ../lib/xgettext.rb:1
msgid "World"
msgstr ""

#: ../lib/xgettext.rb:2
msgctxt "context"
msgid "Hello"
msgstr ""

#: ../templates/xgettext.rhtml:1
msgid "ABC"
msgstr ""

#: ../templates/xgettext.rhtml:2
msgid "123"
msgstr ""
        POT
      end

      def test_sort_by_msgid
        assert_equal(<<-POT, generate("--sort-by-msgid"))
#{header}
#: ../templates/xgettext.rhtml:2
msgid "123"
msgstr ""

#: ../templates/xgettext.rhtml:1
msgid "ABC"
msgstr ""

#: ../lib/xgettext.rb:2
msgctxt "context"
msgid "Hello"
msgstr ""

#: ../lib/xgettext.rb:1
msgid "World"
msgstr ""
        POT
      end

      private
      def generate(*command_line_options)
        File.open(@rhtml_file_path, "w") do |rhtml_file|
          rhtml_file.puts(<<-RHTML)
<%= _("ABC") %>
<%= _("123") %>
          RHTML
        end

        File.open(@rb_file_path, "w") do |rb_file|
          rb_file.puts(<<-RUBY)
_('World')
p_('context', 'Hello')
          RUBY
        end

        command_line = ["--output", @pot_file_path]
        command_line += command_line_options
        command_line += [@rhtml_file_path, @rb_file_path]
        @xgettext.run(*command_line)

        File.read(@pot_file_path)
      end
    end

    class TestWidth < self
      def msgid
        <<-MSGID.chomp
Hello very long line! This line is very long. Yes! This line is very long! Very very long line!
        MSGID
      end

      def test_default
        assert_equal(<<-POT, generate("_('#{msgid}')"))
#{header}
#: ../lib/xgettext.rb:1
msgid ""
"Hello very long line! This line is very long. Yes! This line is very long! Ver"
"y very long line!"
msgstr ""
        POT
      end

      def test_width
        assert_equal(<<-POT, generate("_('#{msgid}')", "--width", "70"))
#{header}
#: ../lib/xgettext.rb:1
msgid ""
"Hello very long line! This line is very long. Yes! This line is very l"
"ong! Very very long line!"
msgstr ""
        POT
      end

      def test_no_wrap
        assert_equal(<<-POT, generate("_('#{msgid}')", "--no-wrap"))
#{header}
#: ../lib/xgettext.rb:1
msgid "Hello very long line! This line is very long. Yes! This line is very long! Very very long line!"
msgstr ""
        POT
      end
    end
  end

  class TestAddParser < self
    setup
    def setup_default_parsers
      @default_parsers = default_parsers.dup
    end

    teardown
    def teardown_default_parsers
      default_parsers.replace(@default_parsers)
    end

    def test_class_method
      GetText::Tools::XGetText.add_parser(mock_html_parser)
      xgettext = GetText::Tools::XGetText.new
      xgettext.parse(["index.html"])
    end

    def test_old_style_parser
      parser = Object.new
      def parser.target?(path)
        true
      end
      def parser.parse(path)
        # It doesn't receive options argument as the second argument
        [["Message"]]
      end

      GetText::Tools::XGetText.add_parser(parser)
      xgettext = GetText::Tools::XGetText.new
      po_entry = GetText::POEntry.new(:normal)
      po_entry.msgid = "Message"
      assert_equal([po_entry],
                   xgettext.parse(["index.html"]).to_a)
    end

    def test_instance_method
      @xgettext.add_parser(mock_html_parser)
      @xgettext.parse(["index.html"])
    end

    private
    def default_parsers
      GetText::Tools::XGetText.module_eval("@@default_parsers")
    end

    def mock_html_parser
      html_parser = Object.new
      mock(html_parser).target?("index.html") {true}
      mock(html_parser).parse("index.html") {[]}
      html_parser
    end
  end
end
