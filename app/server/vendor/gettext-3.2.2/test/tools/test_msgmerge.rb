# -*- coding: utf-8 -*-
#
# Copyright (C) 2012-2015  Kouhei Sutou <kou@clear-code.com>
# Copyright (C) 2010  Eddie Lau <tatonlto@gmail.com>
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

require 'gettext/tools/msgmerge'

class TestToolsMsgMerge < Test::Unit::TestCase
  class TestMerger < self
    def setup
      @po = GetText::PO.new
      @pot = GetText::PO.new
      @config = GetText::Tools::MsgMerge::Config.new
    end

    def parse_po(po, output)
      parser = GetText::POParser.new
      parser.report_warning = false
      parser.parse(po, output)
    end

    def merge
      merger = GetText::Tools::MsgMerge::Merger.new(@pot, @po, @config)
      merger.merge
    end

    class TestUpdate < self
      def test_msgstr
        pot = <<-POT
msgid "hello"
msgstr ""
        POT
        po = <<-PO
msgid "hello"
msgstr "bonjour"
        PO
        merged_po = merge(pot, po)

        assert_equal(<<-PO, merged_po)
msgid "hello"
msgstr "bonjour"
        PO
      end

      private
      def merge(pot, po)
        parse_po(pot, @pot)
        parse_po(po, @po)
        super().to_s
      end
    end

    class TestObosleteEntry < self
      def test_in_po
        pot = <<-POT
msgid "hello"
msgstr ""
        POT
        po = <<-PO
msgid "hello"
msgstr "bonjour"

#~ msgid "he"
#~ msgstr "il"
        PO

        assert_equal(<<-PO, merge(pot, po))
msgid "hello"
msgstr "bonjour"

#~ msgid "he"
#~ msgstr "il"
        PO
      end

      private
      def merge(pot, po)
        parse_po(pot, @pot)
        parse_po(po, @po)
        super().to_s
      end
    end

    def test_different_msgstr
      @po["hello"] = "salut"
      @pot["hello"] = "bonjour"
      merged_po = merge

      assert_equal("salut", merged_po["hello"].msgstr)
    end

    def test_translator_comment
      @po["hello"] = generate_entry(:msgid => "hello",
                                    :msgstr => "bonjour",
                                    :translator_comment => "comment")

      @pot["hello"] = generate_entry(:msgid => "hello",
                                     :msgstr => "",
                                     :translator_comment => "It's comments")

      merged_po = merge
      assert_equal("bonjour", merged_po["hello"].msgstr)
      assert_equal("comment", merged_po["hello"].translator_comment)
    end

    def test_extracted_comment
      @po["hello"] = generate_entry(:msgid => "hello",
                                    :msgstr => "bonjour",
                                    :extracted_comment => "comment")

      @pot["hello"] = generate_entry(:msgid => "hello",
                                     :msgstr => "",
                                     :extracted_comment => "extracted comments")

      merged_po = merge
      assert_equal("bonjour", merged_po["hello"].msgstr)
      assert_equal("extracted comments", merged_po["hello"].extracted_comment)
    end

    def test_references
      references = ["file.rb:10", "helper.rb:10"]
      pot_references = ["file.rb:10", "test.rb:25"]
      @po["hello"] = generate_entry(:msgid => "hello",
                                    :msgstr => "bonjour",
                                    :references => references)

      @pot["hello"] = generate_entry(:msgid => "hello",
                                     :msgstr => "",
                                     :references => pot_references)

      merged_po = merge
      assert_equal("bonjour", merged_po["hello"].msgstr)
      assert_equal(pot_references, merged_po["hello"].references)
    end

    def test_flag
      @po["hello"] = generate_entry(:msgid => "hello",
                                    :msgstr => "bonjour",
                                    :flag => "c-format")

      @pot["hello"] = generate_entry(:msgid => "hello",
                                     :msgstr => "",
                                     :flag => "no-c-format")

      merged_po = merge
      assert_equal("bonjour", merged_po["hello"].msgstr)
      assert_equal("no-c-format", merged_po["hello"].flag)
    end

    def test_previous
      @po["hello"] = generate_entry(:msgid => "hello",
                                    :msgstr => "bonjour",
                                    :previous => "hi")

      @pot["hello"] = generate_entry(:msgid => "hello",
                                     :msgstr => "")

      merged_po = merge
      assert_equal("bonjour", merged_po["hello"].msgstr)
      assert_equal(nil, merged_po["hello"].previous)
    end

    class TestAddNoFuzzy < self
      def test_add_to_nontranslated_entry
        @po["helol"] = generate_entry(:msgid => "helol",
                                      :msgstr => nil)
        @pot["hello"] = generate_entry(:msgid => "hello",
                                       :msgstr => nil)
        merged_po = merge
        assert_true(merged_po.has_key?("hello"))
        assert_nil(merged_po["hello"].flag)
      end

      def test_fuzzy_header
        @po[""] = generate_entry(:msgid => "",
                                 :msgstr => "header\nentry",
                                 :translator_comment => "header comment")

        @pot[""] = generate_entry(:msgid => "",
                                  :msgstr => "uninitialized\ncomment",
                                  :translator_comment => "uninitialized comment",
                                  :flag => "fuzzy")

        merged_po = merge
        assert_equal("header\nentry", merged_po[""].msgstr)
        assert_equal("header comment", merged_po[""].translator_comment)
        assert_equal(nil, merged_po[""].flag)
      end

      def test_fuzzy_header_including_pot_creation_date
        creation_date_mark = "POT-Creation-Date: "
        po_creation_date = "#{creation_date_mark}2012-11-15 08:15+0900"
        pot_creation_date = "#{creation_date_mark}2012-11-20 14:15+0900"
        @po[""] = generate_entry(:msgid => "",
                                 :msgstr => po_creation_date,
                                 :translator_comment => "header comment")

        @pot[""] = generate_entry(:msgid => "",
                                  :msgstr => pot_creation_date,
                                  :translator_comment => "header comment",
                                  :flag => "fuzzy")

        merged_po = merge
        assert_equal(pot_creation_date, merged_po[""].msgstr)
      end

      def test_already_fuzzy_po
        @po["hello"] = generate_entry(:msgid => "hello",
                                      :msgstr => "bonjour",
                                      :flag => "fuzzy")
        @pot["hello"] = generate_entry(:msgid => "hello",
                                       :msgstr => nil)
        @pot["helol"] = generate_entry(:msgid => "helol",
                                       :msgstr => nil,
                                       :flag => "fuzzy")
        merged_po = merge
        assert_true(merged_po.has_key?("helol"))
        assert_equal(["fuzzy"], merged_po["helol"].flags)
      end

      def test_already_fuzzy_pot
        @po["hello"] = generate_entry(:msgid => "hello",
                                      :msgstr => "bonjour")
        @pot["helol"] = generate_entry(:msgid => "helol",
                                       :msgstr => nil,
                                       :flag => "fuzzy")
        merged_po = merge
        assert_true(merged_po.has_key?("helol"))
        assert_equal(["fuzzy"], merged_po["helol"].flags)
      end
    end

    class TestAddFuzzy < self
      def test_nonexistent_msgctxt
        @po["normal", "hello"] = generate_entry(:msgctxt => "normal",
                                                :msgid => "hello",
                                                :msgstr => "salut")
        @pot["hello"] = generate_entry(:msgid => "hello",
                                       :msgstr => "")
        merged_po = merge

        assert_false(merged_po.has_key?("normal", "hello"))
        assert_true(merged_po.has_key?("hello"))
        assert_equal("salut", merged_po["hello"].msgstr)
        assert_equal("fuzzy", merged_po["hello"].flag)
      end

      def test_msgid_plural
        @po["he"] = generate_entry(:msgid => "he",
                                   :msgid_plural => "thye",
                                   :msgstr => "il\000ils")
        @pot["he"] = generate_entry(:msgid => "he",
                                    :msgid_plural => "they",
                                    :msgstr => "")
        merged_po = merge

        assert_equal("il\000ils", merged_po["he"].msgstr)
        assert_equal("they", merged_po["he"].msgid_plural)
        assert_equal("fuzzy", merged_po["he"].flag)
      end

      def test_fuzzy_matching_entry
        @po["helol"] = "bonjour"
        @pot["hello"] = ""
        merged_po = merge

        assert_false(merged_po.has_key?("helol"))
        assert_true(merged_po.has_key?("hello"))
        assert_equal("bonjour", merged_po["hello"].msgstr)
        assert_equal("fuzzy", merged_po["hello"].flag)
      end

      def test_fuzzy_matching_included_source
        base_msgid = "hello"
        new_msgid = base_msgid + ("!" * 10)
        @po[base_msgid] = "bonjour"
        @pot[new_msgid] = ""
        merged_po = merge

        puts merged_po
        assert_equal("bonjour", merged_po[new_msgid].msgstr)
        assert_equal("fuzzy", merged_po[new_msgid].flag)
      end

      def test_merged_entry_from_fuzzy_entry
        @po["hello"] = generate_entry(:msgid => "hello",
                                      :msgstr => "bonjuor",
                                      :flag => "fuzzy")

        @pot["hello"] = generate_entry(:msgid => "hello",
                                       :msgstr => "")

        merged_po = merge
        assert_equal("bonjuor", merged_po["hello"].msgstr)
        assert_equal("fuzzy", merged_po["hello"].flag)
      end
    end

    def test_obsolete_entry
      @po["hello"] = "bonjour"
      @pot["hi"] = "salut"
      merged_po = merge

      assert_equal("salut", merged_po["hi"].msgstr)
      assert_false(merged_po.has_key?("hello"))

      obsolete_comment = <<-EOC
msgid "hello"
msgstr "bonjour"
EOC
      assert_equal(obsolete_comment, merged_po[:last].comment)
    end

    private
    def generate_entry(options)
      msgctxt = options[:msgctxt]
      msgid_plural = options[:msgid_plural]
      type = detect_entry_type(msgctxt, msgid_plural)

      entry = GetText::POEntry.new(type)
      entry.translator_comment = options[:translator_comment]
      entry.extracted_comment = options[:extracted_comment]
      entry.references = options[:references] || []
      entry.flag = options[:flag]
      entry.previous = options[:previous]
      entry.msgctxt = msgctxt
      entry.msgid = options[:msgid]
      entry.msgid_plural = msgid_plural
      entry.msgstr = options[:msgstr]
      entry.comment = options[:comment]
      entry
    end

    def detect_entry_type(msgctxt, msgid_plural)
      if msgctxt.nil?
        if msgid_plural.nil?
          :normal
        else
          :plural
        end
      else
        if msgid_plural.nil?
          :msgctxt
        else
          :msgctxt_plural
        end
      end
    end
  end

  class TestCommand < self
    include GetTextTestUtils

    def setup
      @msgmerge = GetText::Tools::MsgMerge.new
    end

    setup :setup_tmpdir
    teardown :teardown_tmpdir

    setup
    def setup_paths
      @pot_file_path = File.join(@tmpdir, "po", "msgmerge.pot")
      @po_file_path = File.join(@tmpdir, "po", "ja", "msgmerge.po")
      FileUtils.mkdir_p(File.dirname(@po_file_path))
    end

    setup
    def setup_content
      @pot_formatted_time = "2012-08-19 19:08+0900"
      @po_formatted_time = "2012-08-19 18:59+0900"
      File.open(@pot_file_path, "w") do |pot_file|
        pot_file.puts(pot_content)
      end

      File.open(@po_file_path, "w") do |po_file|
        po_file.puts(po_content)
      end
    end

    private
    def pot_content
      <<-EOP
# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER
# This file is distributed under the same license as the PACKAGE package.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
msgid ""
msgstr ""
"Project-Id-Version: PACKAGE VERSION\\n"
"POT-Creation-Date: #{@pot_formatted_time}\\n"
"PO-Revision-Date: #{@pot_formatted_time}\\n"
"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n"
"Language-Team: LANGUAGE <LL@li.org>\\n"
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=UTF-8\\n"
"Content-Transfer-Encoding: 8bit\\n"
"Plural-Forms: nplurals=INTEGER; plural=EXPRESSION;\\n"

#: hello.rb:2
msgid "World"
msgstr ""

#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:3
msgid "Good-bye"
msgstr ""
EOP
    end

    def po_header(creation_date, revision_date)
      <<-EOH
# Hello Application.
# Copyright (C) 2012 Kouhei Sutou <kou@clear-code.com>
# This file is distributed under the same license as the Hello package.
# Kouhei Sutou <kou@clear-code.com> , 2012.
#
msgid ""
msgstr ""
"Project-Id-Version: Hello 1.0.0\\n"
"POT-Creation-Date: #{creation_date}\\n"
"PO-Revision-Date: #{revision_date}\\n"
"Last-Translator: Kouhei Sutou <kou@clear-code.com>\\n"
"Language-Team: Japanese <hello-ja@example.com>\\n"
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=UTF-8\\n"
"Content-Transfer-Encoding: 8bit\\n"
"Plural-Forms: nplurals=1; plural=0;\\n"
EOH
    end

    def po_content
      <<-EOP
#{po_header(@po_formatted_time, @po_formatted_time)}

#: hello.rb:1
msgid "World"
msgstr "Translated World"
EOP
    end

    class TestFuzzy < self
      class TestHeader < self
        def test_remove_fuzzy_from_pot
          @msgmerge.run("--update", @po_file_path, @pot_file_path)
          assert_equal(<<-EOP, File.read(@po_file_path))
#{po_header(@pot_formatted_time, @po_formatted_time)}
#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:2
msgid "World"
msgstr "Translated World"

#: hello.rb:3
msgid "Good-bye"
msgstr ""
EOP
        end
      end

      class TestMessage < self
        def po_content
          <<-PO
#{po_header(@po_formatted_time, @po_formatted_time)}

#, fuzzy
msgid "World"
msgstr "Translated World"
         PO
        end

        def test_merge_metadata
          @msgmerge.run("--update", @po_file_path, @pot_file_path)
          assert_equal(<<-PO, File.read(@po_file_path))
#{po_header(@pot_formatted_time, @po_formatted_time)}
#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:2
#, fuzzy
msgid "World"
msgstr "Translated World"

#: hello.rb:3
msgid "Good-bye"
msgstr ""
          PO
        end
      end
    end

    class TestSort < self
      def test_default
        @msgmerge.run("--update", @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
#{po_header(@pot_formatted_time, @po_formatted_time)}
#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:2
msgid "World"
msgstr "Translated World"

#: hello.rb:3
msgid "Good-bye"
msgstr ""
        PO
      end

      def test_sort_output
        @msgmerge.run("--update",
                      "--sort-output",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
#{po_header(@pot_formatted_time, @po_formatted_time)}
#: hello.rb:3
msgid "Good-bye"
msgstr ""

#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:2
msgid "World"
msgstr "Translated World"
        PO
      end

      def test_sort_by_file
        @msgmerge.run("--update",
                      "--sort-by-file",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
#{po_header(@pot_formatted_time, @po_formatted_time)}
#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:2
msgid "World"
msgstr "Translated World"

#: hello.rb:3
msgid "Good-bye"
msgstr ""
        PO
      end

      def test_sort_by_location
        @msgmerge.run("--update",
                      "--sort-by-location",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
#{po_header(@pot_formatted_time, @po_formatted_time)}
#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:2
msgid "World"
msgstr "Translated World"

#: hello.rb:3
msgid "Good-bye"
msgstr ""
        PO
      end

      def test_sort_by_msgid
        @msgmerge.run("--update",
                      "--sort-by-msgid",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
#{po_header(@pot_formatted_time, @po_formatted_time)}
#: hello.rb:3
msgid "Good-bye"
msgstr ""

#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:2
msgid "World"
msgstr "Translated World"
        PO
      end
    end

    class TestLocation < self
      def test_location
        @msgmerge.run("--update",
                      "--location",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
#{po_header(@pot_formatted_time, @po_formatted_time)}
#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:2
msgid "World"
msgstr "Translated World"

#: hello.rb:3
msgid "Good-bye"
msgstr ""
        PO
      end

      def test_no_location
        @msgmerge.run("--update",
                      "--no-location",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
#{po_header(@pot_formatted_time, @po_formatted_time)}
msgid "Hello"
msgstr ""

msgid "World"
msgstr "Translated World"

msgid "Good-bye"
msgstr ""
        PO
      end
    end

    class TestWidth < self
      def pot_content
        <<-POT
#: hello.rb:1
msgid "Hello very long line! This line is very long. Yes! This line is very long! Very very long line!"
msgstr ""

#: hello.rb:3
msgid "Good-bye"
msgstr ""
        POT
      end

      def po_content
        <<-PO
#: hello.rb:3
msgid "Good-bye"
msgstr "Translated Good-bye. This translation is very long. Yes! Very long translation!!!"
        PO
      end

      def test_default
        @msgmerge.run("--update",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
#: hello.rb:1
msgid ""
"Hello very long line! This line is very long. Yes! This line is very long! Ver"
"y very long line!"
msgstr ""

#: hello.rb:3
msgid "Good-bye"
msgstr ""
"Translated Good-bye. This translation is very long. Yes! Very long translation"
"!!!"
        PO
      end

      def test_width
        @msgmerge.run("--update",
                      "--width", "70",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
#: hello.rb:1
msgid ""
"Hello very long line! This line is very long. Yes! This line is very l"
"ong! Very very long line!"
msgstr ""

#: hello.rb:3
msgid "Good-bye"
msgstr ""
"Translated Good-bye. This translation is very long. Yes! Very long tra"
"nslation!!!"
        PO
      end

      def test_no_wrap
        @msgmerge.run("--update",
                      "--no-wrap",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
#: hello.rb:1
msgid "Hello very long line! This line is very long. Yes! This line is very long! Very very long line!"
msgstr ""

#: hello.rb:3
msgid "Good-bye"
msgstr "Translated Good-bye. This translation is very long. Yes! Very long translation!!!"
        PO
      end
    end

    class TestFuzzyMatching < self
      def pot_content
        <<-POT
msgid "Hello"
msgstr ""
        POT
      end

      def po_content
        <<-PO
msgid "Hello!"
msgstr "Bonjour!"
        PO
      end

      def test_default
        @msgmerge.run("--update",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
#, fuzzy
msgid "Hello"
msgstr "Bonjour!"

#~ msgid "Hello!"
#~ msgstr "Bonjour!"
        PO
      end

      def test_fuzzy_matching
        @msgmerge.run("--update",
                      "--fuzzy-matching",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
#, fuzzy
msgid "Hello"
msgstr "Bonjour!"

#~ msgid "Hello!"
#~ msgstr "Bonjour!"
        PO
      end

      def test_no_fuzzy_matching
        @msgmerge.run("--update",
                      "--no-fuzzy-matching",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
msgid "Hello"
msgstr ""

#~ msgid "Hello!"
#~ msgstr "Bonjour!"
        PO
      end
    end

    class TestObsoleteEntries < self
      def pot_content
        <<-POT
msgid "World"
msgstr ""
        POT
      end

      def po_content
        <<-PO
msgid "Hello!"
msgstr "Bonjour!"
        PO
      end

      def test_default
        @msgmerge.run("--update",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
msgid "World"
msgstr ""

#~ msgid "Hello!"
#~ msgstr "Bonjour!"
        PO
      end

      def test_no_obsolete_entries
        @msgmerge.run("--update",
                      "--no-obsolete-entries",
                      @po_file_path, @pot_file_path)
        assert_equal(<<-PO, File.read(@po_file_path))
msgid "World"
msgstr ""
        PO
      end
    end
  end
end
