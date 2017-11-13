# -*- coding: utf-8 -*-
#
# Copyright (C) 2012  Haruka Yoshihara <yoshihara@clear-code.com>
# Copyright (C) 2012-2013  Kouhei Sutou <kou@clear-code.com>
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

require "gettext/po"

class TestPO < Test::Unit::TestCase
  def setup
    @po = nil
  end

  class TestHasKey < self
    def setup
      @po = GetText::PO.new
    end

    def test_msgid_exist
      @po["msgid"] = "msgstr"

      assert_true(@po.has_key?("msgid"))
      assert_true(@po.has_key?(nil, "msgid"))
    end

    def test_msgid_notexistent
      assert_false(@po.has_key?("msgid"))
      assert_false(@po.has_key?(nil, "msgid"))
    end

    def test_msgctxt_and_msgid_exist
      @po["msgctxt", "msgid"] = "msgstr"

      assert_false(@po.has_key?("msgid"))
      assert_true(@po.has_key?("msgctxt", "msgid"))
    end

    def test_wrong_arguments
      @po["msgctxt", "msgid"] = "msgstr"

      assert_raise(ArgumentError) do
        @po.has_key?("msgctxt", "msgid", "wrong_argument")
      end
    end
  end

  class TestEach < self
    def setup
      @hello = "hello"
      @hello_translation = "bonjour"
      @he = "he"
      @he_translation = "il"

      @po = GetText::PO.new
      @po[@hello] = @hello_translation
      @po[@he] = @he_translation
    end

    def test_block_given
      entries = []
      @po.each do |entry|
        entries << entry
      end

      entries = entries.sort_by do |entry|
        entry.msgid
      end

      assert_equal(expected_entries, entries)
    end

    def test_no_block_given
      entries = @po.each.sort_by do |entry|
        entry.msgid
      end

      assert_equal(expected_entries, entries)
    end

    private
    def expected_entries
      he_entry = POEntry.new(:normal)
      he_entry.msgid = @he
      he_entry.msgstr = @he_translation
      hello_entry = POEntry.new(:normal)
      hello_entry.msgid = @hello
      hello_entry.msgstr = @hello_translation

      [he_entry, hello_entry]
    end
  end

  class TestSetEntry < self
    def test_normal
      msgid = "msgid"
      msgstr = "msgstr"

      @po = GetText::PO.new
      @po[msgid] = msgstr

      entry = POEntry.new(:normal)
      entry.msgid = msgid
      entry.msgstr = msgstr
      assert_equal(entry, @po[msgid])
    end

    def test_msgctxt
      msgctxt = "msgctxt"
      msgid = "msgid"
      msgstr = "msgstr"

      @po = GetText::PO.new
      @po[msgctxt, msgid] = msgstr

      entry = POEntry.new(:msgctxt)
      entry.msgctxt = msgctxt
      entry.msgid = msgid
      entry.msgstr = msgstr
      assert_equal(entry, @po[msgctxt, msgid])
    end

    def test_wrong_arguments
      msgctxt = "msgctxt"
      msgid = "msgid"
      msgstr = "msgstr"

      @po = GetText::PO.new
      assert_raise(ArgumentError) do
        @po[msgctxt, msgid, "wrong argument"] = msgstr
      end
    end

    def test_update_existing_entry
      test_normal

      msgid = "msgid"
      new_msgstr = "new_msgstr"
      @po[msgid] = new_msgstr

      entry = POEntry.new(:normal)
      entry.msgid = msgid
      entry.msgstr = new_msgstr
      assert_equal(entry, @po[msgid])
    end

    def test_po_entry
      @po = GetText::PO.new

      msgid = "msgid"
      msgstr = "msgstr"
      entry = POEntry.new(:normal)
      entry.msgid = msgid
      entry.msgstr = msgstr

      @po[msgid] = entry
      assert_true(@po.has_key?(nil, msgid))
      assert_equal(msgstr, @po[msgid].msgstr)
    end
  end

  class TestComment < self
    def test_add
      msgid = "msgid"
      comment = "comment"

      @po = GetText::PO.new
      @po.set_comment(msgid, comment)

      entry = POEntry.new(:normal)
      entry.msgid = msgid
      entry.comment = comment
      assert_equal(entry, @po[msgid])
      assert_equal(nil, @po[msgid].msgstr)
    end

    def test_add_to_existing_entry
      msgid = "msgid"
      msgstr = "msgstr"
      @po = GetText::PO.new
      @po[msgid] = msgstr

      comment = "comment"
      @po.set_comment(msgid, comment)

      entry = POEntry.new(:normal)
      entry.msgid = msgid
      entry.msgstr = msgstr
      entry.comment = comment
      assert_equal(entry, @po[msgid])
    end
  end

  class TestOrder < self
    def setup
      @po = GetText::PO.new
      parser = GetText::POParser.new
      parser.parse(<<-PO, @po)
#: hello.rb:2
msgid "World"
msgstr ""

#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:3
msgid "Hello World"
msgstr ""
      PO
    end

    def test_nil
      @po.order = nil
      assert_equal(<<-PO, @po.to_s)
#: hello.rb:2
msgid "World"
msgstr ""

#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:3
msgid "Hello World"
msgstr ""
      PO
    end

    def test_msgid
      @po.order = :msgid
      assert_equal(<<-PO, @po.to_s)
#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:3
msgid "Hello World"
msgstr ""

#: hello.rb:2
msgid "World"
msgstr ""
      PO
    end

    def test_reference
      @po.order = :reference
      assert_equal(<<-PO, @po.to_s)
#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:2
msgid "World"
msgstr ""

#: hello.rb:3
msgid "Hello World"
msgstr ""
      PO
    end

    def test_references
      raise "Remove :references support!" if GetText::VERSION >= "4.0.0"
      @po.order = :references
      assert_equal(<<-PO, @po.to_s)
#: hello.rb:1
msgid "Hello"
msgstr ""

#: hello.rb:2
msgid "World"
msgstr ""

#: hello.rb:3
msgid "Hello World"
msgstr ""
      PO
    end
  end

  class TestToS < self
    def setup
      @po = GetText::PO.new
    end

    class TestHeader < self
      def test_no_entry
        @po[""] = <<-HEADER
Project-Id-Version: test 1.0.0
POT-Creation-Date: 2012-10-31 12:40+0900
PO-Revision-Date: 2012-11-01 17:46+0900
Last-Translator: FULLNAME <MAIL@ADDRESS>
Language-Team: Japanese
Language: 
MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
Plural-Forms: nplurals=2; plural=(n != 1)
        HEADER
        @po[""].translator_comment = <<-HEADER_COMMENT
Japanese translations for test package.
Copyright (C) 2012 THE PACKAGE'S COPYRIGHT HOLDER
This file is distributed under the same license as the PACKAGE package.
FULLNAME <MAIL@ADDRESS>, 2012.

        HEADER_COMMENT

        assert_equal(<<-PO, @po.to_s)
# Japanese translations for test package.
# Copyright (C) 2012 THE PACKAGE'S COPYRIGHT HOLDER
# This file is distributed under the same license as the PACKAGE package.
# FULLNAME <MAIL@ADDRESS>, 2012.
#
msgid ""
msgstr ""
"Project-Id-Version: test 1.0.0\\n"
"POT-Creation-Date: 2012-10-31 12:40+0900\\n"
"PO-Revision-Date: 2012-11-01 17:46+0900\\n"
"Last-Translator: FULLNAME <MAIL@ADDRESS>\\n"
"Language-Team: Japanese\\n"
"Language: \\n"
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=UTF-8\\n"
"Content-Transfer-Encoding: 8bit\\n"
"Plural-Forms: nplurals=2; plural=(n != 1)\\n"
        PO
      end
    end

    class TestReferenceComment < self
      def test_same_filename
        hello = "hello"
        hello_translation = "こんにちは"
        hello_references = ["file.rb:10"]
        hello_comment = "#: file.rb:10"
        bye = "bye"
        bye_translation = "さようなら"
        bye_references = ["file.rb:20"]
        bye_comment = "#: file.rb:20"

        @po[hello] = hello_translation
        @po[hello].references = hello_references

        @po[bye] = bye_translation
        @po[bye].references = bye_references

        expected_po =<<EOP
#{hello_comment}
msgid "#{hello}"
msgstr "#{hello_translation}"

#{bye_comment}
msgid "#{bye}"
msgstr "#{bye_translation}"
EOP
        assert_equal(expected_po, @po.to_s)
      end

      def test_different_filename
        hello = "hello"
        hello_translation = "こんにちは"
        hello_references = ["file.rb:10"]
        hello_comment = "#: file.rb:10"
        bye = "bye"
        bye_translation = "さようなら"
        bye_references = ["test.rb:10"]
        bye_comment = "#: test.rb:10"

        @po[hello] = hello_translation
        @po[hello].references = hello_references

        @po[bye] = bye_translation
        @po[bye].references = bye_references

        expected_po =<<EOP
#{hello_comment}
msgid "#{hello}"
msgstr "#{hello_translation}"

#{bye_comment}
msgid "#{bye}"
msgstr "#{bye_translation}"
EOP
        assert_equal(expected_po, @po.to_s)
      end

      def test_including_colon_filename
        hello = "hello"
        hello_translation = "こんにちは"
        hello_references = ["file.rb:10"]
        hello_comment = "#: file.rb:10"
        bye = "bye"
        bye_translation = "さようなら"
        bye_references = ["file:2.rb:10"]
        bye_comment = "#: file:2.rb:10"

        @po[hello] = hello_translation
        @po[hello].references = hello_references

        @po[bye] = bye_translation
        @po[bye].references = bye_references

        expected_po =<<EOP
#{hello_comment}
msgid "#{hello}"
msgstr "#{hello_translation}"

#{bye_comment}
msgid "#{bye}"
msgstr "#{bye_translation}"
EOP
        assert_equal(expected_po, @po.to_s)
      end

      def test_no_file_number
        hello = "hello"
        hello_translation = "こんにちは"
        hello_references = ["file.rb"]
        hello_comment = "#: file.rb"
        bye = "bye"
        bye_translation = "さようなら"
        bye_references = ["test.rb"]
        bye_comment = "#: test.rb"

        @po[hello] = hello_translation
        @po[hello].references = hello_references

        @po[bye] = bye_translation
        @po[bye].references = bye_references

        expected_po =<<EOP
#{hello_comment}
msgid "#{hello}"
msgstr "#{hello_translation}"

#{bye_comment}
msgid "#{bye}"
msgstr "#{bye_translation}"
EOP
        assert_equal(expected_po, @po.to_s)
      end

      def test_multiple_filename
        hello = "hello"
        hello_translation = "こんにちは"
        hello_references = ["file.rb:10"]
        hello_comment = "#: file.rb:10"
        bye = "bye"
        bye_translation = "さようなら"
        bye_references = ["test.rb:10", "file.rb:110", "file.rb:20"]
        bye_comment = "#: file.rb:20 file.rb:110 test.rb:10"

        @po[hello] = hello_translation
        @po[hello].references = hello_references

        @po[bye] = bye_translation
        @po[bye].references = bye_references

        expected_po =<<EOP
#{hello_comment}
msgid "#{hello}"
msgstr "#{hello_translation}"

#{bye_comment}
msgid "#{bye}"
msgstr "#{bye_translation}"
EOP
        assert_equal(expected_po, @po.to_s)
      end

      class TestIncludeReferenceComment < self
        def setup
          super
          @po["Hi"] = "Bonjour"
          @po["Hi"].references = ["hi.rb:29"]
        end

        def test_false
          assert_equal(<<-PO, @po.to_s(:include_reference_comment => false))
msgid "Hi"
msgstr "Bonjour"
          PO
        end
      end
    end

    class TestObsoleteComment < self
      def setup
        @po = GetText::PO.new
        @po["hello"] = ""
        @po.set_comment(:last, <<-OBSOLETE_COMMENT)
# hello.rb:20
msgid "hi"
msgstr "Bonjour"
        OBSOLETE_COMMENT
      end

      def test_default
        assert_equal(<<-PO, @po.to_s)
msgid "hello"
msgstr ""

# hello.rb:20
#~ msgid "hi"
#~ msgstr "Bonjour"
        PO
      end
    end
  end

  class TestEmpty < self
    def setup
      @po = GetText::PO.new
    end

    def test_true
      assert_true(@po.empty?)
    end

    def test_false
      @po["Hello"] = "Bonjour"
      assert_false(@po.empty?)
    end
  end
end
