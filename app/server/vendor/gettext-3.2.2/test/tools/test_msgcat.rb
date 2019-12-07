# Copyright (C) 2014  Kouhei Sutou <kou@clear-code.com>
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

require "stringio"
require "tempfile"

require "gettext/tools/msgcat"

class TestToolsMsgCat < Test::Unit::TestCase
  private
  def run_msgcat(input_pos, *options)
    inputs = input_pos.collect do |po|
      input = Tempfile.new("msgcat-input")
      input.write(po)
      input.close
      input
    end
    output = Tempfile.new("msgcat-output")
    command_line = ["--output", output.path]
    command_line.concat(options)
    command_line.concat(inputs.collect(&:path))
    @stdout, @stderr = capture_output do
      GetText::Tools::MsgCat.run(*command_line)
    end
    output.read
  end

  def capture_output
    original_stdout = $stdout
    original_stderr = $stderr
    begin
      stdout = StringIO.new
      stderr = StringIO.new
      $stdout = stdout
      $stderr = stderr
      yield
      [stdout.string, stderr.string]
    ensure
      $stdout = original_stdout
      $stderr = original_stderr
    end
  end

  class TestHeader < self
    def setup
      @po1 = <<-PO
msgid ""
msgstr ""
"Project-Id-Version: gettext 3.0.0\\n"
      PO
      @po2 = <<-PO
msgid ""
msgstr ""
"Language: ja\\n"
      PO
    end

    def test_default
      assert_equal(@po1, run_msgcat([@po1, @po2]))
    end
  end

  class TestNoDuplicated < self
    class TestTranslated < self
      def setup
        @po1 = <<-PO
msgid "Hello"
msgstr "Bonjour"
        PO

        @po2 = <<-PO
msgid "World"
msgstr "Monde"
        PO
      end

      def test_default
        assert_equal(<<-PO.chomp, run_msgcat([@po1, @po2]))
#{@po1}
#{@po2}
        PO
      end
    end
  end

  class TestDuplicated < self
    class TestTranslated < self
      def test_same
        po = <<-PO
msgid "Hello"
msgstr "Bonjour"
        PO

        assert_equal(po, run_msgcat([po, po]))
      end

      def test_different
        po1 = <<-PO
msgid "Hello"
msgstr "Bonjour"
        PO

        po2 = <<-PO
msgid "Hello"
msgstr "Salut"
        PO

        assert_equal(po1, run_msgcat([po1, po2]))
      end

      def test_one_not_translated
        po_not_translated = <<-PO
msgid "Hello"
msgstr ""
        PO

        po_translated = <<-PO
msgid "Hello"
msgstr "Bonjour"
        PO

        assert_equal(po_translated,
                     run_msgcat([po_not_translated, po_translated]))
      end
    end
  end

  class TestSort < self
    class TestByMsgid < self
      def setup
        @po_alice = <<-PO
msgid "Alice"
msgstr ""
        PO

        @po_bob = <<-PO
msgid "Bob"
msgstr ""
        PO

        @po_charlie = <<-PO
msgid "Charlie"
msgstr ""
        PO
      end

      def test_sort_by_msgid
        sorted_po = <<-PO.chomp
#{@po_alice}
#{@po_bob}
#{@po_charlie}
        PO
        assert_equal(sorted_po,
                     run_msgcat([@po_charlie, @po_bob, @po_alice],
                                "--sort-by-msgid"))
      end

      def test_sort_output
        sorted_po = <<-PO.chomp
#{@po_alice}
#{@po_bob}
#{@po_charlie}
        PO
        assert_equal(sorted_po,
                     run_msgcat([@po_charlie, @po_bob, @po_alice],
                                "--sort-output"))
      end

      def test_no_sort_output
        not_sorted_po = <<-PO.chomp
#{@po_charlie}
#{@po_bob}
#{@po_alice}
        PO
        assert_equal(not_sorted_po,
                     run_msgcat([@po_charlie, @po_bob, @po_alice],
                                "--no-sort-output"))
      end
    end

    class TestByReference < self
      def setup
        @po_a1 = <<-PO
#: a.rb:1
msgid "Hello 3"
msgstr ""
        PO

        @po_a2 = <<-PO
#: a.rb:2
msgid "Hello 2"
msgstr ""
        PO

        @po_b1 = <<-PO
#: b.rb:1
msgid "Hello 1"
msgstr ""
        PO
      end

      def test_sort_by_location
        sorted_po = <<-PO.chomp
#{@po_a1}
#{@po_a2}
#{@po_b1}
        PO
        assert_equal(sorted_po,
                     run_msgcat([@po_b1, @po_a2, @po_a1],
                                "--sort-by-location"))
      end

      def test_sort_by_file
        sorted_po = <<-PO.chomp
#{@po_a1}
#{@po_a2}
#{@po_b1}
        PO
        assert_equal(sorted_po,
                     run_msgcat([@po_b1, @po_a2, @po_a1],
                                "--sort-by-file"))
      end
    end
  end

  class TestComment < self
    class TestReference < self
      def setup
        @po = <<-PO
# translator comment
#: a.rb:1
msgid "Hello"
msgstr ""
        PO
      end

      def test_no_location
        assert_equal(<<-PO, run_msgcat([@po], "--no-location"))
# translator comment
msgid "Hello"
msgstr ""
        PO
      end
    end

    class TestTranslator < self
      def setup
        @po = <<-PO
# translator comment
#: a.rb:1
msgid "Hello"
msgstr ""
        PO
      end

      def test_no_translator_comment
        assert_equal(<<-PO, run_msgcat([@po], "--no-translator-comment"))
#: a.rb:1
msgid "Hello"
msgstr ""
        PO
      end
    end

    class TestExtracted < self
      def setup
        @po = <<-PO
#. extracted comment
#: a.rb:1
msgid "Hello"
msgstr ""
        PO
      end

      def test_no_extracted_comment
        assert_equal(<<-PO, run_msgcat([@po], "--no-extracted-comment"))
#: a.rb:1
msgid "Hello"
msgstr ""
        PO
      end
    end

    class TestFlag < self
      def setup
        @po = <<-PO
#, c-format
#: a.rb:1
msgid "Hello"
msgstr ""
        PO
      end

      def test_no_flag_comment
        assert_equal(<<-PO, run_msgcat([@po], "--no-flag-comment"))
#: a.rb:1
msgid "Hello"
msgstr ""
        PO
      end
    end

    class TestPrevious < self
      def setup
        @po = <<-PO
#| msgid "hello"
#: a.rb:1
msgid "Hello"
msgstr ""
        PO
      end

      def test_no_previous_comment
        assert_equal(<<-PO, run_msgcat([@po], "--no-previous-comment"))
#: a.rb:1
msgid "Hello"
msgstr ""
        PO
      end
    end

    class TestAll < self
      def setup
        @po = <<-PO
# translator comment
#. extracted comment
#: hello.rb:1
#, c-format
#| msgid "Hello"
msgid "Hello"
msgstr ""
        PO
      end

      def test_no_all_comments
        assert_equal(<<-PO, run_msgcat([@po], "--no-all-comments"))
msgid "Hello"
msgstr ""
        PO
      end
    end
  end

  class TestWidth < self
    def setup
      @po = <<-PO
msgid "long long long long long long long long long long long long long long long line"
msgstr ""
      PO
    end

    def test_default
      assert_equal(<<-PO, run_msgcat([@po]))
msgid ""
"long long long long long long long long long long long long long long long lin"
"e"
msgstr ""
      PO
    end

    def test_width
      assert_equal(<<-PO, run_msgcat([@po], "--width=40"))
msgid ""
"long long long long long long long long "
"long long long long long long long line"
msgstr ""
      PO
    end


    def test_wrap
      assert_equal(<<-PO, run_msgcat([@po], "--wrap"))
msgid ""
"long long long long long long long long long long long long long long long lin"
"e"
msgstr ""
      PO
    end

    def test_no_wrap
      assert_equal(<<-PO, run_msgcat([@po], "--no-wrap"))
msgid "long long long long long long long long long long long long long long long line"
msgstr ""
      PO
    end
  end

  class TestFuzzy < self
    class TestAllFuzzy < self
      def setup
        @po_fuzzy1 = <<-PO
#, fuzzy
msgid "Hello"
msgstr "Bonjour1"
        PO

        @po_fuzzy2 = <<-PO
#, fuzzy
msgid "Hello"
msgstr "Bonjour2"
        PO
      end

      def test_default
        assert_equal(<<-PO, run_msgcat([@po_fuzzy1, @po_fuzzy2]))
#, fuzzy
msgid "Hello"
msgstr "Bonjour1"
        PO
      end

      def test_no_fuzzy
        assert_equal("", run_msgcat([@po_fuzzy1, @po_fuzzy2], "--no-fuzzy"))
      end
    end

    class TestHaveNoFuzzy < self
      def setup
        @po_fuzzy = <<-PO
#, fuzzy
msgid "Hello"
msgstr "Bonjour1"
        PO

        @po_not_fuzzy = <<-PO
msgid "Hello"
msgstr "Bonjour2"
        PO
      end

      def test_default
        assert_equal(<<-PO, run_msgcat([@po_fuzzy, @po_not_fuzzy]))
msgid "Hello"
msgstr "Bonjour2"
        PO
      end

      def test_no_fuzzy
        assert_equal(<<-PO, run_msgcat([@po_fuzzy, @po_not_fuzzy], "--no-fuzzy"))
msgid "Hello"
msgstr "Bonjour2"
        PO
      end
    end

    class TestNoAllComments < self
      def setup
        @po_fuzzy1 = <<-PO
#, fuzzy
msgid "Hello"
msgstr "Bonjour1"
        PO

        @po_fuzzy2 = <<-PO
#, fuzzy
msgid "Hello"
msgstr "Bonjour2"
        PO
      end

      def test_default
        pos = [@po_fuzzy1, @po_fuzzy2]
        assert_equal(<<-PO, run_msgcat(pos, "--no-all-comments"))
msgid "Hello"
msgstr "Bonjour1"
        PO
      end

      def test_no_fuzzy
        pos = [@po_fuzzy1, @po_fuzzy2]
        assert_equal("", run_msgcat(pos, "--no-all-comments", "--no-fuzzy"))
      end
    end
  end

  class TestWarning < self
    def setup
      @po_fuzzy = <<-PO
#, fuzzy
msgid "Hello World"
msgstr "Bonjour"
      PO
    end

    def test_default
      run_msgcat([@po_fuzzy])
      assert_not_empty(@stderr)
    end

    def test_no_report_warning
      run_msgcat([@po_fuzzy], "--no-report-warning")
      assert_empty(@stderr)
    end
  end

  class TestObsoleteEntries < self
    def setup
      @po_obsolete = <<-PO
#~ msgid "Hello World"
#~ msgstr "Bonjour"
      PO
    end

    def test_default
      assert_equal(<<-PO, run_msgcat([@po_obsolete]))
#~ msgid "Hello World"
#~ msgstr "Bonjour"
      PO
    end

    def test_no_obsolete_entries
      assert_equal("", run_msgcat([@po_obsolete], "--no-obsolete-entries"))
    end
  end

  class TestRemoveHeaderField < self
    def setup
      @po_header = <<-PO
msgid ""
msgstr ""
"Project-Id-Version: gettext 3.0.0\\n"
"POT-Creation-Date: 2014-02-23 19:02+0900\\n"
"Language: ja\\n"
      PO
    end

    def test_one
      options = ["--remove-header-field=POT-Creation-Date"]
      assert_equal(<<-PO, run_msgcat([@po_header], *options))
msgid ""
msgstr ""
"Project-Id-Version: gettext 3.0.0\\n"
"Language: ja\\n"
      PO
    end

    def test_multiple
      options = [
        "--remove-header-field=Project-Id-Version",
        "--remove-header-field=POT-Creation-Date",
      ]
      assert_equal(<<-PO, run_msgcat([@po_header], *options))
msgid ""
msgstr ""
"Language: ja\\n"
      PO
    end
  end
end
