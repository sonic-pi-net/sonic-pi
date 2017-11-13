# -*- coding: utf-8 -*-
#
# Copyright (C) 2012  Haruka Yoshihara <yoshihara@clear-code.com>
# Copyright (C) 2012-2013  Kouhei Sutou <kou@clear-code.com>
# Copyright (C) 2010  masone (Christian Felder) <ema@rh-productions.ch>
# Copyright (C) 2009  Vladimir Dobriakov <vladimir@geekq.net>
# Copyright (C) 2009-2010  Masao Mutoh
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

require "tempfile"
require "gettext/tools/parser/ruby"
require "gettext/tools/parser/glade"
require "gettext/tools/parser/erb"

require "gettext/tools/xgettext"

class TestGetTextParser < Test::Unit::TestCase
  def setup
    @xgettext = GetText::Tools::XGetText.new
  end

  class TestRuby < self
    def test__
      @xgettext.parse_options[:comment_tag] = "TRANSLATORS:"
      @ary = @xgettext.parse(['fixtures/_.rb'])

      assert_target 'jjj', ['fixtures/_.rb:71']
      assert_target 'kkk', ['fixtures/_.rb:72']
      assert_target 'lllmmm', ['fixtures/_.rb:76']
      assert_target "nnn\nooo", ['fixtures/_.rb:84']
      assert_target "\#", ['fixtures/_.rb:88', 'fixtures/_.rb:92']
      assert_target "\\taaa", ['fixtures/_.rb:96']
      assert_target "Here document1\nHere document2\n", ['fixtures/_.rb:100']
      assert_target "Francois Pinard", ['fixtures/_.rb:120'] do |t|
        assert_match(/proper name/, t.extracted_comment)
        assert_match(/Pronunciation/, t.extracted_comment)
      end

      assert_target("No TRANSLATORS comment", ["fixtures/_.rb:123"]) do |t|
        assert_nil(t.comment)
      end

      assert_target "self explaining", ['fixtures/_.rb:128'] do |t|
        assert_nil t.comment
      end

      assert_target "This is a # including string.", ["fixtures/_.rb:132"]

      # TODO: assert_target "in_quote", ['fixtures/_.rb:118']
    end

    def test_N_
      @ary = @xgettext.parse(['fixtures/upper_n_.rb'])

      assert_target('aaa',
                    ['fixtures/upper_n_.rb:10'])
      assert_target("aaa\n",
                    ['fixtures/upper_n_.rb:14'])
      assert_target("bbb\nccc",
                    ['fixtures/upper_n_.rb:18'])
      assert_target("bbb\nccc\nddd\n",
                    ['fixtures/upper_n_.rb:22'])
      assert_target('eee',
                    [
                      'fixtures/upper_n_.rb:29',
                      'fixtures/upper_n_.rb:33',
                    ])
      assert_target('fff',
                    ['fixtures/upper_n_.rb:33'])
      assert_target('ggghhhiii',
                    ['fixtures/upper_n_.rb:37'])
      assert_target('a"b"c"',
                    ['fixtures/upper_n_.rb:43'])
      assert_target('d"e"f"',
                    ['fixtures/upper_n_.rb:47'])
      assert_target('jjj',
                    ['fixtures/upper_n_.rb:51'])
      assert_target('kkk',
                    ['fixtures/upper_n_.rb:52'])
      assert_target('lllmmm',
                    ['fixtures/upper_n_.rb:56'])
      assert_target("nnn\nooo",
                    ['fixtures/upper_n_.rb:64'])
    end

    def test_n_
      @xgettext.parse_options[:comment_tag] = "TRANSLATORS:"
      @ary = @xgettext.parse(['fixtures/lower_n_.rb'])
      assert_plural_target("aaa", "aaa2",
                           ['fixtures/lower_n_.rb:29'])
      assert_plural_target("bbb\n", "ccc2\nccc2",
                           ['fixtures/lower_n_.rb:33'])
      assert_plural_target("ddd\nddd", "ddd2\nddd2",
                           ['fixtures/lower_n_.rb:37'])
      assert_plural_target("eee\neee\n", "eee2\neee2\n",
                           ['fixtures/lower_n_.rb:42'])
      assert_plural_target("ddd\neee\n", "ddd\neee2",
                           ['fixtures/lower_n_.rb:48'])
      assert_plural_target("fff", "fff2",
                           [
                             'fixtures/lower_n_.rb:55',
                             'fixtures/lower_n_.rb:59',
                           ])
      assert_plural_target("ggg", "ggg2",
                           ['fixtures/lower_n_.rb:59'])
      assert_plural_target("ggghhhiii", "jjjkkklll",
                           ['fixtures/lower_n_.rb:63'])
      assert_plural_target("a\"b\"c\"", "a\"b\"c\"2",
                           ['fixtures/lower_n_.rb:72'])
      assert_plural_target("mmmmmm", "mmm2mmm2",
                           ['fixtures/lower_n_.rb:80'])
      assert_plural_target("nnn", "nnn2", ['fixtures/lower_n_.rb:81'])
      assert_plural_target("comment", "comments",
                           ['fixtures/lower_n_.rb:97']) do |t|
        assert_equal "TRANSLATORS:please provide translations for all\n the plural forms!",
                       t.extracted_comment
      end
    end

    def test_p_
      @xgettext.parse_options[:comment_tag] = "TRANSLATORS:"
      @ary = @xgettext.parse(['fixtures/p_.rb'])
      assert_target_in_context "AAA", "BBB", ["fixtures/p_.rb:29", "fixtures/p_.rb:33"]
      assert_target_in_context "AAA|BBB", "CCC", ["fixtures/p_.rb:37"]
      assert_target_in_context "AAA", "CCC", ["fixtures/p_.rb:41"]
      assert_target_in_context "CCC", "BBB", ["fixtures/p_.rb:45"]
      assert_target_in_context "program", "name", ['fixtures/p_.rb:55'] do |t|
        assert_equal "TRANSLATORS:please translate 'name' in the context of 'program'.\n Hint: the translation should NOT contain the translation of 'program'.", t.extracted_comment
      end
    end
  end

  class TestGlade < self
    def test_old_style
      # Old style (~2.0.4)
      ary = GetText::GladeParser.parse('fixtures/gladeparser.glade')

      assert_equal(['window1', 'fixtures/gladeparser.glade:8'], ary[0])
      assert_equal(['normal text', 'fixtures/gladeparser.glade:29'], ary[1])
      assert_equal(['1st line\n2nd line\n3rd line', 'fixtures/gladeparser.glade:50'], ary[2])
      assert_equal(['<span color="red" weight="bold" size="large">markup </span>', 'fixtures/gladeparser.glade:73'], ary[3])
      assert_equal(['<span color="red">1st line markup </span>\n<span color="blue">2nd line markup</span>', 'fixtures/gladeparser.glade:94'], ary[4])
      assert_equal(['<span>&quot;markup&quot; with &lt;escaped strings&gt;</span>', 'fixtures/gladeparser.glade:116'], ary[5])
      assert_equal(['duplicated', 'fixtures/gladeparser.glade:137', 'fixtures/gladeparser.glade:158'], ary[6])
    end
  end

  class TestErbParser < self
    include GetTextTestUtils

    def test_detect_encoding
      euc_file = Tempfile.new("euc-jp.rhtml")
      euc_file.open
      euc_file.puts("<%#-*- coding: euc-jp -*-%>")
      euc_file.close

      erb_source = ERB.new(File.read(euc_file.path)).src
      encoding = GetText::ErbParser.new(euc_file.path).detect_encoding(erb_source)

      assert_equal("EUC-JP", encoding)
    end

    def test_ascii
      @ary = GetText::ErbParser.parse('fixtures/erb/ascii.rhtml')

      assert_target 'aaa', ['fixtures/erb/ascii.rhtml:8']
      assert_target "aaa\n", ['fixtures/erb/ascii.rhtml:11']
      assert_target 'bbb', ['fixtures/erb/ascii.rhtml:12']
      assert_plural_target "ccc1", "ccc2", ['fixtures/erb/ascii.rhtml:13']
    end

    def test_non_ascii
      fixture_path = "fixtures/erb/non_ascii.rhtml"
      @ary = GetText::ErbParser.parse(fixture_path)

      assert_target('わたし', ["#{fixture_path}:11"])
    end
  end

  def test_xgettext_parse
    GetText::ErbParser.init(:extnames => ['.rhtml', '.rxml'])
    @ary = @xgettext.parse(['fixtures/erb/ascii.rhtml'])
    assert_target 'aaa', ['fixtures/erb/ascii.rhtml:8']
    assert_target "aaa\n", ['fixtures/erb/ascii.rhtml:11']
    assert_target 'bbb', ['fixtures/erb/ascii.rhtml:12']
    assert_plural_target "ccc1", "ccc2", ['fixtures/erb/ascii.rhtml:13']

    @ary = @xgettext.parse(['fixtures/erb/ascii.rxml'])
    assert_target 'aaa', ['fixtures/erb/ascii.rxml:9']
    assert_target "aaa\n", ['fixtures/erb/ascii.rxml:12']
    assert_target 'bbb', ['fixtures/erb/ascii.rxml:13']
    assert_plural_target "ccc1", "ccc2", ['fixtures/erb/ascii.rxml:14']

    @ary = @xgettext.parse(['fixtures/lower_n_.rb'])
    assert_plural_target("ooo", "ppp",
                         [
                           'fixtures/lower_n_.rb:85',
                           'fixtures/lower_n_.rb:86',
                         ])
    assert_plural_target("qqq", "rrr",
                         [
                           'fixtures/lower_n_.rb:90',
                           'fixtures/lower_n_.rb:91',
                         ])
  end

  private

  def assert_target(msgid, references = nil)
    t = @ary.detect {|elem| elem.msgid == msgid}
    if t
      if references
        assert_equal references.sort, t.references.sort, 'Translation target references do not match.'
      end
      yield t if block_given?
    else
      flunk "Expected a translation target with id '#{msgid}'. Not found."
    end
  end

  def assert_plural_target(msgid, plural, references = nil)
    assert_target msgid, references do |t|
      assert_equal plural, t.msgid_plural, 'Expected plural form'
      yield t if block_given?
    end
  end

  def assert_target_in_context(msgctxt, msgid, references = nil)
    t = @ary.detect {|elem| elem.msgid == msgid && elem.msgctxt == msgctxt}
    if t
      if references
        assert_equal references.sort, t.references.sort, 'Translation target references do not match.'
      end
      yield t if block_given?
    else
      flunk "Expected a translation target with id '#{msgid}' and context '#{msgctxt}'. Not found."
    end
  end
end
