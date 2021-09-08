# -*- coding: utf-8 -*-
#
# Copyright (C) 2012  Haruka Yoshihara <yoshihara@clear-code.com>
# Copyright (C) 2012-2021  Sutou Kouhei <kou@clear-code.com>
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
      assert_target "\\taaa'bbb\\ccc", ['fixtures/_.rb:96']
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

    def test_Nn_
      @ary = @xgettext.parse(['fixtures/upper_nn_.rb'])

      assert_plural_target('aaa', 'aaas',
                           ['fixtures/upper_nn_.rb:10'])
      assert_plural_target("aaa\n", "aaas\n",
                           ['fixtures/upper_nn_.rb:14'])
      assert_plural_target("bbb\nccc", "bbbs\ncccs",
                           ['fixtures/upper_nn_.rb:18'])
      assert_plural_target("bbb\nccc\nddd\n", "bbbs\ncccs\nddds\n",
                           ['fixtures/upper_nn_.rb:22'])
      assert_plural_target('eee', 'eees',
                           [
                             'fixtures/upper_nn_.rb:33',
                             'fixtures/upper_nn_.rb:37',
                           ])
      assert_plural_target('fff', 'fffs',
                           ['fixtures/upper_nn_.rb:37'])
      assert_plural_target('ggghhhiii', 'gggshhhsiiis',
                           ['fixtures/upper_nn_.rb:41'])
      assert_plural_target('a"b"c"', 'as"bs"cs"',
                           ['fixtures/upper_nn_.rb:50'])
      assert_plural_target('d"e"f"', 'ds"es"fs"',
                           ['fixtures/upper_nn_.rb:54'])
      assert_plural_target('jjj', 'jjjs',
                           ['fixtures/upper_nn_.rb:58'])
      assert_plural_target('kkk', 'kkks',
                           ['fixtures/upper_nn_.rb:59'])
      assert_plural_target('lllmmm', 'lllsmmms',
                           ['fixtures/upper_nn_.rb:63'])
      assert_plural_target("nnn\nooo", "nnns\nooos",
                           ['fixtures/upper_nn_.rb:71'])
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

  class TestErbParser < self
    include Helper::Path

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
      path = fixture_path("erb", "ascii.rhtml")
      @ary = GetText::ErbParser.parse(path)

      assert_target 'aaa', ["#{path}:8"]
      assert_target "aaa\n", ["#{path}:11"]
      assert_target 'bbb', ["#{path}:12"]
      assert_plural_target "ccc1", "ccc2", ["#{path}:13"]
    end

    def test_non_ascii
      path = fixture_path("erb", "non_ascii.rhtml")
      @ary = GetText::ErbParser.parse(path)

      assert_target('わたし', ["#{path}:12"])
    end

    def test_minus
      path = fixture_path("erb", "minus.rhtml")
      @ary = GetText::ErbParser.parse(path)

      assert_target("Hello", ["#{path}:8"])
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
