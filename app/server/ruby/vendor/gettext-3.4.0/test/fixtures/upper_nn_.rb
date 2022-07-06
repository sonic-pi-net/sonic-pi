# -*- coding: utf-8 -*-

require 'gettext'
include GetText

class TestRubyParser_Nn
  bindtextdomain("testNn_rubyparser", :path => "locale")

  def testNn_1
    Nn_("aaa", "aaas")
  end

  def testNn_2
    Nn_("aaa\n", "aaas\n")
  end

  def testNn_3
    Nn_("bbb\nccc", "bbbs\ncccs")
  end

  def testNn_4
    Nn_("bbb
ccc
ddd
",
        "bbbs
cccs
ddds
")
  end

  def testNn_5
    Nn_("eee", "eees")
  end

  def testNn_6
    Nn_("eee", "eees") + "foo" + Nn_("fff", "fffs")
  end

  def testNn_7
    Nn_("ggg"\
        "hhh"\
        "iii",
        "gggs"\
        "hhhs"\
        "iiis")
  end

  def testNn_8
    Nn_('a"b"c"', 'as"bs"cs"')
  end

  def testNn_9
    Nn_("d\"e\"f\"", "ds\"es\"fs\"")
  end

  def testNn_10
    Nn_("jjj", "jjjs") +
      Nn_("kkk", "kkks")
  end

  def testNn_11
    Nn_("lll" + "mmm", "llls" + "mmms")
  end

  def testNn_12
    puts Nn_(msg, msgs), "ppp"  #Ignored
  end

  def testNn_13
    Nn_("nnn\n" +
        "ooo",
        "nnns\n" +
        "ooos")
  end
end

