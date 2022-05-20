# -*- coding: utf-8 -*-

require 'gettext'
include GetText

class TestRubyParser_N
  bindtextdomain("testN_rubyparser", :path => "locale")

  def testN_1
    N_("aaa")
  end

  def testN_2
    N_("aaa\n")
  end

  def testN_3
    N_("bbb\nccc")
  end

  def testN_4
     N_("bbb
ccc
ddd
")
  end

  def testN_5
    N_("eee")
  end

  def testN_6
    N_("eee") + "foo" + N_("fff")
  end

  def testN_7
    N_("ggg"\
      "hhh"\
      "iii")
  end

  def testN_8
    N_('a"b"c"')
  end

  def testN_9
    N_("d\"e\"f\"")
  end

  def testN_10
    N_("jjj") +
    N_("kkk")
  end

  def testN_11
    N_("lll" + "mmm")
  end

  def testN_12
    puts N_(msg), "ppp"  #Ignored
  end

  def testN_13
    N_("nnn\n" +
      "ooo")
  end
end

