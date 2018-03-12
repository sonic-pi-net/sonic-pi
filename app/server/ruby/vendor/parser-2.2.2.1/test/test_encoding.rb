# encoding: binary

require 'helper'

class TestEncoding < Minitest::Test
  include AST::Sexp

  def recognize(string)
    Parser::Source::Buffer.recognize_encoding(string)
  end

  if defined?(Encoding)
    require 'parser/all'

    def test_default
      assert_equal nil, recognize('foobar')
    end

    def test_bom
      assert_equal Encoding::UTF_8, recognize("\xef\xbb\xbf\nfoobar")
      assert_equal Encoding::UTF_8, recognize("\xef\xbb\xbf# coding:koi8-r\nfoobar")
    end

    def test_magic_comment
      assert_equal Encoding::KOI8_R, recognize("# coding:koi8-r\nfoobar")
    end

    def test_shebang
      assert_equal Encoding::KOI8_R, recognize("#!/bin/foo\n# coding:koi8-r\nfoobar")
    end

    def test_case
      assert_equal Encoding::KOI8_R, recognize("# coding:KoI8-r\nfoobar")
    end

    def test_space
      assert_equal Encoding::KOI8_R, recognize("# coding : koi8-r\nfoobar")
    end

    def test_empty
      assert_equal nil, recognize('')
    end

    def test_no_comment
      assert_equal nil, recognize(%{require 'cane/encoding_aware_iterator'})
    end

    def test_adjacent
      assert_equal nil, recognize('# codingkoi8-r')
      assert_equal nil, recognize('# coding koi8-r')
    end

    def test_utf8_mac
      assert_equal Encoding::UTF8_MAC, recognize('# coding: utf8-mac')
    end

    def test_suffix
      assert_equal Encoding::UTF_8, recognize('# coding: utf-8-dos')
      assert_equal Encoding::UTF_8, recognize('# coding: utf-8-unix')
      assert_equal Encoding::UTF_8, recognize('# coding: utf-8-mac')

      assert_raises(ArgumentError) do
        assert_equal nil,           recognize('# coding: utf-8-dicks')
      end
    end

    def test_parse_18_invalid_enc
      ast = Parser::Ruby18.parse("# encoding:feynman-diagram\n1")
      assert_equal ast, s(:int, 1)
    end

    def test_parse_19_invalid_enc
      assert_raises(ArgumentError) do
        Parser::Ruby19.parse("# encoding:feynman-diagram\n1")
      end
    end
  end
end
