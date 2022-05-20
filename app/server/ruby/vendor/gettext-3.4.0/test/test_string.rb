class TestGetTextString < Test::Unit::TestCase
  class TestFormat < self
    include Helper::Warning

    def test_basic
      assert_equal("foo is a number", "%{msg} is a number" % {:msg => "foo"})
      assert_equal("bar is a number", "%s is a number" % ["bar"])
      assert_equal("bar is a number", "%s is a number" % "bar")
      assert_equal("1, test", "%{num}, %{record}" % {:num => 1, :record => "test"})
      assert_equal("test, 1", "%{record}, %{num}" % {:num => 1, :record => "test"})
      assert_equal("1, test", "%d, %s" % [1, "test"])
      assert_equal("test, 1", "%2$s, %1$d" % [1, "test"])
      assert_raise(ArgumentError) { "%-%" % [1] }
    end

    def test_placeholder_include_non_english
      assert_equal("a", "%{foo+foo}" % {"foo+foo".to_sym => "a"})
      assert_equal("a", "%{foo.foo}" % {"foo.foo".to_sym => "a"})
      assert_equal("a }", "%{foo+foo} }" % {"foo+foo".to_sym => "a"})
      assert_equal("a { b }", "%{foo+foo} { %{bar bar-} }" % {"foo+foo".to_sym => "a", "bar bar-".to_sym => "b"})
    end

    def test_percent
      assert_equal("% 1", "%% %<num>d" % {:num => 1.0})
      suppress_warning do
        assert_equal("%{num} %<num>d", "%%{num} %%<num>d" % {:num => 1})
      end
    end

    def test_percent_in_replacement
      assert_equal("%<not_translated>s", "%{msg}" % { :msg => '%<not_translated>s', :not_translated => 'should not happen' })
    end

    def test_no_placeholder
      suppress_warning do
        assert_equal("aaa", "aaa" % {:num => 1})
        assert_equal("bbb", "bbb" % [1])
      end
    end

    def test_ruby19_style
      assert_equal("1", "%<num>d" % {:num => 1})
      assert_equal("0b1", "%<num>#b" % {:num => 1})
      assert_equal("foo", "%<msg>s" % {:msg => "foo"})
      assert_equal("1.000000", "%<num>f" % {:num => 1.0})
      assert_equal("  1", "%<num>3.0f" % {:num => 1.0})
      assert_equal("100.00", "%<num>2.2f" % {:num => 100.0})
      assert_equal("0x64", "%<num>#x" % {:num => 100.0})
      assert_equal("a", "%<foo.foo>s" % {"foo.foo".to_sym => "a"})
      assert_raise(ArgumentError) { "%<num>,d" % {:num => 100} }
      assert_raise(ArgumentError) { "%<num>/d" % {:num => 100} }
    end

    def test_old_style
      assert_equal("foo 1.000000", "%s %f" % ["foo", 1.0])
    end

    class TestMix < self
      def test_brace_and_angle_bracket
        assert_equal("foo 1.000000",
                     "%{name} %<num>f" % {:name => "foo", :num => 1.0})
      end
    end
  end
end
