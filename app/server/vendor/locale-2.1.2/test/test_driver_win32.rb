begin
  require 'locale/driver/win32'
  require 'test/unit'

  class TestDiverWin32 < Test::Unit::TestCase

    def setup
      ENV["LC_ALL"] = nil
      ENV["LC_CTYPE"] = nil
      ENV["LANG"] = nil
      ENV["LANGUAGE"] = nil
      Locale::Driver::Win32.set_thread_locale_id(nil)
    end

    def test_charset
      Locale::Driver::Win32.set_thread_locale_id(0x0404)
      assert_equal "CP950", Locale::Driver::Win32.charset

      Locale::Driver::Win32.set_thread_locale_id(0x0411)
      assert_equal "CP932", Locale::Driver::Win32.charset

      Locale::Driver::Win32.set_thread_locale_id(0x201A)
      assert_equal "CP1251", Locale::Driver::Win32.charset

      Locale::Driver::Win32.set_thread_locale_id(0x0465)
      assert_equal "UNICODE", Locale::Driver::Win32.charset

      Locale::Driver::Win32.set_thread_locale_id(0x2222) # invalid
      assert_equal "CP1252", Locale::Driver::Win32.charset
    end

    def test_locales
      Locale::Driver::Win32.set_thread_locale_id(0x0404)
      assert_equal Locale::Tag::Common.parse("zh-TW"), Locale::Driver::Win32.locales[0]

      Locale::Driver::Win32.set_thread_locale_id(0x0411)
      assert_equal Locale::Tag::Common.parse("ja-JP"), Locale::Driver::Win32.locales[0]

      Locale::Driver::Win32.set_thread_locale_id(0x201A)
      assert_equal Locale::Tag::Common.parse("bs-Cyrl-BA"), Locale::Driver::Win32.locales[0]

      Locale::Driver::Win32.set_thread_locale_id(0x0465)
      assert_equal Locale::Tag::Common.parse("div-MV"), Locale::Driver::Win32.locales[0]

      Locale::Driver::Win32.set_thread_locale_id(0x2222) # invalid
      assert_equal nil, Locale::Driver::Win32.locales
    end

    def test_locales_with_env
      ENV["LC_ALL"] = "ja_JP.UTF-8"
      assert_equal Locale::Tag::Posix.parse("ja_JP.UTF-8"), Locale::Driver::Win32.locales[0]
      assert_equal "UTF-8", Locale::Driver::Win32.charset

      ENV["LC_ALL"] = "ja_JP"
      assert_equal Locale::Tag::Posix.parse("ja_JP"), Locale::Driver::Win32.locales[0]
      assert_equal "CP932", Locale::Driver::Win32.charset

      ENV["LC_ALL"] = "C"
      assert_equal Locale::Tag::Posix.parse("C"), Locale::Driver::Win32.locales[0]
      assert_equal "CP1252", Locale::Driver::Win32.charset
    end
  end
rescue LoadError, Fiddle::DLError, NameError
  puts "win32 test was skipped."
end
