# -*- coding: utf-8 -*-

module MultiTextDomain
  class C11
    include GetText
    def initialize
      bindtextdomain("test1", :path => "locale")
      bindtextdomain("test2", :path => "locale")
    end
    def test
      _("language")
    end
    def test2
      _("LANGUAGE")
    end
  end

  class C12
    include GetText
    bindtextdomain("test1", :path => "locale")
    bindtextdomain("test2", :path => "locale")

    def test
      _("language")
    end
    def test2
      _("LANGUAGE")
    end
  end

  class C21 < C11
  end

  class C22 < C12
  end

  module M1
    include GetText
    bindtextdomain("test1", :path => "locale")

    module_function
    def test
      _("language")
    end

    module M1M1
      include GetText
      module_function
      def test
        _("language")
      end
      # Doesn't translate
      def test2
        _("LANGUAGE")
      end
    end

    class M1C1
      include GetText
      bindtextdomain("test2", :path => "locale")
      def test
        _("language")
      end
      def test2
        _("LANGUAGE")
      end
    end

    class M1C2
      include GetText
      def initialize
        bindtextdomain("test2", :path => "locale")
      end
      def test
        _("language")
      end
      def test2
        _("LANGUAGE")
      end
    end

  end

  class C2
    include GetText
    def initialize
      bindtextdomain("test1", :path => "locale")
    end

    def test
      _("language")
    end

    def test_eval
      eval("_(\"language\")")
    end
  end

  class C3
    include GetText
    bindtextdomain("test1", :path => "locale")

    def test
      _("language")
    end

    def self.test
      _("language")
    end
  end

  class C4 < C3
    bindtextdomain("test2", :path => "locale")

    def test2
      _("LANGUAGE")
    end

    def test3
      _("no data")
    end
  end

  class C51
    include GetText
    bindtextdomain("test3", :path => "locale")
    def test
      _("language")
    end
  end

  class C52 < C12
    bindtextdomain("test3", :path => "locale")
  end
end
