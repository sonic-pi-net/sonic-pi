require "test_helper"

class DotgitTest < Rugged::TestCase
  def test_dotgit
    assert Rugged.dotgit_modules?(".gitmodules")
    assert Rugged.dotgit_modules?(".git\xe2\x80\x8cmodules")
    assert Rugged.dotgit_modules?("GITMOD~1")
    assert Rugged.dotgit_modules?("gi7eba~9")

    refute Rugged.dotgit_modules?("gitmodules")
    refute Rugged.dotgit_modules?("GI7EBA~0")

    assert Rugged.dotgit_ignore?(".gitignore")
    refute Rugged.dotgit_ignore?(".gitignores")

    assert Rugged.dotgit_attributes?(".gitattributes")
    refute Rugged.dotgit_attributes?(".gittattributtes")

  end
end
