# Foo class
class Foo
  def initialize
    @foo = "baz"
  end

  def bar
    @foo
  end

  #:nocov:
  def skipped
    @foo * 2
  end
  #:nocov:
end
