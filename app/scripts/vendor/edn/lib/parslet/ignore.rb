class IgnoreParslet < Parslet::Atoms::Base
  def initialize(parslet)
    @parslet = parslet
  end
  def to_s_inner(prec)
    @parslet.to_s(prec)
  end
  def try(source, context)
    success, value = result = @parslet.try(source, context)

    return succ(nil) if success
    return result
  end
end

module IgnoreDSL
  def ignore
    IgnoreParslet.new(self)
  end
end

class Parslet::Atoms::Base
  include IgnoreDSL
end
