# Augments all parslet atoms with an accept method that will call back 
# to the visitor given.

# 
module Parslet::Atoms
  class Base
    def accept(visitor)
      raise NotImplementedError, "No #accept method on #{self.class.name}."
    end
  end
  
  class Str
    # Call back visitors #visit_str method. See parslet/export for an example.
    #
    def accept(visitor)
      visitor.visit_str(str)
    end
  end
  
  class Entity
    # Call back visitors #visit_entity method. See parslet/export for an
    # example. 
    #
    def accept(visitor)
      visitor.visit_entity(name, block)
    end
  end
  
  class Named
    # Call back visitors #visit_named method. See parslet/export for an
    # example. 
    #
    def accept(visitor)
      visitor.visit_named(name, parslet)
    end
  end
  
  class Sequence
    # Call back visitors #visit_sequence method. See parslet/export for an
    # example. 
    #
    def accept(visitor)
      visitor.visit_sequence(parslets)
    end
  end
  
  class Repetition
    # Call back visitors #visit_repetition method. See parslet/export for an
    # example. 
    #
    def accept(visitor)
      visitor.visit_repetition(@tag, min, max, parslet)
    end
  end
  
  class Alternative
    # Call back visitors #visit_alternative method. See parslet/export for an
    # example. 
    #
    def accept(visitor)
      visitor.visit_alternative(alternatives)
    end
  end
  
  class Lookahead
    # Call back visitors #visit_lookahead method. See parslet/export for an
    # example. 
    #
    def accept(visitor)
      visitor.visit_lookahead(positive, bound_parslet)
    end
  end
  
  class Re
    # Call back visitors #visit_re method. See parslet/export for an example. 
    #
    def accept(visitor)
      visitor.visit_re(match)
    end
  end
end

class Parslet::Parser
  # Call back visitors #visit_parser method. 
  #
  def accept(visitor)
    visitor.visit_parser(root)
  end
end
