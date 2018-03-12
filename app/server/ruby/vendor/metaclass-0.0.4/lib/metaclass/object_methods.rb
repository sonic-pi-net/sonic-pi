module Metaclass::ObjectMethods
  def __metaclass__
    class << self
      self
    end
  end
end

class Object
  include Metaclass::ObjectMethods
end