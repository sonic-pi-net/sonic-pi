require 'mocha/integration'

module Mocha
  def self.activate
    Integration.activate
  end
end

Mocha.activate
