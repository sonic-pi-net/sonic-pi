module Mocha
  module Debug
    OPTIONS = (ENV['MOCHA_OPTIONS'] || '').split(',').inject({}) do |hash, key|
      hash[key] = true; hash
    end.freeze

    def self.puts(message)
      $stderr.puts(message) if OPTIONS['debug']
    end
  end
end
