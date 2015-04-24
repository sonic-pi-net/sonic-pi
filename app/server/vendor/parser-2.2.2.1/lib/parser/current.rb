module Parser
  class << self
    def warn_syntax_deviation(feature, version)
      warn "warning: parser/current is loading #{feature}, which recognizes"
      warn "warning: #{version}-compliant syntax, but you are running #{RUBY_VERSION}."
    end
    private :warn_syntax_deviation
  end

  case RUBY_VERSION
  when /^1\.8\./
    if RUBY_VERSION != '1.8.7'
      warn_syntax_deviation 'parser/ruby18', '1.8.7'
    end

    require 'parser/ruby18'
    CurrentRuby = Ruby18

  when /^1\.9\./
    if RUBY_VERSION != '1.9.3'
      warn_syntax_deviation 'parser/ruby19', '1.9.3'
    end

    require 'parser/ruby19'
    CurrentRuby = Ruby19

  when /^2\.0\./
    require 'parser/ruby20'
    CurrentRuby = Ruby20

  when /^2\.1\./
    if RUBY_VERSION != '2.1.6'
      warn_syntax_deviation 'parser/ruby21', '2.1.6'
    end

    require 'parser/ruby21'
    CurrentRuby = Ruby21

  when /^2\.2\./
    require 'parser/ruby22'
    CurrentRuby = Ruby22

  else # :nocov:
    # Keep this in sync with released Ruby.
    warn_syntax_deviation 'parser/ruby22', '2.2'
    require 'parser/ruby22'
    CurrentRuby = Ruby22
  end
end
