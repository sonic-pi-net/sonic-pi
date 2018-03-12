require 'racc/grammarfileparser'

# Unfortunately, Ruby's Coverage module ignores module_eval statements,
# which Racc uses to map `parser.y` locations in the generated
# `parser.rb`.
module RaccCoverage
  @coverage  = {}
  @base_path = nil
  @trace     = nil

  def self.start(parsers, base_path)
    @base_path = base_path

    parsers.each do |parser|
      @coverage[parser] = extract_interesting_lines(parser, base_path)
    end

    @trace = TracePoint.trace(:line) do |trace|
      lineno = trace.lineno - 1

      if (line_coverage = @coverage[trace.path])
        if line_coverage[lineno]
          line_coverage[lineno] += 1
        end
      end
    end
  end

  def self.stop
    @trace.disable
  end

  # Ruby's TracePoint#lineno will point only on "interesting" lines,
  # i.e.: only code (no comments or empty lines), no `end` keywords,
  # and for multi-line statements, only the first line of the statement.
  #
  # This method implements a very dumb Ruby parser, which skips empty lines
  # or lines with just comments, `end` keywords, and correctly handles
  # multi-line statements of the following form:
  #
  #  * All lines of the statement except the last must end with `,`, `.` or `(`.
  #
  # Coverage can be disabled for code regions with annotations :nocov: and :cov:.
  #
  # Also, for best results, all actions should be delimited by at least
  # one non-action line.
  #
  def self.extract_interesting_lines(parser, base_path)
    grammar_source = File.join(@base_path, parser)
    grammar_file   = Racc::GrammarFileParser.parse_file(grammar_source)

    ruby_sources = [
      # Header and footer aren't passed through module_eval
      # in Racc-generated file, so the location info is lost.
      *grammar_file.params.inner,
    ].compact

    grammar_file.grammar.each_rule do |rule|
      source = rule.action.source
      next if source.nil?

      ruby_sources << source
    end

    lines = []

    ruby_sources.each do |source|
      first_line = source.lineno

      state = :first_line

      source.text.each_line.with_index do |line, index|
        line = line.strip

        continues = line.end_with?(',')   ||
                      line.end_with?('(') ||
                      line.end_with?('.')

        case state
        when :first_line
          if line =~ /:nocov/
            state = :nocov
            next
          elsif line.empty?   ||
                  line == 'end' ||
                  line.start_with?('#')
            next
          elsif continues
            state = :mid_line
          end

          lines[first_line + index - 1] = 0

        when :mid_line
          unless continues
            state = :first_line
          end

        when :nocov
          if line =~ /:cov:/
            state = :first_line
          end
        end
      end
    end

    lines
  end

  def self.result
    result =
      @coverage.map do |parser, coverage|
        [File.join(@base_path, parser), coverage]
      end

    Hash[result]
  end
end

class << SimpleCov
  def result_with_racc_coverage
    @result ||= SimpleCov::Result.new(
                  Coverage.result.merge(RaccCoverage.result))

    result_without_racc_coverage
  end

  alias result_without_racc_coverage result
  alias result result_with_racc_coverage
end
