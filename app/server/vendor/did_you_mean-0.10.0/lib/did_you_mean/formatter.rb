module DidYouMean
  class Formatter
    def initialize(suggestions = [])
      @suggestions = suggestions
    end

    def to_s
      return "" if @suggestions.empty?

      output = "\n\n"
      output << "    Did you mean? #{format(@suggestions.first)}\n"
      output << @suggestions.drop(1).map{|word| "#{' ' * 18}#{format(word)}\n" }.join
      output << " " # for rspec
    end

    def format(name)
      name
    end
  end
end
