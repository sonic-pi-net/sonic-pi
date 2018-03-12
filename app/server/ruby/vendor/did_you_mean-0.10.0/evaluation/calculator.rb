require 'benchmark'

def report(message, &block)
  time = 1000 * Benchmark.realtime(&block)

  if (time / 1000 / 60) >= 1
    minutes = (time / 1000 / 60).floor
    seconds = (time % (60 * 1000)) / 1000

    puts " \e[36m%2dm%.3fs:\e[0m %s" % [minutes, seconds, message]
  elsif (time / 1000) >= 1
    seconds = (time % (60 * 1000)) / 1000

    puts " \e[36m%9.3fs:\e[0m %s" % [seconds, message]
  else
    puts " \e[36m%8.1fms:\e[0m %s" % [time, message]
  end

  time
end

puts "\n"

report "loading program" do
  require 'yaml'
  require 'set'
  require 'did_you_mean'

  begin
    require 'jaro_winkler'
    DidYouMean::JaroWinkler.module_eval do
      module_function
      def distance(str1, str2)
        ::JaroWinkler.distance(str1, str2)
      end if RUBY_ENGINE != 'jruby'
    end
  rescue LoadError, NameError
  end

  class DidYouMean::WordCollection
    include DidYouMean::BaseFinder

    def initialize(words)
      @words = words
    end

    def similar_to(input, filter = EMPTY)
      @suggestions, @input = nil, input
      suggestions
    end

    def searches
      { @input => @words }
    end

    private
    def normalize(str); str; end
  end if !defined?(DidYouMean::WordCollection)
end

report "loading dictionary" do
  yaml = open("evaluation/dictionary.yml").read
  yaml = YAML.load(yaml).map{|word| word.downcase.tr(" ".freeze, "_".freeze) }

  DICTIONARY = Set.new(yaml)
end

report "loading corrent/incorrect words" do
  COLLECTION      = DidYouMean::WordCollection.new(DICTIONARY)
  INCORRECT_WORDS = YAML.load(open("evaluation/incorrect_words.yaml").read)
end

total_count         = 0
correct_count       = 0
words_not_corrected = []
filename            = "log/words_not_corrected_#{Time.now.to_i}.yml"

puts "
 Total number of test data: #{INCORRECT_WORDS.size}
      did_you_mean version: #{DidYouMean::VERSION}

"

report "calculating accuracy" do
  index = 0
  INCORRECT_WORDS.each do |correct, incorrect|
    if DICTIONARY.include?(correct)
      total_count += 1

      corrections = COLLECTION.similar_to(incorrect)
      if corrections.first == correct
        correct_count += 1
      else
        words_not_corrected << {
          correct         => incorrect,
          'result'.freeze => corrections
        }
      end
    end

    index += 1
    puts "processed #{index} items" if index % 100 == 0
  end

  puts "\n"
end

puts "
Evaulation result

  Total count  : #{total_count}
  Correct count: #{correct_count}
  Accuracy     : #{correct_count.to_f / total_count}

"

Dir.mkdir('log') unless File.exist?('log')
File.open(filename, 'w') do |file|
  file.write(words_not_corrected.to_yaml)
end

puts "Incorrect suggestions were logged to #{filename}."
