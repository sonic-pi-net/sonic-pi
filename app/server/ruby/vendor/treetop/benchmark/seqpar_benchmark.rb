# Benchmarking written by Bernard Lambeau, improved by Jason Garber
#
# To test your optimizations:
#   1. Run ruby seqpar_benchmark.rb
#   2. cp after.dat before.dat 
#   3. Make your modifications to the treetop code
#   4. Run ruby seqpar_benchmark.rb
#   5. Run gnuplot seqpar.gnuplot
#
$LOAD_PATH << File.join(File.dirname(__FILE__), '..', 'lib')
require 'treetop'
require 'benchmark'

srand(47562) # So it runs the same each time

class Array; def sum; inject( nil ) { |sum,x| sum ? sum+x : x }; end; end
class Array; def mean; sum / size; end; end

class SeqParBenchmark
  
  OPERATORS = ["seq", "fit", "art"*5, "par", "sequence"]
  
  # Loads the grammar and returns a parser
  def initialize
    compiler = Treetop::Compiler::GrammarCompiler.new
    @where = File.expand_path(File.dirname(__FILE__))
    grammar = File.join(@where, 'seqpar.treetop')
    output = File.join(@where, 'seqpar.rb')
    compiler.compile(grammar, output)
    load output
    File.delete(output)
    @parser = SeqParParser.new
  end
  
  # Checks the grammar
  def check
    ["Task",
     "seq Task end",
     "par Task end",
     "seq Task Task end",
     "par Task Task end",
     "par seq Task end Task end",
     "par seq seq Task end end Task end",
     "seq Task par seq Task end Task end Task end"].each do |input|
      raise ParseError.new(@parser) if @parser.parse(input).nil?
    end
  end
  
  # Generates an input text
  def generate(depth=0)
    return "Task" if depth>7
    return "seq #{generate(depth+1)} end" if depth==0
    which = rand(OPERATORS.length)
    case which
      when 0
        "Task"
      else
        raise unless OPERATORS[which]
        buffer = "#{OPERATORS[which]} "
        0.upto(rand(4)+1) do 
          buffer << generate(depth+1) << " "
        end
        buffer << "end"
        buffer
    end
  end
  
  # Launches benchmarking
  def benchmark
    number_by_size = Hash.new {|h,k| h[k] = 0}
    time_by_size = Hash.new {|h,k| h[k] = 0}
    0.upto(250) do |i|
      input = generate
      length = input.length
      puts "Launching #{i}: #{input.length}"
      # puts input
      tms = Benchmark.measure { @parser.parse(input) }
      number_by_size[length] += 1
      time_by_size[length] += tms.total*1000
    end
    # puts number_by_size.inspect
    # puts time_by_size.inspect
    
    File.open(File.join(@where, 'after.dat'), 'w') do |dat|
      number_by_size.keys.sort.each do |size|
        dat << "#{size} #{(time_by_size[size]/number_by_size[size]).truncate}\n"
      end
    end
    
    if File.exists?(File.join(@where, 'before.dat'))
      before = {}
      performance_increases = []
      File.foreach(File.join(@where, 'before.dat')) do |line|
        size, time = line.split(' ')
        before[size] = time
      end
      File.foreach(File.join(@where, 'after.dat')) do |line|
        size, time = line.split(' ')
        performance_increases << (before[size].to_f - time.to_f) / before[size].to_f unless time == "0" || before[size] == "0"
      end
      puts "Average performance increase: #{(performance_increases.mean * 100 * 10).round / 10.0}%"
    end
  end
  
end

SeqParBenchmark.new.benchmark