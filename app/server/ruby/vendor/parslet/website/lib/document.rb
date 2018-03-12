class Document
  def initialize(name)
    @name = name
    @state = :outside
    @line_count = 1
  end
  
  def process
    @target = Tempfile.new('erdoc')

    File.open(@name, 'r') do |original|
      while line = original.gets
        consume(line.chomp)
      end
    end
    
    @target.close(false)
    FileUtils.mv(@target.path, @name)
  end
  
  def consume(line)
    @line_count += 1
    
    a = lambda { |*args| Case::Array[*args] }
    any = Case::Any
    
    case [@state, line]
      when a[:outside, /<pre class="sh_ruby"><code title="(.*)">/]
        @target.puts(line)
        @state = :inside
        extract_title(line)        
      when a[:inside, %r(</code></pre>)]
        @state = :outside
        run_example
        
      when a[:outside, /<pre class="output">/]
        @target.puts(line)
        @state = :inside_old_output
      when a[:inside_old_output, %r(</pre>)]
        @target.puts(@last_output) if @last_output
        @state = :outside

      when a[:inside, any]
        @example << line
    else
      # do nothing
    end
    
    # Write lines outside a code block directly to target.
    @target.puts(line) if @state == :outside
  end
  
  def run_example
    String.highlighter = Text::ANSIHighlighter.new
    print " Running #@example... "
    
    if @example.skip?
      puts "Skipped, no inspection points."
      return
    end
    
    # Prevent Ruby from buffering the script for too long. After a fork, 
    # Ruby buffers become a problem. 
    @target.flush
    
    unless @example.run
      puts "error".red
      @example.output[:err].lines.each { |line| 
        print "   " + line.magenta }
    else
      puts 'ok.'.green
      
      # Stores last output for an eventual <pre class="output"> that might 
      # come somewhere.
      @last_output = @example.output[:out]
    end
    
    @example.check_expectations

    @target.puts @example.produce_modified_code
    @example = nil
  end
  def extract_title(line)
    if md=line.match(/title="(.*)"/)
      @example = Example.new(md[1], @name, @line_count)
    end
  end
end