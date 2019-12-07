require 'tempfile'
require 'cod'

require 'site'
require 'fail_site'

class Example
  def initialize(title, file, line)
    @title = title
    @file, @line = file, line
    
    @lines = []
    
    @sites = {}
    @site_by_line = {}
  end
  
  def to_s
    "'#@title'"
  end
  
  def <<(line)
    @lines << line
  end
  
  attr_reader :output
  
  def skip?
    !@lines.grep(/# =>/)
  end
  
  def run
    # Create a tempfile per output
    tempfiles = [:err, :out].inject({}) { |h, name| 
      h[name] = Tempfile.new(name.to_s); h }
    
    # Where code results are communicated.  
    $instrumentation = Cod.pipe
    
    code = produce_example_code
    pid = fork do
      redirect_streams(tempfiles)
      # puts example_code
      eval(code, nil, @file, @line-2) 
    end
    Process.wait(pid)

    # Read these tempfiles.
    @output = tempfiles.inject({}) { |h, (name, io)| 
      io.rewind
      h[name] = io.read; 
      io.close 
      h }
            
    loop do
      begin
        site_id, probe_value = $instrumentation.get
      rescue Exception => ex
        break if ex.message.match(/All pipe ends/)
      end
      fail "No such site #{site_id}." unless @sites.has_key?(site_id)

      @sites[site_id].store probe_value
    end
    
    $instrumentation.close; $instrumentation = nil

    return $?.success?
  end
  
  def redirect_streams(io_hash)
    {
      out: $stdout, 
      err: $stderr
    }.each do |name, io|
      io.reopen(io_hash[name])
    end
  end
  
  def produce_example_code
    root = File.expand_path(File.dirname(__FILE__))

    '' <<
      "$:.unshift #{root.inspect}\n" <<
      "load 'prelude.rb'\n" <<
      instrument(@lines).join("\n") <<
      "\nload 'postscriptum.rb'\n"
  end
  def instrument(code)
    code.map { |line| 
      md = line.match(/(?<pre>.*)# (?<type>=>|raises) (?<expectation>.*)/) 
      next line unless md
      
      if md[:type] == 'raises'
        site = FailSite.new(line, md[:pre], md[:expectation].strip)
      else
        site = Site.new(line, md[:pre], md[:expectation].strip) 
      end

      add_site site
      site.to_instrumented_line }
  end
  def add_site(site)
    @sites[site.id] = site
    @site_by_line[site.original_line] = site
  end
  
  def produce_modified_code
    @lines.map { |line| 
      site = @site_by_line[line]
      next line unless site 
      
      site.format_documentation_line }
  end
  
  def check_expectations
    @sites.each do |_, site|
      site.check
    end
  end
end