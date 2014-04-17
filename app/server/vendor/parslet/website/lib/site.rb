class Site
  attr_reader :original_line
  
  def initialize(original_line, code, expectation)
    @original_line = original_line
    @code = code
    @expectation = expectation
    @values = []
  end
  def id
    object_id
  end
  
  def to_instrumented_line
    "begin "+
      "(#@code).tap { |o| $instrumentation.put [#{id}, [:ok, o]] }; "+
    "rescue Exception => exception; "+
      "$instrumentation.put [#{id}, [:raised, exception]];"+
      "raise;"+
    "end"
  end
  
  def format_documentation_line
    value_str = format_values
    "#@code# => #{value_str}"
  end
  def format_values
    return 'NOT REACHED!' if @values.empty?
    
    v = @values.size == 1 ? @values.first : @values
    s = v.inspect
    
    max_len = 60 - 3
    s.size > max_len ? s[0,max_len] + '...' : s
  end
  def check
    return true if !@expectation || @expectation.match(/^\s*$/)

    str = format_values
    if str != @expectation
      puts "      #{@code.strip} # => #{str.red}"
      puts "      #{' '*@code.strip.size} # expected: #@expectation"
    else
      puts "      #{@code.strip} # => #{str.green}"
    end
  end
  def store(msg)
    store_if(:ok, msg)
  end
  
private
  def store_if(cond, msg)
    code, value = msg 
    @values << value if code == cond
  end
end
