class Numeric 
  # Convert time intervals to seconds 
  def milliseconds; self/1000.0; end 
  def seconds; self; end 
  def minutes; self*60; end 
  def hours; self*60*60; end 
  def days; self*60*60*24; end 
  def weeks; self*60*60*24*7; end 
  
  # Convert seconds to other intervals 
  def to_milliseconds; self*1000; end 
  def to_seconds; self; end 
  def to_minutes; self/60.0; end 
  def to_hours; self/(60*60.0); end 
  def to_days; self/(60*60*24.0); end 
  def to_weeks; self/(60*60*24*7.0); end 
end 
