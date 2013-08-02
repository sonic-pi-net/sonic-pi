class Time
  
  def to_ntp
    self.to_f + 2208988800
  end 
end