$captured_out = []

def $captured_out.write(str)
  push str
end

def $captured_out.close
end

def get_captured_output
  size = $captured_out.size
  $captured_out.shift(size).join
end
