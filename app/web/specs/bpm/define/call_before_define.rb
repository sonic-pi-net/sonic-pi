# calling a function before it exists is an error, with and without args
use_bpm 120   # at twice the tempo: specs/define/call_before_define.rb
begin
  later
rescue NameError => e
  puts e.class.name
end
later(1)
