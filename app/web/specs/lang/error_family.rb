# native's language errors are one family: a map's odd args and a get from a future time_warp are SonicPiErrors,
# the second a TimingError; a single odd arg is Ruby's own complaint
begin
  map :a, 1, :b
rescue SonicPi::Lang::Core::SonicPiError => e
  puts e.class
end
begin
  map :a
rescue ArgumentError => e
  puts e.message
end
set :x, 1
time_warp 1 do
  begin
    get(:x)
  rescue SonicPi::Lang::Core::TimingError => e
    puts e.message
  end
end
