# a thread name given as a string is not the same name as the symbol: both threads run
in_thread(name: "drums") do
  play 36
end
in_thread(name: :drums) do
  play 99
end
