# an fx inside a thread wraps only that thread's synths
in_thread do
  with_fx :reverb do
    play 60
  end
end
play 62
