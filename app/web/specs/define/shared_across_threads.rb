# functions are shared: defined in one thread, callable from another
define :ping do
  play 84
end
in_thread do
  sleep 0.25
  ping
end
ping
in_thread do
  define :pong do
    play 48
  end
end
sleep 0.5
pong
