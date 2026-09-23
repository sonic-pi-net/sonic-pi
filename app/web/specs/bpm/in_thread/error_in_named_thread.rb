# an error in a named thread names the thread
use_bpm 120   # at twice the tempo: specs/in_thread/error_in_named_thread.rb
in_thread(name: :fragile) do
  play nosuchthing
end
