# a path that does not exist is skipped with a message, not an error
use_bpm 120   # at twice the tempo: specs/sample/missing_path.rb
sample "/nonexistent/thing.wav"
play 60
