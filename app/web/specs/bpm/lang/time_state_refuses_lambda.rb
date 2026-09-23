# a lambda cannot be kept in Time State: it is not thread safe
use_bpm 120   # at twice the tempo: specs/lang/time_state_refuses_lambda.rb
set :f, lambda { 1 }
