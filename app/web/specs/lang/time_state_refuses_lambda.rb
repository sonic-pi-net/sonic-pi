# a lambda cannot be kept in Time State: it is not thread safe
set :f, lambda { 1 }
