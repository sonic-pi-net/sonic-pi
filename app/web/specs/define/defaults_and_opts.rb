# default parameters and an opts hash behave as Ruby's do
define :note_with do |n = 60, opts = {}|
  play n, opts
end
note_with
note_with 62
note_with 64, amp: 0.5
