# Demonstrates that we have a compatibility fix to mathn's weird idea of 
# integer mathematics. 
# This was contributed by Jonathan Hinkle (https://github.com/hynkle). Thanks!

$:.unshift File.dirname(__FILE__) + "/../lib"

require 'parslet'
require 'parslet/convenience'
include Parslet

def attempt_parse
  possible_whitespace = match['\s'].repeat

  cephalopod =
    str('octopus') |
    str('squid')

  parenthesized_cephalopod =
    str('(') >>
    possible_whitespace >>
    cephalopod >>
    possible_whitespace >>
    str(')')

  parser =
    possible_whitespace >>
    parenthesized_cephalopod >>
    possible_whitespace

  # This parse fails, but that is not the point. When mathn is in the current
  # ruby environment, it modifies integer division in a way that makes 
  # parslet loop indefinitely.
  parser.parse %{(\nsqeed)\n}
rescue Parslet::ParseFailed
end

attempt_parse 
puts 'it terminates before we require mathn'

puts "requiring mathn now"
require 'mathn'
puts "and trying again (will hang without the fix)"
attempt_parse # but it doesn't terminate after requiring mathn
puts "okay!"