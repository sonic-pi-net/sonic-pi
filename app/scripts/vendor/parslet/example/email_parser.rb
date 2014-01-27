#!/usr/bin/env ruby

# Example contributed by Hal Brodigan (postmodern). Thanks!

$:.unshift File.dirname(__FILE__) + "/../lib"
require 'parslet'
require 'parslet/convenience'

class EmailParser < Parslet::Parser
  rule(:space) { match('\s').repeat(1) }
  rule(:space?) { space.maybe }
  rule(:dash?) { match['_-'].maybe }

  rule(:at) {
    str('@') |
    (dash? >> (str('at') | str('AT')) >> dash?)
  }
  rule(:dot) {
    str('.') |
    (dash? >> (str('dot') | str('DOT')) >> dash?)
  }

  rule(:word) { match('[a-z0-9]').repeat(1).as(:word) >> space? }
  rule(:separator) { dot.as(:dot) >> space? | space }
  rule(:words) { word >> (separator >> word).repeat }

  rule(:email) {
    (words.as(:username) >> space? >> at >> space? >> words).as(:email)
  }

  root(:email)
end

class EmailSanitizer < Parslet::Transform
  rule(:dot => simple(:dot), :word => simple(:word)) { ".#{word}" }
  rule(:word => simple(:word)) { word }

  rule(:username => sequence(:username)) { username.join + "@" }
  rule(:username => simple(:username)) { username.to_s + "@" }

  rule(:email => sequence(:email)) { email.join }
end

parser = EmailParser.new
sanitizer = EmailSanitizer.new

input = ARGV[0] || begin
  default = "a.b.c.d@gmail.com"
  STDERR.puts "usage: #{$0} \"EMAIL_ADDR\""
  STDOUT.puts "since you haven't specified any EMAIL_ADDR, for testing purposes we're using #{default}"
  default
end

p sanitizer.apply(parser.parse_with_debug(input))
