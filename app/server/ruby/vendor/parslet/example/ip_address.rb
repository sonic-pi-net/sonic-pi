# This example is heavily inspired by citrus' ip.citrus. Have a look at both
# of these to get some choice!

# The grammars in this file conform to the ABNF given in Appendix A of RFC 3986
# Uniform Resource Identifier (URI): Generic Syntax.
#
# See http://tools.ietf.org/html/rfc3986#appendix-A for more information.

$:.unshift File.dirname(__FILE__) + "/../lib"

require 'pp'
require 'parslet'

module IPv4
  include Parslet
  
  # A host identified by an IPv4 literal address is represented in
  # dotted-decimal notation (a sequence of four decimal numbers in the range 0
  # to 255, separated by "."), as described in [RFC1123] by reference to
  # [RFC0952].  Note that other forms of dotted notation may be interpreted on
  # some platforms, as described in Section 7.4, but only the dotted-decimal
  # form of four octets is allowed by this grammar.
  rule(:ipv4) {
    (dec_octet >> str('.') >> dec_octet >> str('.') >>
      dec_octet >> str('.') >> dec_octet).as(:ipv4)
  }
  
  rule(:dec_octet) {
    str('25') >> match("[0-5]") |
    str('2') >> match("[0-4]") >> digit |
    str('1') >> digit >> digit |
    match('[1-9]') >> digit |
    digit
  }
  
  rule(:digit) {
    match('[0-9]')
  }
end

# Must be used in concert with IPv4
module IPv6 
  include Parslet
  
  rule(:colon) { str(':') }
  rule(:dcolon) { colon >> colon }
  
  # h16 :
  def h16r(times)
    (h16 >> colon).repeat(times, times)
  end
  
  # : h16
  def h16l(times)
    (colon >> h16).repeat(0,times)
  end
  
  # A 128-bit IPv6 address is divided into eight 16-bit pieces. Each piece is
  # represented numerically in case-insensitive hexadecimal, using one to four
  # hexadecimal digits (leading zeroes are permitted). The eight encoded
  # pieces are given most-significant first, separated by colon characters.
  # Optionally, the least-significant two pieces may instead be represented in
  # IPv4 address textual format. A sequence of one or more consecutive
  # zero-valued 16-bit pieces within the address may be elided, omitting all
  # their digits and leaving exactly two consecutive colons in their place to
  # mark the elision.
  rule(:ipv6) {
    (
      (
        h16r(6) |
        dcolon >> h16r(5) | 
        h16.maybe >> dcolon >> h16r(4) |
        (h16 >> h16l(1)).maybe >> dcolon >> h16r(3) |
        (h16 >> h16l(2)).maybe >> dcolon >> h16r(2) |
        (h16 >> h16l(3)).maybe >> dcolon >> h16r(1) |
        (h16 >> h16l(4)).maybe >> dcolon
      ) >> ls32 |
      (h16 >> h16l(5)).maybe >> dcolon >> h16 |
      (h16 >> h16l(6)).maybe >> dcolon
    ).as(:ipv6)
  }
  
  rule(:h16) {
    hexdigit.repeat(1,4)
  }
  
  rule(:ls32) {
    (h16 >> colon >> h16) |
    ipv4
  }

  rule(:hexdigit) {
    digit | match("[a-fA-F]")
  }
end

class Parser
  include IPv4
  include IPv6
  
  def parse(str)
    (ipv4 | ipv6).parse(str)
  end
end

%W(
  0.0.0.0
  255.255.255.255
  255.255.255
  1:2:3:4:5:6:7:8
  12AD:34FC:A453:1922::
  12AD::34FC
  12AD::
  ::
  1:2
).each do |address|
  parser = Parser.new
  printf "%30s -> ", address
  begin
    result = parser.parse(address)
    puts result.inspect
  rescue Parslet::ParseFailed => m
    puts "Failed: #{m}"
  end
end
