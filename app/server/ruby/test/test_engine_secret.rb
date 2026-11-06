#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

# The per-boot secret SuperSonic asks of every client (its native guide,
# "The secret"): 32 random bytes the daemon makes, writes to the engine's
# stdin as 64 hex digits and a newline, and hands to the GUI and Spider.

require 'minitest/autorun'
require_relative '../lib/sonicpi/engine_secret'

class EngineSecretTest < Minitest::Test
  def test_a_secret_is_32_random_bytes
    a = SonicPi::EngineSecret.generate
    b = SonicPi::EngineSecret.generate
    assert_equal 32, a.bytesize
    assert_equal Encoding::BINARY, a.encoding
    refute_equal a, b
  end

  def test_the_stdin_line_is_64_lowercase_hex_digits_and_a_newline
    secret = SonicPi::EngineSecret.generate
    line = SonicPi::EngineSecret.stdin_line(secret)
    assert_match(/\A[0-9a-f]{64}\n\z/, line)
    assert_equal secret, SonicPi::EngineSecret.from_hex(line.chomp)
  end

  def test_from_hex_refuses_anything_but_32_bytes
    assert_raises(ArgumentError) { SonicPi::EngineSecret.from_hex("abcd") }
    assert_raises(ArgumentError) { SonicPi::EngineSecret.from_hex("zz" * 32) }
  end
end
