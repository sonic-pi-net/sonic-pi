 # --
 # This file is part of Sonic Pi: http://sonic-pi.net
 # Full project source: https://github.com/sonic-pi-net/sonic-pi
 # License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md

 # Copyright 2021 by Sam Aaron (http://sam.aaron.name/)
 # All rights reserved.

 # Permission is granted for use, copying, modification, and
 # distribution of modified versions of this work as long as this
 # notice is included.
 # ++

defmodule ErlangTauOSCTest do
  use ExUnit.Case

  test "basic osc encode/decode" do
    a = :osc.encode(['/test', 1, 3.5, -100, -3.5, 'bar', true, false])
    {:cmd, ['/test', 1, 3.5, -100, -3.5, 'bar', true, false]} = :osc.decode(a)
  end

  test "osc encode/decode int64" do
    a = :osc.encode(['/testi64', {:int64, 347873045749854}])
    {:cmd, ['/testi64', 347873045749854]} = :osc.decode(a)
  end

  test "decode osc binary" do
    bin = (<<35,98,117,110,100,108,101,0,
      218,114,254,188,137,88,216,0,0,0,0,16,
      47,102,111,111,0,0,0,0,44,115,0,0,98,97,114,0>>)

    {:bundle, _, [{_n, internal_osc}]} = :osc.decode(bin)
    {:cmd, ['/foo', 'bar']} = :osc.decode(internal_osc)

  end

  test "making a bundle with a timestamp" do
    t = :osc.now()
    bin = :osc.pack_ts(t, ['/forward', 'localhost', 6000, '/sendmidi', 12, 34, 56])
    {:bundle, new_t, [{_n, internal_osc}]} = :osc.decode(bin)
    # take into account encoding/decoding rounding differences
    assert(abs(t - new_t) < 0.0001)
    {:cmd, ['/forward', 'localhost', 6000, '/sendmidi', 12, 34, 56]} = :osc.decode(internal_osc)
  end
end
