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
    a = :osc.encode([~c"/test", 1, 3.5, -100, -3.5, ~c"bar", true, false])
    {:cmd, [~c"/test", 1, 3.5, -100, -3.5, ~c"bar", true, false]} = :osc.decode(a)
  end

  test "osc encode/decode int64" do
    a = :osc.encode([~c"/testi64", {:int64, 347_873_045_749_854}])
    {:cmd, [~c"/testi64", 347_873_045_749_854]} = :osc.decode(a)
  end

  test "decode osc binary" do
    bin =
      <<35, 98, 117, 110, 100, 108, 101, 0, 218, 114, 254, 188, 137, 88, 216, 0, 0, 0, 0, 16, 47,
        102, 111, 111, 0, 0, 0, 0, 44, 115, 0, 0, 98, 97, 114, 0>>

    {:bundle, _, [[~c"/foo", ~c"bar"]]} = :osc.decode(bin)
  end

  test "making a bundle with a timestamp" do
    t = :osc.now()
    bin = :osc.pack_ts(t, [~c"/forward", ~c"localhost", 6000, ~c"/sendmidi", 12, 34, 56])

    {:bundle, new_t, [[~c"/forward", ~c"localhost", 6000, ~c"/sendmidi", 12, 34, 56]]} =
      :osc.decode(bin)

    # take into account encoding/decoding rounding differences
    assert(abs(t - new_t) < 0.0001)
  end
end
