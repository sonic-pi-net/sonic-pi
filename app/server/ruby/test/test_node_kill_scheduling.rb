#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "./setup_test"
require_relative "../lib/sonicpi/node"
require_relative "../lib/sonicpi/group"

module SonicPi
  # Node lifecycle commands travel by one of two routes, and which one is
  # chosen decides whether the server can reorder them:
  #
  #   now == false -> a timestamped bundle, queued and run by the server's
  #                   own clock, in order with every other timestamped
  #                   message (which is how all synth triggers are sent)
  #   now == true  -> sent immediately, skipping that queue entirely
  #
  # Mixing the two is an ordering inversion waiting to happen. An immediate
  # kill can overtake a trigger that was sent first but is still waiting on
  # its timestamp, and the server then reports
  # "/s_new failed - Group N not found" while the note is silently dropped.
  # The margin that hides this is jitter-sized, which is why it showed up on
  # a host with a wobblier clock and not on a steadier one.
  class NodeKillSchedulingTester < Minitest::Test

    # Records how each kill was routed, standing in for the comms layer.
    class FakeComms
      attr_reader :kills

      def initialize
        @kills = []
      end

      def kill_node(node, now)
        @kills << { id: node.to_i, now: now }
      end

      # Node registers server-event handlers on construction; the routing
      # under test does not depend on them.
      def async_add_event_handlers(*); end
      def async_rm_event_handlers(*); end
    end

    def setup
      @comms = FakeComms.new
    end

    def test_kill_defaults_to_scheduled
      # The default matters: the FX teardown path calls kill with no
      # argument, relying on it queueing behind already-triggered synths.
      node = Node.new(42, @comms)
      node.kill

      assert_equal 1, @comms.kills.size
      assert_equal 42, @comms.kills.first[:id]
      refute @comms.kills.first[:now],
             "kill must default to the scheduled route, or it can overtake " \
             "a synth trigger that was queued before it"
    end

    def test_kill_now_is_available_but_explicit
      # Immediate remains reachable — it is right when the point IS to
      # bypass the queue (a panic stop) — but it has to be asked for.
      node = Node.new(43, @comms)
      node.kill(true)

      assert_equal 1, @comms.kills.size
      assert @comms.kills.first[:now]
    end

    def test_groups_follow_the_same_rule
      # The FX container is a Group; it inherits Node's routing and must not
      # diverge from it.
      group = Group.new(44, @comms)
      group.kill

      assert_equal 1, @comms.kills.size
      refute @comms.kills.first[:now]
    end

    def test_a_destroyed_node_is_not_freed_again
      # /n_end already confirmed the node is gone. Freeing it a second time
      # only draws "Node N not found" from the server — the benign-but-noisy
      # warning that sits alongside the real one.
      node = Node.new(45, @comms)
      node.send(:handle_n_end, nil)   # the server's /n_end
      assert node.destroyed?
      node.kill

      assert_empty @comms.kills
    end
  end
end
