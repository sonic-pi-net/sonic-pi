# SPDX-License-Identifier: AGPL-3.0-or-later
module SonicPi
  # Sonic Pi's Time State: every cue the session has seen, filed under its own
  # address.
  #
  # A sync or a get walks to the address it asked about and reads the cues sent
  # there, and nothing else. A single list read end to end by every sync and
  # every get would make each cue the session had ever seen cost about three
  # microseconds on every tick after it: a MIDI controller sending 140 a
  # second would put every sound out late, by more the longer you played,
  # heard as notes arriving unevenly.
  #
  # Native Sonic Pi's event_history.rb is the same shape, and the reason to
  # follow it. What is deliberately not here is its wait_for_threads, which
  # sleeps a millisecond before every read to give its real threads a chance to
  # write first. Ours are resumed one at a time in key order, so what counts as
  # "before me" is settled by CueEvent.compare rather than by who got there
  # first, and there is nothing to wait for.
  class CueHistory
    # One address. Its children are the addresses below it; its events are the
    # cues sent to it exactly, in the order they arrived.
    class Node
      attr_reader :children, :events
      def initialize
        @children = {}
        @events = []
      end
    end

    def initialize
      @root = Node.new
      @nodes = [@root]   # every node, flat: trim! walks these rather than the tree
      @count = 0
    end

    attr_reader :count

    # The cue, under its address. "/midi:t:1/cc" files under "" → "midi:t:1" → "cc",
    # the same split a pattern is matched by, so the two always agree.
    def add(ev)
      node = @root
      each_segment(ev.address) do |seg|
        child = node.children[seg]
        unless child
          child = Node.new
          node.children[seg] = child
          @nodes << child
        end
        node = child
      end
      node.events << ev
      @count += 1
      ev
    end

    # The earliest cue to any of these addresses after this key: what a sync
    # takes when the cue it waits for has already been sent. The addresses may
    # be OSC patterns (Scheduler::ADDRESS_PATTERN).
    def after(key, addresses)
      best = nil
      each_node(addresses) do |node|
        node.events.each do |ev|
          next unless CueEvent.compare(ev.key, key) > 0
          best = ev if best.nil? || CueEvent.compare(ev.key, best.key) < 0
        end
      end
      best
    end

    # The latest cue to one of these addresses strictly before this key: what
    # get reads. Exact addresses only, as get has always taken.
    def before(key, addresses)
      best = nil
      addresses.each do |address|
        node = node_at(address) or next
        node.events.each do |ev|
          next unless CueEvent.compare(ev.key, key) < 0
          best = ev if best.nil? || CueEvent.compare(ev.key, best.key) > 0
        end
      end
      best
    end

    # What no thread can still sync on goes, address by address; the newest at
    # each address stays, since that is what get reads however long ago it was
    # sent. The horizon is the earliest moment a thread can still be at, and
    # nil when none can: then only the newest is worth keeping, because
    # whatever runs next starts after every cue here.
    #
    # This only bounds memory. What a read costs does not depend on how much
    # is kept, which is the whole point of filing cues by address.
    def trim!(horizon)
      return if @count < 256
      @count = 0
      @nodes.each do |node|
        events = node.events
        if events.size > 1
          newest = events[0]
          events.each { |ev| newest = ev if CueEvent.compare(ev.key, newest.key) > 0 }
          events = events.select { |ev| (horizon && ev.time >= horizon) || ev.equal?(newest) }
          node.events.replace(events)
        end
        @count += events.size
      end
    end

    private

    def each_segment(address)
      from = 0
      while (at = address.index("/", from))
        yield address[from...at]
        from = at + 1
      end
      yield address[from..] || ""
    end

    def node_at(address)
      node = @root
      each_segment(address) do |seg|
        node = node.children[seg]
        return nil unless node
      end
      node
    end

    # Every node an address asks for: the one node when it names an address,
    # and each node a pattern reaches when it is one. A pattern part never
    # crosses a "/" (Scheduler.address_re builds "*" as "[^/]*"), so a pattern
    # matches segment by segment and reaches only its own depth.
    def each_node(addresses, &block)
      addresses.each do |address|
        if address.match?(Scheduler::ADDRESS_PATTERN)
          segs = []
          each_segment(address) { |s| segs << s }
          walk(@root, segs, 0, &block)
        else
          node = node_at(address)
          block.call(node) if node
        end
      end
    end

    def walk(node, segs, idx, &block)
      return block.call(node) if idx == segs.size
      seg = segs[idx]
      if seg.match?(Scheduler::ADDRESS_PATTERN)
        re = Scheduler.address_re(seg)
        node.children.each { |name, child| walk(child, segs, idx + 1, &block) if re.match?(name) }
      else
        child = node.children[seg]
        walk(child, segs, idx + 1, &block) if child
      end
    end
  end
end
