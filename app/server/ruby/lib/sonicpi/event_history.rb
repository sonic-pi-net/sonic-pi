#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2017 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "cueevent"

module SonicPi

  module EventMatcherUtil
    def safe_matcher_call(matcher, event)
      return true unless matcher
      begin
        return matcher.call(event)
      rescue Exception
        return false
      end
    end
  end

  class EventHistoryNode
    attr_accessor :children, :events

    def initialize
      @children = {}
      @events = []
    end

    def count_nodes(node_total=0, event_total=0)

      node_total += 1
      event_total += @events.size

      @children.map do |k, n|
        nt, et = n.count_nodes(node_total, event_total)
        node_total += nt
        event_total += et
      end

      [node_total, event_total]
    end
  end

  class EventMatcher
    include EventMatcherUtil

    attr_reader :handle, :prom, :ce

    def initialize(ce, val_matcher=nil, handle=nil, prom=nil)
      path = String.new(ce.path)

      # get rid of white space
      path.strip!

      # remove initial / if present
      path[0] = '' if path.start_with?('/')

      path = Regexp.escape(path)

      # replace glob-style ** with regexp .*
      path.gsub!(/\/\s*\\\*\\\*\s*\//, '/.*/')

      # replace ** at end of string (sans /) with .*
      path.gsub!(/\/\s*\\\*\\\*\s*\Z/, '/.*')

      # handle standard *foo, bar* and baz*boz
      path.gsub!(/(?<!\.)\\\*/, '[^/]*')

      # handle word options /{foo,bar}/baz
      path.gsub!(/\\\{([^\/]*)\\\}/, '(\1)')

      # handle char ranges /[a-g]oo/baz/
      path.gsub!(/\\\[!([^\/]*)\\\]/, '[^\1]')

      # handle negative char ranges /[!a-g]/baz
      path.gsub!(/\\\[([^!\/]+[^\/]*)\\\]/, '[\1]')

      # swap , for | and unescape - for char ranges
      path.gsub!(',', '|')
      path.gsub!('\\-', '-')

      # handle single chars /?oo/baz/
      path.gsub!('\\?', '.')

      # convert to a regexp
      matcher_str = "\\A/?#{path}/?\\Z"

      @matcher = Regexp.new(matcher_str)

      @val_matcher = val_matcher
      @alive = true
      @prom = prom
      @handle = handle
      @ce = ce
    end

    def kill
      @alive = false
    end

    def dead?
      !@alive
    end

    def path_match(path, val=:sonic_pi_no_match_val)

      if @val_matcher && (val != :sonic_pi_no_match_val)
        @matcher.match(path) && safe_matcher_call(@val_matcher, val)
      else
        @matcher.match(path)
      end
    end
  end


  class EventMatchers
    attr_reader :matchers

    def initialize
      @matchers = []
    end

    def put(ce, val_matcher, thread_id, prom)
      matcher = EventMatcher.new(ce, val_matcher, thread_id, prom)
      @matchers << matcher
      return matcher
    end

    def match(ce)
      @matchers.delete_if do |matcher|
        if matcher.path_match(ce.path, ce.val) && ce > matcher.ce
          matcher.prom.deliver! true if matcher.prom
          matched = true
        else
          matched = false
        end
        matcher.dead? || matched
      end
    end

    def prune(handle_to_remove)
      @matchers.delete_if { |m| m.dead? || m.handle == handle_to_remove }
    end
  end



  class EventHistory
    include EventMatcherUtil
    include Util

    attr_accessor :event_matchers

    def initialize(all_threads=nil, thread_mut=nil)
      @trim_history = true
      @min_history_size = 20
      @history_depth = 32
      @state = EventHistoryNode.new
      @event_matchers = EventMatchers.new
      @process_mut = Mutex.new
      @matcher_mut = Mutex.new
      @sync_notification_mut = Mutex.new
      @sync_notifiers = Hash.new([])
      @get_mut = Mutex.new
    end

    def size_info
      s = @state.count_nodes
      "nodes: #{s[0]}, events: #{s[1]}"
    end

    # Get the last seen version (at or before the current time)
    def get(t, p, i, d, b, m, path, val_matcher=nil, get_next=false)
      wait_for_threads(t)
      res = nil
      get_event = CueEvent.new(t, p, i, d, b, m, path, [])
      res = get_w_mutex(get_event, val_matcher, get_next)
      return res
    end

    # Get next version (after current time)
    # return nil if nothing found
    def get_next(t, p, i, d, b, m, path, val_matcher=nil)
      get(t, p, i, d, b, m, path, val_matcher, true)
    end

    # Register cue event for time t
    # Do not modify time
    def set(t, p, i, d, b, m, path, val)
      ce = CueEvent.new(t, p, i, d, b, m, path, val)
      @process_mut.synchronize do
        __insert_event!(ce)
      end
      @matcher_mut.synchronize do
        @event_matchers.match(ce)
      end
    end

    # Get the next version (after the current time)
    # Set time to time of cue

    def sync(t, p, i, d, b, m, path, val_matcher=nil)

      wait_for_threads(t)
      prom = nil
      ge = CueEvent.new(t, p, i, d, b, m, path, [])
      res = get_w_mutex(ge, val_matcher, true)
      return res if res
      prom = Promise.new
      @matcher_mut.synchronize do
        @event_matchers.put ge, val_matcher, i, prom
      end
      prom.get
      # have to do a get_next again in case
      # an event with an earlier timestamp arrived
      # after this one
      wait_for_threads(t)
      res = get_w_mutex(ge, val_matcher, true)
      if res
        return res
      end
      raise "sync error - couldn't find result for #{[t.to_f, i, p, d, b, path]}"
    end

    # Wait for the first out of the list of cues to arrive
    # Set time to time of first cue
    # Once first cue has arrived - no longer wait for other cues
    def sync_first(t, p, i, d, b, m, paths, val_matcher, timeout=nil)

    end

    # Wait for all cues to arrive (after the current time)
    # Set time to time of last cue
    def sync_all(t, p, i, d, b, m, paths, val_matcher, timeout=nil)

    end

    def prune(thread_id)
      @get_mut.synchronize do
        @event_matchers.prune(thread_id)
      end
    end

    @@split_path_cache = Hash.new
    private
    def get_w_mutex(ge, val_matcher, get_next=false)
      # get value or return default
      if ge.path.start_with? '/'
        if ge.path.include?('/**/**')
          path = String.new(ge.path)  # multiple sequential ** matchers
        else
          path = ge.path
        end
      else
        path = String.new("/#{ge.path}")
      end

      # Remove multiple sequential ** matchers
      path.gsub!(/(\/\*\*)+/, '/**') if ge.path.include?('/**/**')

      split_path = @@split_path_cache.fetch(path) do |k|
        @@split_path_cache[path] = path.split('/').drop(1).map do |segment|
          stripped = segment.strip
          if stripped == '**'
            stripped
          elsif matcher?(segment)
            segment = Regexp.escape(segment)
            segment.gsub!('\*', '.*')
            segment.gsub!(/\\\{(.*)\\\}/, '(\1)')
            segment.gsub!(',', '|')
            segment.gsub!('\?', '.')
            segment.gsub!(/\\\[([^!].*)\\\]/, '[\1]')
            segment.gsub!(/\\\[!(.*)\\\]/, '[^\1]')
            segment.gsub!('\-', '-')
            begin
              Regexp.new(/\A#{segment}\Z/)
            rescue
              stripped
            end
          else
            stripped
          end
        end
      end
      @process_mut.synchronize do
        return __get(ge, split_path, 0, val_matcher, @state, nil, get_next)
      end

    end

    def __get(ge, split_path, idx, val_matcher, sn, res, get_next)
      if idx == split_path.size
        # we are at the leaf node
        # see if we can find a result!
        if get_next
          return find_next_event(ge, val_matcher, sn.events)
        else
          return find_most_recent_event(ge, val_matcher, sn.events)
        end
      end

      # abort early if we know there's nothing good at this
      # node or in its children
      path_segment = split_path[idx]
      return res unless path_segment
      if path_segment.is_a?(Regexp)
        sn.children.each do |k, v|
          if path_segment.match(k)
            res2 = __get(ge, split_path, idx+1, val_matcher, v, res, get_next)
            if res2
              if res
                if get_next
                  res = res2 if res2 < res
                else
                  res = res2 if res2 > res
                end
              else
                res = res2
              end
            end
          end
        end
      elsif path_segment == '**'
        if split_path.size - 1 == idx
          # this is the last path_segment
          # search through all remaining ancestors for the
          # first logically timed event

          if get_next
            return next_ancestor_event(ge, val_matcher, sn, res)
          else
            return most_recent_ancestor_event(ge, val_matcher, sn, res)
          end
        else
          # if there is a next path segment then do a search
          # through all ancesters but only as far down as
          # ones with grand children matching the next segment
          # then continue as normal
          matching_ancestors(split_path[idx + 1], sn).each do |an|
            res2 = __get(ge, split_path, idx+2, val_matcher, an, res, get_next)
            if res2
              if res
                if get_next
                  res = res2 if res2 < res
                else
                  res = res2 if res2 > res
                end
              else
                res = res2
              end
            end
          end
        end
      else
        v = sn.children[path_segment]
        if v
          res2 = __get(ge, split_path, idx+1, val_matcher, v, res, get_next)
          if res2
            if res
              if get_next
                res = res2 if res2 < res
              else
                res = res2 if res2 > res
              end

            else
              res = res2
            end
          end
        end
      end
      return res

    end


    def __insert_event!(e, idx=0, sn=@state)
      if idx == e.path_size
        # we are at the leaf node

        sn.events.unshift(e)
        bubble_up_sort!(sn.events)

        # Auto-trim history Keep at least @min_history_size elements and
        # only remove elements older than @history_depth seconds ago
        # (this may be opened to tuning in the future)
        if @trim_history
          cutoff_t = (Time.now - @history_depth).to_i
          while (sn.events.size > @min_history_size) && (sn.events.last.time.to_i < cutoff_t)
            sn.events.pop
          end
        end
        return sn
      end

      # we are not at a leaf node, drill down....

      # get path segment

      path_segment = e.path_segment(idx)
      raise "Error inserting event - idx grew too large (#{idx} is bigger than #{e.path_size})" unless path_segment

      # get (or create) child node

      child_node = sn.children[path_segment] ||= EventHistoryNode.new

      # insert event into the child node
      __insert_event!(e, idx + 1, child_node)
      return sn
    end

    def bubble_up_sort!(events)
      # we assume that the events list is already ordered
      # however the item at idx may not be in the correct
      # place - therefore bubble it up by swapping with
      # the preceding elements in turn until the correct
      # place is found.

      idx = 0

      while (idx < events.size - 1) && (events[idx] < events[idx + 1])
        events[idx], events[idx + 1] = events[idx + 1], events[idx]
        idx += 1
      end
    end

    def wait_for_threads(vt)
      # Time sync on all other threads checking their last write promise
      # times

      # unless @all_threads
      Kernel.sleep 0.001
      return true
      # end

      # The code below is a work in progress. For now, simply sleep on
      # reads, but eventually we will just wait for all the tother
      # hreads to have moved on sufficiently to ensure deterministic
      # behaviour.

      promises = []

      # p = __system_thread_locals.get(:sonic_pi_spider_thread_priority, 0)
      # id = __system_thread_locals.get :sonic_pi_spider_thread_id_path

      # First grab the currently running threads. We do this by
      # obtaining a lock to the creation of new threads so we can get
      # a consistent view.
      current_threads = []
      @thread_mut.synchronize do
        @all_threads.values.each do |thread_set|
          thread_set.each do |t|
            current_threads << t
          end
        end
      end

      # Work through each thread and see if it's behind or ahead of
      # us. If it's behind, we ask it to notify us when it jumps ahead
      # by inserting a promise into its state waiters list.
      current_threads.each do |t|
        unless t == Thread.current
          __system_thread_locals(t).get(:sonic_pi_spider_time_change).synchronize do
            tvt = __system_thread_locals(t).get(:sonic_pi_spider_time)
            if tvt && ((tvt <= vt))
              prom = Promise.new
              promises << prom
              waiters = __system_thread_locals(t).get(:sonic_pi_spider_state_waiters)
              waiters << {:vt => vt, :prom => prom}
            end
          end
        end

      end

      if promises.empty?
        return true
      else
        # we have to wait for at least one thread, block until we can
        # continue

        promises.each { |p| p.get }
        # The threads we waited for might have spawned new
        # threads. Check again...
        wait_for_threads(vt)
      end
    end

    def matching_ancestors(partial, n, res=[])
      matcher = partial.is_a? Regexp
      n.children.each do |k, v|
        if matcher
          res << v if partial.match(k)
        else
          res << v if partial == k
        end

        matching_ancestors(partial, v, res)
      end
      return res
    end


    def most_recent_ancestor_event(ge, val_matcher, n, res)
      n.children.values.each do |c|
        candidate = find_most_recent_event(ge, val_matcher, c.events)
        if candidate
          if res
            res = candidate if candidate > res
          else
            res = candidate
          end
        end

        ancestor_candidate = most_recent_ancestor_event(ge, val_matcher, c, res)
        if ancestor_candidate
          if res
            res = ancestor_candidate if ancestor_candidate > res
          else
            res = ancestor_candidate
          end
        end
      end
      return res
    end

    def next_ancestor_event(ge, val_matcher, n, res)
      n.children.values.each do |c|
        candidate = find_next_event(ge, val_matcher, c.events)
        if candidate
          if res
            res = candidate if candidate < res
          else
            res = candidate
          end
        end

        ancestor_candidate = next_ancestor_event(ge, val_matcher, c, res)
        if ancestor_candidate
          if res
            res = ancestor_candidate if ancestor_candidate < res
          else
            res = ancestor_candidate
          end
        end
      end
      return res
    end

    def find_most_recent_event(ge, val_matcher, events)
      if val_matcher
        events.find { |e|  e <= ge  && safe_matcher_call(val_matcher, e.val) }
      else
        events.find { |e|  e <= ge }
      end
    end

    def find_next_event(ge, val_matcher, events)
      return nil if events.empty?

      # events are ordered with events[0] > events[max] later events are
      # therefore closer to the start of the list

      # Find the first match where time is greater than time t, d events
      # are ordered largest t ... smallest t so actually find the first
      # match where event's time is same or lt t, d
      #
      # Additionally if val_matcher is not nil, it is assumed to be a
      # lambda representing an arg matching fn. This will then be used
      # asqo a constraint over the val when finding a given event.

      # Find the first event that's less than the time t, d.

      idx = events.find_index { |e| e <= ge }
      if idx && idx > 0
        return events[idx - 1] unless val_matcher
        while idx > 0
          idx -= 1
          return events[idx] if safe_matcher_call(val_matcher, events[idx].val)
        end
      end

      last = events.last
      if val_matcher
        return last if last && (last > ge) && safe_matcher_call(val_matcher, last.val)
      else
        return last if last && (last > ge)
      end
      return nil
    end

    def matcher?(p)
      p.include?('*') || p.include?('{') || p.include?('?') || p.include?('[')
    end
  end
end
