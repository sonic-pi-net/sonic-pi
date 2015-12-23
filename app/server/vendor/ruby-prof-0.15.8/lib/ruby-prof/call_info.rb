# encoding: utf-8

module RubyProf
  class CallInfo
    # part of this class is defined in C code.
    # it provides the following attributes pertaining to tree structure:
    # depth:      tree level (0 == root)
    # parent:     parent call info (can be nil)
    # children:   array of call info children (can be empty)
    # target:     method info (containing an array of call infos)

    attr_reader :recursive

    def non_recursive?
      @non_recursive
    end

    def detect_recursion(visited_methods = Hash.new(0))
      @recursive = (visited_methods[target] += 1) > 1
      @non_recursive = true
      children.each do |child|
        @non_recursive = false if child.detect_recursion(visited_methods)
      end
      visited_methods.delete(target) if (visited_methods[target] -= 1) == 0
      return !@non_recursive
    end

    def recalc_recursion(visited_methods = Hash.new(0))
      return if @non_recursive
      target.clear_cached_values_which_depend_on_recursiveness
      @recursive = (visited_methods[target] += 1) > 1
      children.each do |child|
        child.recalc_recursion(visited_methods)
      end
      visited_methods.delete(target) if (visited_methods[target] -= 1) == 0
    end

    def children_time
      children.inject(0) do |sum, call_info|
        sum += call_info.total_time
      end
    end

    def stack
      @stack ||= begin
        methods = Array.new
        call_info = self

        while call_info
          methods << call_info.target
          call_info = call_info.parent
        end
        methods.reverse
      end
    end

    def call_sequence
      @call_sequence ||= begin
        stack.map {|method| method.full_name}.join('->')
      end
    end

    def root?
      self.parent.nil?
    end

    def descendent_of(other)
      p = self.parent
      while p && p != other && p.depth > other.depth
        p = p.parent
      end
      p == other
    end

    def self.roots_of(call_infos)
      roots = []
      sorted = call_infos.sort_by(&:depth).reverse
      while call_info = sorted.shift
        roots << call_info unless sorted.any?{|p| call_info.descendent_of(p)}
      end
      roots
    end

    def to_s
      "#{target.full_name} (c: #{called}, tt: #{total_time}, st: #{self_time}, ct: #{children_time})"
    end

    def inspect
      super + "(#{target.full_name}, d: #{depth}, c: #{called}, tt: #{total_time}, st: #{self_time}, ct: #{children_time})"
    end

    # eliminate call info from the call tree.
    # adds self and wait time to parent and attaches called methods to parent.
    # merges call trees for methods called from both praent end self.
    def eliminate!
      # puts "eliminating #{self}"
      return unless parent
      parent.add_self_time(self)
      parent.add_wait_time(self)
      children.each do |kid|
        if call = parent.find_call(kid)
          call.merge_call_tree(kid)
        else
          parent.children << kid
          # $stderr.puts "setting parent of #{kid}\nto #{parent}"
          kid.parent = parent
        end
      end
      parent.children.delete(self)
    end

    # find a specific call in list of children. returns nil if not found.
    # note: there can't be more than one child with a given target method. in other words:
    # x.children.grep{|y|y.target==m}.size <= 1 for all method infos m and call infos x
    def find_call(other)
      matching = children.select { |kid| kid.target == other.target }
      raise "inconsistent call tree" unless matching.size <= 1
      matching.first
    end

    # merge two call trees. adds self, wait, and total time of other to self and merges children of other into children of self.
    def merge_call_tree(other)
      # $stderr.puts "merging #{self}\nand #{other}"
      self.called += other.called
      add_self_time(other)
      add_wait_time(other)
      add_total_time(other)
      other.children.each do |other_kid|
        if kid = find_call(other_kid)
          # $stderr.puts "merging kids"
          kid.merge_call_tree(other_kid)
        else
          other_kid.parent = self
          children << other_kid
        end
      end
      other.children.clear
      other.target.call_infos.delete(other)
    end
  end
end
