# encoding: utf-8

module RubyProf
  class MethodInfo
    include Comparable

    def <=>(other)
      if self.total_time < other.total_time
        -1
      elsif self.total_time > other.total_time
        1
      elsif self.min_depth < other.min_depth
        1
      elsif self.min_depth > other.min_depth
        -1
      else
        self.full_name <=> other.full_name
      end
    end

    def called
      @called ||= begin
        call_infos.inject(0) do |sum, call_info|
          sum += call_info.called
        end
      end
    end

    def total_time
      @total_time ||= begin
        call_infos.inject(0) do |sum, call_info|
          sum += call_info.total_time unless call_info.recursive
          sum
        end
      end
    end

    def self_time
      @self_time ||= begin
        call_infos.inject(0) do |sum, call_info|
          sum += call_info.self_time  unless call_info.recursive
          sum
        end
      end
    end

    def wait_time
      @wait_time ||= begin
        call_infos.inject(0) do |sum, call_info|
          sum += call_info.wait_time unless call_info.recursive
          sum
        end
      end
    end

    def children_time
      @children_time ||= begin
        call_infos.inject(0) do |sum, call_info|
          sum += call_info.children_time  unless call_info.recursive
          sum
        end
      end
    end

    def min_depth
      @min_depth ||= call_infos.map do |call_info|
        call_info.depth
      end.min
    end

    def root?
      @root ||= begin
        call_infos.find do |call_info|
          not call_info.root?
        end.nil?
      end
    end

    def recursive?
      call_infos.detect do |call_info|
        call_info.recursive
      end
    end

    def children
      @children ||= begin
        call_infos.map do |call_info|
          call_info.children
        end.flatten
      end
    end

    def aggregate_parents
      # Group call info's based on their parents
      groups = self.call_infos.inject(Hash.new) do |hash, call_info|
        key = call_info.parent ? call_info.parent.target : self
        (hash[key] ||= []) << call_info
        hash
      end

      groups.map do |key, value|
        AggregateCallInfo.new(value)
      end
    end

    def aggregate_children
      # Group call info's based on their targets
      groups = self.children.inject(Hash.new) do |hash, call_info|
        key = call_info.target
        (hash[key] ||= []) << call_info
        hash
      end

      groups.map do |key, value|
        AggregateCallInfo.new(value)
      end
    end

    def to_s
      "#{self.full_name} (c: #{self.called}, tt: #{self.total_time}, st: #{self.self_time}, ct: #{self.children_time})"
    end

    # remove method from the call graph. should not be called directly.
    def eliminate!
      # $stderr.puts "eliminating #{self}"
      call_infos.each{ |call_info| call_info.eliminate! }
      call_infos.clear
    end

  end
end
