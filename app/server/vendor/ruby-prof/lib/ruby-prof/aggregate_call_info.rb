# encoding: utf-8

module RubyProf
  class AggregateCallInfo
    attr_reader :call_infos

    def initialize(call_infos)
      if call_infos.length == 0
        raise(ArgumentError, "Must specify at least one call info.")
      end
      @call_infos = call_infos
    end

    def target
      call_infos.first.target
    end

    def parent
      call_infos.first.parent
    end

    def line
      call_infos.first.line
    end

    def children
      call_infos.inject(Array.new) do |result, call_info|
        result.concat(call_info.children)
      end
    end

    def total_time
      aggregate_without_recursion(:total_time)
    end

    def self_time
      aggregate_without_recursion(:self_time)
    end

    def wait_time
      aggregate_without_recursion(:wait_time)
    end

    def children_time
      aggregate_without_recursion(:children_time)
    end

    def called
      aggregate(:called)
    end

    def to_s
      "#{call_infos.first.target.full_name}"
    end

    private

    def aggregate(method_name)
      call_infos.inject(0) do |sum, call_info|
        sum + call_info.send(method_name)
      end
    end

    def aggregate_without_recursion(method_name)
      call_infos.inject(0) do |sum, call_info|
        call_info.recursive ? sum : sum + call_info.send(method_name)
      end
    end
  end
end
