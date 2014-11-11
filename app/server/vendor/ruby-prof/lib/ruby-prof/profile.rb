# encoding: utf-8

require 'set'
module RubyProf
  class Profile
    # This method gets called once profiling has been completed
    # but before results are returned to the user.  Thus it provides
    # a hook to do any necessary post-processing on the call graph.
    def post_process
      self.threads.each do |thread|
        detect_recursion(thread)
      end
    end

    # This method detect recursive calls in the call graph.
    def detect_recursion(thread)
      visited_methods = Hash.new do |hash, key|
        hash[key] = 0
      end

      visitor = CallInfoVisitor.new(thread)
      visitor.visit do |call_info, event|
        case event
        when :enter
          if (visited_methods[call_info.target] += 1) > 1
            call_info.recursive = true
          end
        when :exit
          if (visited_methods[call_info.target] -= 1) == 0
            visited_methods.delete(call_info.target)
          end
        end
      end
    end

    # eliminate some calls from the graph by merging the information into callers.
    # matchers can be a list of strings or regular expressions or the name of a file containing regexps.
    def eliminate_methods!(matchers)
      matchers = read_regexps_from_file(matchers) if matchers.is_a?(String)
      eliminated = []
      threads.each do |thread|
        matchers.each{ |matcher| eliminated.concat(eliminate_methods(thread.methods, matcher)) }
      end
      eliminated
    end

    private

    # read regexps from file
    def read_regexps_from_file(file_name)
      matchers = []
      File.open(file_name).each_line do |l|
        next if (l =~ /^(#.*|\s*)$/) # emtpy lines and lines starting with #
        matchers << Regexp.new(l.strip)
      end
    end

    # eliminate methods matching matcher
    def eliminate_methods(methods, matcher)
      eliminated = []
      i = 0
      while i < methods.size
        method_info = methods[i]
        method_name = method_info.full_name
        if matcher === method_name
          raise "can't eliminate root method" if method_info.root?
          eliminated << methods.delete_at(i)
          method_info.eliminate!
        else
          i += 1
        end
      end
      eliminated
    end

  end
end
