# The call info visitor class does a depth-first traversal
# across a thread's call stack.  At each call_info node,
# the visitor executes the block provided in the
# #visit method.  The block is passed two parameters, the
# event and the call_info instance.  Event will be
# either :enter or :exit.
#
#   visitor = RubyProf::CallInfoVisitor.new(result.threads.first)
#
#   method_names = Array.new
#
#   visitor.visit do |call_info, event|
#     method_names << call_info.target.full_name if event == :enter
#   end
#
#   puts method_names

module RubyProf
  class CallInfoVisitor

    def initialize(thread)
      @thread = thread
    end

    def visit(&block)
      @thread.top_methods.each do |method_info|
        method_info.call_infos.each do |call_info|
          visit_call_info(call_info, &block)
        end
      end
    end

    private
    def visit_call_info(call_info, &block)
      yield call_info, :enter
      call_info.children.each do |child|
        visit_call_info(child, &block)
      end
      yield call_info, :exit
    end
  end
end
