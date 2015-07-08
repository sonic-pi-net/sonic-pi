# encoding: utf-8

module RubyProf
  # Generates flat[link:files/examples/flat_txt.html] profile reports as text.
  # To use the flat printer with line numbers:
  #
  #   result = RubyProf.profile do
  #     [code to profile]
  #   end
  #
  #   printer = RubyProf::FlatPrinterWithLineNumbers.new(result)
  #   printer.print(STDOUT, {})
  #
  class FlatPrinterWithLineNumbers < FlatPrinter
    def print_methods(thread)
      total_time = thread.total_time

      methods = thread.methods.sort_by(&sort_method).reverse
      sum = 0
      methods.each do |method|
        self_percent = (method.self_time / total_time) * 100
        next if self_percent < min_percent

        sum += method.self_time
        #self_time_called = method.called > 0 ? method.self_time/method.called : 0
        #total_time_called = method.called > 0? method.total_time/method.called : 0

        @output << "%6.2f  %9.3f %9.3f %9.3f %9.3f %8d  %s%s" % [
            method.self_time / total_time * 100, # %self
            method.total_time,                   # total
            method.self_time,                    # self
            method.wait_time,                    # wait
            method.children_time,                # children
            method.called,                       # calls
            method.recursive? ? "*" : " ",       # cycle
            method_name(method)                  # name
        ]
         if method.source_file == 'ruby_runtime'
           @output << "\n"
         else
           @output << "\n      defined at:\n"
           @output << "          %s:%s\n" % [File.expand_path(method.source_file), method.line]
         end

         callers = []
         method.call_infos.each do |ci|
           if ci.parent && ci.parent.target.source_file != 'ruby_runtime'
             callers << [method_name(ci.parent.target), File.expand_path(ci.parent.target.source_file), ci.parent.target.line]
           end
         end
         # make sure callers are unique
         callers.uniq!

         unless callers.empty?
           @output << "      called from:\n"
           callers.each do |args|
             @output << "          %s (%s:%s)\n" % args
           end
         end
         @output << "\n"
      end
    end
  end
end
