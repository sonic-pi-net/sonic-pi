# encoding: utf-8

require 'erb'
require 'fileutils'
require 'base64'

module RubyProf
  # prints a HTML visualization of the call tree
  class CallStackPrinter < AbstractPrinter
    include ERB::Util

    # Specify print options.
    #
    # options - Hash table
    #   :min_percent - Number 0 to 100 that specifes the minimum
    #                  %self (the methods self time divided by the
    #                  overall total time) that a method must take
    #                  for it to be printed out in the report.
    #                  Default value is 0.
    #
    #   :print_file  - True or false. Specifies if a method's source
    #                  file should be printed.  Default value if false.
    #
    #   :threshold   - a float from 0 to 100 that sets the threshold of
    #                  results displayed.
    #                  Default value is 1.0
    #
    #   :title       - a String to overide the default "ruby-prof call tree"
    #                  title of the report.
    #
    #   :expansion   - a float from 0 to 100 that sets the threshold of
    #                  results that are expanded, if the percent_total
    #                  exceeds it.
    #                  Default value is 10.0
    #
    #   :application - a String to overide the name of the application,
    #                  as it appears on the report.
    #
    def print(output = STDOUT, options = {})
      @output = output
      setup_options(options)
      if @graph_html = options.delete(:graph)
        @graph_html = "file://" + @graph_html if @graph_html[0]=="/"
      end

      print_header

      @overall_threads_time = @result.threads.inject(0) do |val, thread|
        val += thread.total_time
      end

      @result.threads.each do |thread|
        @current_thread_id = thread.fiber_id
        @overall_time = thread.total_time
        thread_info = "Thread: #{thread.id}"
        thread_info << ", Fiber: #{thread.fiber_id}" unless thread.id == thread.fiber_id
        thread_info << " (#{"%4.2f%%" % ((@overall_time/@overall_threads_time)*100)} ~ #{@overall_time})"
        @output.print "<div class=\"thread\">#{thread_info}</div>"
        @output.print "<ul name=\"thread\">"
        thread.methods.each do |m|
          # $stderr.print m.dump
          next unless m.root?
          m.call_infos.each do |ci|
            next unless ci.root?
            print_stack ci, thread.total_time
          end
        end
        @output.print "</ul>"
      end

      print_footer

    end

    def print_stack(call_info, parent_time)
      total_time = call_info.total_time
      percent_parent = (total_time/parent_time)*100
      percent_total = (total_time/@overall_time)*100
      return unless percent_total > min_percent
      color = self.color(percent_total)
      kids = call_info.children
      visible = percent_total >= threshold
      expanded = percent_total >= expansion
      display = visible ? "block" : "none"
      @output.print "<li class=\"color#{color}\" style=\"display:#{display}\">"
      if kids.empty?
        @output.print "<a href=\"#\" class=\"toggle empty\" ></a>"
      else
        visible_children = kids.any?{|ci| (ci.total_time/@overall_time)*100 >= threshold}
        image = visible_children ? (expanded ? "minus" : "plus") : "empty"
        @output.print "<a href=\"#\" class=\"toggle #{image}\" ></a>"
      end
      @output.printf "<span> %4.2f%% (%4.2f%%) %s %s</span>\n", percent_total, percent_parent, link(call_info), graph_link(call_info)
      unless kids.empty?
        if expanded
          @output.print "<ul>"
        else
          @output.print '<ul style="display:none">'
        end
        kids.sort_by{|c| -c.total_time}.each do |callinfo|
          print_stack callinfo, total_time
        end
        @output.print "</ul>"
      end
      @output.print "</li>"
    end

    def name(call_info)
      method = call_info.target
      method.full_name
    end

    def link(call_info)
      method = call_info.target
      file = File.expand_path(method.source_file)
      if file =~ /\/ruby_runtime$/
        h(name(call_info))
      else
        if RUBY_PLATFORM =~ /darwin/
          "<a href=\"txmt://open?url=file://#{file}&line=#{method.line}\">#{h(name(call_info))}</a>"
        else
          "<a href=\"file://#{file}##{method.line}\">#{h(name(call_info))}</a>"
        end
      end
    end

    def graph_link(call_info)
      total_calls = call_info.target.call_infos.inject(0){|t, ci| t += ci.called}
      href = "#{@graph_html}##{method_href(call_info.target)}"
      totals = @graph_html ? "<a href='#{href}'>#{total_calls}</a>" : total_calls.to_s
      "[#{call_info.called} calls, #{totals} total]"
    end

    def method_href(method)
      h(method.full_name.gsub(/[><#\.\?=:]/,"_") + "_" + @current_thread_id.to_s)
    end

    def total_time(call_infos)
      sum(call_infos.map{|ci| ci.total_time})
    end

    def sum(a)
      a.inject(0.0){|s,t| s+=t}
    end

    def dump(ci)
      $stderr.printf "%s/%d t:%f s:%f w:%f  \n", ci, ci.object_id, ci.total_time, ci.self_time, ci.wait_time
    end

    def color(p)
      case i = p.to_i
      when 0..5
        "01"
      when 5..10
        "05"
      when 100
        "9"
      else
        "#{i/10}"
      end
    end

    def application
      @options[:application] || $PROGRAM_NAME
    end

    def arguments
      ARGV.join(' ')
    end

    def title
      @title ||= @options.delete(:title) || "ruby-prof call tree"
    end

    def threshold
      @options[:threshold] || 1.0
    end

    def expansion
      @options[:expansion] || 10.0
    end

    def print_header
      @output.puts "<html><head>"
      @output.puts '<meta http-equiv="content-type" content="text/html; charset=utf-8">'
      @output.puts "<title>#{h title}</title>"
      print_css
      print_java_script
      @output.puts '</head><body><div style="display: inline-block;">'
      print_title_bar
      print_commands
      print_help
    end

    def print_footer
      @output.puts '<div id="sentinel"></div></div></body></html>'
    end

    def open_asset(file)
      path = File.join(File.expand_path('../../assets', __FILE__), file)
      File.open(path, 'rb').read
    end

    def print_css
      html = open_asset('call_stack_printer.css.html')
      @output.puts html.gsub('%s', base64_image)
    end

    def base64_image
      file = open_asset('call_stack_printer.png')
      Base64.encode64(file).gsub(/\n/, '')
    end

    def print_java_script
      html = open_asset('call_stack_printer.js.html')
      @output.puts html
    end

    def print_title_bar
      @output.puts <<-"end_title_bar"
<div id="titlebar">
Call tree for application <b>#{h application} #{h arguments}</b><br/>
Generated on #{Time.now} with options #{h @options.inspect}<br/>
</div>
end_title_bar
    end

    def print_commands
      @output.puts <<-"end_commands"
<div id=\"commands\">
<span style=\"font-size: 11pt; font-weight: bold;\">Threshold:</span>
<input value=\"#{h threshold}\" size=\"3\" id=\"threshold\" type=\"text\">
<input value=\"Apply\" onclick=\"setThreshold();\" type=\"submit\">
<input value=\"Expand All\" onclick=\"expandAll(event);\" type=\"submit\">
<input value=\"Collapse All\" onclick=\"collapseAll(event);\" type=\"submit\">
<input value=\"Show Help\" onclick=\"toggleHelp(this);\" type=\"submit\">
</div>
end_commands
    end

    def print_help
      @output.puts <<-'end_help'
<div style="display: none;" id="help">
&#8226; Enter a decimal value <i>d</i> into the threshold field and click "Apply"
        to hide all nodes marked with time values lower than <i>d</i>.<br>
&#8226; Click on "Expand All" for full tree expansion.<br>
&#8226; Click on "Collapse All" to show only top level nodes.<br>
&#8226; Use a, s, d, w as in Quake or Urban Terror to navigate the tree.<br>
&#8226; Use f and b to navigate the tree in preorder forward and backwards.<br>
&#8226; Use x to toggle visibility of a subtree.<br>
&#8226; Use * to expand/collapse a whole subtree.<br>
&#8226; Use h to navigate to thread root.<br>
&#8226; Use n and p to navigate between threads.<br>
&#8226; Click on background to move focus to a subtree.<br>
</div>
end_help
    end
  end
end

