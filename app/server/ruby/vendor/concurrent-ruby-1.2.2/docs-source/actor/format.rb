require 'rubygems'
require 'bundler/setup'
require 'pry'
require 'pp'

root        = File.dirname(File.expand_path(Process.argv0))
input_paths = if ARGV.empty?
                Dir.glob("#{root}/*.in.rb")
              else
                ARGV
              end.map { |p| File.expand_path p }

input_paths.each_with_index do |input_path, i|

  pid = fork do
    require_relative 'init'

    begin
      output_path = input_path.gsub /\.in\.rb$/, '.out.rb'
      input       = File.readlines(input_path)

      chunks = []
      line   = ''

      while !input.empty?
        line += input.shift
        if Pry::Code.complete_expression? line
          chunks << line
          line = ''
        end
      end

      raise unless line.empty?

      chunks.map! { |chunk| [chunk, [chunk.split($/).size, 1].max] }
      environment = Module.new.send :binding
      evaluate    = ->(code, line) do
        eval(code, environment, input_path, line)
      end

      indent = 50

      line_count = 1
      output     = ''
      chunks.each do |chunk, lines|
        result = evaluate.(chunk, line_count)
        unless chunk.strip.empty? || chunk =~ /\A *#/
          pre_lines = chunk.lines.to_a
          last_line = pre_lines.pop
          output << pre_lines.join

          if last_line =~ /\#$/
            output << last_line.gsub(/\#$/, '')
          else
            if last_line.size < indent && result.inspect.size < indent
              output << "%-#{indent}s %s" % [last_line.chomp, "# => #{result.inspect}\n"]
            else
              output << last_line << "    # => #{result.inspect}\n"
            end
          end
        else
          output << chunk
        end
        line_count += lines
      end

      puts "#{input_path}\n -> #{output_path}"
      #puts output
      File.write(output_path, output)
    rescue => ex
      puts "#{ex} (#{ex.class})\n#{ex.backtrace * "\n"}"
    end
  end

  Process.wait pid
end
