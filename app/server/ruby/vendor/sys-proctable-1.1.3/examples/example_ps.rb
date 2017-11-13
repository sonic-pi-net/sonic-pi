#######################################################################
# example_ps.rb
#
# Generic test program that demonstrates the use of ProcTable.ps. You
# can run this via the 'rake example' task.
#
# Modify as you see fit
#######################################################################
require 'sys/proctable'
include Sys

puts "VERSION: " + ProcTable::VERSION
sleep 2

ProcTable.ps{ |s|
   ProcTable.fields.each{ |field|
      puts "#{field}: " + s.send(field).to_s    
   }
   puts '=' * 30
}
