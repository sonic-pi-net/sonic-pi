require 'mkmf'

have_type('rb_pid_t', 'ruby.h')
create_makefile('sys/proctable', 'sys')