/* -*- c -*- */
#include "vm_opts.h"

#ifndef	_PROBES_H
#define	_PROBES_H
#define DTRACE_PROBES_DISABLED 1

#define RUBY_DTRACE_METHOD_ENTRY_ENABLED() 0
#define RUBY_DTRACE_METHOD_ENTRY(arg0, arg1, arg2, arg3) do { } while(0)
#define RUBY_DTRACE_METHOD_RETURN_ENABLED() 0
#define RUBY_DTRACE_METHOD_RETURN(arg0, arg1, arg2, arg3) do { } while(0)

#define RUBY_DTRACE_CMETHOD_ENTRY_ENABLED() 0
#define RUBY_DTRACE_CMETHOD_ENTRY(arg0, arg1, arg2, arg3) do { } while(0)
#define RUBY_DTRACE_CMETHOD_RETURN_ENABLED() 0
#define RUBY_DTRACE_CMETHOD_RETURN(arg0, arg1, arg2, arg3) do { } while(0)

#define RUBY_DTRACE_REQUIRE_ENTRY_ENABLED() 0
#define RUBY_DTRACE_REQUIRE_ENTRY(arg0, arg1, arg2) do { } while(0)

#define RUBY_DTRACE_REQUIRE_RETURN_ENABLED() 0
#define RUBY_DTRACE_REQUIRE_RETURN(arg0, arg1, arg2) do { } while(0)

#define RUBY_DTRACE_FIND_REQUIRE_ENTRY_ENABLED() 0
#define RUBY_DTRACE_FIND_REQUIRE_ENTRY(arg0, arg1, arg2) do { } while(0)

#define RUBY_DTRACE_FIND_REQUIRE_RETURN_ENABLED() 0
#define RUBY_DTRACE_FIND_REQUIRE_RETURN(arg0, arg1, arg2) do { } while(0)

#define RUBY_DTRACE_LOAD_ENTRY_ENABLED() 0
#define RUBY_DTRACE_LOAD_ENTRY(arg0, arg1, arg2) do { } while(0)

#define RUBY_DTRACE_LOAD_RETURN_ENABLED() 0
#define RUBY_DTRACE_LOAD_RETURN(arg0, arg1, arg2) do { } while(0)

#define RUBY_DTRACE_RAISE_ENABLED() 0
#define RUBY_DTRACE_RAISE(arg0, arg1, arg2) do { } while(0)

#define RUBY_DTRACE_OBJECT_CREATE_ENABLED() 0
#define RUBY_DTRACE_OBJECT_CREATE(arg0, arg1, arg2) do { } while(0)

#define RUBY_DTRACE_ARRAY_CREATE_ENABLED() 0
#define RUBY_DTRACE_ARRAY_CREATE(arg0, arg1, arg2) do { } while(0)

#define RUBY_DTRACE_HASH_CREATE_ENABLED() 0
#define RUBY_DTRACE_HASH_CREATE(arg0, arg1, arg2) do { } while(0)

#define RUBY_DTRACE_STRING_CREATE_ENABLED() 0
#define RUBY_DTRACE_STRING_CREATE(arg0, arg1, arg2) do { } while(0)

#define RUBY_DTRACE_SYMBOL_CREATE_ENABLED() 0
#define RUBY_DTRACE_SYMBOL_CREATE(arg0, arg1, arg2) do { } while(0)

#define RUBY_DTRACE_PARSE_BEGIN_ENABLED() 0
#define RUBY_DTRACE_PARSE_BEGIN(arg0, arg1) do { } while(0)

#define RUBY_DTRACE_PARSE_END_ENABLED() 0
#define RUBY_DTRACE_PARSE_END(arg0, arg1) do { } while(0)

#if VM_COLLECT_USAGE_DETAILS
#define RUBY_DTRACE_INSN_ENABLED() 0
#define RUBY_DTRACE_INSN(arg0) do { } while(0)
#define RUBY_DTRACE_INSN_OPERAND_ENABLED() 0
#define RUBY_DTRACE_INSN_OPERAND(arg0, arg1) do { } while(0)
#endif

#define RUBY_DTRACE_GC_MARK_BEGIN_ENABLED() 0
#define RUBY_DTRACE_GC_MARK_BEGIN() do { } while(0)

#define RUBY_DTRACE_GC_MARK_END_ENABLED() 0
#define RUBY_DTRACE_GC_MARK_END() do { } while(0)

#define RUBY_DTRACE_GC_SWEEP_BEGIN_ENABLED() 0
#define RUBY_DTRACE_GC_SWEEP_BEGIN() do { } while(0)

#define RUBY_DTRACE_GC_SWEEP_END_ENABLED() 0
#define RUBY_DTRACE_GC_SWEEP_END() do { } while(0)

#define RUBY_DTRACE_METHOD_CACHE_CLEAR_ENABLED() 0
#define RUBY_DTRACE_METHOD_CACHE_CLEAR(arg0, arg1, arg2) do { } while(0)

#endif	/* _PROBES_H */

