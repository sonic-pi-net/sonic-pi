package org.pryrepl;

import org.jruby.runtime.builtin.IRubyObject;
import org.jruby.runtime.EventHook;
import org.jruby.runtime.RubyEvent;
import org.jruby.runtime.ThreadContext;
import org.jruby.RubyException;
import org.jruby.RubyBinding;
import org.jruby.RubyProc;

public class InterceptionEventHook extends EventHook {

    private RubyProc proc;

    public InterceptionEventHook(RubyProc proc) {
        this.proc = proc;
    }

    public boolean isInterestedInEvent(RubyEvent event) {
        return event == RubyEvent.RAISE;
    }

    public void eventHandler(ThreadContext context, String eventName, String file, int line, String name, IRubyObject type) {
        RubyBinding binding = RubyBinding.newBinding(context.runtime, context.currentBinding());
        RubyException exception = (RubyException)context.runtime.getGlobalVariables().get("$!");
        proc.call(context, new IRubyObject[] {exception, binding});
    }
}
