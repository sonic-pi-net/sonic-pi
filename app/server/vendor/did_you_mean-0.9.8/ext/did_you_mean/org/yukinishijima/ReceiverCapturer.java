package org.yukinishijima;

import org.jruby.anno.JRubyMethod;
import org.jruby.exceptions.RaiseException;
import org.jruby.runtime.builtin.IRubyObject;
import org.jruby.runtime.Block;
import org.jruby.runtime.ThreadContext;
import org.jruby.runtime.Visibility;
import org.jruby.Ruby;
import org.jruby.RubyArray;
import org.jruby.RubyException;
import org.jruby.RubyModule;
import org.jruby.RubyNoMethodError;
import org.jruby.internal.runtime.methods.CallConfiguration;
import org.jruby.internal.runtime.methods.DynamicMethod;
import org.jruby.internal.runtime.methods.JavaMethod.JavaMethodNBlock;

public class ReceiverCapturer extends JavaMethodNBlock {
    private DynamicMethod methodMissingMethod;

    public ReceiverCapturer(RubyModule implementationClass, DynamicMethod methodMissingMethod) {
        super(implementationClass, Visibility.PRIVATE, CallConfiguration.FrameFullScopeNone);
        this.methodMissingMethod = methodMissingMethod;
    }

    @Override
    public IRubyObject call(ThreadContext context, IRubyObject self, RubyModule clazz, String name, IRubyObject[] args, Block block) {
        try {
            return this.methodMissingMethod.call(context, self, clazz, name, args, block);
        } catch (RaiseException exception) {
            appendReceiverToException(exception, self);
            throw exception;
        }
    }

    @JRubyMethod
    public static void setup(Ruby runtime) {
        RubyModule module = runtime.defineModule("Kernel");

        DynamicMethod privateMethodMissing = runtime.getPrivateMethodMissing();
        DynamicMethod protectedMethodMissing = runtime.getProtectedMethodMissing();
        DynamicMethod superMethodMissing = runtime.getSuperMethodMissing();
        DynamicMethod normalMethodMissing = runtime.getNormalMethodMissing();

        runtime.setPrivateMethodMissing(new ReceiverCapturer(module, privateMethodMissing));
        runtime.setProtectedMethodMissing(new ReceiverCapturer(module, protectedMethodMissing));
        runtime.setSuperMethodMissing(new ReceiverCapturer(module, superMethodMissing));
        runtime.setNormalMethodMissing(new ReceiverCapturer(module, normalMethodMissing));
    }

    private void appendReceiverToException(RaiseException exception, IRubyObject self) {
        if (exception.getException() instanceof RubyNoMethodError) {
            RubyNoMethodError noMethodError = (RubyNoMethodError) exception.getException();
            noMethodError.setInstanceVariable("@receiver", self);
        }
    }
}
