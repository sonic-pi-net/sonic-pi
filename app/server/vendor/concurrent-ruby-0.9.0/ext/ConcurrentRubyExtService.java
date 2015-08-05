import java.io.IOException;

import org.jruby.Ruby;
import org.jruby.runtime.load.BasicLibraryService;

public class ConcurrentRubyExtService implements BasicLibraryService {
    public boolean basicLoad(final Ruby runtime) throws IOException {
        new com.concurrent_ruby.ext.AtomicReferenceLibrary().load(runtime, false);
        new com.concurrent_ruby.ext.JavaAtomicBooleanLibrary().load(runtime, false);
        new com.concurrent_ruby.ext.JavaAtomicFixnumLibrary().load(runtime, false);
        new com.concurrent_ruby.ext.JavaSemaphoreLibrary().load(runtime, false);
        new com.concurrent_ruby.ext.SynchronizationLibrary().load(runtime, false);
        return true;
    }
}
