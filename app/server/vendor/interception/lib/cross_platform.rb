# Platform specific implementations of Interception.start and Interception.stop
class << Interception
  private

  # For Rubinius we just monkeypatch Kernel#raise_exception,
  #
  # This is normally a thin wrapper around raising an exception on the VM
  # (so the layer of abstraction below Kernel#raise).
  if defined? Rubinius

    def start
      class << Rubinius

        alias raise_with_no_interception raise_exception

        def raise_exception(exc)
          bt = Rubinius::VM.backtrace(1, true).drop_while do |x|
            x.variables.method.file.to_s.start_with?("kernel/")
          end.first
          b = Binding.setup(bt.variables, bt.variables.method, bt.constant_scope, bt.variables.self, bt)

          Interception.rescue(exc, b)
          raise_with_no_interception(exc)
        end
      end
    end

    def stop
      class << Rubinius
        alias raise_exception raise_with_no_interception
      end
    end

  # For JRuby we use the underlying hooks mechanism.
  #
  # It seems to work even if I don't pass --debug, but it still
  # warns about it. So disable the warnings and install the hook.
  elsif defined?(JRuby)

    require 'java'
    $CLASSPATH << File.expand_path('../../ext/', __FILE__)
    java_import org.pryrepl.InterceptionEventHook

    def start
      old_verbose = $VERBOSE
      $VERBOSE = nil
      JRuby.runtime.add_event_hook(hook)
    ensure
      $VERBOSE  = old_verbose
    end

    def stop
      JRuby.runtime.remove_event_hook(hook)
    end

    def hook
      @hook ||= InterceptionEventHook.new(proc do |e, b|
        self.rescue(e, b)
      end)
    end

  # For MRI
  # @note For Ruby 2.0 and later we use the new TracePoint API.
  elsif RUBY_VERSION.to_f >= 2.0 && RUBY_ENGINE == 'ruby'

    def start
      @tracepoint ||= TracePoint.new(:raise) do |tp|
        self.rescue(tp.raised_exception, tp.binding)
      end

      @tracepoint.enable
    end

    def stop
      @tracepoint.disable
    end

  # For old MRI
  else

    require File.expand_path('../../ext/interception', __FILE__)

  end
end
