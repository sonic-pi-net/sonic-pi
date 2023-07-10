require 'concurrent/executor/single_thread_executor'
require 'concurrent/executor/thread_pool_executor'

module Concurrent
  RSpec.describe SingleThreadExecutor do
    if Concurrent.on_jruby?
      it 'inherits from JavaSingleThreadExecutor' do
        expect(SingleThreadExecutor.ancestors).to include(JavaSingleThreadExecutor)
      end
    else
      it 'inherits from RubySingleThreadExecutor' do
        expect(SingleThreadExecutor.ancestors).to include(RubySingleThreadExecutor)
      end
    end
  end

  RSpec.describe ThreadPoolExecutor do
    if Concurrent.on_jruby?
      it 'inherits from JavaThreadPoolExecutor' do
        expect(ThreadPoolExecutor.ancestors).to include(JavaThreadPoolExecutor)
      end
    else
      it 'inherits from RubyThreadPoolExecutor' do
        expect(ThreadPoolExecutor.ancestors).to include(RubyThreadPoolExecutor)
      end
    end
  end
end
