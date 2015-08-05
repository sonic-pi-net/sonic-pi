module Concurrent

  describe Configuration, notravis: true do

    before(:all) do
      reset_gem_configuration
    end

    after(:each) do
      reset_gem_configuration
    end

    context 'global executors' do

      it 'creates a global timer set' do
        expect(Concurrent.global_timer_set).not_to be_nil
        expect(Concurrent.global_timer_set).to respond_to(:post)
      end

      it 'creates a global fast executor' do
        expect(Concurrent.global_fast_executor).not_to be_nil
        expect(Concurrent.global_fast_executor).to respond_to(:post)
      end

      it 'creates a global io executor' do
        expect(Concurrent.global_io_executor).not_to be_nil
        expect(Concurrent.global_io_executor).to respond_to(:post)
      end

      specify '#terminate_pools! acts on all executors with auto_terminate: true' do
        # The 'at_least(:once)' clauses account for global config reset
        expect(Concurrent.global_fast_executor).to receive(:kill).at_least(:once).with(no_args).and_call_original
        expect(Concurrent.global_io_executor).to receive(:kill).at_least(:once).with(no_args).and_call_original
        expect(Concurrent.global_timer_set).to receive(:kill).at_least(:once).with(no_args).and_call_original
        Concurrent.terminate_pools!
      end
    end
  end
end
