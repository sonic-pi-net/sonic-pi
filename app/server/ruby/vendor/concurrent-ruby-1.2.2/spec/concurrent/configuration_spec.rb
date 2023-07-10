require 'concurrent/configuration'

module Concurrent

   RSpec.describe 'configuration' do

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

    end
   end
end
