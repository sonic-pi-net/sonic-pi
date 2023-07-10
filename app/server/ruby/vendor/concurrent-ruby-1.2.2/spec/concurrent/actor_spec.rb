require 'concurrent/actor'

module Concurrent
  module Actor
    AdHoc = Utils::AdHoc

    # FIXME better tests!

    RSpec.describe 'Concurrent::Actor', edge: true do

      def terminate_actors(*actors)
        actors.each do |actor|
          unless actor.ask!(:terminated?)
            actor.ask!(:terminate!)
          end
        end
      end

      class Ping < Context
        def initialize(queue)
          @queue = queue
        end

        def on_message(message)
          case message
          when :child
            AdHoc.spawn!(:pong, @queue) { |queue| -> m { queue << m } }
          else
            @queue << message
            message
          end
        end
      end

      it 'forbids Immediate executor' do
        expect { Utils::AdHoc.spawn! name: 'test', executor: ImmediateExecutor.new }.to raise_error(ArgumentError)
      end

      describe 'spawning' do
        describe 'Actor#spawn!' do
          behaviour = -> v { -> _ { v } }
          subjects  = { spawn:                 -> { Actor.spawn!(AdHoc, :ping, 'arg', &behaviour) },
                        context_spawn:         -> { AdHoc.spawn!(:ping, 'arg', &behaviour) },
                        spawn_by_hash:         -> { Actor.spawn!(class: AdHoc, name: :ping, args: ['arg'], &behaviour) },
                        context_spawn_by_hash: -> { AdHoc.spawn!(name: :ping, args: ['arg'], &behaviour) } }

          subjects.each do |desc, subject_definition|
            describe desc do
              subject(:actor, &subject_definition)
              after { terminate_actors actor }

              describe '#path' do
                subject { super().path }
                it { is_expected.to eq '/ping' }
              end

              describe '#parent' do
                subject { super().parent }
                it { is_expected.to eq Actor.root }
              end

              describe '#name' do
                subject { super().name }
                it { is_expected.to eq 'ping' }
              end
              it('executor should be global') { expect(subject.executor).to eq Concurrent.global_io_executor }

              describe '#reference' do
                subject { super().reference }
                it { is_expected.to eq subject }
              end
              it 'returns arg' do
                expect(subject.ask!(:anything)).to eq 'arg'
              end
            end
          end
        end

        it 'terminates on failed initialization' do
          a = AdHoc.spawn(name: :fail, logger: Concurrent::NULL_LOGGER) { raise }
          expect(a.ask(nil).wait.rejected?).to be_truthy
          expect(a.ask!(:terminated?)).to be_truthy
        end

        it 'terminates on failed initialization and raises with spawn!' do
          expect do
            AdHoc.spawn!(name: :fail, logger: Concurrent::NULL_LOGGER) { raise 'm' }
          end.to raise_error(StandardError, 'm')
        end

        it 'terminates on failed message processing' do
          a = AdHoc.spawn!(name: :fail, logger: Concurrent::NULL_LOGGER) { -> _ { raise } }
          expect(a.ask(nil).wait.rejected?).to be_truthy
          expect(a.ask!(:terminated?)).to be_truthy
        end
      end

      describe 'messaging' do
        subject { AdHoc.spawn!(:add) { c = 0; -> v { c = c + v } } }
        specify do
          subject.tell(1).tell(1)
          subject << 1 << 1
          expect(subject.ask(0).value!).to eq 4
        end
        after { terminate_actors subject }
      end

      describe 'children' do
        let(:parent) do
          AdHoc.spawn!(:parent) do
            -> message do
              if message == :child
                AdHoc.spawn!(:child) { -> _ { parent } }
              else
                children
              end
            end
          end
        end

        it 'has children set after a child is created' do
          child = parent.ask!(:child)
          expect(parent.ask!(nil)).to include(child)
          expect(child.ask!(nil)).to eq parent

          terminate_actors parent, child
        end
      end

      describe 'envelope' do
        subject { AdHoc.spawn!(:subject) { -> _ { envelope } } }
        specify do
          envelope = subject.ask!('a')
          expect(envelope).to be_a_kind_of Envelope
          expect(envelope.message).to eq 'a'
          expect(envelope.future).to be_resolved
          expect(envelope.future.value).to eq envelope
          expect(envelope.sender).to eq Thread.current
          terminate_actors subject
        end
      end

      describe 'termination' do
        subject do
          AdHoc.spawn!(:parent) do
            child = AdHoc.spawn!(:child) { -> v { v } }
            -> v { child }
          end
        end

        it 'terminates with all its children' do
          child = subject.ask! :child
          expect(subject.ask!(:terminated?)).to be_falsey
          subject.ask(:terminate!).wait
          expect(subject.ask!(:terminated?)).to be_truthy
          expect(child.ask!(:terminated?)).to be_truthy

          terminate_actors subject, child
        end
      end

      describe 'dead letter routing' do
        it 'logs by deafault' do
          ping = Ping.spawn! :ping, []
          ping << :terminate!
          ping << 'asd'
          sleep 0.1
          # TODO
        end
      end

      describe 'message redirecting' do
        let(:parent) do
          AdHoc.spawn!(:parent) do
            child = AdHoc.spawn!(:child) { -> m { m + 1 } }
            -> message do
              if message == :child
                child
              else
                redirect child
              end
            end
          end
        end

        it 'is evaluated by child' do
          expect(parent.ask!(1)).to eq 2
        end
      end

      it 'links' do
        queue   = Queue.new
        failure = nil
        # FIXME this leads to weird message processing ordering
        # failure = AdHoc.spawn!(:failure) { -> m { terminate! } }
        monitor = AdHoc.spawn!(:monitor) do
          failure = AdHoc.spawn!(:failure) { -> m { m } }
          failure << :link
          -> m { queue << [m, envelope.sender] }
        end
        failure << :hehe
        failure << :terminate!
        expect(queue.pop).to eq [[:terminated, nil], failure]

        terminate_actors monitor
      end

      it 'links atomically' do
        queue   = Queue.new
        failure = nil
        monitor = AdHoc.spawn!(:monitor) do
          failure = AdHoc.spawn!(name: :failure, link: true) { -> m { m } }
          -> m { queue << [m, envelope.sender] }
        end

        failure << :hehe
        failure << :terminate!
        expect(queue.pop).to eq [[:terminated, nil], failure]

        terminate_actors monitor
      end

      describe 'pausing' do
        it 'pauses on error and resumes' do
          queue              = Queue.new
          resuming_behaviour = Behaviour.restarting_behaviour_definition(:resume!)

          test = AdHoc.spawn! name: :tester, behaviour_definition: resuming_behaviour do
            actor = AdHoc.spawn! name: :pausing, behaviour_definition: Behaviour.restarting_behaviour_definition do
              queue << :init
              -> m { m == :add ? 1 : pass }
            end

            actor << :link
            queue << actor.ask!(:linked)
            actor << nil
            queue << actor.ask(:add)

            -> m { queue << m }
          end

          expect(queue.pop).to eq :init
          expect(queue.pop).to include(test)
          expect(queue.pop.value).to eq 1
          expect(queue.pop).to eq :resumed
          terminate_actors test
        end

        it 'pauses on error and resets' do
          queue = Queue.new
          test  = AdHoc.spawn! name: :tester, behaviour_definition: Behaviour.restarting_behaviour_definition do
            actor = AdHoc.spawn! name: :pausing, behaviour_definition: Behaviour.restarting_behaviour_definition do
              queue << :init
              -> m { m == :object_id ? self.object_id : pass }
            end

            queue << actor.ask!(:linked)
            queue << actor.ask!(:object_id)
            actor << nil
            queue << actor.ask(:object_id)

            -> m do
              queue << m
            end
          end

          expect(queue.pop).to eq :init
          expect(queue.pop).to include(test)
          first_id  = queue.pop
          second_id = queue.pop.value
          expect(first_id).not_to eq second_id # context already reset
          expect(queue.pop).to eq :init # rebuilds context
          expect(queue.pop).to eq :reset
          terminate_actors test
        end

        it 'pauses on error and restarts' do
          queue              = Queue.new
          resuming_behaviour = Behaviour.restarting_behaviour_definition.map do |c, *args|
            if Behaviour::Supervising == c
              [c, *[:restart!, :one_for_one]]
            else
              [c, *args]
            end
          end

          test = AdHoc.spawn! name: :tester, behaviour_definition: resuming_behaviour do

            actor = AdHoc.spawn! name:                 :pausing,
                                 behaviour_definition: Behaviour.restarting_behaviour_definition do
              queue << :init
              -> m { m == :add ? 1 : pass }
            end

            actor << :link
            queue << actor.ask!(:linked)
            actor << nil
            queue << actor.ask(:add)

            -> m do
              queue << m
            end
          end

          expect(queue.pop).to eq :init
          expect(queue.pop).to include(test)
          expect(queue.pop.wait.reason).to be_a_kind_of(ActorTerminated)
          expect(queue.pop).to eq :init
          expect(queue.pop).to eq :restarted
          terminate_actors test
        end

      end

      describe 'pool' do
        it 'supports asks' do
          children = Queue.new
          pool     = Concurrent::Actor::Utils::Pool.spawn! 'pool', 5 do |index|
            worker = Concurrent::Actor::Utils::AdHoc.spawn! name: "worker-#{index}", supervised: true do
              lambda do |message|
                fail if message == :fail
                5 + message
              end
            end
            children.push worker
            worker
          end

          10.times { expect(pool.ask!(5)).to eq 10 }
          expect(pool.ask(:fail).reason).to be_kind_of RuntimeError
          expect(pool.ask!(5)).to eq 10
          expect(pool.ask!(:terminate!)).to be_truthy
          5.times { expect(children.pop.ask!(:terminated?)).to be_truthy }

          terminate_actors pool
        end
      end

    end
  end
end
