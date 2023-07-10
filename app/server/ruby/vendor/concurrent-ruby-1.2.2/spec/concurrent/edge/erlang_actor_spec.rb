require 'concurrent/edge/erlang_actor'

RSpec.describe 'Concurrent' do
  describe 'ErlangActor', edge: true do
    # TODO (pitr-ch 06-Feb-2019): include constants instead
    ANY      ||= Concurrent::ErlangActor::ANY
    TIMEOUT  ||= Concurrent::ErlangActor::TIMEOUT
    And      ||= Concurrent::ErlangActor::And
    identity = -> v { v }

    shared_examples 'erlang actor' do

      specify "run to termination" do
        expect(Concurrent::ErlangActor.spawn(type: type) do
          :v
        end.terminated.value!).to eq :v
      end

      specify "run to termination with arguments" do
        expect(Concurrent::ErlangActor.
            spawn(1, 2, type: type) { |a, b| a + b }.terminated.value!).
            to eq 3
      end

      specify '#receive' do
        succ = -> v { v.succ }

        [[[:v], -> { receive }, :v],
         [[:v], -> { receive on(ANY, &identity) }, :v],
         [[:v, 1], -> { receive Numeric }, 1],
         [[:v, 1], -> { receive(Numeric, &succ) }, 2],

         [[:v], -> { receive Numeric, timeout: 0 }, nil],
         [[:v], -> { receive(Numeric, timeout: 0, &succ) }, nil],
         [[:v], -> { receive Numeric, timeout: 0, timeout_value: :timeout }, :timeout],
         [[:v], -> { receive(Numeric, timeout: 0, timeout_value: :timeout, &succ) }, :timeout],

         [[:v, 1], -> { receive Numeric, timeout: 1 }, 1],
         [[:v, 1], -> { receive(Numeric, timeout: 1, &succ) }, 2],
         [[:v, 1], -> { receive Numeric, timeout: 1, timeout_value: :timeout }, 1],
         [[:v, 1], -> { receive(Numeric, timeout: 1, timeout_value: :timeout, &succ) }, 2],

         [[:v], -> { receive on(Numeric, &identity), on(TIMEOUT, nil), timeout: 0 }, nil],
         [[:v], -> { receive on(Numeric, &succ), on(TIMEOUT, nil), timeout: 0 }, nil],
         [[:v], -> { receive on(Numeric, &identity), on(TIMEOUT, :timeout), timeout: 0 }, :timeout],
         [[:v], -> { receive on(Numeric, &succ), on(TIMEOUT, :timeout), timeout: 0 }, :timeout],

         [[:v, 1], -> { receive on(Numeric, &identity), on(TIMEOUT, nil), timeout: 1 }, 1],
         [[:v, 1], -> { receive on(Numeric, &succ), on(TIMEOUT, nil), timeout: 1 }, 2],
         [[:v, 1], -> { receive on(Numeric, &identity), on(TIMEOUT, :timeout), timeout: 1 }, 1],
         [[:v, 1], -> { receive on(Numeric, &succ), on(TIMEOUT, :timeout), timeout: 1 }, 2],
        ].each_with_index do |(messages, body, result), i|
          a = Concurrent::ErlangActor.spawn(type: type, &body)
          messages.each { |m| a.tell m }
          expect(a.terminated.value!).to eq(result), "body: #{body}"
        end
      end

      specify 'pid has name' do
        actor = Concurrent::ErlangActor.spawn(type: type, name: 'test') {}
        expect(actor.to_s).to match(/test/)
        expect(actor.inspect).to match(/test/)
      end

      specify "receives message" do
        actor = Concurrent::ErlangActor.spawn(type: type,
                                              &{ on_thread: -> { receive },
                                                 on_pool:   -> { receive on(ANY, &identity) } }.fetch(type))
        actor.tell :v
        expect(actor.terminated.value!).to eq :v
      end

      specify "receives message with matchers" do
        body  = { on_thread:
                      -> do
                        [receive(on(Symbol, &identity)),
                         receive(on(Numeric, &:succ)),
                         receive(on(Numeric, :got_it), timeout: 0, timeout_value: :nothing)]
                      end,
                  on_pool:
                      -> do
                        @arr = []
                        receive(on(Symbol) do |v1|
                          @arr.push v1
                          receive(on(Numeric) do |v2|
                            @arr << v2.succ
                            receive(on(Numeric, :got_it), on(TIMEOUT) { @arr << :nothing; @arr }, timeout: 0)
                          end)
                        end)
                      end }
        actor = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
        actor.tell 'junk'
        actor.tell 1
        actor.tell :v
        expect(actor.terminated.value!).to eq [:v, 2, :nothing]
      end

      describe "monitoring" do
        specify "(de)monitor" do
          body_receive = { on_thread:
                               -> { receive },
                           on_pool:
                               -> { receive { |v| v } } }

          body = { on_thread:
                       -> do
                         actor     = receive
                         reference = monitor actor
                         monitored = monitoring? reference
                         demonitor reference
                         result = [monitored, monitoring?(reference)]
                         actor.tell :finish
                         result
                       end,
                   on_pool:
                       -> do
                         receive do |actor|
                           reference = monitor actor
                           monitored = monitoring? reference
                           demonitor reference
                           result = [monitored, monitoring?(reference)]
                           actor.tell :finish
                           result
                         end
                       end }
          a1   = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
          a2   = Concurrent::ErlangActor.spawn(type: type, &body_receive.fetch(type))
          a1.tell a2
          expect(a1.terminated.value!).to eq [true, false]
          expect(a2.terminated.value!).to eq :finish
        end

        specify "demonitor" do
          body = { on_thread:
                       -> do
                         actor     = receive
                         reference = monitor actor
                         monitored = monitoring? reference
                         actor.tell :done
                         actor.terminated.wait
                         demonitor = demonitor reference, :flush, :info
                         [monitored, monitoring?(reference), demonitor, receive(timeout: 0)]
                       end,
                   on_pool:
                       -> do
                         receive do |actor|
                           reference = monitor actor
                           monitored = monitoring? reference
                           actor.tell :done
                           actor.terminated.wait
                           demonitor = demonitor reference, :flush, :info
                           results   = [monitored, monitoring?(reference), demonitor]
                           receive(on(ANY) { |v| [*results, v] },
                                   on(TIMEOUT) { [*results, nil] },
                                   timeout: 0)
                         end
                       end }

          a1   = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
          body = { on_thread: -> { receive },
                   on_pool:   -> { receive(&identity) } }
          a2   = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
          a1.tell a2

          a1.terminated.wait
          expect(a1.terminated.value!).to eq [true, false, false, nil]
          expect(a2.terminated.value!).to eq :done
        end

        specify "demonitor should leave the down message in the inbox if it's already there" do
          body = { on_thread:
                       -> do
                         actor     = receive
                         reference = monitor actor
                         monitored = monitoring? reference
                         actor.tell :done
                         actor.terminated.wait
                         demonitor = demonitor reference, :info
                         [reference, monitored, monitoring?(reference), demonitor, receive(timeout: 0)]
                       end,
                   on_pool:
                       -> do
                         receive do |actor|
                           reference = monitor actor
                           monitored = monitoring? reference
                           actor.tell :done
                           actor.terminated.wait
                           demonitor = demonitor reference, :info
                           results   = [reference, monitored, monitoring?(reference), demonitor]
                           receive(on(ANY) { |v| [*results, v] },
                                   on(TIMEOUT) { [*results, nil] },
                                   timeout: 0)
                         end
                       end }

          a1   = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
          body = { on_thread: -> { receive },
                   on_pool:   -> { receive(&identity) } }
          a2   = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
          a1.tell a2

          reference, monitored, monitoring, demonitor, message = a1.terminated.value!
          expect(monitored).to eq true
          expect(monitoring).to eq false
          expect(demonitor).to eq false
          expect(message).to eq Concurrent::ErlangActor::Down.new(a2, reference, :normal)
          expect(a2.terminated.value!).to eq :done
        end

        specify "notifications 1" do
          body = { on_thread:
                       -> do
                         b   = spawn { [:done, receive] }
                         ref = monitor b
                         b.tell 42
                         [b, ref, receive]
                       end,
                   on_pool:
                       -> do
                         b   = spawn { receive on(ANY) { |v| [:done, v] } }
                         ref = monitor b
                         b.tell 42
                         receive on(ANY) { |v| [b, ref, v] }
                       end }

          a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

          b, ref, down = a.terminated.value!
          expect(down).to eq Concurrent::ErlangActor::Down.new(b, ref, :normal)
          expect(b.terminated.value!).to eq [:done, 42]
        end

        specify "notifications 2" do
          body = { on_thread:
                       -> do
                         b = spawn { :done }
                         b.terminated.wait
                         ref = monitor b
                         [b, ref, receive(timeout: 1, timeout_value: :timeout)]
                       end,
                   on_pool:
                       -> do
                         b = spawn { :done }
                         b.terminated.wait
                         ref = monitor b
                         receive(timeout: 1) { |v| [b, ref, v] }
                       end }

          a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

          b, ref, down = a.terminated.value!
          expect(down).to eq Concurrent::ErlangActor::Down.new(b, ref, Concurrent::ErlangActor::NoActor.new(b))
          expect(b.terminated.value!).to eq :done
        end

        # FIXME (pitr-ch 20-Jan-2019): test concurrent exit and monitor(), same for link
      end

      describe 'linking' do
        body_receive_test_linked = { on_thread:
                                         -> { linked?(receive) },
                                     on_pool:
                                         -> { receive { |a| linked? a } } }

        specify 'links' do
          body1 = { on_thread:
                        -> do
                          actor = receive
                          link actor
                          linked = linked? actor
                          actor.tell pid
                          linked
                        end,
                    on_pool:
                        -> do
                          receive do |actor|
                            link actor
                            linked = linked? actor
                            actor.tell pid
                            linked
                          end
                        end }

          a1 = Concurrent::ErlangActor.spawn(type: type, &body1.fetch(type))
          a2 = Concurrent::ErlangActor.spawn(type: type, &body_receive_test_linked.fetch(type))

          a1.tell a2
          expect(a1.terminated.value!).to be_truthy
          expect(a2.terminated.value!).to be_truthy
        end

        specify 'unlinks' do
          body1 = { on_thread:
                        -> do
                          actor = receive
                          link actor
                          unlink actor
                          linked = linked? actor
                          actor.tell pid
                          linked
                        end,
                    on_pool:
                        -> do
                          receive do |actor|
                            link actor
                            unlink actor
                            linked = linked? actor
                            actor.tell pid
                            linked
                          end
                        end }

          a1 = Concurrent::ErlangActor.spawn(type: type, &body1.fetch(type))
          a2 = Concurrent::ErlangActor.spawn(type: type, &body_receive_test_linked.fetch(type))
          a1.tell a2
          expect(a1.terminated.value!).to be_falsey
          expect(a2.terminated.value!).to be_falsey
        end

        specify 'link dead' do
          a = Concurrent::ErlangActor.spawn(type: type) do
            b = spawn { :done }
            b.terminated.wait
            link b
          end
          expect { a.terminated.value! }.to raise_error Concurrent::ErlangActor::NoActor
        end

        specify 'link dead when trapping' do
          body1 = { on_thread:
                        -> do
                          b = spawn { :done }
                          b.terminated.wait
                          sleep 0.1
                          trap
                          link b
                          [b, receive]
                        end,
                    on_pool:
                        -> do
                          b = spawn { :done }
                          b.terminated.wait
                          sleep 0.1
                          trap
                          link b
                          receive { |v| [b, v] }
                        end }

          a = Concurrent::ErlangActor.spawn(type: type, &body1.fetch(type))

          b, captured = a.terminated.value!
          expect(captured).to eq Concurrent::ErlangActor::Terminated.new(b, Concurrent::ErlangActor::NoActor.new(b))
        end


        describe 'exit/1 when linked' do
          # https://learnyousomeerlang.com/errors-and-processes#links
          specify 1 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { :ok }
                           [receive(timeout: 0.01), b]
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) { :ok }
                           receive(on(ANY) { |v| [v, b] },
                                   on(TIMEOUT) { |v| [nil, b] },
                                   timeout: 0.01)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            message, b = a.terminated.value!
            expect(message).to eq nil
            expect(b.terminated.value!).to eq :ok
          end

          specify 2 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { :ok }
                           trap
                           [receive(timeout: 1), b]
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) { :ok }
                           trap
                           receive(on(ANY) { |v| [v, b] },
                                   on(TIMEOUT) { |v| [nil, b] },
                                   timeout: 1)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            message, b = a.terminated.value!
            expect(message).to eq Concurrent::ErlangActor::Terminated.new(b, :normal)
            expect(b.terminated.value!).to eq :ok
          end

          specify 3 do
            body = { on_thread:
                         -> do
                           spawn(link: true) { terminate :boom }
                           receive(timeout: 1)
                         end,
                     on_pool:
                         -> do
                           spawn(link: true) { terminate :boom }
                           receive(on(ANY) { |v| [v, b] },
                                   on(TIMEOUT) { |v| [nil, b] },
                                   timeout: 1)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            expect(a.terminated.reason).to eq :boom
          end

          specify 4 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { terminate :boom }
                           trap
                           [receive(timeout: 1), b]
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) { terminate :boom }
                           trap
                           receive(on(ANY) { |v| [v, b] },
                                   on(TIMEOUT) { |v| [nil, b] },
                                   timeout: 1)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            trapped_exit, b = a.terminated.value!
            expect(trapped_exit).to eq Concurrent::ErlangActor::Terminated.new(b, :boom)
            expect(b.terminated.reason).to eq :boom
          end

          specify 5 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { terminate :normal, value: :ok }
                           [receive(timeout: 0.01), b]
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) { terminate :normal, value: :ok }
                           receive(on(ANY) { |v| [v, b] },
                                   on(TIMEOUT) { |v| [nil, b] },
                                   timeout: 0.01)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            message, b = a.terminated.value!
            expect(message).to eq nil
            expect(b.terminated.value!).to eq :ok
          end

          specify 6 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { terminate :normal, value: :ok }
                           trap
                           [receive(timeout: 1), b]
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) { terminate :normal, value: :ok }
                           trap
                           receive(on(ANY) { |v| [v, b] },
                                   on(TIMEOUT) { |v| [nil, b] },
                                   timeout: 1)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            message, b = a.terminated.value!
            expect(message).to eq Concurrent::ErlangActor::Terminated.new(b, :normal)
            expect(b.terminated.value!).to eq :ok
          end

          specify 7 do
            body = { on_thread:
                         -> do
                           spawn(link: true) { raise 'err' }
                           receive(timeout: 1)
                         end,
                     on_pool:
                         -> do
                           spawn(link: true) { raise 'err' }
                           receive(timeout: 1) { |v| v }
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            expect { a.terminated.value! }.to raise_error(RuntimeError, 'err')
          end

          specify 8 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { raise 'err' }
                           trap
                           [receive(timeout: 1), b]
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) { raise 'err' }
                           trap
                           receive(on(ANY) { |v| [v, b] },
                                   on(TIMEOUT) { |v| [nil, b] },
                                   timeout: 1)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            trapped_exit, b = a.terminated.value!
            expect(trapped_exit).to be_a Concurrent::ErlangActor::Terminated
            expect(trapped_exit.from).to eq b
            expect(trapped_exit.reason).to eq b.terminated.reason
            expect(trapped_exit.reason).to be_a RuntimeError
            expect(trapped_exit.reason.message).to eq 'err'
          end

          specify 9 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { throw :uncaught }
                           trap
                           [receive(timeout: 1), b]
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) { throw :uncaught }
                           trap
                           receive(on(ANY) { |v| [v, b] },
                                   on(TIMEOUT) { |v| [nil, b] },
                                   timeout: 1)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            trapped_exit, b = a.terminated.value!
            expect(trapped_exit).to be_a Concurrent::ErlangActor::Terminated
            expect(trapped_exit.from).to eq b
            expect(trapped_exit.reason).to eq b.terminated.reason
            expect(trapped_exit.reason).to be_a ArgumentError
            expect(trapped_exit.reason.message).to match(/uncaught throw :uncaught/)
          end
        end

        describe 'exit/2 when linked' do
          # https://learnyousomeerlang.com/errors-and-processes#links
          specify 1 do
            body = { on_thread:
                         -> do
                           terminate pid, :normal # sends the signal to mailbox
                           # TODO (pitr-ch 17-Jan-2019): does erlang require receive to process signals?
                           receive(timeout: 0.01)
                           :continued
                         end,
                     on_pool:
                         -> do
                           terminate pid, :normal # sends the signal to mailbox
                           receive(on(ANY, :continued),
                                   on(TIMEOUT, :timeout),
                                   timeout: 0.01)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            expect(a.terminated.value!).to eq nil
          end

          specify 2 do
            body = { on_thread:
                         -> do
                           terminate pid, :normal
                           trap
                           receive(timeout: 0)
                         end,
                     on_pool:
                         -> do
                           terminate pid, :normal
                           trap
                           receive(on(ANY, &identity), on(TIMEOUT, nil), timeout: 0)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            captured_exit = a.terminated.value!
            expect(captured_exit).to eq Concurrent::ErlangActor::Terminated.new(a, :normal)
          end

          specify 3 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { receive timeout: 0.01, timeout_value: :timeout }
                           terminate b, :normal
                           b
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) do
                             receive(on(ANY, :not_happening),
                                     on(TIMEOUT, :timeout),
                                     timeout: 0.01)
                           end

                           terminate b, :normal
                           b
                         end }
            a    = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            b = a.terminated.value!
            expect(b.terminated.value!).to eq :timeout
          end

          specify 4 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { trap; receive timeout: 1, timeout_value: :timeout }
                           terminate b, :normal
                           b
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) do
                             trap
                             receive(on(ANY, &identity),
                                     on(TIMEOUT, :timeout),
                                     timeout: 1)
                           end

                           terminate b, :normal
                           b
                         end }
            a    = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            b = a.terminated.value!
            expect(b.terminated.value!).to eq Concurrent::ErlangActor::Terminated.new(a, :normal)
          end

          specify 5 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { receive timeout: 0.01; terminate :continued }
                           terminate b, :normal
                           trap
                           [b, receive(timeout: 1)]
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) do
                             receive(on(ANY, :not_happening),
                                     on(TIMEOUT) { terminate :continued },
                                     timeout: 0.01)
                           end

                           terminate b, :normal
                           trap
                           receive(on(ANY) { |v| [b, v] }, on(TIMEOUT, :timeout), timeout: 1)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            b, captured = a.terminated.value!
            expect(b.terminated.reason).to eq :continued
            # normal is never send from b to a back
            expect(captured).to eq Concurrent::ErlangActor::Terminated.new(b, :continued)
          end

          specify 6 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { receive timeout: 1; :done }
                           terminate b, :remote_err
                           receive timeout: 1
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) { receive(on(ANY, :done), on(TIMEOUT, :timeout), timeout: 1) }
                           terminate b, :remote_err
                           receive(on(ANY) { |v| [b, v] },
                                   on(TIMEOUT, :timeout),
                                   timeout: 1)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
            a.terminated.wait
            expect(a.terminated.reason).to eq :remote_err
          end

          specify 7 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { receive timeout: 1; :done }
                           terminate b, :remote_err
                           trap
                           [b, receive(timeout: 1)]
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) { receive(on(ANY, :done), on(TIMEOUT, :timeout), timeout: 1) }
                           terminate b, :remote_err
                           trap
                           receive(on(ANY) { |v| [b, v] },
                                   on(TIMEOUT, :timeout),
                                   timeout: 1)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            b, captured = a.terminated.value!
            expect(b.terminated.reason).to eq :remote_err
            expect(captured.reason).to eq :remote_err
          end

          specify 8 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { receive timeout: 1; :done }
                           terminate b, :kill
                           receive timeout: 1
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) { receive(on(ANY, :done), on(TIMEOUT, :done), timeout: 1) }
                           terminate b, :kill
                           receive(on(ANY, &identity), on(TIMEOUT, :timeout), timeout: 1)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            expect(a.terminated.reason).to eq :killed
          end

          specify 9 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { receive timeout: 1; :done }
                           terminate b, :kill
                           trap
                           [b, receive(timeout: 1)]
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) { receive(on(ANY, :done), on(TIMEOUT, :done), timeout: 1) }
                           terminate b, :kill
                           trap
                           receive(on(ANY) { |v| [b, v] }, on(TIMEOUT, :timeout), timeout: 1)
                         end }

            a           = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
            b, captured = a.terminated.value!
            expect(b.terminated.reason).to eq :killed
            expect(captured.reason).to eq :killed
          end

          specify 10 do
            body = { on_thread:
                         -> do
                           terminate pid, :kill
                           receive timeout: 0
                         end,
                     on_pool:
                         -> do
                           terminate pid, :kill
                           receive(on(ANY, :continued), on(TIMEOUT, :timeout), timeout: 0)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            expect(a.terminated.reason).to eq :killed
          end

          specify 11 do
            body = { on_thread:
                         -> do
                           terminate pid, :kill
                           trap
                           receive timeout: 0
                         end,
                     on_pool:
                         -> do
                           terminate pid, :kill
                           trap
                           receive(on(ANY, &identity), on(TIMEOUT, :timeout), timeout: 0)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            expect(a.terminated.reason).to eq :killed
          end

          # explained in
          # http://erlang.org/pipermail/erlang-questions/2009-October/047241.html

          specify 12 do
            body = { on_thread:
                         -> do
                           spawn(link: true) { terminate :kill }
                           receive timeout: 1
                         end,
                     on_pool:
                         -> do
                           spawn(link: true) { terminate :kill }
                           receive(on(ANY, :continued),
                                   on(TIMEOUT, :timeout),
                                   timeout: 1)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            expect(a.terminated.reason).to eq :kill
          end

          specify 13 do
            body = { on_thread:
                         -> do
                           b = spawn(link: true) { terminate :kill }
                           trap
                           [b, receive(timeout: 1)]
                         end,
                     on_pool:
                         -> do
                           b = spawn(link: true) { terminate :kill }
                           trap
                           receive(on(ANY) { |v| [b, v] },
                                   on(TIMEOUT, :timeout),
                                   timeout: 1)
                         end }

            a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))

            b, captured = a.terminated.value!

            expect(b.terminated.reason).to eq :kill
            expect(captured).to eq Concurrent::ErlangActor::Terminated.new(b, :kill)
          end

        end
      end

      specify 'spawn(link: true)' do
        a = Concurrent::ErlangActor.spawn(type: type) do
          b = spawn(link: true) { :v }
          linked? b
        end
        expect(a.terminated.value!).to be_truthy

        a = Concurrent::ErlangActor.spawn(type: type) do
          b = spawn { :v }
          linked? b
        end
        expect(a.terminated.value!).to be_falsey
      end

      specify 'termination' do
        a = Concurrent::ErlangActor.spawn(type: type) { :v }
        expect(a.terminated.value!).to eq :v

        a = Concurrent::ErlangActor.spawn(type: type) { raise 'err' }
        expect { a.terminated.value! }.to raise_error(RuntimeError, 'err')

        a = Concurrent::ErlangActor.spawn(type: type) { terminate :normal, value: :val }
        expect(a.terminated.value!).to eq :val

        a = Concurrent::ErlangActor.spawn(type: type) { terminate :er }
        expect(a.terminated.reason).to eq :er
      end

      describe 'asking' do
        specify "replies" do
          body = { on_thread: -> { reply receive },
                   on_pool:   -> { receive { |v| reply v } } }
          a    = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
          expect(a.ask(:v)).to eq :v

          body = { on_thread: -> { v = receive; reply v; reply v; },
                   on_pool:   -> { receive { |v| reply v; reply v } } }
          a    = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
          expect(a.ask(:v)).to eq :v
          expect(a.terminated.value!).to be_falsey

          body = { on_thread:
                       -> do
                         v = receive
                         reply v
                         reply_resolution true, v.to_s, nil
                       end,
                   on_pool:
                       -> do
                         receive do |v|
                           reply v
                           reply_resolution true, v.to_s, nil
                         end
                       end }
          a    = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
          expect(a.ask(:v)).to eq :v
          expect(a.terminated.value!).to be_falsey

          body = { on_thread: -> { reply_resolution false, nil, receive },
                   on_pool:   -> { receive { |v| reply_resolution false, nil, v } } }
          a    = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
          expect { a.ask(:err) }.to raise_error StandardError, 'err'

          body = { on_thread: -> { reply_resolution false, nil, receive },
                   on_pool:   -> { receive { |v| reply_resolution false, nil, v } } }
          a    = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
          expect(a.ask_op(:err).reason).to eq :err
        end

        specify "timing out" do
          skip('flaky on truffleruby') if Concurrent.on_truffleruby?

          count_down = Concurrent::CountDownLatch.new
          body = { on_thread: -> { m = receive; count_down.wait; reply m },
                   on_pool:   -> { receive { |m| count_down.wait; reply m } } }
          a    = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
          expect(a.ask(:err, 0, 42)).to eq 42
          count_down.count_down
          expect(a.terminated.value!).to eq false

          body = { on_thread: -> { reply receive },
                   on_pool:   -> { receive { |m| reply m } } }
          b    = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
          expect(b.ask(:v, 1, 42)).to eq :v
          expect(b.terminated.value!).to eq true
        end

        specify "rejects on no reply" do
          body = { on_thread: -> { receive; receive },
                   on_pool:   -> { receive { receive {} } } }

          a = Concurrent::ErlangActor.spawn(type: type, &body.fetch(type))
          expect(a.ask_op(:v).reason).to eq Concurrent::ErlangActor::NoReply
          expect { raise a.ask_op(:v).wait }.to raise_error Concurrent::ErlangActor::NoActor
          expect { raise a.ask(:v) }.to raise_error Concurrent::ErlangActor::NoActor
        end

      end
    end

    describe 'on thread' do
      let(:type) { :on_thread }
      it_behaves_like 'erlang actor'

      specify do
        actor = Concurrent::ErlangActor.spawn(type: :on_thread) do
          Thread.abort_on_exception = true
          while true
            receive on(Symbol) { |s| reply s.to_s },
                    on(And[Numeric, -> v { v >= 0 }]) { |v| reply v.succ },
                    # put last works as else
                    on(ANY) { |v| reply :bad_message; terminate [:bad_message, v] }
          end
        end
        expect(actor.ask(1)).to eq 2
        expect(actor.ask(:value)).to eq 'value'
        expect(actor.ask(-1)).to eq :bad_message
        expect { actor.ask 'junk' }.to raise_error Concurrent::ErlangActor::NoActor
        expect(actor.terminated.reason).to eq [:bad_message, -1]
      end
    end

    describe 'on pool' do
      let(:type) { :on_pool }
      it_behaves_like 'erlang actor'

      include Concurrent::ErlangActor::EnvironmentConstants

      specify "receives message repeatedly with keep" do
        actor = Concurrent::ErlangActor.spawn(type: :on_pool) do
          receive on(ANY) { |v| v == :done ? terminate(:normal, value: 42) : reply(v) },
                  keep: true
        end
        expect(actor.ask(1)).to eq 1
        expect(actor.ask(2)).to eq 2
        actor.tell :done
        expect(actor.terminated.value!).to eq 42
      end

      specify "class defined" do
        definition_module = Module.new do
          def start
            @sum = 0
            receive on(Numeric, &method(:count)),
                    on(:done, &method(:stop)),
                    on(TIMEOUT, &method(:fail)),
                    keep:    true,
                    timeout: 0.1
          end

          def count(message)
            reply @sum += message
          end

          def stop(_message)
            terminate :normal, value: @sum
          end

          def fail(_message)
            terminate :timeout
          end
        end
        definition_class  = Class.new Concurrent::ErlangActor::Environment do
          include definition_module
        end

        actor = Concurrent::ErlangActor.spawn(type: :on_pool, environment: definition_class) { start }
        actor.tell 1
        expect(actor.ask(2)).to eq 3
        actor.tell :done
        expect(actor.terminated.value!).to eq 3

        actor = Concurrent::ErlangActor.spawn(type: :on_pool, environment: definition_module)
        actor.tell 1
        expect(actor.ask(2)).to eq 3
        expect(actor.terminated.reason).to eq :timeout
      end

    end
  end
end
