require 'concurrent/edge/processing_actor'

RSpec.describe 'Concurrent::ProcessingActor' do
  specify do
    actor = Concurrent::ProcessingActor.act do |the_actor|
      the_actor.receive.then do |message|
        # the actor ends with message
        message
      end
    end #

    actor.tell! :a_message
    expect(actor.termination.value!).to eq :a_message

    def count(actor, count)
      # the block passed to receive is called when the actor receives the message
      actor.receive.then do |number_or_command, answer|
        # number_or_command, answer = p a
        # p number_or_command, answer

        # code which is evaluated after the number is received
        case number_or_command
        when :done
          # this will become the result (final value) of the actor
          count
        when :count
          # reply the current count
          answer.fulfill count
          # continue running
          count(actor, count)
        when Integer
          # this will call count again to set up what to do on next message, based on new state `count + numer`
          count(actor, count + number_or_command)
        end
      end
      # evaluation of count ends immediately
      # code which is evaluated before the number is received, should be empty
    end

    counter = Concurrent::ProcessingActor.act { |a| count a, 0 }
    answer  = counter.tell!(2).ask_op { |a| [:count, a] }.value!
    expect(counter.tell!(3).tell!(:done).termination.value!).to eq 5
    expect(answer.value!).to eq 2

    add_once_actor = Concurrent::ProcessingActor.act do |the_actor|
      the_actor.receive.then do |a, b, reply|
        result = a + b
        reply.fulfill result
        # terminate with result value
        result
      end
    end

    expect(add_once_actor.ask_op { |a| [1, 2, a] }.value!.value!).to eq 3
    # expect(add_once_actor.ask_operation(%w(ab cd)).reason).to be_a_kind_of RuntimeError
    expect(add_once_actor.termination.value!).to eq 3

    def pair_adder(actor)
      (actor.receive & actor.receive).then do |(value1, answer1), (value2, answer2)|
        result = value1 + value2
        answer1.fulfill result if answer1
        answer2.fulfill result if answer2
        pair_adder actor
      end
    end

    pair_adder = Concurrent::ProcessingActor.act { |a| pair_adder a }
    pair_adder.ask_op { |a| [2, a] }
    answer = pair_adder.ask_op { |a| [3, a] }.value!
    expect(answer.value!).to eq 5
    expect((pair_adder.ask_op { |a| ['a', a] }.value! & pair_adder.ask_op { |a| ['b', a] }.value!).value!).to eq %w[ab ab]
    expect((pair_adder.ask_op { |a| ['a', a] }.value! | pair_adder.ask_op { |a| ['b', a] }.value!).value!).to eq 'ab'
  end
end
