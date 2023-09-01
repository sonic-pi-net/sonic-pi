require 'concurrent/edge/channel'

RSpec.describe 'Concurrent' do
  describe 'Promises::Channel', edge: true do
    specify "#capacity" do
      channel = Concurrent::Promises::Channel.new 2
      expect(channel.capacity).to be 2
    end

    specify "#to_s" do
      channel = Concurrent::Promises::Channel.new
      expect(channel.to_s).to match(/Channel.*unlimited/)
      channel = Concurrent::Promises::Channel.new 2
      expect(channel.to_s).to match(/Channel.*0.*2/)
      channel.push :value
      expect(channel.to_s).to match(/Channel.*1.*2/)
    end

    specify "#(try_)push(_op)" do
      channel = Concurrent::Promises::Channel.new 1

      expect(channel.size).to eq 0
      expect(channel.try_push(:v1)).to be_truthy
      expect(channel.size).to eq 1
      expect(channel.try_push(:v2)).to be_falsey
      expect(channel.size).to eq 1

      channel = Concurrent::Promises::Channel.new 1
      expect(channel.push(:v1)).to eq channel
      expect(channel.size).to eq 1
      thread = in_thread { channel.push :v2 }
      is_sleeping thread
      expect(channel.size).to eq 1
      channel.pop
      expect(channel.size).to eq 1
      expect(thread.value).to eq channel
      channel.pop
      expect(channel.size).to eq 0

      channel = Concurrent::Promises::Channel.new 1
      expect(channel.push(:v1)).to eq channel
      expect(channel.size).to eq 1
      thread = in_thread { channel.push :v2, 0.01 }
      is_sleeping thread
      expect(channel.size).to eq 1
      expect(thread.value).to eq false
      channel.pop
      expect(channel.size).to eq 0
      expect(channel.push(:v3, 0)).to eq true
      expect(channel.size).to eq 1
      thread = in_thread { channel.push :v2, 1 }
      is_sleeping thread
      channel.pop
      expect(channel.size).to eq 1
      expect(thread.value).to eq true

      channel = Concurrent::Promises::Channel.new 1
      expect(channel.push_op(:v1).value!).to eq channel
      expect(channel.size).to eq 1
      push_op = channel.push_op :v2
      expect(channel.size).to eq 1
      expect(push_op.pending?).to be_truthy
      channel.pop
      expect(channel.size).to eq 1
      expect(push_op.value!).to eq channel
      channel.pop
      expect(channel.size).to eq 0
    end

    specify "#(try_)pop(_op)" do
      channel = Concurrent::Promises::Channel.new 1
      channel.push :v1

      expect(channel.size).to eq 1
      expect(channel.try_pop).to eq :v1
      expect(channel.size).to eq 0
      expect(channel.try_pop).to eq nil
      expect(channel.size).to eq 0

      channel = Concurrent::Promises::Channel.new 1
      channel.push :v1
      expect(channel.pop).to eq :v1
      expect(channel.size).to eq 0
      thread = in_thread { channel.pop }
      is_sleeping thread
      expect(channel.size).to eq 0
      channel.push :v2
      expect(thread.value).to eq :v2
      expect(channel.size).to eq 0

      channel = Concurrent::Promises::Channel.new 1
      channel.push :v1
      expect(channel.pop).to eq :v1
      expect(channel.size).to eq 0
      thread = in_thread { channel.pop 0.01 }
      is_sleeping thread
      expect(channel.size).to eq 0
      expect(thread.value).to eq nil
      channel.push :v2
      expect(channel.size).to eq 1
      expect(channel.pop).to eq :v2
      expect(channel.size).to eq 0
      thread = in_thread { channel.pop 1 }
      is_sleeping thread
      channel.push :v3
      expect(channel.size).to eq 0
      expect(thread.value).to eq :v3
      channel.push :v4
      expect(channel.pop(0)).to eq :v4

      channel = Concurrent::Promises::Channel.new 1
      channel.push :v1
      expect(channel.pop_op.value!).to eq :v1
      expect(channel.size).to eq 0
      pop_op = channel.pop_op
      expect(channel.size).to eq 0
      expect(pop_op.pending?).to be_truthy
      channel.push :v2
      expect(channel.size).to eq 0
      expect(pop_op.value!).to eq :v2
    end

    specify "#(try_)pop(_op)_matching" do
      channel = Concurrent::Promises::Channel.new 2
      channel.push 'junk'
      channel.push :v1

      expect(channel.size).to eq 2
      expect(channel.try_pop_matching(Symbol)).to eq :v1
      expect(channel.size).to eq 1
      expect(channel.try_pop_matching(Symbol)).to eq nil
      expect(channel.size).to eq 1

      channel = Concurrent::Promises::Channel.new 2
      channel.push 'junk'
      channel.push :v1
      expect(channel.pop_matching(Symbol)).to eq :v1
      expect(channel.size).to eq 1
      thread = in_thread { channel.pop_matching(Symbol) }
      is_sleeping thread
      expect(channel.size).to eq 1
      channel.push 'junk'
      channel.pop
      channel.push :v2
      expect(thread.value).to eq :v2
      expect(channel.size).to eq 1

      channel = Concurrent::Promises::Channel.new 2
      channel.push 'junk'
      channel.push :v1
      expect(channel.pop_matching(Symbol)).to eq :v1
      expect(channel.size).to eq 1
      thread = in_thread { channel.pop_matching(Symbol, 0.01) }
      is_sleeping thread
      expect(channel.size).to eq 1
      expect(thread.value).to eq nil
      channel.push :v2
      expect(channel.size).to eq 2
      expect(channel.pop_matching(Symbol)).to eq :v2
      expect(channel.size).to eq 1
      thread = in_thread { channel.pop_matching(Symbol,1) }
      is_sleeping thread
      channel.push :v3
      expect(channel.size).to eq 1
      expect(thread.value).to eq :v3
      channel.push :v4
      expect(channel.pop_matching(Symbol,0)).to eq :v4

      channel = Concurrent::Promises::Channel.new 2
      channel.push 'junk'
      channel.push :v1
      expect(channel.pop_op_matching(Symbol).value!).to eq :v1
      expect(channel.size).to eq 1
      pop_op = channel.pop_op_matching(Symbol)
      expect(channel.size).to eq 1
      expect(pop_op.pending?).to be_truthy
      channel.push :v2
      expect(channel.size).to eq 1
      expect(pop_op.value!).to eq :v2
    end

    specify "#(try_)select(_op)" do
      channel1 = Concurrent::Promises::Channel.new 1
      channel2 = Concurrent::Promises::Channel.new 1

      expect(channel1.try_select(channel2)).to eq nil
      expect(Concurrent::Promises::Channel.try_select([channel1, channel2])).to eq nil
      channel1.push :v1
      expect(channel1.try_select(channel2)).to eq [channel1, :v1]
      expect(channel1.size).to eq 0
      expect(channel2.size).to eq 0

      channel1 = Concurrent::Promises::Channel.new 1
      channel2 = Concurrent::Promises::Channel.new 1
      channel1.push :v1
      expect(Concurrent::Promises::Channel.select([channel1, channel2])).to eq [channel1, :v1]
      channel1.push :v1
      expect(channel1.select(channel2)).to eq [channel1, :v1]
      expect(channel1.size).to eq 0
      expect(channel2.size).to eq 0
      thread = in_thread { channel1.select(channel2) }
      is_sleeping thread
      expect(channel1.size).to eq 0
      channel2.push :v2
      expect(thread.value).to eq [channel2, :v2]
      expect(channel1.size).to eq 0
      expect(channel2.size).to eq 0

      channel1 = Concurrent::Promises::Channel.new 1
      channel2 = Concurrent::Promises::Channel.new 1
      channel1.push :v1
      expect(channel1.select(channel2)).to eq [channel1, :v1]
      expect(channel1.size).to eq 0
      expect(channel2.size).to eq 0
      thread = in_thread { channel1.select(channel2, 0.01) }
      is_sleeping thread
      expect(channel1.size).to eq 0
      expect(channel2.size).to eq 0
      expect(thread.value).to eq nil
      channel2.push :v2
      expect(channel1.size).to eq 0
      expect(channel2.size).to eq 1
      expect(channel2.select(channel1)).to eq [channel2, :v2]
      expect(channel1.size).to eq 0
      expect(channel2.size).to eq 0

      channel1 = Concurrent::Promises::Channel.new 1
      channel2 = Concurrent::Promises::Channel.new 1
      channel1.push :v1
      expect(channel1.select_op(channel2).value!).to eq [channel1, :v1]
      channel1.push :v1
      expect(Concurrent::Promises::Channel.select_op([channel1, channel2]).value!).to eq [channel1, :v1]
      expect(channel1.size).to eq 0
      expect(channel2.size).to eq 0
      select_op = channel2.select_op(channel1)
      expect(channel1.size).to eq 0
      expect(channel2.size).to eq 0
      expect(select_op.pending?).to be_truthy
      channel2.push :v2
      expect(channel1.size).to eq 0
      expect(channel2.size).to eq 0
      expect(select_op.value!).to eq [channel2, :v2]
    end

    def push_first(push_type, pop_type)
      channel = Concurrent::Promises::Channel.new 0
      message = Object.new

      case push_type
      when :push
        thread = in_thread { channel.push message }
        is_sleeping thread
      when :push_op
        push = channel.push_op message
        expect(push.pending?).to eq true
      else
        raise
      end

      expect(channel.size).to eq 0

      case pop_type
      when :try_pop
        expect(channel.try_pop).to eq message
      when :pop
        expect(channel.pop).to eq message
      when :pop_op
        expect(channel.pop_op.value!).to eq message
      else
        raise
      end

      expect(channel.size).to eq 0

      case push_type
      when :push
        expect(thread.value).to eq channel
      when :push_op
        expect(push.value!).to eq channel
      else
        raise
      end
    end

    def pop_first(pop_type, push_type)
      channel = Concurrent::Promises::Channel.new 0
      message = Object.new

      case pop_type
      when :pop
        thread = in_thread { channel.pop }
        is_sleeping thread
      when :pop_op
        pop = channel.pop_op
        expect(pop.pending?).to eq true
      else
        raise
      end

      expect(channel.size).to eq 0

      case push_type
      when :try_push
        expect(channel.try_push message).to eq true
      when :push
        expect(channel.push(message)).to eq channel
      when :push_op
        expect(channel.push_op(message).value!).to eq channel
      else
        raise
      end

      expect(channel.size).to eq 0

      case pop_type
      when :pop
        expect(thread.value).to eq message
      when :pop_op
        expect(pop.value!).to eq message
      else
        raise
      end
    end


    specify 'exchanging' do
      push_first :push, :try_pop
      push_first :push, :pop
      push_first :push, :pop_op
      push_first :push_op, :try_pop
      push_first :push_op, :pop
      push_first :push_op, :pop_op

      pop_first :pop, :try_push
      pop_first :pop, :push
      pop_first :pop, :push_op
      pop_first :pop_op, :try_push
      pop_first :pop_op, :push
      pop_first :pop_op, :push_op

      ch1       = Concurrent::Promises::Channel.new 0
      ch2       = Concurrent::Promises::Channel.new 0
      selection = ch1.select_op(ch2)
      expect(ch2.try_push(:v3)).to be_truthy
      expect(selection.value!).to eq [ch2, :v3]
    end

    specify 'integration' do
      ch1 = Concurrent::Promises::Channel.new
      ch2 = Concurrent::Promises::Channel.new
      ch3 = Concurrent::Promises::Channel.new

      add = -> *_ do
        (ch1.pop_op & ch2.pop_op).then do |a, b|
          if a == :done && b == :done
            :done
          else
            # do not add again until push is done
            ch3.push_op(a + b).then(&add)
          end
        end
      end

      ch1.push_op 1
      ch2.push_op 2
      ch1.push_op 'a'
      ch2.push_op 'b'
      ch1.push_op nil
      ch2.push_op true

      result = Concurrent::Promises.future(&add).run.result
      expect(result[0..1]).to eq [false, nil]
      expect(result[2]).to be_a_kind_of(NoMethodError)
      expect(ch3.pop_op.value!).to eq 3
      expect(ch3.pop_op.value!).to eq 'ab'

      ch1.push_op 1
      ch2.push_op 2
      ch1.push_op 'a'
      ch2.push_op 'b'
      ch1.push_op :done
      ch2.push_op :done

      expect(Concurrent::Promises.future(&add).run.result).to eq [true, :done, nil]
      expect(ch3.pop_op.value!).to eq 3
      expect(ch3.pop_op.value!).to eq 'ab'
    end
  end
end
