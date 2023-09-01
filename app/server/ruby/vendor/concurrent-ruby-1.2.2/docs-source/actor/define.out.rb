Message = Struct.new :action, :value 

class AnActor < Concurrent::Actor::RestartingContext
  def initialize(init)
    @counter = init
  end

  # override #on_message to define actor's behaviour on message received
  def on_message(message)
    case message.action
    when :add
      @counter = @counter + message.value
    when :subtract
      @counter = @counter - message.value
    when :value
      @counter
    else
      pass
    end
  end

  # set counter to zero when there is an error
  def on_event(event)
    if event == :reset
      @counter = 0 # ignore initial value
    end
  end
end 

an_actor = AnActor.spawn name: 'an_actor', args: 10 
an_actor << Message.new(:add, 1) << Message.new(:subtract, 2) 
an_actor.ask!(Message.new(:value, nil))            # => 9
an_actor << :boo << Message.new(:add, 1) 
an_actor.ask!(Message.new(:value, nil))            # => 1
an_actor << :terminate!
    # => #<Concurrent::Actor::Reference:0x7fbedc137688 /an_actor (AnActor)>

