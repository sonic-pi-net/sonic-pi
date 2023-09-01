class Adder < Concurrent::Actor::RestartingContext
  def initialize(init)
    @count = init
  end

  def on_message(message)
    case message
    when :add
      @count += 1
    else
      # pass to ErrorsOnUnknownMessage behaviour, which will just fail
      pass
    end
  end
end 

# `link: true` makes the actor linked to root actor and supervised
# which is default behavior
adder = Adder.spawn(name: :adder, link: true, args: [1])
    # => #<Concurrent::Actor::Reference:0x7fbedd8e3d40 /adder (Adder)>
adder.parent
    # => #<Concurrent::Actor::Reference:0x7fbedbaa1e90 / (Concurrent::Actor::Root)>

# tell and forget
adder.tell(:add).tell(:add)
    # => #<Concurrent::Actor::Reference:0x7fbedd8e3d40 /adder (Adder)>
# ask to get result
adder.ask!(:add)                                   # => 4
# fail the actor
adder.ask!(:bad) rescue $!
    # => #<Concurrent::Actor::UnknownMessage: :bad from #<Thread:0x007fbedb8809b8>>
# actor is restarted with initial values
adder.ask!(:add)                                   # => 2
adder.ask!(:terminate!)                            # => true
