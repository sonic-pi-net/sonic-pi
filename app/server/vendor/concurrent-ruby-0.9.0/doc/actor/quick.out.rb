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
    # => #<Concurrent::Actor::Reference:0x7fb6fc88ff58 /adder (Adder)>
adder.parent
    # => #<Concurrent::Actor::Reference:0x7fb6fe0f5db8 / (Concurrent::Actor::Root)>

# tell and forget
adder.tell(:add).tell(:add)
    # => #<Concurrent::Actor::Reference:0x7fb6fc88ff58 /adder (Adder)>
# ask to get result
adder.ask!(:add)                                   # => 4
# fail the actor
adder.ask!(:bad) rescue $!                         # => #<Concurrent::Actor::UnknownMessage: :bad>
# actor is restarted with initial values
adder.ask!(:add)                                   # => 2
adder.ask!(:terminate!)                            # => true
