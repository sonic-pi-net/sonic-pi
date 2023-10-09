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
end #

# `link: true` makes the actor linked to root actor and supervised
# which is default behavior
adder = Adder.spawn(name: :adder, link: true, args: [1])
adder.parent

# tell and forget
adder.tell(:add).tell(:add)
# ask to get result
adder.ask!(:add)
# fail the actor
adder.ask!(:bad) rescue $!
# actor is restarted with initial values
adder.ask!(:add)
adder.ask!(:terminate!)
