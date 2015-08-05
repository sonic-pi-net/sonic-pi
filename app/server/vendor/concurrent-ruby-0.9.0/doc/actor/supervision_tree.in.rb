
class Master < Concurrent::Actor::RestartingContext
  def initialize
    # for listener to be child of master
    @listener = Listener.spawn(name: 'listener1', supervise: true)
  end

  def on_message(msg)
    command, *args = msg
    case command
    when :listener
      @listener
    when :reset, :terminated, :resumed, :paused
      log Logger::DEBUG, " got #{msg} from #{envelope.sender}"
    else
      pass
    end
  end

  # TODO turn this into Behaviour and make it default part of RestartingContext
  def on_event(event)
    event_name, _ = event
    case event_name
    when :resetting, :restarting
      @listener << :terminate!
    when Exception, :paused
      @listener << :pause!
    when :resumed
      @listener << :resume!
    end
  end
end #

class Listener < Concurrent::Actor::RestartingContext
  def initialize
    @number = (rand() * 100).to_i
  end

  def on_message(msg)
    case msg
    when :number
      @number
    else
      pass
    end
  end

end #

master   = Master.spawn(name: 'master', supervise: true)
listener = master.ask!(:listener)
listener.ask!(:number)

master << :crash

sleep 0.1

# ask for listener again, old one is terminated
listener.ask!(:terminated?)
listener = master.ask!(:listener)
listener.ask!(:number)

master.ask!(:terminate!)

sleep 0.1
