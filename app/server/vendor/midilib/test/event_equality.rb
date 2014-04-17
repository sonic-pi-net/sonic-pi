# This code defines equality operators for all of the event classes. It's
# used by SeqTester.
#
# I don't think it is necessary to include these methods in the base Event
# classes. If someone disagrees, it would be trivial to move them there.

module MIDI

  class Event
    def ==(an_obj)
      return an_obj.instance_of?(self.class) &&
        @status == an_obj.status &&
        @delta_time == an_obj.delta_time &&
        @time_from_start == an_obj.time_from_start
    end
  end

  class ChannelEvent
    def ==(an_obj)
      return super(an_obj) && @channel == an_obj.channel
    end
  end

  class NoteEvent < ChannelEvent
    def ==(an_obj)
      return super(an_obj) &&
        @note == an_obj.note && @velocity == an_obj.velocity
    end
  end

  class Controller < ChannelEvent
    def ==(an_obj)
      return super(an_obj) &&
        @controller == an_obj.controller && @value == an_obj.value
    end
  end

  class ProgramChange < ChannelEvent
    def ==(an_obj)
      return super(an_obj) && @program == an_obj.program
    end
  end

  class ChannelPressure < ChannelEvent
    def ==(an_obj)
      return super(an_obj) && @pressure == an_obj.pressure
    end
  end

  class PitchBend < ChannelEvent
    def ==(an_obj)
      return super(an_obj) && @value == an_obj.value
    end
  end

  class SystemExclusive < SystemCommon
    def ==(an_obj)
      return super(an_obj) && @data == an_obj.data
    end
  end

  class SongPointer < SystemCommon
    def ==(an_obj)
      return super(an_obj) && @pointer == an_obj.pointer
    end
  end

  class SongSelect < SystemCommon
    def ==(an_obj)
      return super(an_obj) && @song == an_obj.song
    end
  end

  class MetaEvent < Event
    def ==(an_obj)
      return super(an_obj) && @meta_type == an_obj.meta_type &&
        @data == an_obj.data
    end
  end

end
