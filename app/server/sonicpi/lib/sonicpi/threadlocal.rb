#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++


module SonicPi
  class ThreadLocal
    attr_reader :vars, :local_vars
    def initialize(parent=nil)
      raise "ThreadLocal can only be initialized with nil or a parent ThreadLocal" unless parent.nil? || parent.is_a?(ThreadLocal)

      if parent
        @parent_vars = parent.vars.clone
        @vars = parent.vars.clone

      else
        @parent_vars = nil
        @vars = {}
      end

      @local_vars = {}
    end

    def set(name, val)
      raise "Error setting Thread Local - value must be immutable. Got: #{val.inspect} for #{name.inspect}" unless val.sp_thread_safe?
      @vars[name] = val
      @local_vars.delete name
      val
    end

    # These values will not be inherited
    def set_local(name, val)
      @local_vars[name] = val
      @vars.delete name
      val
    end

    def get(name)
      if @local_vars.has_key? name
        return @local_vars[name]
      elsif @vars.has_key? name
        return @vars[name]
      else
        return @parent_vars[name] if @parent_vars
      end
    end
  end
end
