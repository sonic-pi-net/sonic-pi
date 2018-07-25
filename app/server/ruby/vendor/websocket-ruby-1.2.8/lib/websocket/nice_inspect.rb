# frozen_string_literal: true

module WebSocket
  module NiceInspect
    # Recreate inspect as #to_s will be overwritten
    def inspect
      vars = instance_variables.map { |v| "#{v}=#{instance_variable_get(v).inspect}" }.join(', ')
      insp = Kernel.format("#{self.class}:0x%08x", __id__)
      "<#{insp} #{vars}>"
    end
  end
end
