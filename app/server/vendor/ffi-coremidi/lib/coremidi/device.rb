module CoreMIDI

  # A MIDI device may have multiple logically distinct sub-components. For example, one device may 
  # encompass a MIDI synthesizer and a pair of MIDI ports, both addressable via a USB port. Each 
  # such element of a device is called a MIDI entity.
  #
  # https://developer.apple.com/library/ios/documentation/CoreMidi/Reference/MIDIServices_Reference/Reference/reference.html
  class Device

    attr_reader :entities,
                :id, # Unique Numeric id
                :name # Device name from coremidi

    # @param [Fixnum] id The ID for the device
    # @param [Object] device_pointer The underlying device pointer
    # @param [Hash] options
    # @option options [Boolean] :include_offline Whether to include offline entities (default: false)
    def initialize(id, device_pointer, options = {})
      @id = id
      @resource = device_pointer
      @entities = []
      populate(options)
    end
            
    # Endpoints for this device
    # @return [Array<Endpoint>]
    def endpoints
      endpoints = { :source => [], :destination => [] }
      endpoints.keys.each do |key|
        endpoint_group = entities.map { |entity| entity.endpoints[key] }.flatten
        endpoints[key] += endpoint_group
      end
      endpoints
    end
    
    # Assign all of this Device's endpoints an consecutive local id
    # @param [Integer] last_id The highest already used endpoint ID 
    # @return [Integer] The highest used endpoint ID after populating this device's endpoints
    def populate_endpoint_ids(last_id)
      id = 0
      entities.each { |entity| id += entity.populate_endpoint_ids(id + last_id) }
      id
    end

    # All cached devices
    # @param [Hash] options The options to select devices with
    # @option options [Boolean] :cache If false, the device list will never be cached. This would be useful if one needs to alter the device list (e.g. plug in a USB MIDI interface) while their program is running.
    # @option options [Boolean] :include_offline If true, devices marked offline by coremidi will be included in the list
    # @return [Array<Device>] All cached devices
    def self.all(options = {})
      use_cache = options[:cache] || true
      include_offline = options[:include_offline] || false
      if !populated? || !use_cache
        @devices = []
        counter = 0
        while !(device_pointer = API.MIDIGetDevice(counter)).null?
          device = new(counter, device_pointer, :include_offline => include_offline)
          @devices << device
          counter += 1
        end
        populate_endpoint_ids
      end
      @devices
    end

    # Refresh the Device cache. This is needed if, for example a USB MIDI device is plugged in while the program is running
    # @return [Array<Device>] The Device cache
    def self.refresh
      @devices.clear
      @devices
    end

    # Has the device list been populated?
    def self.populated?
      !@devices.nil? && !@devices.empty? 
    end

    private

    # Populate the device name
    def populate_name
      @name = API.get_string(@resource, "name")
      raise RuntimeError.new("Can't get device name") unless @name
    end
    
    # All of the endpoints for all devices a consecutive local id
    def self.populate_endpoint_ids
      counter = 0
      all.each { |device| counter += device.populate_endpoint_ids(counter) }
      counter
    end

    # Populates the entities for this device. These entities are in turn used to gather the endpoints.
    # @param [Hash] options
    # @option options [Boolean] :include_offline Whether to include offline entities (default: false)
    # @return [Fixnum] The number of entities populated
    def populate_entities(options = {})
      include_if_offline = options[:include_offline] || false
      i = 0
      while !(entity_pointer = API.MIDIDeviceGetEntity(@resource, i)).null?
        @entities << Entity.new(entity_pointer, :include_offline => include_if_offline)
        i += 1
      end
      i
    end

    # Populate the instance
    def populate(options = {})
      populate_name
      populate_entities(:include_offline => options[:include_offline])
    end

  end

end
