module CoreMIDI

  # A MIDI entity can have any number of MIDI endpoints, each of which is a source or destination 
  # of a 16-channel MIDI stream. By grouping a device's endpoints into entities, the system has 
  # enough information for an application to make reasonable default assumptions about how to 
  # communicate in a bi-directional manner with each entity, as is necessary in MIDI librarian 
  # applications.
  #
  # https://developer.apple.com/library/ios/documentation/CoreMidi/Reference/MIDIServices_Reference/Reference/reference.html
  class Entity

    attr_reader :endpoints, 
                :manufacturer,
                :model,
                :name,
                :resource
                
    # @param [FFI::Pointer] resource A pointer to the underlying entity
    # @param [Hash] options
    # @option options [Boolean] :include_offline Include offline endpoints in the list
    def initialize(resource, options = {})
      @endpoints = { 
        :source => [], 
        :destination => [] 
      }
      @resource = resource
      populate(options)
    end
    
    # Assign all of this Entity's endpoints an consecutive local id
    # @param [Fixnum] starting_id
    # @return [Fixnum]
    def populate_endpoint_ids(starting_id)
      counter = 0
      @endpoints.values.flatten.each do |endpoint|
        endpoint.id = counter + starting_id
        counter += 1
      end
      counter
    end
    
    # Is the entity online?
    # @return [Boolean]
    def online?
      get_int(:offline) == 0
    end

    private

    # Construct a display name for the entity
    # @return [String]
    def get_name
      "#{@manufacturer} #{@model}"
    end
    
    # Populate endpoints of a specified type for this entity
    # @param [Symbol] type The endpoint type eg :source, :destination
    # @param [Hash] options
    # @option options [Boolean] :include_offline Include offline endpoints in the list
    # @return [Fixnum]
    def populate_endpoints_by_type(type, options = {})
      endpoint_class = Endpoint.get_class(type)
      num_endpoints = number_of_endpoints(type)
      (0..num_endpoints).each do |i|
        endpoint = endpoint_class.new(i, self)
        if endpoint.online? || options[:include_offline]
          @endpoints[type] << endpoint
        end
      end  
      @endpoints[type].size   
    end

    # Populate the endpoints for this entity
    # @param [Hash] options
    # @option options [Boolean] :include_offline Include offline endpoints in the list
    # @return [Fixnum]
    def populate_endpoints(options = {})
      @endpoints.keys.map { |type| populate_endpoints_by_type(type, options) }.reduce(&:+)
    end
    
    # The number of endpoints for this entity
    # @param [Symbol] type The endpoint type eg :source, :destination
    def number_of_endpoints(type)
      case type
        when :source then API.MIDIEntityGetNumberOfSources(@resource)
        when :destination then API.MIDIEntityGetNumberOfDestinations(@resource)
      end
    end
    
    # A CFString property from the underlying entity
    # @param [Symbol, String] name The property name
    # @return [String, nil]
    def get_string(name)
      API.get_string(@resource, name)
    end
    
    # An Integer property from the underlying entity
    # @param [Symbol, String] name The property name
    # @return [Fixnum, nil]
    def get_int(name)
      API.get_int(@resource, name)
    end        

    # Populate the entity properties from the underlying resource
    # @param [Hash] options
    # @option options [Boolean] :include_offline Include offline endpoints in the list
    def populate(options = {})
      @manufacturer = get_string(:manufacturer)
      @model = get_string(:model)
      @name = get_name
      populate_endpoints(options)
    end

  end

end
