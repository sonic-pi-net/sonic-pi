module CoreMIDI

  # A MIDI source or destination, owned by an entity.
  module Endpoint
    
    extend Forwardable

    attr_reader :enabled, # has the endpoint been initialized?
                :entity, # unique local Numeric id of the endpoint
                :id,
                :resource_id, # :input or :output
                :type
                
    def_delegators :entity, :manufacturer, :model, :name

    alias_method :enabled?, :enabled

    # @param [Fixnum] resource_id
    # @param [Entity] entity
    def initialize(resource_id, entity)
      @entity = entity
      @resource_id = resource_id
      @type = get_type
      @enabled = false
    end
    
    # Is this endpoint online?
    # @return [Boolean]
    def online?
      @entity.online? && connect?
    end
    
    # Set the id for this endpoint (the id is immutable)
    # @param [Fixnum] val
    # @return [Fixnum]
    def id=(id)
      @id ||= id
    end

    # Select the first endpoint of the specified type
    # @return [Destination, Source]
    def self.first(type)
      all_by_type[type].first
    end

    # Select the last endpoint of the specified type
    # @return [Destination, Source]
    def self.last(type)
      all_by_type[type].last
    end

    # All source endpoints
    # @return [Array<Source>]
    def self.sources
      Device.all.map { |d| d.endpoints[:source] }.flatten
    end

    # All destination endpoints
    # @return [Array<Destination>]
    def self.destinations
      Device.all.map { |d| d.endpoints[:destination] }.flatten
    end

    # A Hash of :source and :destination endpoints
    # @return [Hash]
    def self.all_by_type
      {
        :source => sources,
        :destination => destinations
      }
    end

    # All endpoints of both types
    # @return [Array<Destination, Source>]
    def self.all
      Device.all.map(&:endpoints).flatten
    end

    # Get the class for the given endpoint type name
    # @param [Symbol] type The endpoint type eg :source, :destination
    # @return [Class] eg Source, Destination
    def self.get_class(type)
      case type
      when :source then Source
      when :destination then Destination
      end  
    end
    
    protected

    # Constructs the endpoint type (eg source, destination) for easy consumption
    def get_type
      class_name = self.class.name.split('::').last
      class_name.downcase.to_sym
    end
    
    # Enables the coremidi MIDI client that will go with this endpoint
    def enable_client
      client = API.create_midi_client(@resource_id, @name)
      @client = client[:resource]
      client[:error]
    end

  end

end
