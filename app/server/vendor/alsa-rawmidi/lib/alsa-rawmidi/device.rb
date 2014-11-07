module AlsaRawMIDI

  # Functionality common to both inputs and outputs
  module Device

    module ClassMethods

      # Select the first device of the given direction
      # @param [Symbol] direction
      # @return [Input, Output]
      def first(direction)
        all_by_type[direction].first
      end

      # Select the last device of the given direction
      # @param [Symbol] direction
      # @return [Input, Output]
      def last(direction)
        all_by_type[direction].last
      end

      # A hash of devices, partitioned by direction
      # @return [Hash]
      def all_by_type
        @devices ||= get_devices
      end

      # All devices
      # @return [Array<Input, Output>]
      def all
        all_by_type.values.flatten
      end

      private

      # Get all available devices from the system
      # @return [Hash]
      def get_devices
        available_devices = {
          :input => [],
          :output => []
        }
        device_count = 0
        32.times do |i|
          card = Soundcard.find(i)
          unless card.nil?
            available_devices.keys.each do |direction|
              devices = card.subdevices[direction]
              devices.each do |dev|
                dev.send(:id=, device_count)
                device_count += 1
              end
              available_devices[direction] += devices
            end
          end
        end
        available_devices
      end

    end

    extend ClassMethods

    attr_reader :enabled, # has the device been initialized?
    :system_id, # the alsa id of the device
    :id, # a local uuid for the device
    :name,
    :subname,
    :type # :input or :output

    alias_method :enabled?, :enabled

    def self.included(base)
      base.send(:extend, ClassMethods)
    end

    # @param [Hash] options
    # @option options [Fixnum] :id
    # @option options [String] :name
    # @option options [String] :subname
    # @option options [String] :system_id
    def initialize(options = {})
      @id = options[:id]
      @name = options[:name]
      @subname = options[:subname]
      @system_id = options[:system_id]
      @type = get_type
      @enabled = false
    end

    private

    # Assign an id
    # @param [Fixnum] id
    # @return [Fixnum]
    def id=(id)
      @id = id
    end

    # Get the device type
    # @return [Symbol]
    def get_type
      self.class.name.split('::').last.downcase.to_sym
    end

  end

end
