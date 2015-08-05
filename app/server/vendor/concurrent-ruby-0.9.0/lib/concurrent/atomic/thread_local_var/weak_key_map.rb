#  Copyright (c) 2013 Brian Durand
#  
#  Permission is hereby granted, free of charge, to any person obtaining
#  a copy of this software and associated documentation files (the
#  "Software"), to deal in the Software without restriction, including
#  without limitation the rights to use, copy, modify, merge, publish,
#  distribute, sublicense, and/or sell copies of the Software, and to
#  permit persons to whom the Software is furnished to do so, subject to
#  the following conditions:
#  
#  The above copyright notice and this permission notice shall be
#  included in all copies or substantial portions of the Software.
#  
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
#  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
#  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
#  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

module Concurrent

  # @!visibility private
  class AbstractThreadLocalVar

    begin
      require 'weakref'

      # @!visibility private 
      class WeakReference

        # The object id of the object being referenced.
        attr_reader :referenced_object_id

        # This implementation of a weak reference simply wraps the standard WeakRef implementation
        # that comes with the Ruby standard library.
        def initialize(obj)
          @referenced_object_id = obj.__id__
          @ref = ::WeakRef.new(obj)
        end

        def object
          @ref.__getobj__
        rescue => e
          # Jruby implementation uses RefError while MRI uses WeakRef::RefError
          if (defined?(RefError) && e.is_a?(RefError)) || (defined?(::WeakRef::RefError) && e.is_a?(::WeakRef::RefError))
            nil
          else
            raise e
          end
        end
      end

    rescue LoadError

      require 'monitor'

      # This is a pure ruby implementation of a weak reference. It is much more
      # efficient than the WeakRef implementation bundled in MRI 1.8 and 1.9
      # subclass Delegator which is very heavy to instantiate and utilizes a
      # because it does not fair amount of memory under Ruby 1.8.
      #
      # @!visibility private 
      class WeakReference

        # The object id of the object being referenced.
        attr_reader :referenced_object_id

        # @!visibility private 
        class ReferencePointer
          def initialize(object)
            @referenced_object_id = object.__id__
            add_backreference(object)
          end

          def cleanup
            obj = ObjectSpace._id2ref(@referenced_object_id) rescue nil
            remove_backreference(obj) if obj
          end

          def object
            obj = ObjectSpace._id2ref(@referenced_object_id)
            obj if verify_backreferences(obj)
          rescue RangeError
            nil
          end

          private

          # Verify that the object is the same one originally set for the weak reference.
          def verify_backreferences(obj)
            return nil unless supports_backreference?(obj)
            backreferences = obj.instance_variable_get(:@__weak_backreferences__) if obj.instance_variable_defined?(:@__weak_backreferences__)
            backreferences && backreferences.include?(object_id)
          end

          # Add a backreference to the object.
          def add_backreference(obj)
            return unless supports_backreference?(obj)
            backreferences = obj.instance_variable_get(:@__weak_backreferences__) if obj.instance_variable_defined?(:@__weak_backreferences__)
            unless backreferences
              backreferences = []
              obj.instance_variable_set(:@__weak_backreferences__, backreferences)
            end
            backreferences << object_id
          end

          # Remove backreferences from the object.
          def remove_backreference(obj)
            return unless supports_backreference?(obj)
            backreferences = obj.instance_variable_get(:@__weak_backreferences__) if obj.instance_variable_defined?(:@__weak_backreferences__)
            if backreferences
              backreferences.dup.delete(object_id)
              obj.send(:remove_instance_variable, :@__weak_backreferences__) if backreferences.empty?
            end
          end

          def supports_backreference?(obj)
            obj.respond_to?(:instance_variable_get) && obj.respond_to?(:instance_variable_defined?)
          rescue NoMethodError
            false
          end
        end

        private_constant :ReferencePointer

        @@weak_references = {}
        @@lock = Monitor.new

        # Finalizer that cleans up weak references when references are destroyed.
        @@reference_finalizer = lambda do |object_id|
          @@lock.synchronize do
            reference_pointer = @@weak_references.delete(object_id)
            reference_pointer.cleanup if reference_pointer
          end
        end

        # Create a new weak reference to an object. The existence of the weak reference
        # will not prevent the garbage collector from reclaiming the referenced object.
        def initialize(obj)
          @referenced_object_id = obj.__id__
          @@lock.synchronize do
            @reference_pointer = ReferencePointer.new(obj)
            @@weak_references[self.object_id] = @reference_pointer
          end
          ObjectSpace.define_finalizer(self, @@reference_finalizer)
        end

        # Get the reference object. If the object has already been garbage collected,
        # then this method will return nil.
        def object
          if @reference_pointer
            obj = @reference_pointer.object
            unless obj
              @@lock.synchronize do
                @@weak_references.delete(object_id)
                @reference_pointer.cleanup
                @reference_pointer = nil
              end
            end
            obj
          end
        end
      end

      private_constant :WeakReference
    end

    # The classes behave similar to Hashes, but the keys in the map are not strong references
    # and can be reclaimed by the garbage collector at any time. When a key is reclaimed, the
    # map entry will be removed.
    #
    # @!visibility private 
    class WeakKeyMap

      # Create a new map. Values added to the hash will be cleaned up by the garbage
      # collector if there are no other reference except in the map.
      def initialize
        @values = {}
        @references_to_keys_map = {}
        @lock = Monitor.new
        @reference_cleanup = lambda{|object_id| remove_reference_to(object_id)}
      end

      # Get a value from the map by key. If the value has been reclaimed by the garbage
      # collector, this will return nil.
      def [](key)
        @lock.synchronize do
          rkey = ref_key(key)
          @values[rkey] if rkey
        end
      end

      # Add a key/value to the map.
      def []=(key, value)
        ObjectSpace.define_finalizer(key, @reference_cleanup)
        @lock.synchronize do
          @references_to_keys_map[key.__id__] = WeakReference.new(key)
          @values[key.__id__] = value
        end
      end

      # Remove the value associated with the key from the map.
      def delete(key)
        @lock.synchronize do
          rkey = ref_key(key)
          if rkey
            @references_to_keys_map.delete(rkey)
            @values.delete(rkey)
          else
            nil
          end
        end
      end

      # Get an array of keys that have not yet been garbage collected.
      def keys
        @values.keys.collect{|rkey| @references_to_keys_map[rkey].object}.compact
      end

      private

      def ref_key (key)
        ref = @references_to_keys_map[key.__id__]
        if ref && ref.object
          ref.referenced_object_id
        else
          nil
        end
      end
    end

    private_constant :WeakKeyMap
  end
end
