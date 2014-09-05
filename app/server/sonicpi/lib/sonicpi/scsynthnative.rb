#/usr/env ruby

#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative "incomingosc"
require 'osc-ruby'
require 'ffi'

module SonicPi
  module SCSynthFFI
    class SoundBuffer < FFI::Struct
      layout :samplerate, :double,
             :sampledur, :double,
             :data, :pointer,
             :channels, :int,
             :samples, :int,
             :frames, :int,
             :mask, :int,
             :mask1, :int,
             :coord, :int,
             :sndfile, :pointer,
             :isLocal, :int
    end

    class SoundBufferUpdates < FFI::Struct
      layout :reads, :int,
             :writes, :int
    end

    class Rate < FFI::Struct
      layout mSampleRate:       :double,
             mSampleDur:        :double,
             mBufDuration:      :double,
             mBufRate:          :double,
             mSlopeFactor:      :double,
             mRadiansPerSample: :double,
             mBufLength:        :int,
             mFilterLoops:      :int,
             mFilterRemain:     :int,
             mFilterSlope:      :double
    end

    class World < FFI::Struct
      layout :hidden_world,             :pointer,
             :interface_table,          :pointer,
             :sample_rate,              :double,
             :buf_length,               :int,
             :buf_counter,              :int,
             :num_audio_bus_channels,   :uint32,
             :num_control_bus_channels, :uint32,
             :num_inputs,               :uint32,
             :num_outputs,              :uint32,
             :audio_busses,             :pointer,
             :control_busses,           :pointer,
             :audio_bus_touched,        :pointer,
             :control_bus_touched,      :pointer,
             :num_snd_bufs,             :uint32,
             :snd_bufs,                 SoundBuffer.ptr,
             :snd_bufs_non_realtime,    SoundBuffer.ptr,
             :snd_buf_updates,          SoundBufferUpdates.ptr,
             :top_group,                :pointer,
             :full_rate,                Rate,
             :buf_rate ,                Rate,
             :num_rgens,                :uint32,
             :rgen,                     :pointer,
             :num_units,                :uint32,
             :num_graphs,               :uint32,
             :num_groups,               :uint32,
             :sample_offset,            :int,
             :nrt_lock,                 :pointer,
             :num_shared_controls,      :uint32,
             :shared_controls,          :pointer,
             :real_time?,               :bool,
             :running?,                 :bool,
             :dump_osc,                 :int,
             :driver_lock,              :pointer,
             :subsample_offset,         :float,
             :verbosity,                :int,
             :error_notification,       :int,
             :local_error_notificaiton, :int,
             :rendezvous?,              :bool,
             :restricted_path,          :pointer
    end


    class WorldOptions < FFI::Struct

      STRING_THINGS = [
        :mPassword,
        :mNonRealTimeCmdFilename,
        :mNonRealTimeInputFilename,
        :mNonRealTimeOutputFilename,
        :mNonRealTimeOutputHeaderFormat,
        :mNonRealTimeOutputSampleFormat,
        :mInputStreamsEnabled,
        :mOutputStreamsEnabled,
        :mInDeviceName,
        :mUGensPluginPath,
        :mOutDeviceName,
        :mRestrictedPath]

      DEFAULTS = {
        mPassword: "",
        mNumBuffers: 1024,
        mMaxLogins: 64,
        mMaxNodes: 1024,
        mMaxGraphDefs: 1024,
        mMaxWireBufs: 64,
        mNumAudioBusChannels: 512,
        mNumInputBusChannels: 8,
        mNumOutputBusChannels: 8,
        mNumControlBusChannels: 4096,
        mBufLength: 64,
        mRealTimeMemorySize: 262144,
        mNumSharedControls: 0,
        mSharedControls: 0,
        mRealTime: true,
        mMemoryLocking: false,
        mNonRealTimeCmdFilename: "",
        mNonRealTimeInputFilename: "",
        mNonRealTimeOutputFilename: "",
        mNonRealTimeOutputHeaderFormat: "",
        mNonRealTimeOutputSampleFormat: "",
        mPreferredSampleRate: 0,
        mNumRGens: 64,
        mPreferredHardwareBufferFrameSize: 0,
        mLoadGraphDefs: 1,
        mInputStreamsEnabled: "",
        mOutputStreamsEnabled: "",
        mInDeviceName: "",
        mVerbosity: -1,
        mRendezvous: false,
        mUGensPluginPath: "/Users/sam/scratch/meta-ex/target/native/macosx/x86_64/",
        mOutDeviceName: "",
        mRestrictedPath: "",
        mSharedMemoryID: 0
      }


             #When using TCP, the session passowrd must be the first command
             #sent
      layout :mPassword,                         :pointer,

             #Number of sample buffers
             :mNumBuffers,                       :uint32,

             #Maximum number of named return addresses stored - also maximum
             #number of TCP connections accepted.
             :mMaxLogins,                        :uint32,

             #Max number of executing nodes allowed in the server
             :mMaxNodes,                         :uint32,

             #Max number of synthdefs allowed
             :mMaxGraphDefs,                     :uint32,

             #Max number of wire buffers
             :mMaxWireBufs,                      :uint32,

             #Number of audio bus channels
             :mNumAudioBusChannels,              :uint32,

             #Number of input bus channels
             :mNumInputBusChannels,              :uint32,

             #Number of output bus channels
             :mNumOutputBusChannels,             :uint32,

             #Number of control bus channels
             :mNumControlBusChannels,            :uint32,

             #
             :mBufLength,                        :uint32,

             # Real time memory size
             :mRealTimeMemorySize,               :uint32,

             #
             :mNumSharedControls,                :int,

             #
             :mSharedControls,                   :pointer,

             #Run in realtime mode? If false, then other nrt flags must be
             #set.
             :mRealTime,                         :bool,
             :mMemoryLocking,                    :bool,
             :mNonRealTimeCmdFilename,           :pointer,
             :mNonRealTimeInputFilename,         :pointer,
             :mNonRealTimeOutputFilename,        :pointer,
             :mNonRealTimeOutputHeaderFormat,    :pointer,
             :mNonRealTimeOutputSampleFormat,    :pointer,

             #
             :mPreferredSampleRate,              :uint32,

             #Number of random seeds
             :mNumRGens,                         :uint32,

             #
             :mPreferredHardwareBufferFrameSize, :uint32,

             # Load synthdefs on boot? 0 or 1
             :mLoadGraphDefs,                    :uint32,

             #
             :mInputStreamsEnabled,              :pointer,
             :mOutputStreamsEnabled,             :pointer,

             # Hardware device name
             :mInDeviceName,                     :pointer,

             # Verbosity mode. 0 is normal behaviour, -1 surppress
             # information messages, -2 suppresses informational and many
             # error messages.
             :mVerbosity,                        :int,

             # Publish to rendezvous? 0 or 1
             :mRendezvous,                       :bool,

             # Path to directory of compiled ugens
             :mUGensPluginPath,                  :pointer,

             #
             :mOutDeviceName,                    :pointer,
             :mRestrictedPath,                   :pointer,
             :mSharedMemoryID,                   :int

      def self.mk_options(opts_h={})
        opts = DEFAULTS.merge(opts_h)
        opts_p = self.new
        opts.each do |k,v|
          if STRING_THINGS.include? k
            v_s = v.to_s
            if v_s.empty?
              opts_p[k] = 0#FFI::MemoryPointer::NULL
            else
              opts_p[k] = FFI::MemoryPointer.from_string(v_s)
            end
          else
            opts_p[k] = v
          end
        end
        opts_p
      end
    end

    extend FFI::Library
    ffi_lib '/Users/sam/Development/RPi/sonic-pi/app/server/native/macosx/libscsynth.dylib'
    callback :reply_callback, [:pointer, :pointer, :int], :void
    attach_function :World_New, [WorldOptions.ptr ], World.ptr
    attach_function :World_WaitForQuit, [World.ptr], :void, :blocking => true
    attach_function :World_SendPacket, [World.ptr, :int, :pointer, :reply_callback], :int
    attach_function :World_Cleanup, [World.ptr], :void
    attach_function :World_OpenUDP, [World.ptr, :int], :int
  end



  class SCSynthNative

    #typedef void (*ReplyFunc)(struct ReplyAddress *inReplyAddr, char* inBuf, int inSize);
    def initialize(opts={}, &callback)
      @world_p = SCSynthFFI.World_New(SCSynthFFI::WorldOptions.mk_options)
      SCSynthFFI.World_OpenUDP(@world_p, 4556)

      @cb = FFI::Function.new( :void, [:pointer, :pointer, :int]) do | a, b, c |
        begin
          s = b.read_bytes(c)
          m = IncomingOSC.new(s)
          callback.call(m.address, m.args)
        rescue Exception => e
          puts "Exeption in FFI function:"
          puts e.message
          puts e.backtrace
        end
      end

      @wait_thread = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :scsynth_native_server)
        SCSynthFFI.World_WaitForQuit(@world_p)
      end

      puts "scsynthnative initialised"
    end


    def shutdown
      send "/quit"
      @wait_thread.kill
    end

    def wait_for_quit
      @wait_thread.join
    end

    def send(*args)
      data = OSC::Message.new(*args).encode
      send_binary data
    end

    def send_at(ts, *args)
      m = OSC::Message.new(*args)
      data = OSC::Bundle.new(ts, m).encode
      send_binary data
    end

    private

    def send_binary(data)
      size = data.size
      mem_buf = FFI::MemoryPointer.new(:char, size)
      mem_buf.put_bytes(0, data)

      SCSynthFFI.World_SendPacket(@world_p, size, mem_buf, @cb)

    end
  end

end
