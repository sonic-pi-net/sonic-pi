#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification and distribution
# of modified versions of this work as long as this notice is included.
# ++

require 'socket'
require 'thread'
require_relative '../docsystem'

module SonicPi
  module Mods
    module Minecraft

      include SonicPi::DocSystem

      @minecraft_queue = nil
      @minecraft_queue_creation_lock = Mutex.new

      class MinecraftError < StandardError ; end
      class MinecraftBlockError < MinecraftError ; end
      class MinecraftConnectionError < MinecraftError ; end
      class MinecraftCommsError < MinecraftError ; end
      class MinecraftLocationError < MinecraftError ; end
      class MinecraftBlockNameError < MinecraftBlockError ; end
      class MinecraftBlockIdError < MinecraftBlockError ; end

      def self.__drain_socket(s)
        res = ""
        begin
          while d = s.recv_nonblock(1024) && !d.empty?
            res << d
          end
        rescue IO::WaitReadable
          # Do nothing, drained!
        end
        return res
      end

      def self.__socket_recv(s, m)
        __drain_socket(s)
        s.send "#{m}\n", 0
        s.recv(1024).chomp
      end

      def self.__drain_queue_and_error_proms!(q)
        while !q.empty?
          m, p = q.pop
          p.deliver! :error
        end
      end

      def self.__comms_queue
        return @minecraft_queue if @minecraft_queue

        q = nil
        socket = nil

        @minecraft_queue_creation_lock.synchronize do
          return @minecraft_queue if @minecraft_queue

          q = SizedQueue.new(10)
          begin
            socket = TCPSocket.new('localhost', 4711)
          rescue => e
            raise MinecraftConnectionError, "Unable to connect to a Minecraft server. Make sure Minecraft Pi Edition is running"
          end
          @minecraft_queue = q
        end

        Thread.new do
          cnt = 0
          loop do
            m, p = q.pop
            if p
              begin
                res = __socket_recv(socket, m)
              rescue => e
                @minecraft_queue = nil
                p.deliver! :error
                __drain_queue_and_error_proms!(q)
                socket.close
                Thread.current.kill
              end
              p.deliver! res
            else
              begin
                socket.send "#{m}\n", 0
              rescue => e
                @minecraft_queue = nil
                __drain_queue_and_error_proms!(q)
                socket.close
                Thread.current.kill
              end
            end

            #Sync with server to avoid flooding
            if (cnt+=1 % 5) == 0
              begin
                __socket_recv(socket, "player.getPos()")
              rescue => e
                @minecraft_queue = nil
                __drain_queue_and_error_proms!(q)
                socket.close
                Thread.current.kill
              end

            end
          end
        end
        return q
      end

      def self.world_send(m)
        __comms_queue << [m, nil]
        true
      end

      def self.world_recv(m)
        p = Promise.new
        __comms_queue << [m, p]
        res = p.get
        raise MinecraftCommsError, "Error communicating with server. Connection reset" if res == :error
        res
      end

      BLOCK_NAME_TO_ID = {
        :air                 => 0,
        :stone               => 1,
        :grass               => 2,
        :dirt                => 3,
        :cobblestone         => 4,
        :wood_plank          => 5,
        :sapling             => 6,
        :bedrock             => 7,
        :water_flowing       => 8,
        :water               => 8,
        :water_stationary    => 9,
        :lava_flowing        => 10,
        :lava                => 10,
        :lava_stationary     => 11,
        :sand                => 12,
        :gravel              => 13,
        :gold_ore            => 14,
        :iron_ore            => 15,
        :coal_ore            => 16,
        :wood                => 17,
        :leaves              => 18,
        :glass               => 20,
        :lapis               => 21,
        :lapis_lazuli_block  => 22,
        :sandstone           => 24,
        :bed                 => 26,
        :cobweb              => 30,
        :grass_tall          => 31,
        :flower_yellow       => 37,
        :flower_cyan         => 38,
        :mushroom_brown      => 39,
        :mushroom_red        => 40,
        :gold_block          => 41,
        :gold                => 41,
        :iron_block          => 42,
        :iron                => 42,
        :stone_slab_double   => 43,
        :stone_slab          => 44,
        :brick               => 45,
        :brick_block         => 45,
        :tnt                 => 46,
        :bookshelf           => 47,
        :moss_stone          => 48,
        :obsidian            => 49,
        :torch               => 50,
        :fire                => 51,
        :stairs_wood         => 53,
        :chest               => 54,
        :diamond_ore         => 56,
        :diamond_block       => 57,
        :diamond             => 57,
        :crafting_table      => 58,
        :farmland            => 60,
        :furnace_inactive    => 61,
        :furnace_active      => 62,
        :door_wood           => 64,
        :ladder              => 65,
        :stairs_cobblestone  => 67,
        :door_iron           => 71,
        :redstone_ore        => 73,
        :snow                => 78,
        :ice                 => 79,
        :snow_block          => 80,
        :cactus              => 81,
        :clay                => 82,
        :sugar_cane          => 83,
        :fence               => 85,
        :glowstone_block     => 89,
        :bedrock_invisible   => 95,
        :stone_brick         => 98,
        :glass_pane          => 102,
        :melon               => 103,
        :fence_gate          => 107,
        :glowing_obsidian    => 246,
        :nether_reactor_core => 247
      }

      BLOCK_ID_TO_NAME = BLOCK_NAME_TO_ID.invert
      BLOCK_IDS = BLOCK_ID_TO_NAME.keys
      BLOCK_NAMES = BLOCK_ID_TO_NAME.values

      def mc_location
        res = Minecraft.world_recv "player.getPos()"
        res = res.split(',').map { |s| s.to_f }
        raise MinecraftLocationError, "Server returned an invalid location: #{res.inspect}" unless res.size == 3
        res
      end
      doc name:           :mc_location,
          introduced:     Version.new(2,5,0),
          summary:        "Get current location",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Returns a list of floats [x, y, z] coords of the current location for Steve. The coordinates are finer grained than raw block coordinates.",
          examples:       [
        "puts mc_location    #=> [10.1, 20.67, 101.34]"   ]


      def mc_teleport(x, y, z)
        Minecraft.world_send "player.setPos(#{x.to_f}, #{y.to_f}, #{z.to_f})"
        true
      end
      doc name:           :mc_set_location,
          introduced:     Version.new(2,5,0),
          summary:        "Set current location",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "",
          examples:       []


      ## Fns matching the API names for those coming from Python
      def mc_get_pos(*args)
        mc_location(*args)
      end

      def mc_set_pos(*args)
        mc_teleport(*args)
      end

      def mc_get_tile
        res = Minecraft.world_recv "player.getTile()"
        res = res.split(',').map { |s| s.to_i }
        raise MinecraftLocationError, "Server returned an invalid location: #{res.inspect}" unless res.size == 3
        res
      end
      doc name:           :mc_get_tile,
          introduced:     Version.new(2,5,0),
          summary:        "Get location of current tile",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "",
          examples:       []


      def mc_surface_teleport(x, z)
        y = mc_get_height(x, z)
        mc_set_location(x.to_f, y, z.to_f)
        true
      end

      def mc_set_ground_pos(*args)
        mc_set_ground_location(*args)
      end

      def mc_message(msg)
        Minecraft.world_send "chat.post(#{msg})"
        msg
      end
      doc name:           :mc_message,
          introduced:     Version.new(2,5,0),
          summary:        "Display message on Minecraft",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "",
          examples:       []


      def mc_chat_post(msg)
        mc_message(msg)
      end

      def mc_get_height(x, z)
        res = Minecraft.world_recv "world.getHeight(#{x.to_i},#{z.to_i})"
        res.to_i
      end
      doc name:           :mc_get_height,
          introduced:     Version.new(2,5,0),
          summary:        "Get current height",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "",
          examples:       []


      def mc_get_block(x, y, z)
        res = Minecraft.world_recv "world.getBlock(#{x.to_i},#{y.to_i},#{z.to_i})"
        mc_block_name(res.to_i)
      end
      doc name:           :mc_get_block,
          introduced:     Version.new(2,5,0),
          summary:        "Get block type",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "",
          examples:       []

      def mc_set_block(x, y, z, block_id)
        block_id = mc_block_id(block_id)
        Minecraft.world_send "world.setBlock(#{x.to_i},#{y.to_i},#{z.to_i},#{block_id.to_i})"
        true
      end
      doc name:           :mc_set_block,
          introduced:     Version.new(2,5,0),
          summary:        "Set block at specific coord",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "",
          examples:       []


      def mc_set_area(x, y, z, x2, y2, z2, block_id)
        block_id = mc_block_id(block_id)
        Minecraft.world_send "world.setBlocks(#{x.to_i},#{y.to_i},#{z.to_i},#{x2.to_i},#{y2.to_i},#{z2.to_i},#{block_id})"
        true
      end
      doc name:           :mc_set_area,
          introduced:     Version.new(2,5,0),
          summary:        "Set area of blocks",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "",
          examples:       []


      def mc_set_tile(x, y, z)
        Minecraft.world_send "player.setPos(#{x.to_f.round}, #{y.to_i}, #{z.to_f.round})"
        true
      end

      def mc_block_id(name)
        case name
        when Symbol
          id = BLOCK_NAME_TO_ID[name]
          raise MinecraftBlockNameError, "Unknown Minecraft block name #{name.inspect}" unless id
        when Numeric
          raise MinecraftBlockIdError, "Invalid Minecraft block id #{id.inspect}" unless BLOCK_ID_TO_NAME[name]
          id = name
        else
          raise MinecraftBlockError, "Unable to convert #{name.inspect} to a block ID. Must be either a Symbol or a Numeric, got #{name.class}."
        end
        id
      end
      doc name:           :mc_block_id,
          introduced:     Version.new(2,5,0),
          summary:        "Normalise block code",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "",
          examples:       []


      def mc_block_name(id)
        case id
        when Numeric
          name = BLOCK_ID_TO_NAME[id]
          raise MinecraftBlockIdError, "Invalid Minecraft block id #{id.inspect}" unless name
        when Symbol
          raise MinecraftBlockNameError, "Unknown Minecraft block name #{id.inspect}" unless BLOCK_NAME_TO_ID[id]
          name = id
        else
          raise MinecraftBlockError, "Unable to convert #{name.inspect} to a block name. Must be either a Symbol or a Numeric, got #{name.class}."
        end
        name
      end
      doc name:           :mc_block_id,
          introduced:     Version.new(2,5,0),
          summary:        "Normalise block name",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "",
          examples:       []

      def mc_block_ids
        BLOCK_IDS
      end
      doc name:           :mc_block_ids,
          introduced:     Version.new(2,5,0),
          summary:        "List all block ids",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "",
          examples:       []


      def mc_block_names
        BLOCK_NAMES
      end
      doc name:           :mc_block_id,
          introduced:     Version.new(2,5,0),
          summary:        "List all block names",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "",
          examples:       []

      def mc_build_box(block, x, y, z, *opts)
        args_h = resolve_synth_opts_hash_or_array(opts)
        size = args_h[:size] || 1
        width = args_h[:width] || size
        height = args_h[:height] || size
        depth = args_h[:depth] || size

        mc_set_area(x, y, z, x+width, y+height, z+depth, block)
      end
      doc name:           :mc_build_box,
          introduced:     Version.new(2,5,0),
          summary:        "Build a box",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "",
          examples:       []
    end
  end
end
