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

module SonicPi
  module Mods
    module Minecraft
      @minecraft_queue = nil
      @minecraft_queue_creation_lock = Mutex.new

      def self.__drain_socket(s)
        res = ""
        begin
          while d = s.recv_nonblock(1024)
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

      def self.__comms_queue
        return @minecraft_queue if @minecraft_queue

        @minecraft_queue_creation_lock.synchronize do
          return @minecraft_queue if @minecraft_queue

          q = SizedQueue.new(10)
          socket = TCPSocket.new('localhost', 4711)
          @minecraft_queue = q
          Thread.new do
            cnt = 0
            loop do
              m, p = q.pop
              if p
                p.deliver! __socket_recv(socket, m)
              else
                socket.send "#{m}\n", 0
              end
              __socket_recv(socket, "player.getPos()") if (cnt+=1 % 5) == 0
            end
          end
        end
        return @minecraft_queue
      end

      def self.world_send(m)
        __comms_queue << [m, nil]
      end

      def self.world_recv(m)
        p = Promise.new
        __comms_queue << [m, p]
        p.get
      end



      BLOCKS_TO_ID = {
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

      IDS_TO_BLOCK = BLOCKS_TO_ID.invert



      def minecraft_location
        res = Minecraft.world_recv "player.getPos()"
        res.split(',').map { |s| s.to_f }
      end

      def minecraft_get_pos
        minecraft_location
      end

      def minecraft_set_location(x, y=nil, z=nil)
        if x.is_a? Array
          x, y, z = x
        end
#        __delayed do
          Minecraft.world_send "player.setPos(#{x.to_f}, #{y.to_f}, #{z.to_f})"
#        end
      end

      def minecraft_set_pos(*args)
        minecraft_set_location(*args)
      end

      def minecraft_set_location_sync(x, y=nil, z=nil)
        if x.is_a? Array
          x, y, z = x
        end
        Minecraft.world_send "player.setPos(#{x.to_f}, #{y.to_f}, #{z.to_f})"
      end

      def minecraft_set_pos_sync(*args)
        minecraft_set_pos_sync(*args)
      end

      def minecraft_set_ground_location(x, z=nil)
        if x.is_a? Array
          if x.size == 3
            a = x
            x = a[0]
            z = a[1]
          else
            a, z = a
          end
        end

        y = minecraft_get_height(x, z)
        minecraft_set_location(x.to_f, y, z.to_f)
      end

      def minecraft_set_ground_pos(*args)
        minecraft_set_ground_location(*args)
      end

      def minecraft_set_ground_location_sync(x, z=nil)
        minecraft_set_ground_location(x, z)
      end

      def minecraft_set_ground_pos_sync(*args)
        minecraft_set_ground_location_sync(*args)
      end

      def minecraft_message(msg)
        Minecraft.world_send "chat.post(#{msg})"
      end

      def minecraft_chat_post(msg)
        minecraft_message(msg)
      end

      def minecraft_message_sync(msg)
        Minecraft.world_send "chat.post(#{msg})"
      end

      def minecraft_chat_post_sync(msg)
        minecraft_message_sync(msg)
      end

      def minecraft_get_height(x, z)
        res = Minecraft.world_recv "world.getHeight(#{x.to_i},#{z.to_i})"
        res.to_i
      end

      def minecraft_get_block(x, y=nil, z=nil)
        if x.is_a? Array
          x, y, z = x
        end
        res = Minecraft.world_recv "world.getBlock(#{x.to_i},#{y.to_i},#{z.to_i})"
        IDS_TO_BLOCK[res.to_i]
      end

      def minecraft_set_block(x, y, z=nil, block_id=nil)
        if x.is_a? Array
          block_id = y
          x, y, z = x
        end
        if block_id.is_a? Symbol
          block_id = BLOCKS_TO_ID[block_id]
        end
#        __delayed do
          Minecraft.world_send "world.setBlock(#{x.to_i},#{y.to_i},#{z.to_i},#{block_id.to_i})"
#        end
      end

      def minecraft_set_block_sync(x, y, z=nil, block_id=nil)
        if x.is_a? Array
          block_id = y
          x, y, z = x
        end
        if block_id.is_a? Symbol
          block_id = BLOCKS_TO_ID[block_id]
        end
        Minecraft.world_send "world.setBlock(#{x.to_i},#{y.to_i},#{z.to_i},#{block_id.to_i})"
      end

      def minecraft_set_area(x, y, z, x2=nil, y2=nil, z2=nil, block_id=nil)
        if x.is_a? Array
          block_id = z
          a1 = x
          a2 = y
          x, y, z = a1
          x2, y2, z2, = a2
        end

        if block_id.is_a? Symbol
          block_id = BLOCKS_TO_ID[block_id]
        end
        Minecraft.world_send "world.setBlocks(#{x.to_i},#{y.to_i},#{z.to_i},#{x2.to_i},#{y2.to_i},#{z2.to_i},#{block_id.to_i})"
      end

      def minecraft_set_area_sync(x, y, z, x2=nil, y2=nil, z2=nil, block_id=nil)
        if x.is_a? Array
          block_id = z
          a1 = x
          a2 = y
          x, y, z = a1
          x2, y2, z2, = a2
        end

        if block_id.is_a? Symbol
          block_id = BLOCKS_TO_ID[block_id]
        end
        Minecraft.world_send "world.setBlocks(#{x.to_i},#{y.to_i},#{z.to_i},#{x2.to_i},#{y2.to_i},#{z2.to_i},#{block_id.to_i})"
      end

      def minecraft_set_tile(x, y=nil, z=nil)
        if x.is_a? Array
          x, y, z = x
        end
        Minecraft.world_send "player.setPos(#{x.to_i}, #{y.to_i}, #{z.to_i})"
      end

      def minecraft_set_tile_sync(x, y=nil, z=nil)
        if x.is_a? Array
          block_id = y
          x, y, z = y
        end
        Minecraft.world_send "player.setPos(#{x.to_i}, #{y.to_i}, #{z.to_i})"
      end

      def minecraft_location
        minecraft_get_tile
      end

      def minecraft_get_tile
        res = Minecraft.world_recv "player.getTile()"
        res.split(',').map { |s| s.to_i }
      end

    end
  end

end
