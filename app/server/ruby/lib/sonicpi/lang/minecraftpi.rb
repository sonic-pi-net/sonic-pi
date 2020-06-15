#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification and distribution
# of modified versions of this work as long as this notice is included.
# ++

require 'socket'
require 'thread'
require_relative 'support/docsystem'

module SonicPi
  module Lang
    module Minecraft

      include SonicPi::Lang::Support::DocSystem

      @minecraft_queue = nil
      @minecraft_queue_creation_lock = Mutex.new

      class MinecraftError < StandardError ; end
      class MinecraftBlockError < MinecraftError ; end
      class MinecraftConnectionError < MinecraftError ; end
      class MinecraftCommsError < MinecraftError ; end
      class MinecraftLocationError < MinecraftError ; end
      class MinecraftBlockNameError < MinecraftBlockError ; end
      class MinecraftBlockIdError < MinecraftBlockError ; end
      class MinecraftBlockTypeIdError < MinecraftBlockError ; end

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
            socket = TCPSocket.new('127.0.0.1', 4711)
          rescue => e
            raise MinecraftConnectionError, "Unable to connect to a Minecraft server. Make sure Minecraft Pi Edition is running"
          end
          @minecraft_queue = q
        end

        Thread.new do
          cnt = 0
          Kernel.loop do
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
          summary:        "Minecraft Pi - get current location",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Returns a list of floats `[x, y, z]` coords of the current location for Steve. The coordinates are finer grained than raw block coordinates but may be used anywhere you might use block coords.",
          examples:       [
        "puts mc_location    #=> [10.1, 20.67, 101.34]",
"x, y, z = mc_location       #=> Find the current location and store in x, y and z variables."      ]




      def mc_teleport(x, y, z)
        Minecraft.world_send "player.setPos(#{x.to_f}, #{y.to_f}, #{z.to_f})"
        true
      end
      doc name:           :mc_teleport,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - teleport to a new location",
          args:           [[:x, :number], [:y, :number], [:z, :number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Magically teleport the player to the location specified by the `x`, `y`, `z` coordinates. Use this for automatically moving the player either small or large distances around the world.",
         examples: ["
mc_teleport 40, 50, 60  # The player will be moved to the position with coords:
                        # x: 40, y: 50, z: 60

        "]




      ## Fns matching the API names for those coming from Python
      def mc_get_pos(*args)
        mc_location(*args)
      end
      doc name:           :mc_get_pos,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - synonym for mc_location",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "See `mc_location`",
          examples:       []




      def mc_set_pos(*args)
        mc_teleport(*args)
      end
      doc name:           :mc_set_pos,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - synonym for mc_teleport",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "See `mc_teleport`",
          examples:       []




      def mc_set_tile(x, y, z)
        Minecraft.world_send "player.setTile(#{x.to_f.round}, #{y.to_i}, #{z.to_f.round})"
        true
      end
      doc name:           :mc_set_tile,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - set location to coords of specified tile/block",
          args:           [[:x, :number], [:y, :number], [:z, :number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "",
          examples:       [""]




      def mc_get_tile
        res = Minecraft.world_recv "player.getTile()"
        res = res.split(',').map { |s| s.to_i }
        raise MinecraftLocationError, "Server returned an invalid location: #{res.inspect}" unless res.size == 3
        res
      end
      doc name:           :mc_get_tile,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - get location of current tile/block",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Returns the coordinates of the nearest block that the player is next to. This is more course grained than `mc_location` as it only returns whole number coordinates.",
          examples:       ["puts mc_get_tile #=> [10, 20, 101]"]




      def mc_surface_teleport(x, z)
        y = mc_get_height(x, z)
        mc_teleport(x.to_f, y, z.to_f)
        true
      end
      doc name:           :mc_surface_teleport,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - teleport to world surface at x and z coords",
          args:           [[:x, :number], [:z, :number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Teleports you to the specified x and z coordinates with the y automatically set to place you on the surface of the world. For example, if the x and z coords target a mountain, you'll be placed on top of the mountain, not in the air or under the ground. See mc_ground_height for discovering the height of the ground at a given x, z point.",
          examples:       ["mc_surface_teleport 40, 50 #=> Teleport user to coords x = 40, y = height of surface, z = 50"]




      def mc_message(*msgs)
        msg = msgs.map{|m| m.to_s}.join(" ")
        Minecraft.world_send "chat.post(#{msg})"
        msg
      end
      doc name:           :mc_message,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - post a chat message",
          args:           [[:msg, :string]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Post contents of `msg` on the Minecraft chat display. You may pass multiple arguments and all will be joined to form a single message (with spaces).",
          examples:       ["mc_message \"Hello from Sonic Pi\" #=> Displays \"Hello from Sonic Pi\" on Minecraft's chat display" ]




      def mc_chat_post(msg)
        mc_message(msg)
      end
      doc name:           :mc_chat_post,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - synonym for mc_message",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "See mc_message",
          examples:       []




      def mc_ground_height(x, z)
        res = Minecraft.world_recv "world.getHeight(#{x.to_f.round},#{z.to_f.round})"
        res.to_i
      end
      doc name:           :mc_ground_height,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - get ground height at x, z coords",
          args:           [[:x, :number], [:z, :number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Returns the height of the ground at the specified `x` and `z` coords.",
          examples:       ["puts mc_ground_height 40, 50 #=> 43 (height of world at x=40, z=50)"]




      def mc_get_height(x, z)
        res = Minecraft.world_recv "world.getHeight(#{x.to_f.round},#{z.to_f.round})"
        res.to_i
      end
      doc name:           :mc_get_height,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - synonym for mc_ground_height",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "See `mc_ground_height`",
          examples:       []




      def mc_get_block(x, y, z)
        res = Minecraft.world_recv "world.getBlock(#{x.to_f.round},#{y.to_i},#{z.to_f.round})"
        mc_block_name(res.to_i)
      end
      doc name:           :mc_get_block,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - get type of block at coords",
          args:           [[:x, :number], [:y, :number], [:z, :number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Returns the type of the block at the coords `x`, `y`, `z` as a symbol.",
          examples:       ["puts mc_get_block 40, 50, 60 #=> :air"]




      def mc_set_block(block_name, x, y, z, *opts)
        opts_h = resolve_synth_opts_hash_or_array(opts)
        block_id = mc_block_id(block_name)
        type = opts_h[:type]
        if type
          begin
            type_id = type.to_i
          rescue Exception
            raise MinecraftBlockTypeIdError, "Invalid block type. Must be a number in the range 0-15, got #{type}."
          end
          raise MinecraftBlockTypeIdError, "Invalid block type. Must be a number in the range 0-15, got #{type_id}." if (type_id < 0 || type_id > 15)
          Minecraft.world_send "world.setBlock(#{x.to_f.round},#{y.to_i},#{z.to_f.round},#{block_id},#{type_id})"
        else
          Minecraft.world_send "world.setBlock(#{x.to_f.round},#{y.to_i},#{z.to_f.round},#{block_id})"
        end
        true
      end
      doc name:           :mc_set_block,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - set block at specific coord",
          args:           [[:x, :number], [:y, :number], [:z, :number], [:block_name, :symbol_or_number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Change the block type of the block at coords `x`, `y`, `z` to `block_type`. The block type may be specified either as a symbol such as `:air` or a number. See `mc_block_ids` and `mc_block_types` for lists of valid symbols and numbers.",
          examples:       ["mc_set_block :glass, 40, 50, 60 #=> set block at coords 40, 50, 60 to type glass"]




      def mc_set_area(block_name, x, y, z, x2, y2, z2, *opts)
        opts_h = resolve_synth_opts_hash_or_array(opts)
        block_id = mc_block_id(block_name)
        type = opts_h[:type]
        if type
          begin
            type_id = type.to_i
          rescue Exception
            raise MinecraftBlockTypeIdError, "Invalid block type. Must be a number in the range 0-15, got #{type}."
          end
          raise MinecraftBlockTypeIdError, "Invalid block type. Must be a number in the range 0-15, got #{type_id}." if (type_id < 0 || type_id > 15)
          Minecraft.world_send "world.setBlocks(#{x.to_f.round},#{y.to_i},#{z.to_f.round},#{x2.to_f.round},#{y2.to_i},#{z2.to_f.round},#{block_id},#{type_id})"
        else
          Minecraft.world_send "world.setBlocks(#{x.to_f.round},#{y.to_i},#{z.to_f.round},#{x2.to_f.round},#{y2.to_i},#{z2.to_f.round},#{block_id})"
        end
        true
      end
      doc name:           :mc_set_area,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - set area of blocks",
          args:           [[:block_name, :symbol_or_number], [:x, :number], [:y, :number], [:z, :number], [:x2, :number], [:y2, :number], [:z2, :number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Set an area/box of blocks of type `block_name` defined by two distinct sets of coordinates.",
          examples:       []




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
          summary:        "Minecraft Pi - normalise block code",
          args:           [[:name, :symbol_or_number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Given a block name or id will return a number representing the id of the block or throw an exception if the name or id isn't valid",
          examples:       ["
puts mc_block_id :air #=> 0",
"puts mc_block_id 0  #=> 0",
"puts mc_block_id 19 #=> Throws an invalid block id exception",
"puts mc_block_id :foo #=> Throws an invalid block name exception"      ]




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
      doc name:           :mc_block_name,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - normalise block name",
          args:           [[:id, :number_or_symbol]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Given a block id or a block name will return a symbol representing the block name or throw an exception if the id or name isn't valid.",
      examples:       ["
puts mc_block_name :air #=> :air",
"puts mc_block_name 0   #=> :air",
"puts mc_block_name 19 #=> Throws an invalid block id exception",
"puts mc_block_name :foo #=> Throws an invalid block name exception"]




      def mc_block_ids
        BLOCK_IDS
      end
      doc name:           :mc_block_ids,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - list all block ids",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Returns a list of all the valid block ids as numbers. Note not all numbers are valid block ids. For example, 19 is not a valid block id.",
          examples:       ["puts mc_block_ids #=> [0, 1, 2, 3, 4, 5... "]




      def mc_block_names
        BLOCK_NAMES
      end
      doc name:           :mc_block_names,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - list all block names",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Returns a list of all the valid block names as symbols",
          examples:       ["puts mc_block_names #=> [:air, :stone, :grass, :dirt, :cobblestone... "]




      def mc_checkpoint_save
        Minecraft.world_send "world.checkpoint.save()"
      end
      doc name:           :mc_checkpoint_save,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - save checkpoint",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Take a snapshot of the world and save it. Restore back with `mc_checkpoint_restore`",
          examples:       [""]




      def mc_checkpoint_restore
        Minecraft.world_send "world.checkpoint.restore()"
      end
      doc name:           :mc_checkpoint_restore,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - restore checkpoint",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Restore the world to the last snapshot taken with `mc_checkpoint_save`.",
          examples:       [""]




      def mc_camera_normal
        Minecraft.world_send "camera.mode.setNormal()"
      end
      doc name:           :mc_camera_normal,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - normal camera mode",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Set the camera mode to normal.",
          examples:       [""]




      def mc_camera_third_person
        Minecraft.world_send "camera.mode.setThirdPerson()"
      end
      doc name:           :mc_camera_third_person,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - third person camera mode",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Set the camera mode to third person",
          examples:       [""]




      def mc_camera_fixed
        Minecraft.world_send "camera.mode.setFixed()"
      end
      doc name:           :mc_camera_fixed,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - fixed camera mode",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Set the camera mode to fixed.",
          examples:       [""]




      def mc_camera_set_location(x, y, z)
        Minecraft.world_send "camera.mode.setPos(#{x.to_i}, #{y.to_i}, #{z.to_i})"
      end
      doc name:           :mc_camera_set_location,
          introduced:     Version.new(2,5,0),
          summary:        "Minecraft Pi - move camera",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Move the camera to a new location.",
          examples:       [""]


      # def mc_build_box(block, x, y, z, *opts)
      #   args_h = resolve_synth_opts_hash_or_array(opts)
      #   size = args_h[:size] || 1
      #   width = args_h[:width] || size
      #   height = args_h[:height] || size
      #   depth = args_h[:depth] || size

      #   mc_set_area(x, y, z, x+width, y+height, z+depth, block)
      # end
      # doc name:           :mc_build_box,
      #     introduced:     Version.new(2,5,0),
      #     summary:        "Build a box",
      #     args:           [[]],
      #     opts:           nil,
      #     accepts_block:  false,
      #     doc:            "",
      #     examples:       []

    end
  end
end
