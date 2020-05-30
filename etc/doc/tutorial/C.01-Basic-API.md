C.1 Basic API

# Basic Minecraft Pi API

Sonic Pi currently supports the following basic interactions with Minecraft Pi:

* Displaying chat messages
* Setting the position of the user
* Getting the position of the user
* Setting the block type at a given coordinate
* Getting the block type at a given coordinate


Let's look at each of these in turn.

## Displaying chat messages

Let's see just how easy it is to control Minecraft Pi from Sonic
Pi. First, make sure you have both Minecraft Pi and Sonic Pi open at the
same time and also make sure you've entered a Minecraft world and can
walk around.

In a fresh Sonic Pi buffer simply enter the following code:

```
mc_message "Hello from Sonic Pi"
```

When you hit the *Run* button, you'll see your message flash up on the
Minecraft window. Congratulations, you've written your first Minecraft
code! That was easy wasn't it.

## Setting the position of the user

Now, let's try a little magic. Let's teleport ourselves somewhere! Try
the following:

```
mc_teleport 50, 50, 50
```

When you hit *Run* - boom! You're instantly transported to a new
place. Most likely it was somewhere in the sky and you fell down either
to dry land or into water. Now, what are those numbers: `50, 50, 50`?
They're the coordinates of the location you're trying to teleport
to. Let's take a brief moment to explore what coordinates are and how
they work because they're really, really important for programming
Minecraft.

## Coordinates

Imagine a pirate's map with a big `X` marking the location of some
treasure. The exact location of the `X` can be described with two
numbers - how far along the map from left to right and how far along the
map from bottom to top. For example `10cm` across and `8cm` up. These
two numbers `10` and `8` are coordinates. You could easily imagine
describing the locations of other stashes of treasure with other pairs
of numbers. Perhaps there's a big chest of gold at `2` across and `9`
up...

Now, in Minecraft two numbers isn't quite enough. We also need to know
how high we are. We therefore need three numbers:

* How far from right to left in the world - `x`
* How far from front to back in the world - `z`
* How high up we are in the world - `y`

One more thing - we typically describe these coordinates in this order
`x`, `y`, `z`.

## Finding your current coordinates

Let's have a play with coordinates. Navigate to a nice place in the
Minecraft map and then switch over to Sonic Pi. Now enter the following:

```
puts mc_location
```

When you hit the *Run* button you'll see the coordinates of your current
position displayed in the log window. Take a note of them, then move
forward in the world and try again. Notice how the coordinates changed!
Now, I recommend you spend some time repeating exactly this - move a bit
in the world, take a look at the coordinates and repeat. Do this until
you start to get a feel for how the coordinates change when you
move. Once you've understood how coordinates work, programming with the
Minecraft API will be a complete breeze.

## Let's Build!

Now that you know how to find the current position and to teleport using
coordinates, you have all the tools you need to start building things in
Minecraft with code. Let's say you want to make the block with
coordinates `40`, `50`, `60` to be glass. That's super easy:

```
mc_set_block :glass, 40, 50, 60
```

Haha, it really was that easy. To see your handywork just teleport
nearby and take a look:

```
mc_teleport 35, 50, 60
```

Now turn around and you should see your glass block! Try changing it to
diamond:

```
mc_set_block :diamond, 40, 50, 60
```

If you were looking in the right direction you might have even seen it
change in front of your eyes! This is the start of something exciting...

## Looking at blocks

Let's look at one last thing before we move onto something a bit more
involved. Given a set of coordinates we can ask Minecraft what the type
of a specific block is. Let's try it with the diamond block you just
created:

```
puts mc_get_block 40, 50, 60
```

Yey! It's `:diamond`. Try changing it back to glass and asking again -
does it now say `:glass`? I'm sure it does :-)

## Available block types

Before you go on a Minecraft Pi coding rampage, you might find this list
of available block types useful:

        :air
        :stone
        :grass
        :dirt
        :cobblestone
        :wood_plank
        :sapling
        :bedrock
        :water_flowing
        :water
        :water_stationary
        :lava_flowing
        :lava
        :lava_stationary
        :sand
        :gravel
        :gold_ore
        :iron_ore
        :coal_ore
        :wood
        :leaves
        :glass
        :lapis
        :lapis_lazuli_block
        :sandstone
        :bed
        :cobweb
        :grass_tall
        :flower_yellow
        :flower_cyan
        :mushroom_brown
        :mushroom_red
        :gold_block
        :gold
        :iron_block
        :iron
        :stone_slab_double
        :stone_slab
        :brick
        :brick_block
        :tnt
        :bookshelf
        :moss_stone
        :obsidian
        :torch
        :fire
        :stairs_wood
        :chest
        :diamond_ore
        :diamond_block
        :diamond
        :crafting_table
        :farmland
        :furnace_inactive
        :furnace_active
        :door_wood
        :ladder
        :stairs_cobblestone
        :door_iron
        :redstone_ore
        :snow
        :ice
        :snow_block
        :cactus
        :clay
        :sugar_cane
        :fence
        :glowstone_block
        :bedrock_invisible
        :stone_brick
        :glass_pane
        :melon
        :fence_gate
        :glowing_obsidian
        :nether_reactor_core
