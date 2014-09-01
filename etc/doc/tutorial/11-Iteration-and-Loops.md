# Iteration and Loops

So far we've spent a lot of time looking at the different sounds you can make with `play`, `sample` and `with_fx` blocks. We've also learned how to trigger these things through time using `sleep`.

As you've probably found out, there's a *lot* of fun you can have with these basic building blocks. However, a whole new dimension of fun opens up when you start using the power of code to structure your music and compositions. In the next few sections we'll explore some of this powerful new tools. First up is iteration and loops.

## Repetition

Have you written some code you'd like to repeat a few times. For example, you might have something like this:

```
play 50
sleep 0.5
sample :elec_blup
sleep 0.5
play 62
sleep 0.25
```

What if we wished to repeat this 3 times? Well we could do something simple and just copy and paste it three times:

```
play 50
sleep 0.5
sample :elec_blup
sleep 0.5
play 62
sleep 0.25
play 50
sleep 0.5
sample :elec_blup
sleep 0.5
play 62
sleep 0.25
play 50
sleep 0.5
sample :elec_blup
sleep 0.5
play 62
sleep 0.25
```

Now that's a lot of code! What happens if you want to change sample to `:elec_plip`? You're going to have to find all the places with the original `:elec_blup` and switch them over. More importantly, what if you wanted to repeat the original piece of code 50 times or 1000? Now that would be a lot of code and a lot of lines of code to alter to make if you wanted to make a change.

## Iteration

What repeating the code as as easy as saying *do this three times*. Well, it pretty much is. Remember our old friend the code block that we met in the previous section on FX? We can use it to mark the start and of the code we'd like to repeat three times. We then use the special code `3.times`. So, instead of writing do this three times, we write `3.times do` - that's not too hard. Just remember to write `end` and the end of the code you'd like to repeat:

```
3.times do
  play 50
  sleep 0.5
  sample :elec_blup
  sleep 0.5
  play 62
  sleep 0.25
end
```

Now isn't that much neater than cutting and pasting! We can use this to create lots of nice repeating structures:

```
4.times do
  play 50
  sleep 0.5
end

8.times do
  play 55, release: 0.2
  sleep 0.25
end

4.times do
  play 50
  sleep 0.5
end
```

## Nesting Iterations

Just like nesting FX, we can put iterations inside other iterations to create interesting patterns. For example:

```
4.times do
  sample :drum_heavy_kick
  2.times do
    sample :elec_blip2, rate: 2
    sleep 0.25
  end
  sample :elec_snare
  4.times do
    sample :drum_tom_mid_soft
    sleep 0.125
  end
end
```

## Looping

If you want something to repeat a lot of times, you might find yourself using really large numbers such as `1000.times do`. In this case, you're probably better off asking Sonic Pi to repeat forever (at least until you press the stop button!). Let's loop the amen break forever:

```
loop do
  sample :loop_amen
  sleep sample_duration :loop_amen
end
```

The important thing to know about loops is that they act like black holes for code. Once the code enters a loop it can never leave until you press stop - it will just go round and round the loop forever. This means if you have code after the loop you will *never* hear it. For example, the cymbal will never play:

```
loop do
  play 50
  sleep 1
end

sample :drum_cymbal_open
```

Now, get structuring your code with iteration and loops!



