# Sliding Parameters

Whilst exploring the synth and FX args, you might have noticed that there are a number of parameters ending with `_slide`. You might have even tried calling them and seeing no effect. This is because they're not normal parameters, they're special parameters that only work when you control synths as introduced in the previous section.

Consider the following example:


```
s = play 60, release: 5
sleep 0.5
control s, note: 65
sleep 0.5
control s, note: 67
sleep 3
control s, note: 72
```

Here, hear the synth pitch changing immediately on each control. However, we might want the pitch to slide between changes. As we're controlling the `note:` parameter, to add slide, we need to set the `note_slide` parameter:

```
s = play 60, release: 5, note_slide: 1
sleep 0.5
control s, note: 65
sleep 0.5
control s, note: 67
sleep 3
control s, note: 72
```

Now we hear the notes being bent between controls. It sounds nice doesn't it. You can speed up the sliding by using a shorter time such as `note_slide: 0.2` or slow it down by using a longer slide time. 

Every parameter that can be controlled has a corresponding `_slide` parameter for you to play with.

# Sliding is sticky

Once you've set a `_slide` parameter on a running synth, it will be remembered and used every time you slide the corresponding parameter. To stop sliding you must set the `_slide` value to 0 before the next control.

# Sliding FX Parameters

It is also possible to slide FX parameters:

```
with_fx :wobble, phase: 1, phase_slide: 5 do |e|
  use_synth :dsaw
  play 50, release: 5
  control e, phase: 0.025
end
```

Now have fun sliding things around for smooth transitions and flowing control...
