# Globally modify audio latency

```
set_audio_latency! 
 <!--- #tr -->milliseconds (number)<!--- #end tr -->
```


On some systems with certain configurations (such as wireless speakers, and even a typical Windows environment with the default audio drivers) the audio latency can be large. If all the user is doing is generating audio via calls such as `play`, `synth` and `sample`, then this latency essentially adds to the schedule ahead time and for the most part can be ignored. However, if the user is combining audio with external MIDI/OSC triggered events, this latency can result in a noticeable offset. This function allows you to address this offset by moving the audio events forwards and backwards in time.

So, for example, if your audio system has an audio latency of 150ms, you can compensate for this by setting Sonic Pi's latency to be a negative value: `set_audio_latency! -150`.

Introduced in v3.1

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
set_audio_latency! 100
                                                 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Audio events will now be scheduled 100ms
# after the schedule ahead time



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 2 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
set_audio_latency! -200
                                                 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Audio events will now be scheduled 200ms
# before the schedule ahead time



```
<!--- #end tr -->

</td>
</tr>
</table>

