# Send MIDI pitch bend message

```
midi_pitch_bend 
 <!--- #tr -->delta (float01)<!--- #end tr -->
```


Sends a MIDI pitch bend message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

Delta value is between 0 and 1 with 0.5 representing no pitch bend, 1 max pitch bend and 0 minimum pitch bend.

Typical MIDI values such as note or cc are represented with 7 bit numbers which translates to the range 0-127. This makes sense for keyboards which have at most 88 keys. However, it translates to a poor resolution when working with pitch bend. Therefore, pitch bend is unlike most MIDI values in that it has a much greater range: 0 - 16383 (by virtue of being represented by 14 bits).

* It is also possible to specify the delta value as a (14 bit) MIDI pitch bend value between 0 and 16383 using the `delta_midi:` opt.
* When using the `delta_midi:` opt no pitch bend is the value 8192

[MIDI 1.0 Specification - Channel Voice Messages - Pitch Bend Change](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)


Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_pitch_bend 0 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Sends MIDI pitch bend message with value 0 to all ports and channels



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
midi_pitch_bend 1 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Sends MIDI pitch bend message with value 16383 to all ports and channels



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 3 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_pitch_bend 0.5 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Sends MIDI pitch bend message with value 8192 to all ports and channels



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 4 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
midi_pitch_bend delta_midi: 8192 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Sends MIDI pitch bend message with value 8192 to all ports and channels



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 5 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_pitch_bend 0, channel: [1, 5] 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Sends MIDI pitch bend message with value 0 on channel 1 and 5 to all ports



```
<!--- #end tr -->

</td>
</tr>
</table>

