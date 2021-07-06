# Send MIDI program change message

```
midi_pc 
 <!--- #tr -->program_num (midi)<!--- #end tr -->
```


Sends a MIDI program change message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

Program number can be passed as a note such as `:e3` and decimal values will be rounded down or up to the nearest whole number - so values between 3.5 and 4 will be rounded up to 4 and values between 3.49999... and 3 will be rounded down to 3.

[MIDI 1.0 Specification - Channel Voice Messages - Program change](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)


Introduced in v3.0.2

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_pc 100 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Sends MIDI pc message to all ports and channels



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
midi_pc :e7 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Sends MIDI pc message to all ports and channels



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
midi_pc 100, channel: 5 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Sends MIDI pc message on channel 5 to all ports



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
midi_pc 100, channel: 5 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Sends MIDI pc message on channel 5 to all ports



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
midi_pc 100, channel: [1, 5] 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Sends MIDI pc message on channel 1 and 5 to all ports



```
<!--- #end tr -->

</td>
</tr>
</table>

