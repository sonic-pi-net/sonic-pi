# Send raw MIDI message

```
midi_raw 
 <!--- #tr --><!--- #end tr -->
```


Sends the raw MIDI message to *all* connected MIDI devices. Gives you direct access to sending the individual bytes of a MIDI message. Typically this should be rarely used - prefer the other `midi_` fns where possible.

A raw MIDI message consists of multiple bytes as numbers in decimal notation (i.e. 176), hex (0xb0) or binary (0b10110000).

See https://www.midi.org/specifications/item/table-1-summary-of-midi-message for a summary of MIDI messages and their corresponding byte structures.


Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_raw 176, 121, 0 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Sends the MIDI reset command



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
midi_raw 176.1, 120.5, 0.49 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Sends the MIDI reset command (values are rounded down, up and down respectively)



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
midi_raw 0xb0, 0x79, 0x0 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Sends the MIDI reset command



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
midi_raw 0b10110000, 0b01111001, 0b00000000 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Sends the MIDI reset command



```
<!--- #end tr -->

</td>
</tr>
</table>

