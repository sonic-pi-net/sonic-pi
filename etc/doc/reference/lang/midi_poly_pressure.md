# Send a MIDI polyphonic key pressure message

```
midi_poly_pressure 
 <!--- #tr -->note (midi), value (midi)<!--- #end tr -->
```


Sends a MIDI polyphonic key pressure message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

Note number and pressure value can be passed as a note such as `:e3` and decimal values will be rounded down or up to the nearest whole number - so values between 3.5 and 4 will be rounded up to 4 and values between 3.49999... and 3 will be rounded down to 3.

You may also optionally pass the pressure value as a floating point value between 0 and 1 such as 0.2 or 0.785 (which will be mapped to MIDI values between 0 and 127) using the `val_f:` opt.

[MIDI 1.0 Specification - Channel Voice Messages - Polyphonic Key Pressure (Aftertouch)](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)


Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_poly_pressure 100, 32 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Sends a MIDI poly key pressure message to control note 100 with value 32 to all ports and channels



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
midi_poly_pressure :e7, 32 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Sends a MIDI poly key pressure message to control note 100 with value 32 to all ports and channels



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
midi_poly_pressure 100, 32, channel: 5 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Sends MIDI poly key pressure message to control note 100 with value 32 on channel 5 to all ports



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
midi_poly_pressure 100, val_f: 0.8, channel: 5 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Sends a MIDI poly key pressure message to control note 100 with value 102 on channel 5 to all ports



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
midi_poly_pressure 100, value: 102, channel: [1, 5] 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Sends MIDI poly key pressure message to control note 100 with value 102 on channel 1 and 5 to all ports



```
<!--- #end tr -->

</td>
</tr>
</table>

