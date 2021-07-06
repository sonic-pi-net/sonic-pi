# Set Omni/Mono/Poly mode

```
midi_mode 
 <!--- #tr -->mode (mode_keyword)<!--- #end tr -->
```


Sends the Omni/Mono/Poly MIDI mode message to *all* connected MIDI devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

Valid modes are:

:omni_off - Omni Mode Off
:omni_on  - Omni Mode On
:mono     - Mono Mode On (Poly Off). Set num_chans: to be the number of channels to use (Omni Off) or 0 (Omni On). Default for num_chans: is 16.
:poly     - Poly Mode On (Mono Off)

Note that this fn also includes the behaviour of `midi_all_notes_off`.

[MIDI 1.0 Specification - Channel Mode Messages - Omni Mode Off | Omni Mode On | Mono Mode On (Poly Off) | Poly Mode On](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)


Introduced in v3.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
midi_mode :omni_on



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Turn Omni Mode On on all ports and channels



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
midi_mode :mono, num_chans: 5



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Mono Mode On, Omni off using 5 channels.



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
midi_mode :mono, num_chans: 0



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> Mono Mode On, Omni on.



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
midi_mode :mono



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> Mono Mode On, Omni off using 16 channels (the default) .



```
<!--- #end tr -->

</td>
</tr>
</table>

