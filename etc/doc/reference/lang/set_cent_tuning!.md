# Global Cent tuning

```
set_cent_tuning! 
 <!--- #tr -->cent_shift (number)<!--- #end tr -->
```


Globally tune Sonic Pi to play with another external instrument.

Uniformly tunes your music by shifting all notes played by the specified number of cents. To shift up by a cent use a cent tuning of 1. To shift down use negative numbers. One semitone consists of 100 cents.

See `use_cent_tuning` for setting the cent tuning value locally for a specific thread or `live_loop`. This is a global value and will shift the tuning for *all* notes. It will also persist for the entire session.

Important note: the cent tuning set by `set_cent_tuning!` is independent of any thread-local cent tuning values set by `use_cent_tuning` or `with_cent_tuning`. 

Introduced in v2.10

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
play 50
set_cent_tuning! 1
play 50



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Plays note 50
 
# Plays note 50.01



```
<!--- #end tr -->

</td>
</tr>
</table>

