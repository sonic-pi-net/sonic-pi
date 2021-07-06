# Control main mixer

```
set_mixer_control! 
 <!--- #tr --><!--- #end tr -->
```


The main mixer is the final mixer that all sound passes through. This fn gives you control over the main mixer allowing you to manipulate all the sound playing through Sonic Pi at once. For example, you can sweep a lpf or hpf over the entire sound. You can reset the controls back to their defaults with `reset_mixer!`.

Introduced in v2.7

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
set_mixer_control! lpf: 30, lpf_slide: 16



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# slide the global lpf to 30 over 16 beats.



```
<!--- #end tr -->

</td>
</tr>
</table>

