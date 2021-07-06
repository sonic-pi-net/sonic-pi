# Return information about the internal SuperCollider sound server

```
scsynth_info 
 <!--- #tr --><!--- #end tr -->
```


Create a map of information about the running audio synthesiser SuperCollider. 

Introduced in v2.11

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts scsynth_info 
                           
                           
                           
                           
                           
                           
                           
                           
                           
                           



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=>  (map sample_rate: 44100.0,
#         sample_dur: 2.2675736545352265e-05,
#         radians_per_sample: 0.00014247585204429924,
#         control_rate: 689.0625,
#         control_dur: 0.001451247138902545,
#         subsample_offset: 0.0,
#         num_output_busses: 16.0,
#         num_input_busses: 16.0,
#         num_audio_busses: 1024.0,
#         num_control_busses: 4096.0,
#         num_buffers: 4096.0)



```
<!--- #end tr -->

</td>
</tr>
</table>

