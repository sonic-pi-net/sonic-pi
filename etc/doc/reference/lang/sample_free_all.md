# Free all loaded samples on the synth server

```
sample_free_all 
 <!--- #tr --><!--- #end tr -->
```


Unloads all samples therefore freeing the memory and resources consumed. Subsequent calls to `sample` and friends will re-load the sample on the server.

Introduced in v2.9

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
sample :loop_amen       
sample :ambi_lunar_land 
sleep 2
sample_free_all
sample :loop_amen       



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# load and play :loop_amen
# load and play :ambi_lunar_land
 
 
# re-loads and plays amen



```
<!--- #end tr -->

</td>
</tr>
</table>

