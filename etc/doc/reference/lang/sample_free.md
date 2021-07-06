# Free a sample on the synth server

```
sample_free 
 <!--- #tr -->path (string)<!--- #end tr -->
```


Frees the memory and resources consumed by loading the sample on the server. Subsequent calls to `sample` and friends will re-load the sample on the server.

You may also specify the same set of source and filter pre-args available to `sample` itself. `sample_free` will then free all matching samples. See `sample`'s docs for more information.

Introduced in v2.9

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
sample :loop_amen
sleep 2
sample :loop_amen
sleep 2
sample_free :loop_amen
sample :loop_amen



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# The Amen break is now loaded into memory and played
 
# The Amen break is not loaded but played from memory
 
# The Amen break is freed from memory
# the Amen break is re-loaded and played



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
puts sample_info(:loop_amen).to_i
puts sample_info(:loop_amen).to_i
                                 
sample_free :loop_amen
puts sample_info(:loop_amen).to_i



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# This returns the buffer id of the sample i.e. 1
# The buffer id remains constant whilst the sample
# is loaded in memory
 
# The Amen break is re-loaded and gets a *new* id.



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
sample :loop_amen
sample :ambi_lunar_land
sleep 2
sample_free :loop_amen, :ambi_lunar_land
sample :loop_amen                       
sample :ambi_lunar_land                 



```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
 
 
 
# re-loads and plays amen
# re-loads and plays lunar land



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

dir = "/path/to/sample/dir"
sample_free dir
sample_free dir, 1
sample_free dir, :foo
sample_free dir, /[Bb]ar/



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Using source and filter pre-args
 
# frees any loaded samples in "/path/to/sample/dir"
# frees sample with index 1 in "/path/to/sample/dir"
# frees sample with name "foo" in "/path/to/sample/dir"
# frees sample which matches regex /[Bb]ar/ in "/path/to/sample/dir"



```
<!--- #end tr -->

</td>
</tr>
</table>

