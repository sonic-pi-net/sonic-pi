# Pre-load all matching samples

```
load_samples 
 <!--- #tr -->paths (list)<!--- #end tr -->
```


Given a directory containing multiple `.wav`, `.wave`, `.aif`, `.aiff`, `.ogg`, `.oga` or `.flac` files, pre-loads all the samples into memory.

 You may also specify the same set of source and filter pre-args available to `sample` itself. `load_sample` will load all matching samples (not just the sample `sample` would play given the same opts) - see `sample`'s docs for more information.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
load_sample :elec_blip
 sample :elec_blip



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# :elec_blip is now loaded and ready to play as a sample
# No delay takes place when attempting to trigger it



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

 dir = "/path/to/sample/dir"
 load_sample dir
 load_sample dir, 1
 load_sample dir, :foo
 load_sample dir, "quux"
 load_sample dir, /[Bb]ar/



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Using source and filter pre-args
 
# loads all samples in "/path/to/sample/dir"
# loads sample with index 1 in "/path/to/sample/dir"
# loads sample with name "foo" in "/path/to/sample/dir"
# loads all samples with file names containing "quux" in "/path/to/sample/dir"
# loads all samples which match regex /[Bb]ar/ in "/path/to/sample/dir"



```
<!--- #end tr -->

</td>
</tr>
</table>

