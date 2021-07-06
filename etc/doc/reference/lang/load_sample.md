# Pre-load first matching sample

```
load_sample 
 <!--- #tr -->path (string)<!--- #end tr -->
```


Given a path to a `.wav`, `.wave`, `.aif`, `.aiff`, `.ogg`, `.oga` or `.flac` file, pre-loads the sample into memory.

You may also specify the same set of source and filter pre-args available to `sample` itself. `load_sample` will then load all matching samples. See `sample`'s docs for more information.

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
 
# loads first matching sample in "/path/to/sample/dir"
# loads sample with index 1 in "/path/to/sample/dir"
# loads sample with name "foo" in "/path/to/sample/dir"
# loads first sample with file name containing "quux" in "/path/to/sample/dir"
# loads first sample which matches regex /[Bb]ar/ in "/path/to/sample/dir"



```
<!--- #end tr -->

</td>
</tr>
</table>

