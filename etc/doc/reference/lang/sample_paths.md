# Sample Pack Filter Resolution

```
sample_paths 
 <!--- #tr -->pre_args (source_and_filter_types)<!--- #end tr -->
```


Accepts the same pre-args and opts as `sample` and returns a ring of matched sample paths.

Introduced in v2.10

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
sample_paths "/path/to/samples/"



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> ring of all top-level samples in /path/to/samples



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
sample_paths "/path/to/samples/**"



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> ring of all nested samples in /path/to/samples



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
sample_paths "/path/to/samples/", "foo"
                                                containing the string "foo" in their filename.


```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> ring of all samples in /path/to/samples
 



```
<!--- #end tr -->

</td>
</tr>
</table>

