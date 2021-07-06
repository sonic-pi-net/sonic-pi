# Block-level merge synth defaults

```
with_merged_synth_defaults 
 <!--- #tr --><!--- #end tr -->
```


Specify synth arg values to be used by any following call to play within the specified `do`/`end` block. Merges the specified values with any previous synth defaults, rather than replacing them. After the `do`/`end` block has completed, previous defaults (if any) are restored.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
with_merged_synth_defaults amp: 0.5, pan: 1 do
  play 50
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
# => plays note 50 with amp 0.5 and pan 1
 



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
play 50
with_merged_synth_defaults amp: 0.5 do
  play 50

  with_merged_synth_defaults pan: -1 do
    with_merged_synth_defaults amp: 0.7 do
      play 50
    end
  end
  play 50
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> plays note 50
 
#=> plays note 50 with amp 0.5
 
 
 
#=> plays note 50 with amp 0.7 and pan -1
 
 
#=> plays note 50 with amp 0.5
 



```
<!--- #end tr -->

</td>
</tr>
</table>

