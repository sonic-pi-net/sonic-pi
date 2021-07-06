# Kill synth

```
kill 
 <!--- #tr -->node (synth_node)<!--- #end tr -->
```


Kill a running synth sound or sample. In order to kill a sound, you need to have stored a reference to it in a variable.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby

foo = play 50, release: 4
sleep 1

kill foo


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# store a reference to a running synth in a variable called foo:
 
 
# foo is still playing, but we can kill it early:
 



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
bar = sample :loop_amen
sleep 0.5
kill bar


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

