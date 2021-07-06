# Block level comment ignoring

```
uncomment 
 <!--- #tr --><!--- #end tr -->
```


Evaluates all of the code within the block. Use to reverse the effect of the comment without having to explicitly remove it.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
uncomment do
    play 50
    sleep 1
    play 62
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# starting a block level comment:
# played
# sleep happens
# played
 



```
<!--- #end tr -->

</td>
</tr>
</table>

