# Block level commenting

```
comment 
 <!--- #tr --><!--- #end tr -->
```


Does not evaluate any of the code within the block. However, any optional args passed before the block *will* be evaluated although they will be ignored. See `uncomment` for switching commenting off without having to remove the comment form.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
comment do
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
# not played
# no sleep happens
# not played
 



```
<!--- #end tr -->

</td>
</tr>
</table>

