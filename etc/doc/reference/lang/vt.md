# Get virtual time

```
vt 
 <!--- #tr --><!--- #end tr -->
```


Get the virtual time of the current thread.

Introduced in v2.1

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts vt
   sleep 1
   puts vt



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# prints 0
 
# prints 1



```
<!--- #end tr -->

</td>
</tr>
</table>

