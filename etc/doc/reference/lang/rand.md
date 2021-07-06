# Generate a random float below a value

```
rand 
 <!--- #tr -->max (number_or_range)<!--- #end tr -->
```


Given a max number, produces a float between `0` and the supplied max value. If max is a range, produces a float within the range. With no args returns a random value between `0` and `1`.

Introduced in v2.0

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
print rand(0.5)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> will print a number like 0.375030517578125 to the output pane



```
<!--- #end tr -->

</td>
</tr>
</table>

