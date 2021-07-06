# Display a message in the output pane

```
print 
 <!--- #tr -->output (anything)<!--- #end tr -->
```


Displays the information you specify as a string inside the output pane. This can be a number, symbol, or a string itself. Useful for debugging. Synonym for `puts`.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
print "hello there"  



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> will print the string "hello there" to the output pane



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
print 5              



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> will print the number 5 to the output pane



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
print foo            



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> will print the contents of foo to the output pane



```
<!--- #end tr -->

</td>
</tr>
</table>

