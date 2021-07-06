# Evaluate the contents of the file as a new Run

```
run_file 
 <!--- #tr -->filename (path)<!--- #end tr -->
```


Reads the full contents of the file with `path` and executes it in a new Run. This works as if the code in the file was in a buffer and Run button was pressed.

Introduced in v2.11

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
run_file "~/path/to/sonic-pi-code.rb"



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> will run the contents of this file



```
<!--- #end tr -->

</td>
</tr>
</table>

