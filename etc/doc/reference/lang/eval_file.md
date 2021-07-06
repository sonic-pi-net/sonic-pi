# Evaluate the contents of the file inline in the current thread like a function.

```
eval_file 
 <!--- #tr -->filename (path)<!--- #end tr -->
```


Reads the full contents of the file with `path` and executes within the current thread like a function call.

Introduced in v3.2

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
eval_file "~/path/to/sonic-pi-code.rb"



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

