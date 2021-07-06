# Load the contents of a file to the current buffer

```
load_buffer 
 <!--- #tr -->path (string)<!--- #end tr -->
```


Given a path to a file, will read the contents and load it into the current buffer. This will replace any previous content.

Introduced in v2.10

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
load_buffer "~/sonic-pi-tracks/phat-beats.rb"



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# will replace content of current buffer with contents of the file



```
<!--- #end tr -->

</td>
</tr>
</table>

