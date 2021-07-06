# Set control delta globally

```
set_control_delta! 
 <!--- #tr -->time (number)<!--- #end tr -->
```


Specify how many seconds between successive modifications (i.e. trigger then controls) of a specific node on a specific thread. Set larger if you are missing control messages sent extremely close together in time.

Introduced in v2.1

## Example

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
set_control_delta! 0.1                

s = play 70, release: 8, note_slide: 8
control s, note: 82                   
                                      
                                      
                                      
                                      
                                      



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Set control delta to 0.1
 
# Play a note and set the slide time
# immediately start sliding note.
# This control message might not be
# correctly handled as it is sent at the
# same virtual time as the trigger.
# If you don't hear a slide, try increasing the
# control delta until you do.



```
<!--- #end tr -->

</td>
</tr>
</table>

