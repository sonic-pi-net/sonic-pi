# Optionally evaluate block

```
on 
 <!--- #tr -->condition (truthy)<!--- #end tr -->
```


Optionally evaluate the block depending on the truthiness of the supplied condition. The truthiness rules are as follows: all values are seen as true except for: false, nil and 0. Lambdas will be automatically called and the truthiness of their results used.

Introduced in v2.10

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
on true do
  play 70    
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
#=> will play 70 as true is truthy
 



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
on 1 do
  play 70    
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
#=> will play 70 as 1 is truthy
 



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
on 0 do
  play 70    
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
#=> will *not* play 70 as 0 is not truthy
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 4 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
on false do
  play 70    
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
#=> will *not* play 70 as false is not truthy
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 5 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
on nil do
  play 70    
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
#=> will *not* play 70 as nil is not truthy
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 6 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
on lambda{true} do
  play 70    
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
#=> will play 70 as the lambda returns a truthy value
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="even head"># Example 7 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
on lambda{false} do
  play 70    
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
 
#=> will *not* play 70 as the lambda does not return a truthy value
 



```
<!--- #end tr -->

</td>
</tr>
<tr>
<th colspan="2" class="odd head"># Example 8 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="odd">

```ruby
on lambda{[true, false].choose} do
  play 70    
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
 
#=> will maybe play 70 depending on the choice in the lambda
 



```
<!--- #end tr -->

</td>
</tr>
</table>

