# Euclidean distribution for beats

```
spread 
 <!--- #tr -->num_accents (number), size (number)<!--- #end tr -->
```


Creates a new ring of boolean values which space a given number of accents as evenly as possible throughout a bar. This is an implementation of the process described in 'The Euclidean Algorithm Generates Traditional Musical Rhythms' (Toussaint 2005).

Introduced in v2.4

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
(spread 3, 8)   



```

</td>
<td class="even">

<!--- #tr -->
```ruby
#=> (ring true, false, false, true, false, false, true, false) a spacing of 332



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
(spread 3, 8, rotate: 1)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
#=> (ring true, false, false, true, false, true, false, false) a spacing of 323



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

  live_loop :euclid_beat do
    sample :elec_bong, amp: 1.5 if (spread 3, 8).tick
    sample :perc_snap, amp: 0.8 if (spread 7, 11).look
    sample :bd_haus, amp: 2 if (spread 1, 4).look
    sleep 0.125
  end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Easily create interesting polyrhythmic beats
 
# Spread 3 bongs over 8
# Spread 7 snaps over 11
# Spread 1 bd over 4
 
 



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

 
  (spread 2, 5) 

  (spread 3, 4) 
                

  (spread 3, 5) 
                
                

  (spread 3, 7) 

  (spread 3, 8) 

  (spread 4, 7) 

  (spread 4, 9) 

  (spread 4, 11)

  (spread 5, 6) 
                

  (spread 5, 7) 

  (spread 5, 8) 

  (spread 5, 9) 

  (spread 5, 11)
                

  (spread 5, 12)
                

  (spread 5, 16)

  (spread 7, 8) 

  (spread 7, 12)

  (spread 7, 16)

  (spread 9, 16)

  (spread 11, 24)

  (spread 13, 24)
                 



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Spread descriptions from
# 'The Euclidean Algorithm Generates Traditional Musical Rhythms' (Toussaint 2005).
# A thirteenth century Persian rhythm called Khafif-e-ramal.
 
# The archetypal pattern of the Cumbria from Columbia, as well
# as a Calypso rhythm from Trinidad
 
# When started on the second onset, is another thirteenth
# century Persian rhythm by the name of Khafif-e-ramal, as well
# as a Romanian folk-dance rhythm.
 
# A ruchenitza rhythm used in a Bulgarian folk-dance.
 
# The Cuban tresillo pattern
 
# Another Ruchenitza Bulgarian folk-dance rhythm
 
# The Aksak rhythm of Turkey.
 
# The metric pattern used by Frank Zappa in his piece Outside Now
 
# Yields the York-Samai pattern, a popular Arab rhythm, when
# started on the second onset.
 
# The Nawakhat pattern, another popular Arab rhythm.
 
# The Cuban cinquillo pattern.
 
# A popular Arab rhythm called Agsag-Samai.
 
# The metric pattern used by Moussorgsky in Pictures at an
# Exhibition
 
# The Venda clapping pattern of a South African children's
# song.
 
# The Bossa-Nova rhythm necklace of Brazil.
 
# A typical rhythm played on the Bendir (frame drum)
 
# A common West African bell pattern.
 
# A Samba rhythm necklace from Brazil.
 
# A rhythm necklace used in the Central African Republic.
 
# A rhythm necklace of the Aka Pygmies of Central Africa.
 
# Another rhythm necklace of the Aka Pygmies of the upper
# Sangha.



```
<!--- #end tr -->

</td>
</tr>
</table>

