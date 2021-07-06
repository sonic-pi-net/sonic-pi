# Create scale

```
scale 
 <!--- #tr -->tonic (symbol), name (symbol)<!--- #end tr -->
```


Creates a ring of MIDI note numbers when given a tonic note and a scale name. Also takes an optional `num_octaves:` parameter (octave `1` is the default). If only passed the scale name, the tonic defaults to 0. See examples.

Introduced in v2.0

## Examples

<table class="examples">
<tr>
<th colspan="2" class="even head"># Example 1 ──────────────────────────────────────────────────────</th>
</tr>
<tr>
<td class="even">

```ruby
puts (scale :C, :major)



```

</td>
<td class="even">

<!--- #tr -->
```ruby
# returns the following ring of MIDI note numbers: (ring 60, 62, 64, 65, 67, 69, 71, 72)



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

play_pattern (scale :C, :major)


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# anywhere you can use a list or ring of notes, you can also use scale
 



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

play_pattern (scale :C, :major, num_octaves: 2)


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# you can use the :num_octaves parameter to get more notes
 



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

puts (scale 50, :minor)
puts (scale 50.1, :minor)
puts (scale :minor)



```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# Scales can start with any note:
#=> (ring 50, 52, 53, 55, 57, 58, 60, 62)
#=> (ring 50.1, 52.1, 53.1, 55.1, 57.1, 58.1, 60.1, 62.1)
#=> (ring 0, 2, 3, 5, 7, 8, 10, 12)



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

live_loop :scale_player do
  play (scale :Eb3, :super_locrian).tick, release: 0.1
  sleep 0.125
end


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# scales are also rings
 
 
 
 



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

live_loop :scaled_sample do
  sample :bass_trance_c, rpitch: (scale 0, :minor).tick
  sleep 1
end


```

</td>
<td class="odd">

<!--- #tr -->
```ruby
# scales starting with 0 are useful in combination with sample's rpitch:
 
 
 
 



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


(scale :C, :diatonic)
(scale :C, :ionian)
(scale :C, :major)
(scale :C, :dorian)
(scale :C, :phrygian)
(scale :C, :lydian)
(scale :C, :mixolydian)
(scale :C, :aeolian)
(scale :C, :minor)
(scale :C, :locrian)
(scale :C, :hex_major6)
(scale :C, :hex_dorian)
(scale :C, :hex_phrygian)
(scale :C, :hex_major7)
(scale :C, :hex_sus)
(scale :C, :hex_aeolian)
(scale :C, :minor_pentatonic)
(scale :C, :yu)
(scale :C, :major_pentatonic)
(scale :C, :gong)
(scale :C, :egyptian)
(scale :C, :shang)
(scale :C, :jiao)
(scale :C, :zhi)
(scale :C, :ritusen)
(scale :C, :whole_tone)
(scale :C, :whole)
(scale :C, :chromatic)
(scale :C, :harmonic_minor)
(scale :C, :melodic_minor_asc)
(scale :C, :hungarian_minor)
(scale :C, :octatonic)
(scale :C, :messiaen1)
(scale :C, :messiaen2)
(scale :C, :messiaen3)
(scale :C, :messiaen4)
(scale :C, :messiaen5)
(scale :C, :messiaen6)
(scale :C, :messiaen7)
(scale :C, :super_locrian)
(scale :C, :hirajoshi)
(scale :C, :kumoi)
(scale :C, :neapolitan_major)
(scale :C, :bartok)
(scale :C, :bhairav)
(scale :C, :locrian_major)
(scale :C, :ahirbhairav)
(scale :C, :enigmatic)
(scale :C, :neapolitan_minor)
(scale :C, :pelog)
(scale :C, :augmented2)
(scale :C, :scriabin)
(scale :C, :harmonic_major)
(scale :C, :melodic_minor_desc)
(scale :C, :romanian_minor)
(scale :C, :hindu)
(scale :C, :iwato)
(scale :C, :melodic_minor)
(scale :C, :diminished2)
(scale :C, :marva)
(scale :C, :melodic_major)
(scale :C, :indian)
(scale :C, :spanish)
(scale :C, :prometheus)
(scale :C, :diminished)
(scale :C, :todi)
(scale :C, :leading_whole)
(scale :C, :augmented)
(scale :C, :purvi)
(scale :C, :chinese)
(scale :C, :lydian_minor)
(scale :C, :blues_major)
(scale :C, :blues_minor)
(scale :C, :cargah)
(scale :C, :buselik)
(scale :C, :buselik_2)
(scale :C, :kurdi)
(scale :C, :rast)
(scale :C, :acemli_rast)
(scale :C, :ussak)
(scale :C, :bayati)
(scale :C, :bayati_2)
(scale :C, :isfahan)
(scale :C, :isfahan_2)
(scale :C, :hicaz_humayun)
(scale :C, :hicaz_humayun_2)
(scale :C, :hicaz)
(scale :C, :hicaz_2)
(scale :C, :uzzal)
(scale :C, :uzzal_2)
(scale :C, :zirguleli_hicaz)
(scale :C, :zirguleli_hicaz_2)
(scale :C, :huseyni)
(scale :C, :huseyni_2)
(scale :C, :muhayyer)
(scale :C, :gulizar)
(scale :C, :neva)
(scale :C, :neva_2)
(scale :C, :tahir)
(scale :C, :tahir_2)
(scale :C, :karcigar)
(scale :C, :suznak)
(scale :C, :suznak_2)
(scale :C, :mahur)
(scale :C, :acem_asiran)
(scale :C, :nihavend)
(scale :C, :nihavend_2)
(scale :C, :sultani_yegah)
(scale :C, :sultani_yegah_2)
(scale :C, :kurdili_hicazkar)
(scale :C, :kurdili_hicazkar_2)
(scale :C, :kurdili_hicazkar_3)
(scale :C, :kurdili_hicazkar_4)
(scale :C, :kurdili_hicazkar_5)
(scale :C, :zirguleli_suznak)
(scale :C, :zirguleli_suznak_2)
(scale :C, :zirguleli_suznak_3)
(scale :C, :hicazkar)
(scale :C, :hicazkar_2)
(scale :C, :evcara)
(scale :C, :evcara_2)
(scale :C, :evcara_3)
(scale :C, :evcara_4)
(scale :C, :suzidil)
(scale :C, :suzidil_2)
(scale :C, :sedaraban)
(scale :C, :sedaraban_2)
(scale :C, :segah)
(scale :C, :segah_2)
(scale :C, :huzzam)
(scale :C, :huzzam_2)
(scale :C, :bayati_araban)
(scale :C, :acem_kurdi)
(scale :C, :sehnaz)
(scale :C, :sehnaz_2)
(scale :C, :sehnaz_3)
(scale :C, :sehnaz_4)
(scale :C, :saba)
(scale :C, :dugah)
(scale :C, :dugah_2)
(scale :C, :evic)
(scale :C, :evic_2)
(scale :C, :bestenigar)
(scale :C, :ferahnak)
(scale :C, :sevkefza)
(scale :C, :sevkefza_2)
(scale :C, :sevkefza_3)
(scale :C, :ferahfeza)
(scale :C, :ferahfeza_2)
(scale :C, :yegah)
(scale :C, :yegah_2)


```

</td>
<td class="even">

<!--- #tr -->
```ruby
# Sonic Pi supports a large range of scales:
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 



```
<!--- #end tr -->

</td>
</tr>
</table>

