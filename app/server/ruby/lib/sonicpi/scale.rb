#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative 'wrappingarray'

module SonicPi
  class Scale < WrappingArray
    # Ported from Overtone: https://github.com/overtone/overtone/blob/master/src/overtone/music/pitch.clj

    class InvalidScaleError < ArgumentError; end ;
    class InvalidDegreeError < ArgumentError; end ;

    # Most of makams consists of one "besli" and one "dortlu"
    # In addition they make 22 + 31 = 53 koma, which is one octave.
    # Since scales consists (54 / 9) * 2 = 12 steps, we need correction.
    # So, an error corrected koma should be 12.0 / 53 not 2.0 / 9.
    # This creates a little amount of dissonance for notes which are not octave of the base note.

    koma           = 12.0 / 53
    bakiyye        = 4 * koma
    kucuk_mucenneb = 5 * koma
    buyuk_mucenneb = 8 * koma
    tanini         = 9 * koma
    artik_ikili    = 12 * koma

    SCALE = lambda{
      ionian_sequence     = [2, 2, 1, 2, 2, 2, 1]
      hex_sequence        = [2, 2, 1, 2, 2, 3]
      pentatonic_sequence = [3, 2, 2, 3, 2]
      # Basic "dortlu"
      cargah_dortlusu        = [tanini, tanini, bakiyye]
      buselik_dortlusu       = [tanini, bakiyye, tanini]
      kurdi_dortlusu         = [bakiyye, tanini, tanini]
      rast_dortlusu          = [tanini, buyuk_mucenneb, kucuk_mucenneb]
      ussak_dortlusu         = [buyuk_mucenneb, kucuk_mucenneb, tanini]
      hicaz_dortlusu         = [kucuk_mucenneb, artik_ikili, kucuk_mucenneb]
      # Basic "besli"
      cargah_beslisi         = cargah_dortlusu + [tanini]
      buselik_beslisi        = buselik_dortlusu + [tanini]
      kurdi_beslisi          = kurdi_dortlusu + [tanini]
      rast_beslisi           = rast_dortlusu + [tanini]
      huseyni_beslisi        = [buyuk_mucenneb, kucuk_mucenneb, tanini, tanini]
      hicaz_beslisi          = hicaz_dortlusu + [tanini]
      # Other "dortlu" and "besli"
      saba_dortlusu          = [buyuk_mucenneb, kucuk_mucenneb, kucuk_mucenneb]
      segah_dortlusu         = [kucuk_mucenneb, tanini, buyuk_mucenneb]
      tam_segah_beslisi      = segah_dortlusu + [tanini]
      eksik_segah_beslisi    = segah_dortlusu + [kucuk_mucenneb]
      mustear_dortlusu       = [tanini, kucuk_mucenneb, buyuk_mucenneb]
      tam_mustear_beslisi    = mustear_dortlusu + [tanini]
      eksik_mustear_beslisi  = mustear_dortlusu + [kucuk_mucenneb]
      huzzam_beslisi         = [kucuk_mucenneb, tanini, kucuk_mucenneb, artik_ikili]
      nikriz_beslisi         = [tanini, kucuk_mucenneb, artik_ikili, kucuk_mucenneb]
      pencgah_beslisi        = [tanini, tanini, buyuk_mucenneb, kucuk_mucenneb]
      tam_ferahnak_beslisi   = [kucuk_mucenneb, tanini, tanini, buyuk_mucenneb]
      eksik_ferahnak_beslisi = [kucuk_mucenneb, tanini, tanini, bakiyye]
      nisabur_dortlusu       = [buyuk_mucenneb, kucuk_mucenneb, tanini]
      nisabur_beslisi        = nisabur_dortlusu + [bakiyye]
      {
           diatonic:           ionian_sequence,
           ionian:             ionian_sequence,
           major:              ionian_sequence,
           dorian:             ionian_sequence.rotate(1),
           phrygian:           ionian_sequence.rotate(2),
           lydian:             ionian_sequence.rotate(3),
           mixolydian:         ionian_sequence.rotate(4),
           aeolian:            ionian_sequence.rotate(5),
           minor:              ionian_sequence.rotate(5),
           locrian:            ionian_sequence.rotate(6),
           hex_major6:         hex_sequence,
           hex_dorian:         hex_sequence.rotate(1),
           hex_phrygian:       hex_sequence.rotate(2),
           hex_major7:         hex_sequence.rotate(3),
           hex_sus:            hex_sequence.rotate(4),
           hex_aeolian:        hex_sequence.rotate(5),
           minor_pentatonic:   pentatonic_sequence,
           yu:                 pentatonic_sequence,
           major_pentatonic:   pentatonic_sequence.rotate(1),
           gong:               pentatonic_sequence.rotate(1),
           egyptian:           pentatonic_sequence.rotate(2),
           shang:              pentatonic_sequence.rotate(2),
           jiao:               pentatonic_sequence.rotate(3),
           zhi:                pentatonic_sequence.rotate(4),
           ritusen:            pentatonic_sequence.rotate(4),
           whole_tone:         [2, 2, 2, 2, 2, 2],
           whole:              [2, 2, 2, 2, 2, 2],
           chromatic:          [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
           harmonic_minor:     [2, 1, 2, 2, 1, 3, 1],
           melodic_minor_asc:  [2, 1, 2, 2, 2, 2, 1],
           hungarian_minor:    [2, 1, 3, 1, 1, 3, 1],
           octatonic:          [2, 1, 2, 1, 2, 1, 2, 1],
           messiaen1:          [2, 2, 2, 2, 2, 2],
           messiaen2:          [1, 2, 1, 2, 1, 2, 1, 2],
           messiaen3:          [2, 1, 1, 2, 1, 1, 2, 1, 1],
           messiaen4:          [1, 1, 3, 1, 1, 1, 3, 1],
           messiaen5:          [1, 4, 1, 1, 4, 1],
           messiaen6:          [2, 2, 1, 1, 2, 2, 1, 1],
           messiaen7:          [1, 1, 1, 2, 1, 1, 1, 1, 2, 1],
           super_locrian:      [1, 2, 1, 2, 2, 2, 2],
           hirajoshi:          [2, 1, 4, 1, 4],
           kumoi:              [2, 1, 4, 2, 3],
           neapolitan_major:   [1, 2, 2, 2, 2, 2, 1],
           bartok:             [2, 2, 1, 2, 1, 2, 2],
           bhairav:            [1, 3, 1, 2, 1, 3, 1],
           locrian_major:      [2, 2, 1, 1, 2, 2, 2],
           ahirbhairav:        [1, 3, 1, 2, 2, 1, 2],
           enigmatic:          [1, 3, 2, 2, 2, 1, 1],
           neapolitan_minor:   [1, 2, 2, 2, 1, 3, 1],
           pelog:              [1, 2, 4, 1, 4],
           augmented2:         [1, 3, 1, 3, 1, 3],
           scriabin:           [1, 3, 3, 2, 3],
           harmonic_major:     [2, 2, 1, 2, 1, 3, 1],
           melodic_minor_desc: [2, 1, 2, 2, 1, 2, 2],
           romanian_minor:     [2, 1, 3, 1, 2, 1, 2],
           hindu:              [2, 2, 1, 2, 1, 2, 2],
           iwato:              [1, 4, 1, 4, 2],
           melodic_minor:      [2, 1, 2, 2, 2, 2, 1],
           diminished2:        [2, 1, 2, 1, 2, 1, 2, 1],
           marva:              [1, 3, 2, 1, 2, 2, 1],
           melodic_major:      [2, 2, 1, 2, 1, 2, 2],
           indian:             [4, 1, 2, 3, 2],
           spanish:            [1, 3, 1, 2, 1, 2, 2],
           prometheus:         [2, 2, 2, 5, 1],
           diminished:         [1, 2, 1, 2, 1, 2, 1, 2],
           todi:               [1, 2, 3, 1, 1, 3, 1],
           leading_whole:      [2, 2, 2, 2, 2, 1, 1],
           augmented:          [3, 1, 3, 1, 3, 1],
           purvi:              [1, 3, 2, 1, 1, 3, 1],
           chinese:            [4, 2, 1, 4, 1],
           lydian_minor:       [2, 2, 2, 1, 1, 2, 2],
           blues_major:        [2, 1, 1, 3, 2, 3],
           blues_minor:        [3, 2, 1, 1, 3, 2],
	   # Basic makams
	   cargah:             cargah_beslisi + cargah_dortlusu,
	   buselik:            buselik_beslisi + kurdi_dortlusu,
	   buselik_2:          buselik_beslisi + hicaz_dortlusu,
	   kurdi:              kurdi_dortlusu + buselik_beslisi,
	   rast:               rast_beslisi + rast_dortlusu,
	   acemli_rast:        rast_beslisi + buselik_dortlusu,
	   ussak:              ussak_dortlusu + buselik_beslisi,
	   bayati:             ussak_dortlusu + buselik_beslisi,
	   bayati_2:           ussak_dortlusu + buselik_beslisi + kurdi_dortlusu,
	   isfahan:            ussak_dortlusu + buselik_beslisi,
	   isfahan_2:          ussak_dortlusu + buselik_beslisi + kurdi_dortlusu,
	   hicaz_humayun:      hicaz_dortlusu + buselik_beslisi,
	   hicaz_humayun_2:    hicaz_dortlusu + buselik_beslisi + kurdi_dortlusu,
	   hicaz:              hicaz_dortlusu + rast_beslisi,
	   hicaz_2:            hicaz_dortlusu + rast_beslisi + buselik_dortlusu,
	   uzzal:              hicaz_beslisi + ussak_dortlusu,
	   uzzal_2:            hicaz_beslisi + ussak_dortlusu + buselik_beslisi,
	   zirguleli_hicaz:    hicaz_beslisi + hicaz_dortlusu,
	   zirguleli_hicaz_2:  hicaz_beslisi + hicaz_dortlusu + buselik_beslisi,
	   huseyni:            huseyni_beslisi + ussak_dortlusu,
	   huseyni_2:          huseyni_beslisi + ussak_dortlusu + buselik_beslisi,
	   muhayyer:           huseyni_beslisi + ussak_dortlusu,
	   gulizar:            huseyni_beslisi + ussak_dortlusu,
	   neva:               ussak_dortlusu + rast_beslisi,
	   neva_2:             ussak_dortlusu + rast_beslisi + buselik_dortlusu,
	   tahir:              ussak_dortlusu + rast_beslisi,
	   tahir_2:            ussak_dortlusu + rast_beslisi + buselik_dortlusu,
	   karcigar:           ussak_dortlusu + hicaz_beslisi,
	   suznak:             rast_beslisi + hicaz_dortlusu,
	   suznak_2:           rast_beslisi + hicaz_dortlusu + buselik_beslisi,
	   # Sedd Makams
	   mahur:              cargah_beslisi + cargah_dortlusu,
	   acem_asiran:        cargah_beslisi + cargah_dortlusu,
	   nihavend:           buselik_beslisi + kurdi_dortlusu,
	   nihavend_2:         buselik_beslisi + hicaz_dortlusu,
	   sultani_yegah:      buselik_beslisi + kurdi_dortlusu,
	   sultani_yegah_2:    buselik_beslisi + hicaz_dortlusu,
	   kurdili_hicazkar:   kurdi_dortlusu + buselik_beslisi,
	   kurdili_hicazkar_2: kurdi_dortlusu + hicaz_dortlusu + buselik_beslisi,
	   kurdili_hicazkar_3: kurdi_dortlusu + hicaz_dortlusu + hicaz_beslisi,
	   kurdili_hicazkar_4: kurdi_dortlusu + ussak_dortlusu + buselik_beslisi,
	   kurdili_hicazkar_5: kurdi_dortlusu + ussak_dortlusu + ussak_dortlusu,
	   zirguleli_suznak:   hicaz_beslisi + hicaz_dortlusu,
	   zirguleli_suznak_2: hicaz_beslisi + hicaz_dortlusu + buselik_beslisi,
	   zirguleli_suznak_3: hicaz_beslisi + hicaz_dortlusu + hicaz_beslisi,
	   hicazkar:           hicaz_beslisi + hicaz_dortlusu,
	   hicazkar_2:         hicaz_beslisi + hicaz_dortlusu + buselik_beslisi,
	   evcara:             hicaz_beslisi + hicaz_dortlusu,
	   evcara_2:           hicaz_beslisi + hicaz_dortlusu + mustear_dortlusu,
	   evcara_3:           hicaz_beslisi + hicaz_dortlusu + eksik_ferahnak_beslisi,
	   evcara_4:           hicaz_beslisi + hicaz_dortlusu + eksik_segah_beslisi,
	   suzidil:            hicaz_beslisi + hicaz_dortlusu,
	   suzidil_2:          hicaz_beslisi + hicaz_dortlusu + hicaz_beslisi + kurdi_dortlusu,
	   sedaraban:          hicaz_beslisi + hicaz_dortlusu,
	   sedaraban_2:        hicaz_beslisi + hicaz_dortlusu + hicaz_dortlusu + buselik_beslisi,
	   segah:              tam_segah_beslisi + hicaz_dortlusu, # There should be more variations of segah
	   segah_2:            [kucuk_mucenneb, tanini] + ussak_dortlusu + buselik_beslisi,
	   huzzam:             huzzam_beslisi + hicaz_dortlusu,
	   huzzam_2:           [kucuk_mucenneb, tanini] + hicaz_dortlusu + buselik_beslisi,
	   bayati_araban:      ussak_dortlusu + hicaz_beslisi + kurdi_dortlusu,
	   acem_kurdi:         kurdi_dortlusu + cargah_beslisi,
	   sehnaz:             hicaz_dortlusu + buselik_beslisi,
	   sehnaz_2:           hicaz_dortlusu + rast_beslisi,
	   sehnaz_3:           hicaz_beslisi + ussak_dortlusu,
	   sehnaz_4:           hicaz_beslisi + hicaz_dortlusu, # There should be more variations of sehnaz
	   saba:               [buyuk_mucenneb, kucuk_mucenneb] + hicaz_beslisi + hicaz_dortlusu,
	   dugah:              [buyuk_mucenneb, kucuk_mucenneb] + hicaz_beslisi + hicaz_dortlusu,
	   dugah_2:            hicaz_beslisi + hicaz_dortlusu,
	   evic:               segah_dortlusu, # There should be more variations of evic
	   evic_2:             eksik_segah_beslisi,
	   bestenigar:         segah_dortlusu + [kucuk_mucenneb] + hicaz_beslisi + hicaz_dortlusu,
	   ferahnak:           tam_ferahnak_beslisi + hicaz_dortlusu, # There should be more variations of ferahnak
	   sevkefza:           cargah_beslisi + cargah_dortlusu,
	   sevkefza_2:         cargah_beslisi + hicaz_beslisi + hicaz_dortlusu,
	   sevkefza_3:         nikriz_beslisi + hicaz_beslisi + hicaz_dortlusu,
	   ferahfeza:          buselik_beslisi + hicaz_dortlusu, # There should be more variations of ferahfeza
	   ferahfeza_2:        buselik_beslisi + ussak_dortlusu,
	   yegah:              rast_beslisi + buselik_dortlusu,
	   yegah_2:            rast_beslisi + ussak_dortlusu + rast_beslisi}}.call

    ROMAN_DIGITS = {
      'I' => 1,
      'V' => 5,
      'X' => 10,
      'L' => 50,
      'C' => 100,
      'D' => 500,
      'M' => 1000
    }

    def self.from_roman(numeral)
      return nil unless /^M{0,4}(CM|CD|D?C{0,4})(XC|XL|L?X{0,4})(IX|IV|V?I{0,4})$/i === numeral.to_s
      vals = numeral.to_s.upcase.split('').map {|c| ROMAN_DIGITS[c]}
      result = 0
      i = 0
      while i < vals.length
        if i < vals.length - 1 && vals[i + 1] > vals[i]
          result += vals[i + 1] - vals[i]
          i += 2
        else
          result += vals[i]
          i += 1
        end
      end
      result
    end

    def self.resolve_degree_index(degree)
      if degree.is_a? Numeric
        num = degree
      else
        begin
          num = Integer(degree.to_s)
        rescue ArgumentError
          num = from_roman degree
        end
      end
      if num.nil? || num <= 0
        raise InvalidDegreeError, "Invalid scale degree #{degree.inspect}, scale degree must be a valid number or roman numeral greater than 0"
      end
      num - 1
    end

    def self.resolve_degree(degree, tonic, scale)
      scale = Scale.resolve_scale(tonic, scale, 1)
      augmentation = 0
      if not degree.is_a? Numeric
        degree = degree.to_s.downcase
        if degree.start_with? 'p'
          augmentation = 0
          degree = degree[1..-1]
        elsif degree.start_with? 'aa'
          augmentation = 2
          degree = degree[2..-1]
        elsif degree.start_with? 'a'
          augmentation = 1
          degree = degree[1..-1]
        elsif degree.start_with? 'dd'
          augmentation = -2
          degree = degree[2..-1]
        elsif degree.start_with? 'd'
          augmentation = -1
          degree = degree[1..-1]
        end
      end
      octave, index = resolve_degree_index(degree).divmod (scale.notes.length - 1)
      scale.notes[index] + octave * 12 + augmentation
    end

    @@scale_cache = Hash.new {|h, k| h[k] = Hash.new {|h2, k2| h2[k2] = {} } }

    def self.resolve_scale(tonic, name, num_octaves)
      tonic = tonic.is_a?(String) ? tonic.to_sym : tonic
      name = name.is_a?(String) ? name.to_sym : name
      cached = @@scale_cache[tonic][name][num_octaves]
      return cached unless cached.nil?
      scale = Scale.new(tonic, name, num_octaves)
      @@scale_cache[tonic][name][num_octaves] = scale
      scale
    end

    attr_reader :name, :tonic, :num_octaves, :notes

    def initialize(tonic, name, num_octaves=1)
      name = name.to_sym
      intervals = SCALE[name]
      raise InvalidScaleError, "Unknown scale name: #{name.inspect}" unless intervals
      intervals = intervals * num_octaves
      current = Note.resolve_midi_note(tonic)
      res = [current]
      intervals.each do |i|
        current += i
        res << current
      end

      @name = name
      @tonic = tonic
      @num_octaves = num_octaves
      @notes = res
      super(res)
    end

    def to_s
      "#<SonicPi::Scale :#{Note.resolve_note_name(@tonic)} :#{@name} #{@notes}>"
    end

    def inspect
      to_s
    end
  end
end