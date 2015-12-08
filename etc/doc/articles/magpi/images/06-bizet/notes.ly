mel = \relative {
  \time 2/4
  \key d \minor
	 d'16 r16 r16 a'16 f'16 r16 a,16 r16
}
lyr = \lyricmode {
    D16 -16 -16 A16 F16 -16 A16 -16
}
<<
  \new Voice = melody \mel
  \new Lyrics  \lyr
>>