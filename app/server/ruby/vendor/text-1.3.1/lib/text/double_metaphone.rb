# encoding: utf-8
#
# Ruby implementation of the Double Metaphone algorithm by Lawrence Philips,
# originally published in the June 2000 issue of C/C++ Users Journal.
#
# Based on Stephen Woodbridge's PHP version - http://swoodbridge.com/DoubleMetaPhone/
#
# Author: Tim Fletcher (mail@tfletcher.com)
#

module Text # :nodoc:
module Metaphone

  # Returns the primary and secondary double metaphone tokens
  # (the secondary will be nil if equal to the primary).
  def double_metaphone(str)
    primary, secondary, current = [], [], 0
    original, length, last = "#{str}     ".upcase, str.length, str.length - 1
    if /^GN|KN|PN|WR|PS$/ =~ original[0, 2]
      current += 1
    end
    if 'X' == original[0, 1]
      primary << :S
      secondary << :S
      current += 1
    end
    while primary.length < 4 || secondary.length < 4
      break if current > str.length
      a, b, c = double_metaphone_lookup(original, current, length, last)
      primary << a if a
      secondary << b if b
      current += c if c
    end
    primary, secondary = primary.join("")[0, 4], secondary.join("")[0, 4]
    return primary, (primary == secondary ? nil : secondary)
  end


  private

  def slavo_germanic?(str)
    /W|K|CZ|WITZ/ =~ str
  end

  def vowel?(str)
    /^A|E|I|O|U|Y$/ =~ str
  end

  def double_metaphone_lookup(str, pos, length, last)
    case str[pos, 1]
      when /^A|E|I|O|U|Y$/
        if 0 == pos
          return :A, :A, 1
        else
          return nil, nil, 1
        end
      when 'B'
        return :P, :P, ('B' == str[pos + 1, 1] ? 2 : 1)
      when 'Ç'
        return :S, :S, 1
      when 'C'
        if pos > 1 &&
          !vowel?(str[pos - 2, 1]) &&
          'ACH' == str[pos - 1, 3] &&
          str[pos + 2, 1] != 'I' && (
            str[pos + 2, 1] != 'E' ||
            str[pos - 2, 6] =~ /^(B|M)ACHER$/
          ) then
          return :K, :K, 2
        elsif 0 == pos && 'CAESAR' == str[pos, 6]
          return :S, :S, 2
        elsif 'CHIA' == str[pos, 4]
          return :K, :K, 2
        elsif 'CH' == str[pos, 2]
          if pos > 0 && 'CHAE' == str[pos, 4]
            return :K, :X, 2
          elsif 0 == pos && (
              ['HARAC', 'HARIS'].include?(str[pos + 1, 5]) ||
              ['HOR', 'HYM', 'HIA', 'HEM'].include?(str[pos + 1, 3])
            ) && str[0, 5] != 'CHORE' then
            return :K, :K, 2
          elsif ['VAN ','VON '].include?(str[0, 4]) ||
                'SCH' == str[0, 3] ||
                ['ORCHES','ARCHIT','ORCHID'].include?(str[pos - 2, 6]) ||
                ['T','S'].include?(str[pos + 2, 1]) || (
                  ((0 == pos) || ['A','O','U','E'].include?(str[pos - 1, 1])) &&
                  ['L','R','N','M','B','H','F','V','W',' '].include?(str[pos + 2, 1])
                ) then
            return :K, :K, 2
          elsif pos > 0
            return ('MC' == str[0, 2] ? 'K' : 'X'), 'K', 2
          else
            return :X, :X, 2
          end
        elsif 'CZ' == str[pos, 2] && 'WICZ' != str[pos - 2, 4]
          return :S, :X, 2
        elsif 'CIA' == str[pos + 1, 3]
          return :X, :X, 3
        elsif 'CC' == str[pos, 2] && !(1 == pos && 'M' == str[0, 1])
          if /^I|E|H$/ =~ str[pos + 2, 1] && 'HU' != str[pos + 2, 2]
            if (1 == pos && 'A' == str[pos - 1, 1]) ||
              /^UCCE(E|S)$/ =~ str[pos - 1, 5] then
              return :KS, :KS, 3
            else
              return :X, :X, 3
            end
          else
            return :K, :K, 2
          end
        elsif /^C(K|G|Q)$/ =~ str[pos, 2]
          return :K, :K, 2
        elsif /^C(I|E|Y)$/ =~ str[pos, 2]
          return :S, (/^CI(O|E|A)$/ =~ str[pos, 3] ? :X : :S), 2
        else
          if /^ (C|Q|G)$/ =~ str[pos + 1, 2]
            return :K, :K, 3
          else
            return :K, :K, (/^C|K|Q$/ =~ str[pos + 1, 1] && !(['CE','CI'].include?(str[pos + 1, 2])) ? 2 : 1)
          end
        end
      when 'D'
        if 'DG' == str[pos, 2]
          if /^I|E|Y$/ =~ str[pos + 2, 1]
            return :J, :J, 3
          else
            return :TK, :TK, 2
          end
        else
          return :T, :T, (/^D(T|D)$/ =~ str[pos, 2] ? 2 : 1)
        end
      when 'F'
        return :F, :F, ('F' == str[pos + 1, 1] ? 2 : 1)
      when 'G'
        if 'H' == str[pos + 1, 1]
          if pos > 0 && !vowel?(str[pos - 1, 1])
            return :K, :K, 2
          elsif 0 == pos
            if 'I' == str[pos + 2, 1]
              return :J, :J, 2
            else
              return :K, :K, 2
            end
          elsif (pos > 1 && /^B|H|D$/ =~ str[pos - 2, 1]) ||
                (pos > 2 && /^B|H|D$/ =~ str[pos - 3, 1]) ||
                (pos > 3 && /^B|H$/   =~ str[pos - 4, 1])
            return nil, nil, 2
          else
            if (pos > 2 && 'U' == str[pos - 1, 1] && /^C|G|L|R|T$/ =~ str[pos - 3, 1])
              return :F, :F, 2
            elsif pos > 0 && 'I' != str[pos - 1, 1]
              return :K, :K, 2
            else
              return nil, nil, 2
            end
          end
        elsif 'N' == str[pos + 1, 1]
          if 1 == pos && vowel?(str[0, 1]) && !slavo_germanic?(str)
            return :KN, :N, 2
          else
            if 'EY' != str[pos + 2, 2] && 'Y' != str[pos + 1, 1] && !slavo_germanic?(str)
              return :N, :KN, 2
            else
              return :KN, :KN, 2
            end
          end
        elsif 'LI' == str[pos + 1, 2] && !slavo_germanic?(str)
          return :KL, :L, 2
        elsif 0 == pos && ('Y' == str[pos + 1, 1] || /^(E(S|P|B|L|Y|I|R)|I(B|L|N|E))$/ =~ str[pos + 1, 2])
          return :K, :J, 2
        elsif (('ER' == str[pos + 1, 2] || 'Y' == str[pos + 1, 1]) &&
               /^(D|R|M)ANGER$/ !~ str[0, 6] &&
               /^E|I$/ !~ str[pos - 1, 1] &&
               /^(R|O)GY$/ !~ str[pos - 1, 3])
          return :K, :J, 2
        elsif /^E|I|Y$/ =~ str[pos + 1, 1] || /^(A|O)GGI$/ =~ str[pos - 1, 4]
          if (/^V(A|O)N $/ =~ str[0, 4] || 'SCH' == str[0, 3]) || 'ET' == str[pos + 1, 2]
            return :K, :K, 2
          else
            if 'IER ' == str[pos + 1, 4]
              return :J, :J, 2
            else
              return :J, :K, 2
            end
          end
        elsif 'G' == str[pos + 1, 1]
          return :K, :K, 2
        else
          return :K, :K, 1
        end
      when 'H'
        if (0 == pos || vowel?(str[pos - 1, 1])) && vowel?(str[pos + 1, 1])
          return :H, :H, 2
        else
          return nil, nil, 1
        end
      when 'J'
        if 'JOSE' == str[pos, 4] || 'SAN ' == str[0, 4]
          if (0 == pos && ' ' == str[pos + 4, 1]) || 'SAN ' == str[0, 4]
            return :H, :H, 1
          else
            return :J, :H, 1
          end
        else
          current = ('J' == str[pos + 1, 1] ? 2 : 1)

          if 0 == pos && 'JOSE' != str[pos, 4]
            return :J, :A, current
          else
            if vowel?(str[pos - 1, 1]) && !slavo_germanic?(str) && /^A|O$/ =~ str[pos + 1, 1]
              return :J, :H, current
            else
              if last == pos
                return :J, nil, current
              else
                if /^L|T|K|S|N|M|B|Z$/ !~ str[pos + 1, 1] && /^S|K|L$/ !~ str[pos - 1, 1]
                  return :J, :J, current
                else
                  return nil, nil, current
                end
              end
            end
          end
        end
      when 'K'
        return :K, :K, ('K' == str[pos + 1, 1] ? 2 : 1)
      when 'L'
        if 'L' == str[pos + 1, 1]
          if (((length - 3) == pos && /^(ILL(O|A)|ALLE)$/ =~ str[pos - 1, 4]) ||
              ((/^(A|O)S$/ =~ str[last - 1, 2] || /^A|O$/ =~ str[last, 1]) && 'ALLE' == str[pos - 1, 4]))
            return :L, nil, 2
          else
            return :L, :L, 2
          end
        else
          return :L, :L, 1
        end
      when 'M'
        if ('UMB' == str[pos - 1, 3] &&
            ((last - 1) == pos || 'ER' == str[pos + 2, 2])) || 'M' == str[pos + 1, 1]
          return :M, :M, 2
        else
          return :M, :M, 1
        end
      when 'N'
        return :N, :N, ('N' == str[pos + 1, 1] ? 2 : 1)
      when 'Ñ'
        return :N, :N, 1
      when 'P'
        if 'H' == str[pos + 1, 1]
          return :F, :F, 2
        else
          return :P, :P, (/^P|B$/ =~ str[pos + 1, 1] ? 2 : 1)
        end
      when 'Q'
        return :K, :K, ('Q' == str[pos + 1, 1] ? 2 : 1)
      when 'R'
        current = ('R' == str[pos + 1, 1] ? 2 : 1)

        if last == pos && !slavo_germanic?(str) && 'IE' == str[pos - 2, 2] && /^M(E|A)$/ !~ str[pos - 4, 2]
          return nil, :R, current
        else
          return :R, :R, current
        end
      when 'S'
        if /^(I|Y)SL$/ =~ str[pos - 1, 3]
          return nil, nil, 1
        elsif 0 == pos && 'SUGAR' == str[pos, 5]
          return :X, :S, 1
        elsif 'SH' == str[pos, 2]
          if /^H(EIM|OEK|OLM|OLZ)$/ =~ str[pos + 1, 4]
            return :S, :S, 2
          else
            return :X, :X, 2
          end
        elsif /^SI(O|A)$/ =~ str[pos, 3] || 'SIAN' == str[pos, 4]
          return :S, (slavo_germanic?(str) ? :S : :X), 3
        elsif (0 == pos && /^M|N|L|W$/ =~ str[pos + 1, 1]) || 'Z' == str[pos + 1, 1]
          return :S, :X, ('Z' == str[pos + 1, 1] ? 2 : 1)
        elsif 'SC' == str[pos, 2]
          if 'H' == str[pos + 2, 1]
            if /^OO|ER|EN|UY|ED|EM$/ =~ str[pos + 3, 2]
              return (/^E(R|N)$/ =~ str[pos + 3, 2] ? :X : :SK), :SK, 3
            else
              return :X, ((0 == pos && !vowel?(str[3, 1]) && ('W' != str[pos + 3, 1])) ? :S : :X), 3
            end
          elsif /^I|E|Y$/ =~ str[pos + 2, 1]
            return :S, :S, 3
          else
            return :SK, :SK, 3
          end
        else
          return (last == pos && /^(A|O)I$/ =~ str[pos - 2, 2] ? nil : 'S'), 'S', (/^S|Z$/ =~ str[pos + 1, 1] ? 2 : 1)
        end
      when 'T'
        if 'TION' == str[pos, 4]
          return :X, :X, 3
        elsif /^T(IA|CH)$/ =~ str[pos, 3]
          return :X, :X, 3
        elsif 'TH' == str[pos, 2] || 'TTH' == str[pos, 3]
          if /^(O|A)M$/ =~ str[pos + 2, 2] || /^V(A|O)N $/ =~ str[0, 4] || 'SCH' == str[0, 3]
            return :T, :T, 2
          else
            return 0, :T, 2
          end
        else
          return :T, :T, (/^T|D$/ =~ str[pos + 1, 1] ? 2 : 1)
        end
      when 'V'
        return :F, :F, ('V' == str[pos + 1, 1] ? 2 : 1)
      when 'W'
        if 'WR' == str[pos, 2]
          return :R, :R, 2
        end
        pri, sec = nil, nil

        if 0 == pos && (vowel?(str[pos + 1, 1]) || 'WH' == str[pos, 2])
          pri = :A
          sec = vowel?(str[pos + 1, 1]) ? :F : :A
        end

        if (last == pos && vowel?(str[pos - 1, 1])) || 'SCH' == str[0, 3] ||
            /^EWSKI|EWSKY|OWSKI|OWSKY$/ =~ str[pos - 1, 5]
          return pri, "#{sec}F".intern, 1
        elsif /^WI(C|T)Z$/ =~ str[pos, 4]
          return "#{pri}TS".intern, "#{sec}FX".intern, 4
        else
          return pri, sec, 1
        end
      when 'X'
        current = (/^C|X$/ =~ str[pos + 1, 1] ? 2 : 1)

        if !(last == pos && (/^(I|E)AU$/ =~ str[pos - 3, 3] || /^(A|O)U$/ =~ str[pos - 2, 2]))
          return :KS, :KS, current
        else
          return nil, nil, current
        end
      when 'Z'
        if 'H' == str[pos + 1, 1]
          return :J, :J, 2
        else
          current = ('Z' == str[pos + 1, 1] ? 2 : 1)

          if /^Z(O|I|A)$/ =~ str[pos + 1, 2] || (slavo_germanic?(str) && (pos > 0 && 'T' != str[pos - 1, 1]))
            return :S, :TS, current
          else
            return :S, :S, current
          end
        end
      else
        return nil, nil, 1
    end
  end # def double_metaphone_lookup

  extend self

end # module Metaphone
end # module Text
