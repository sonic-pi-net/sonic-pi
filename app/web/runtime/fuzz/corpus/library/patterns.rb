# regular expressions and pattern matching
m = /(?<word>\w+)-(\d+)(?:x)?/.match("alpha-42x beta-7")
puts m[0], m[:word], m.pre_match.size, m.post_match, m.begin(0), m.end(0), m.captures.inspect, m.names.inspect, m.to_a.size
puts "a1b22c333".scan(/[a-z]\d+/).inspect, "a,b;c".split(/[,;]/).inspect, "aaa".gsub(/a/, "a" => "b"), "x-y".sub(/-/, "\\\\"), "AbC" =~ /b/i
puts "line1\nline2" =~ /^line2$/, "tab\there".match?(/\s/), Regexp.escape("a.b*c"), Regexp.union("a", /b/).source, /a{2,3}?b+c*[^d]|e\b/.match("aabbe ").to_s
puts "éß".size, "éß".bytes.size, "日本語"[1], "日本語".reverse, "é" == "é", "x".encoding rescue puts "bytes"
case [1, [2, 3], { k: "v" }]
in [Integer => a, [b, *c], { k: String => v }] then puts a, b, c.inspect, v
end
case { name: "x", n: 5 }
in { name:, n: 1.. => n } if n > 2 then puts name, n
end
r = (5 in Integer | Float)
puts r
play 60 + m[2].to_i % 12
