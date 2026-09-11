#!/usr/bin/env python3
# Convert mdaPianoData.h (short pianoData[] = {...};) -> raw little-endian int16 .dat
import re, sys, struct

src = sys.argv[1]
dst = sys.argv[2]
text = open(src).read()
# grab everything between the first '{' and the matching final '};'
body = text[text.index('{') + 1 : text.rindex('}')]
nums = re.findall(r'-?\d+', body)
vals = [int(n) for n in nums]
# clamp/verify int16 range
for v in vals:
    if v < -32768 or v > 32767:
        raise SystemExit(f"value out of int16 range: {v}")
data = struct.pack('<%dh' % len(vals), *vals)
open(dst, 'wb').write(data)
print(f"count={len(vals)} bytes={len(data)}")
