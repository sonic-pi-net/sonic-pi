# The share link format

A Sonic Pi program, or a whole set of buffers, carried in a link: nothing is stored anywhere, the link *is* the
program. The app's Share menu writes these links, the app reads them back, and the site's live cards hand their code
to the app the same way. Links get pasted into chats, printed on worksheets and put in QR codes, so a link once
written has to read back forever. This page is the whole format, so anyone can read one without Sonic Pi's code.

## The link

```
https://sonic-pi.net/code.html#code=1A_VpAA
                                    │└──── the program: its bytes, base64url, no padding
                                    └───── the format: 1
```

or, for a QR code, the same bytes as decimal digits, which fit a QR code's numeric mode (a third more than its byte
mode):

```
https://sonic-pi.net/code.html#code=N10066414848
                                    ││└─── the program: its bytes as digits
                                    │└──── the format: 1
                                    └───── N: digits follow
```

The program is in the fragment (after `#`), which a browser never sends to a server: no server sees the code, logs
it, or can be asked for it.

The format is one character, a digit or a letter. A new format is a new character, and a reader keeps every format
it has known, so every link it could ever read, it still reads. A reader shown a format it does not know says the
link is from a newer Sonic Pi.

## Reading a format 1 link

1. **Bytes.** After `1`, base64url (RFC 4648 §5, `-` and `_`, no `=` padding). After `N1`, digits: every 17 digits
   are 7 bytes (a big-endian number); the last group, shorter, is as many bytes as its length says:

   | digits | 0 | 3 | 5 | 8 | 10 | 13 | 15 | 17 |
   |--------|---|---|---|---|----|----|----|----|
   | bytes  | 0 | 1 | 2 | 3 | 4  | 5  | 6  | 7  |

   A group whose number does not fit its bytes, or a tail of another length, is not a link.

2. **Inflate.** The bytes are raw DEFLATE (RFC 1951, no zlib or gzip header), compressed against a preset
   dictionary: format 1's 32,768 bytes, the whole of DEFLATE's window (`dictionary` in `app/src/share-table.js`,
   base64). With zlib, that is `inflateSetDictionary` on a raw inflate: in Python,
   `zlib.decompressobj(wbits=-15, zdict=dictionary)`.

3. **Unpack.** What comes out is a token stream: Sonic Pi's words as one, two or three bytes each, from format 1's
   vocabulary (`vocab` in `app/src/share-table.js`, 836 of them: the language's functions, its synths, fx, samples
   and their opts, Ruby's keywords, a few common phrases):

   | bytes                | text                                                        |
   |----------------------|-------------------------------------------------------------|
   | `80`–`DF`            | `vocab[b − 0x80]` (tokens 0–95)                            |
   | `F0`–`FF`            | `vocab[96 + b − 0xF0]` (tokens 96–111)                     |
   | `01 n`               | `vocab[112 + n]`                                            |
   | `02 hi lo`           | `vocab[368 + hi × 256 + lo]`                                |
   | `E0`–`EF`            | a newline, then `b − 0xE0` indents of two spaces           |
   | `03 b`               | the byte `b` itself (an escape for the bytes below)         |
   | any other byte       | itself                                                      |

   The bytes that are the text's own (the last two rows), taken together between tokens, are UTF-8.

4. **The text.** A buffer's code, or, when it starts `#-- Sonic Pi Set v1`, a set: native Sonic Pi's `.sonicpi`
   file (`app/src/set-bundle.js`, `app/gui/utils/setbundle.cpp`).

A reader in Python, from this page alone (the vectors below are its test):

```python
import base64, json, re, zlib

def load_table(path):   # app/src/share-table.js
    table = json.loads(re.search(r"export default (\{.*\});", open(path, encoding="utf-8").read(), re.S).group(1))
    return table["vocab"], base64.b64decode(table["dictionary"])

def from_digits(s):
    tail = [0, 3, 5, 8, 10, 13, 15, 17]   # k bytes -> digits
    return b"".join(int(s[i:i + 17]).to_bytes(tail.index(len(s[i:i + 17])), "big") for i in range(0, len(s), 17))

def unpack(buf, vocab):
    out, raw, i = [], bytearray(), 0
    def flush():
        if raw: out.append(raw.decode("utf-8")); raw.clear()
    while i < len(buf):
        b = buf[i]
        if b >= 0xF0:   flush(); out.append(vocab[96 + b - 0xF0]); i += 1
        elif b >= 0xE0: flush(); out.append("\n" + "  " * (b - 0xE0)); i += 1
        elif b >= 0x80: flush(); out.append(vocab[b - 0x80]); i += 1
        elif b == 0x01: flush(); out.append(vocab[112 + buf[i + 1]]); i += 2
        elif b == 0x02: flush(); out.append(vocab[368 + (buf[i + 1] << 8) + buf[i + 2]]); i += 3
        elif b == 0x03: raw.append(buf[i + 1]); i += 2
        else:           raw.append(b); i += 1
    flush()
    return "".join(out)

def decode(link, vocab, dictionary):
    code = link.split("#code=", 1)[-1]
    digits = code[0] == "N"
    assert code[1 if digits else 0] == "1", "not format 1"
    data = from_digits(code[2:]) if digits else base64.urlsafe_b64decode(code[1:] + "=" * (-len(code[1:]) % 4))
    inflate = zlib.decompressobj(wbits=-15, zdict=dictionary)
    return unpack(inflate.decompress(data) + inflate.flush(), vocab)
```

## Writing one

The app (`app/src/share.js`) packs the text greedily: at each point the longest token that matches, where a token
that starts or ends with a letter, digit or `_` does not start or end inside a word; a newline followed by an even
number of spaces, up to 30, as one `E0`–`EF` byte; bytes `00`–`08` and every byte of a non-ASCII character behind
`03`. Then DEFLATE at its best (fflate, level 9) against the dictionary. Any token stream that unpacks to the text is
as good: another packer, or another DEFLATE, may write other bytes for the same program, and they read back the same.

## The table

`app/src/share-table.js` holds format 1's vocabulary and dictionary, made by `scripts/build-share-table.mjs` and
never changed after: a link's bytes mean what they mean only with this table. The tests pin its SHA-256
(`app/test/share-link.test.mjs`), so a table rebuilt by mistake fails them. A better table is a new format.

- **The vocabulary** is every name the language has (from the docs' reference), ranked by what it saves (how often it
  appears, times its length less one) across Sonic Pi's own programs: the most useful 112 get one byte.
- **The dictionary** is the token stream's commonest pieces, chosen so DEFLATE can point back to them as if the
  program had said them already: COVER (Liao, Petri, Moffat and Wirth, 2016, the method behind zstd's dictionary
  trainer) over 4,526 programs: the examples, the quickstart cards, the tutorial, the reference's examples, the
  specs, 2,984 programs posted to in-thread.sonic-pi.net, the Mehackit course's, and sets made of them. Segments of
  32 bytes, scored by the 6-byte pieces the most programs share, each piece counted once; the best nearest the end,
  and nearest of all a set's own fixed text, which every set link has.

Judged on programs it was not made from (5-fold cross-validation), against the table it replaced (a vocabulary and
6 KB of common lines): the median link 105 characters after `#code=` rather than 135, a program's link a third of
its
length rather than two fifths, in-thread's programs 43% of theirs rather than 54%.

## Test vectors

Each link reads back as exactly this text (as JSON strings), in every Sonic Pi that reads format 1:

| text | link | digits |
|------|------|--------|
| `"play 60"` | `1A_VpAA` | `N10066414848` |
| `"live_loop :drums do\n  sample :bd_haus, amp: 2\n  sleep 0.5\nend\n"` | `1QypHIFdtQYsQAA` | `N11890530841325907309113600` |
| `"# héllo — ünïcode, 🎹 and a tab\there\nplay :e3\r\nsleep 1"` | `1m5oBOj4HOLYPGqNpYJ6iAOTuyQMS60F1ho4C8wfm-cx9zDuB2Q-4gzMxiRPUrgLNh6ca84KGagA` | |
| `"\u0001\u0002\u0003 x \u007f"` | `1Y2ZkZmJmBi7AqwcA` | `N1279786040948423740200800929536` |
| `"#-- Sonic Pi Set v1\n#-- meta {\"current\":2,\"names\":[\"x\"],\"zooms\":[5,2,2,2,2,2,2,2,2,2]}\n#-- buffer 0\nplay 1\n\n#-- buffer 2\n#-- ~#-- tricky\nplay 3\n\n"` | `1I9YdwBwOHusBWgbcHRGLsNoUr9UdoCEeZHvrQATojJvsSqCk8YMHAA` | |

Format 1's table: SHA-256 of `JSON.stringify({ version, vocab, dictionary })` is
`6dff3b3f31e0aaad2772c70c33883d2dd23e2ff1897eb3d3162d543f9af5e8e3`.
