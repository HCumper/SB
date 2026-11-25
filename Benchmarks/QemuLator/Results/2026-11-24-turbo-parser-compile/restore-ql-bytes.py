"""Rebuild a QL-native parser509: each U+FFFD (EF BF BD) in the 5.09 text copy stands for one
QL byte lost in conversion. Take those bytes from the same text in the QL-native parser502_bas.
Usage: python restore-ql-bytes.py <parser509.bas> <parser502_bas> <output>"""
import re, sys, pathlib, hashlib

src = pathlib.Path(sys.argv[1]).read_bytes()
ql = pathlib.Path(sys.argv[2]).read_bytes()
out_path = pathlib.Path(sys.argv[3])
lost = b"\xef\xbf\xbd"
restored = []
for line in src.split(b"\n"):
    if lost not in line:
        restored.append(line)
        continue
    number, _, body = line.partition(b" ")
    pieces = body.split(lost)
    pattern = b"\n\\d+ " + b"(.)".join(re.escape(p) for p in pieces) + b"\n"
    matches = list(re.finditer(pattern, ql, re.S))
    if len(matches) != 1:
        raise SystemExit(f"line {number!r}: {len(matches)} matches in parser502")
    found = [matches[0].group(i + 1) for i in range(len(pieces) - 1)]
    rebuilt = pieces[0]
    for byte, piece in zip(found, pieces[1:]):
        rebuilt += byte + piece
    restored.append(number + b" " + rebuilt)
    print(number.decode(), [b.hex() for b in found])
data = b"\n".join(restored)
assert lost not in data
out_path.write_bytes(data)
print("wrote", out_path, len(data), "bytes, sha256", hashlib.sha256(data).hexdigest())
