"""The JS ROM's SuperBASIC has no $hex literals (Minerva and SMSQ/E do), so it loads those lines
as mistakes. Rewrite each $hex literal in code (not in strings or REMarks) as its decimal value,
reading 8-digit values with the top bit set as signed 32-bit, as SMSQ/E does."""
import re, sys, pathlib

src = pathlib.Path(sys.argv[1])
dst = pathlib.Path(sys.argv[2])
literal = re.compile(rb"(?<![A-Za-z0-9_$%])\$([0-9A-Fa-f]+)(?![0-9A-Za-z_])")

def decimal(match):
    digits = match.group(1)
    value = int(digits, 16)
    if len(digits) == 8 and value >= 0x80000000:
        value -= 1 << 32
    return str(value).encode("ascii")

def rewrite(line):
    out, i, quote = b"", 0, None
    code = b""
    while i < len(line):
        c = line[i:i + 1]
        if quote:
            out += c
            if c == quote:
                quote = None
            i += 1
        elif c in (b'"', b"'"):
            out += literal.sub(decimal, code) + c
            code, quote = b"", c
            i += 1
        elif re.match(rb"REM(ARK)?\b", line[i:], re.I) and re.search(rb"(^\d+\s*|:\s*)$", code + b""):
            out += literal.sub(decimal, code) + line[i:]
            return out
        else:
            code += c
            i += 1
    return out + literal.sub(decimal, code)

lines = src.read_bytes().split(b"\n")
changed = 0
result = []
for line in lines:
    new = rewrite(line)
    if new != line:
        changed += 1
        print(line.decode("latin-1"), "\n   ->", new.decode("latin-1"))
    result.append(new)
dst.write_bytes(b"\n".join(result))
print(changed, "lines changed")
