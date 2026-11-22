"""Independent host references; does not invoke either compiler or runtime."""
from pathlib import Path
from fractions import Fraction
from collections import Counter
import argparse
import hashlib
import json
import re

parser = argparse.ArgumentParser()
parser.add_argument("--collection", type=Path, default=Path(__file__).resolve().parent / ("Programs" if (Path(__file__).resolve().parent / "Programs").exists() else "collection"))
parser.add_argument("--output", type=Path)
args = parser.parse_args()
suite = json.loads((args.collection / "suite.json").read_text())


def checksum(name, passes, source):
    if name == "integer_arithmetic":
        values = []
        for p in range(1, passes+1):
            # Add all increments at once; applying MOD in each iteration is equivalent.
            values.append((p + sum((i % 97)*3+1 for i in range(1, 1001))) % 8192)
        return sum(values) % 10000, "Closed-form reduction of accumulated increments"
    if name == "sieve":
        # Trial division, rather than the BASIC sieve.
        primes = [n for n in range(2,1001) if all(n % d for d in range(2,int(n**0.5)+1))]
        assert len(primes) == 168
        return passes * len(primes) % 10000, "Independent trial division"
    if name == "matrix_multiply":
        total = 0
        for p in range(1,passes+1):
            for r in range(8):
                for c in range(8):
                    value = sum(((r+k+p) % 7) * ((k+2*c+p) % 5) for k in range(8))
                    total += value * (1+(r+c) % 7)
        return total % 10000, "Direct mathematical dot products without mutable result arrays"
    if name == "string_operations":
        return sum(259 if p % 2 else 263 for p in range(1,passes+1)) % 10000, "Analytical endpoint characters after an even rotation"
    if name == "float_polynomial":
        total = sum(((Fraction(5,4)*Fraction(i,1000)-Fraction(3,4))*Fraction(i,1000)+Fraction(1,2))*Fraction(i,1000)+2 for i in range(1,1001))
        assert abs(total-2313) < Fraction(2,100)
        return passes * int(total+Fraction(1,2)) % 10000, f"Exact rational sum {total} within the BASIC tolerance"
    if name == "float_transcendentals":
        return 256 * passes % 10000, "SIN squared plus COS squared identity, with per-iteration guest tolerance checks"
    if name == "eight_queens":
        boards = []
        def search(columns, left, right, board):
            if columns == 255:
                boards.append(board)
                return
            free = 255 & ~(columns | left | right)
            while free:
                bit = free & -free
                free -= bit
                search(columns | bit, (left | bit) << 1, (right | bit) >> 1, board+[bit.bit_length()])
        search(0,0,0,[])
        assert len(boards) == 92
        histogram = Counter(b[0] for b in boards)
        assert dict(sorted(histogram.items())) == {1:4,2:8,3:16,4:18,5:18,6:16,7:8,8:4}
        value = sum(sum(c*r*r for r,c in enumerate(b,1)) for b in boards)
        value += sum(c*count for c,count in histogram.items()) + len(boards)
        return passes * value % 10000, "Bitboard enumeration and independently known 92-solution histogram"
    if name == "route_planner":
        dist = [[30000]*32 for _ in range(32)]
        for t in range(32):
            dist[t][t] = 0
            for v,w in (((t+1) % 32,3+t % 9),((t+7) % 32,10+t % 13)):
                dist[t][v] = dist[v][t] = w
        for via in range(32):
            for start in range(32):
                for end in range(32):
                    dist[start][end] = min(dist[start][end],dist[start][via]+dist[via][end])
        value = sum(row[c]*(1+c % 7) for row in dist for c in range(32))
        return passes * value % 10000, "Floyd-Warshall, independent of the BASIC Dijkstra implementation"
    if name == "concordance":
        corpus = re.findall(r'corpus\$\(\d+\) = "([^"]*)"',source)
        assert len(corpus) == 12
        words = Counter(re.findall('[a-z]+',' '.join(corpus).lower()))
        value = sum(count*i+sum(ord(c)*k for k,c in enumerate(word,1)) for i,(word,count) in enumerate(sorted(words.items()),1))
        return passes * value % 10000, "Regular-expression tokenizer, Counter and host sort over the source corpus"
    raise ValueError(name)


rows = []
for case in suite["benchmarks"]:
    path = args.collection / case["file"]
    source = path.read_text()
    passes = int(re.search(r'^\d+ passes_i = (\d+)$', source,re.M)[1])
    assert passes == case["passes"]
    actual, method = checksum(case["name"],passes,source)
    expected = case["expectedChecksum"]
    if actual != expected:
        raise ValueError(f'{case["name"]}: computed {actual}, manifest expects {expected}')
    rows.append(dict(benchmark=case["name"],passes=passes,checksum=actual,method=method,
                     sourceSha256=hashlib.sha256(path.read_bytes()).hexdigest().upper()))
    print(f'PASS {case["name"]}: {actual}')
if args.output:
    args.output.write_text(json.dumps(rows,indent=2)+"\n")
